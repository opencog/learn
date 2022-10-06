;
; similarity.scm
;
; Generic functions for working with similarities between words.
; Provide lists of top-ranked words and word-pairs.
;
; Copyright (c) 2021,2022 Linas Vepstas
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------

(define-public (rank-words LLOBJ)
"
  rank-words LLOBJ -- Return a list of all words, ranked by count.
  If counts are equal, then rank by support. This may take half-a-
  minute to run.  Assumes that supports have been computed and are
  available.

  Here, a 'word' is any item appearing in the left-basis of LLOBJ.
  Thus, it might include word-classes, not just words.
"
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))

	(define wrds (LLOBJ 'left-basis))
	(sort wrds
		(lambda (ATOM-A ATOM-B)
			(define na (nobs ATOM-A))
			(define nb (nobs ATOM-B))
			(if (equal? na nb)
				(> (nsup ATOM-A) (nsup ATOM-B))
				(> na nb))))
)

; ---------------------------------------------------------------

(define (recomp-all-sim LLOBJ WLIST)
"
  recomp-all-sim LLOBJ WLIST - Recompute all existing similarities for
  all words in WLIST. The recomputation is unconditional.

  For each word in WLIST, recompute *all* existing similarities between
  that word and any other word that it already has similarities to. No
  new pairings are created.
"
	(define e (make-elapsed-secs))
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define sms (add-pair-stars sap))
	(define compute-sim (make-gram-mi-simmer LLOBJ))

	(define (recomp-one WX LIGNORE)
		; Loop over all pairs, except the ones we've done already.
		; (as otherwise, each similarity pair gets computed twice)
		(define todo-list (atoms-subtract (sms 'left-duals WX) LIGNORE))
		(compute-sim WX WX) ; Always compute self-similarity.
		(for-each (lambda (WRD)
				(when (not (nil? (sap 'get-pair WRD WX)))
					(compute-sim WRD WX)))
			todo-list))

	; Compute only the triangle of N(N-1)/2 similarities.
	(define (redo-list WX WLI WDONE)
		(recomp-one WX (cons WX WDONE))
		(when (not (nil? WLI))
			(redo-list (car WLI) (cdr WLI) (cons WX WDONE))))

	; all-words are all the words that have similarities.
	(define all-wrds (sms 'left-basis))

	; unaff are all the unaffected words.
	(define unaff (atoms-subtract all-wrds WLIST))

	; aff are the "affected words" - the intersection of provided
	; word list with the words that already have similarities.
	(define aff (atoms-subtract all-wrds unaff))

	(format #t "Will recompute sims for ~3D words (~A unaffected) out of ~3D\n"
		(length aff) (length unaff) (length all-wrds))

	(if (not (nil? aff)) (redo-list (car aff) (cdr aff) '()))

	(format #t "Recomputed sims for ~3D words out of ~3D in ~A secs\n"
		(length aff) (length all-wrds) (e))
)

; ---------------------------------------------------------------

(define-public (setup-initial-similarities LLOBJ
	SIM-API SIM-EXTENDER NRANK
)
"
  setup-initial-similarities LLOBJ SIM-API SIM-EXTENDER NRANK -- Compute
  a block matrix of similarities for the top-ranked words.

  All of the words appearing in the left-basis of LLOBJ are ranked
  by the total observation count on them. Then the top NRANK of them
  are taken, and the similarities between them are computed.

  If similarity values have already been recorded for any given
  word-pair, they will NOT bet recomputed.
"
	(define e (make-elapsed-secs))

	; Start by getting the ranked words.  Note that this may include
	; WordClass nodes as well as words.
	(define ranked-words (rank-words LLOBJ))
	(format #t "Done ranking words in ~A secs\n" (e))

	; Load similarity-pairs; pointless to recompute if we have them!
	(SIM-API 'fetch-pairs)

	; Create similarities for the initial set.
	(loop-upper-diagonal SIM-EXTENDER ranked-words 0 NRANK)
	(format #t "Done setting up similarity to ~A in ~A secs\n" NRANK (e))
)

; ---------------------------------------------------------------
