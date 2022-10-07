;
; mi-similarity.scm
;
; Compute grammatical-MI similarities between words. Provide lists of
; top-ranked word-pairs.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------

(define (add-gram-mi-sim-api LLOBJ)
"
	add-gram-mi-sim-api LLOBJ -- Return the correct similarity API.
"
	; The SIM-ID is the key under which the actual values are stored.
	(define SIM-ID "shape-mi")
	(add-similarity-api LLOBJ #f SIM-ID))

; ---------------------------------------------------------------

(define (make-gram-mi-simmer LLOBJ)
"
  make-gram-mi-simmer LLOBJ -- return function that computes and stores
  grammatical-MI's between words.

  This computes and stores both the grammatical-MI and the Ranked-MI scores.

  The computation is performed unconditionally; a new MI is computed,
  even if there is an existing one cached.
"
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define smi (add-symmetric-mi-compute LLOBJ))

	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (if (< 0 x) (* (log x) ol2) -inf.0))

	(define mmt-q (smi 'mmt-q))

	; Compute and save both the fmi and the ranked-MI.
	; The marginal is sum_d P(w,d)P(*,d) / sum_d P(*,d)P(*,d)
	; The mmt-q is sum_d P(*,d)P(*,d) =
	;              sum_d N(*,d)N(*,d) / [ sum_d N(*,d) ]^2
	(define (compute-sim WA WB)
		(define fmi (smi 'mmt-fmi WA WB))
		(define mwa (smi 'mmt-marginal WA))
		(define mwb (smi 'mmt-marginal WB))
		(define rmi (+ fmi (* 0.5 (log2 (* mwa mwb))) mmt-q))

		; Print something, so user has something to look at.
		(if (< 6 fmi)
			(format #t "\tMI(`~A`, `~A`) = ~6F  rank-MI = ~6F\n"
				(cog-name WA) (cog-name WB) fmi rmi))
		(store-atom
			(sap 'set-pair-similarity
				(sap 'make-pair WA WB)
				(FloatValue fmi rmi))))

	; Return the function that computes the MI for pairs.
	compute-sim
)

; ---------------------------------------------------------------

(define (get-mi-ranked-pairs LLOBJ MI-CUTOFF)
"
  get-mi-ranked-pairs LLOBJ MI-CUTOFF - get a ranked list of word pairs

  This returns a list of word-pairs sorted by rank-MI, from greatest
  to least.  All pairs in the list will have an MI of greater than
  MI-CUTOFF.  An MI-CUTOFF of 4 is recommended, maybe down to 2.
  Setting this too low will provide poor merge suggestions, in addition
  to making it take more time (because setting it low will admit more
  pairs, which take more time to sort.)
"
	; General setup of things we need
	(define sap (add-gram-mi-sim-api LLOBJ))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	; Get all the similarities. We're going to just hack this, for
	; now, because we SimilarityLinks with both WordNode and WordClassNode
	; in them.
	(define all-sim-pairs (cog-get-atoms 'SimilarityLink))

	; Exclude pairs with a low MI, and also self-similar pairs.
	(define good-sims
		(filter
			(lambda (SIM)
				(define WA (gar SIM))
				(define WB (gdr SIM))
				(and (< MI-CUTOFF (mi-sim WA WB)) (not (equal? WA WB))))
			all-sim-pairs))

	;; Create a word-pair ranking function
	(define (rank-pairs PRLI FUN)
		(sort PRLI
			(lambda (ATOM-A ATOM-B)
				(> (FUN ATOM-A) (FUN ATOM-B)))))

	;; Now sort all of the available pairs according to ranked-MI
	(rank-pairs good-sims (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM))))
)

; ---------------------------------------------------------------

(define (prt-mi-sorted-pairs LLOBJ LST START N)
"
  prt-mi-sorted-pairs LLOBJ PAIR-LST START NUM - print list of word pairs
  and thier grammatical-MI and ranked-MI similarities.

  Handy-dandy debug utility. Starting at START positions deep into the
  PAIR-LST, print the next NUM similarities.
"
	(define sap (add-gram-mi-sim-api LLOBJ))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	(define len (length LST))
	(define start (min START len))   ; start is just START unless too large.
	(define num (min N (max 0 (- len START))))  ; num is just N unless too large

	(for-each
		(lambda (PR)
			(format #t "ranked-MI = ~6F MI = ~6F (`~A`, `~A`)\n"
				(ranked-mi-sim (gar PR) (gdr PR))
				(mi-sim (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(take (drop LST start) num))
)

; ---------------------------------------------------------------
