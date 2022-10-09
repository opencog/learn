;
; agglo-goe.scm
;
; Loop over all words, merging them into grammatical categories.
; Agglomerative clustering, using GOE similarity to determine
; membership.
;
; Copyright (c) 2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file manages the top-most loop for traversing over all words,
; and assigning them to grammatical clusters using GOE similarity.
; This file does not provide tools for judging similarity, nor does
; it provide the low-level merge code.  It only manages the top loop.
;
; Under construction.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
;
(define (add-goe-sim LLOBJ)
"
  add-goe-sim LLOBJ -- add wrapper that returns GOE similarity with the
  'get-count and 'pair-count methods. Specifically, these two methods
  will return floating point numbers (and NOT a FloatValue!)
"
	(define (get-ref PR IDX)
		; Expect FloatValue always. IDX=0 is the MI sims, and 1 is the RMI
		(cog-value-ref (LLOBJ 'get-count PR) IDX))

	(define (get-pair-count WA WB)
		(get-ref (Similarity WA WB) 0))

	(lambda (message . args)
		(case message
			((get-count)   (get-ref (car args) 0))
			((pair-count)  (apply get-pair-count args))
			(else          (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------

(define (goe-cluster LLOBJ NRANK
	#:key
		(QUORUM 0.7)
		(COMMONALITY 0.2)
		(NOISE 4)
)
"
  goe-cluster LLOBJ -- perform GOE-based clustering.

  NRANK is the number of words for which we have GOE similarities.

  Under construction.
"

	; Does the same thing:
	; (define gram-mi-api (add-similarity-api LLOBJ #f "shape-mi"))
	(define gram-mi-api (add-gram-mi-sim-api LLOBJ))
	(define goe-api (add-similarity-api gram-mi-api #f "goe"))
	(define goe-sim (add-goe-sim goe-api))

	(define e (make-elapsed-secs))

	; Log the parameters that were supplied.
	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(cog-set-value! *-log-anchor-* (Predicate "goe-quorum-comm-noise")
		(FloatValue QUORUM COMMONALITY NOISE))

	; Record the classes as they are created.
	(define log-class (make-class-logger LLOBJ))

	; Create the function that determines group membership.
	(define jaccard-select (make-jaccard-selector LLOBJ
		QUORUM COMMONALITY NOISE))

	; Create the function that performs the merge.
	(define merge-majority (make-merge-majority LLOBJ QUORUM NOISE #t))

	; Start by getting the ranked words.  Note that this may include
	; WordClass nodes as well as words.
	(define ranked-words (rank-words LLOBJ))

	; Words with GOE sims
	(define words-with-sims (take ranked-words NRANK))

	; ------------------------------
	; Main workhorse function
	(define (perform-merge N WA WB)
		(define e (make-elapsed-secs))
		(format #t "------ Start GOE merge ~D with seed pair `~A` and `~A`\n"
			N (cog-name WA) (cog-name WB))

		(define in-grp (jaccard-select WA WB words-with-sims))
		(format #t "In-group size=~A:" (length in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD))) in-grp)
		(format #t "\n")

		(define wclass (make-class-node LLOBJ in-grp))
		(merge-majority wclass in-grp)

		(format #t "------ Merged into `~A` in ~A secs\n"
			(cog-name wclass) (e))
	)

	; --------------------------------------------
	; Get the word-pairs sorted by GOE cosine.
	(define (get-sorted-goe-pairs)
		(define all-cosi (goe-api 'get-all-elts))
		(define uniq-cosi
			(remove (lambda (PR) (equal? (gar PR) (gdr PR))) all-cosi))
		(define (lessi A B)
			(> (cog-value-ref (gos 'get-count A) 0)
				(cog-value-ref (gos 'get-count B) 0)))
		; return the sort.
		(sort uniq-cosi lessi)
	)

	(define (prt-sorted-pairs LST)
		(define sap (add-gram-mi-sim-api LLOBJ))
		(define (mi-sim PR)
			(define miv (sap 'get-count PR))
			(if miv (cog-value-ref miv 0) -inf.0))

		(for-each
			(lambda (PR)
				(define gcos (goe-sim 'get-count PR))
				(format #t "goe-theta= ~6F cos= ~6F mi= ~6F (`~A`, `~A`)\n"
					(acos gcos) gcos
					(mi-sim PR)
					(cog-name (gar PR))
					(cog-name (gdr PR))))
			LST))

	(define (top-ranked-goe-pairs)
		(define sorted-pairs (get-sorted-goe-pairs))

		(format #t "------ Round ~A Next in line:\n"
			(get-merge-iteration LLOBJ))
		(prt-sorted-pairs (take sorted-pairs 12))

		; Return the list of sorted pairs.
		sorted-pairs
	)
)

; ---------------------------------------------------------------------
#! ========
;
; Example usage.

; Load disjuncts into RAM, and create the matching shapes.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

(fetch-atom (AnchorNode "data logger"))

; This should have been done much much earlier, and stored!
; It's needed for grammatical-MI similarity computations.
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

; Also, grammatical-MI similarities should have been computed and
; stored! Fetch these!
(define smi (add-similarity-api pcs #f "shape-mi"))
(smi 'fetch-pairs)

; Here's where the GOE simillarities are stored.
(define gos (add-similarity-api smi #f "goe"))
(gos 'pair-count (Word "she") (Word "he"))
(gos 'get-count (Similarity (Word "she") (Word "he")))

(define layer-one (cog-new-atomspace "layer one" (cog-atomspace)))
(cog-set-atomspace! layer-one)

(goe-merge pcs)

==== !#