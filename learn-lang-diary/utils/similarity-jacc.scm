;
; similarity-jacc.scm
; September 2021 version
;
; Bulk computation of MI and Jaccard similarity. (See also the earlier
; `similarity.scm`, which did cosines).
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))


; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sim (add-similarity-compute pcs))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

; ---------------------------------------
; General exploration. Skip this section.

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))

	(define ranked-words
		(sort! (LLOBJ 'left-basis)
			(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

	(define short-list (take ranked-words NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

(define wli (top-ranked pcs 100))

; ---------------------------------------
; Compute overlaps.
; Want ability to restart.
(define overlap-done? (make-once-predicate))

; The 'right-overlap returns similarity: 1.0 means perfectly similar.
(define (overlap WA WB)
	(if (overlap-done? (Unordered WA WB))
		-100000
		(log2 (sim 'right-overlap WA WB))))

(define (prt-overlap WA WB)
	(define rv (overlap WA WB))
	(if (< -20 rv)
		(format #t ">>~A<< -- >>~A<< log2 overlap = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

; We're going to take -8.0 as the cutoff, because this was seen
; in run-3 experiments (documented in diary part-two) That is,
; bad intercluster values were around -8 so that seems like a
; reasonable place to halt comparison, for now.
(define bover (batch-similarity pcs #f "overlap" -8.0 prt-overlap))
(bover 'batch-compute 10)

(for-each store-atom (cog-get-atoms 'Similarity))

; ---------------------------------------

(define condjacc-done? (make-once-predicate))

; The 'right-cond-jacc returns distance: 0.0 means very close.
; So we subtract from 1.0 to get similarity.
(define (condjacc WA WB)
	(if (condjacc-done? (Unordered WA WB))
		-100000
		(log2 (- 1.0 (sim 'right-cond-jacc WA WB)))))

(define (prt-condjacc WA WB)
	(define rv (condjacc WA WB))
	(if (< -20 rv)
		(format #t ">>~A<< -- >>~A<< log2 condjacc = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

(define bcond (batch-similarity pcs #f "condjacc" -8.0 prt-condjacc))
(bcond 'batch-compute 10)

; ---------------------------------------

(define mi-done? (make-once-predicate))

(define cmi (add-symmetric-mi-compute pcs))

(define (mi WA WB)
	(if (mi-done? (Unordered WA WB))
		-100000
		(cmi 'mmt-fmi WA WB)))

(define (prt-mi WA WB)
	(define rv (mi WA WB))
	(if (< -20 rv)
		(format #t ">>~A<< -- >>~A<< mi = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

(define bami (batch-similarity pcs #f "mi" -8.0 prt-mi))
(bami 'batch-compute 10)

; ---------------------------------------------------------------
