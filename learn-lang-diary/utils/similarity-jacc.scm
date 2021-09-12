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
(define sma (add-similarity-api pcs #f "foo"))
(define sim (add-similarity-compute pcs))
(define sup (add-support-api pcs))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)



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

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

(define (overlap WA WB)
	(log2 (sim 'right-overlap WA WB)))

(define bover (batch-similarity pcs #f "overlap" -8.0 overlap))
(bover 'batch-compute 10)

overlap on diagonal is 1.0
cond-jac on diagonal is 0.0 ..?? because its the distance!


(sim 'right-cond-jacc

(define (sim-mi-and-jacc WA WB)

	(xxx 'set-pair-similarity simpr Flaot
	(define simpr (Similarity WA WB))
	(store-atom simpr)
)

Fuuuuu
(batch-similarity pcs #f id cutoff fun


; ---------------------------------------------------------------
