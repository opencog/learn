;
; similarity-graphs.scm
;
; Ad hoc scripts for greating assorted graphs and datasets comparing
; different similarity measures. This is for diary Part Three, Sept
; 2021.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))


; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sap (add-similarity-api pcs))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)
(sap 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

; ---------------------------------------

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))

	(define wrds (LLOBJ 'left-basis))
	(define ranked-words
		(sort wrds
			(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

	(define short-list (take ranked-words NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

(define wli (top-ranked pcs 650))

; ---------------------------------------
