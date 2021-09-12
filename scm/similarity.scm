;
; similarity.scm
;
; Bulk computation of similarity.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; It is useful to be able to compare different kinds of similarity
; measures.  This means having the similarity measures for thousands
; of pairs at one's fingertips. Unfortunately, similarities can take a
; lot of CPU time to compute.  The code here just precomputes the
; similarities between the top most frequent N words, and stores them
; in the database for later fast retreival.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
(define-public (batch-compute-similarity LLOBJ NUM-TOP FUN)
"
  batch-compute-similarity LLOBJ NUM-TOP FUN

  LLOBJ is assumed to have stars already.
  NUM-TOP is number of words to keep.
"
	(define sup (add-support-api LLOBJ))

	(define (top-ranked NTOP)

		; nobs == number of observations
		(define (nobs WRD) (sup 'right-count WRD))

		(define ranked-words
			(sort! (LLOBJ 'left-basis)
				(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

		(define short-list (take ranked-words NTOP))
		(format #t "After sorting, kept ~A words out of ~A\n"
			(length short-list) (LLOBJ 'left-basis-size))
		short-list)

	()
)

; ---------------------------------------------------------------
; Example usage
;
; (define pca (make-pseudo-cset-api))
; (define psa (add-pair-stars pca))
; (define mio (add-symmetric-mi-compute psa))
; (is-info-similar? mio 4.0 (Word "he") (Word "she"))
