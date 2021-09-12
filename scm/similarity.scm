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
  batch-compute-similarity LLOBJ NUM-TOP FUN -- Similarity between words.

  Given the LLOBJ containing word-disjunct pairs, obtain the NUM-TOP
  top-ranked words, and then computer the similarity between those
  words, using FUN to perform that calculation. This is nothing more
  than the double-loop over the word-pairs.

  FUN is called as (FUN word-a word-b), and is responsible for both
  computations and for caching the results. It is assumed that FUN is
  symmetric, and so only the triangle of all possible word-pairs is
  looped over.

  The looping starts with the most frequent two words, and proceeds
  a row at a time, starting at the diagonal and moving away from the
  diagonal.

  It is assumed that the support marginals have been computed and are
  already in RAM. The support marginals are needed, in order to obtain
  the number of times a word has been observed.

  It is assumed that all word-disjunct pairs are in RAM already.
  (Both assumptions could be relaxed, at the cost of more CPU.)

  LLOBJ is assumed to have stars already (e.g. it might be filtered)
"
	(define sup (add-support-api LLOBJ))

	; Return a sorted list of the NTOP most frequent words.
	(define (top-ranked NTOP)

		; Optionally, fetch each marginal from RAM:
		; (fetch-atom (LLOBJ 'right-wildcard WRD))

		; nobs == number of observations
		(define (nobs WRD) (sup 'right-count WRD))

		(define ranked-words
			(sort! (LLOBJ 'left-basis)
				(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

		(define short-list (take ranked-words NTOP))
		(format #t "After sorting, kept ~A words out of ~A\n"
			(length short-list) (LLOBJ 'left-basis-size))
		short-list)

	; Do one row. Start at the diagonal of the matrix, and work back to
	; the edge. The diagonal is at the tail of the list, so we reverse
	; in order to do that.  N must be 2 or larger. N must be less than
	; the total length of the list.
	(define (do-row-n FULL-LST N)
		(define row-wrds (reverse (take FULL-LST N)))
		(define wrs (car row-wrds))
		(for-each (lambda (wrd) (FUN wrs wrd)) row-wrds))

	; Get the top-ranked words.
	(define word-list (top-ranked NUM-TOP))

	; Loop over rows of the matrix.
	(for-each
		(lambda (N) (do-row-n word-list N))
		(iota (- NUM-TOP 1) 1))
)

; ---------------------------------------------------------------
; Example usage
;
; (define pca (make-pseudo-cset-api))
; (define psa (add-pair-stars pca))
; (define mio (add-symmetric-mi-compute psa))
; (is-info-similar? mio 4.0 (Word "he") (Word "she"))
