;
; similarity-batch.scm
;
; Bulk computation of similarity.
; Unlike `similarity.scm`, this is written in a fairly principled way,
; and perhaps might be eligible for inclusion into the main module.
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

; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sma (add-similarity-api pcs #f "foo"))
(define sim (add-similarity-compute pcs))

(define (sim-mi-and-jacc WA WB)

	(xxx 'set-pair-similarity simpr Flaot
	(define simpr (Similarity WA WB))
	(store-atom simpr)
)

Fuuuuu
(batch-similarity pcs #f id cutoff fun


; ---------------------------------------------------------------
; Example usage
;
; (define pca (make-pseudo-cset-api))
; (define psa (add-pair-stars pca))
; (define mio (add-symmetric-mi-compute psa))
; (is-info-similar? mio 4.0 (Word "he") (Word "she"))
