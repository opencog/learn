;
; cliques.scm
;
; Obtain cliques of similar words. They can be used to form a cluster.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Given a word-pair with a high similarity score, expand that word-pair
; into a clique or an almost-clique, such that all similarity scores in
; that clique are no less than epsilon below the similarity score of the
; initial pair.  A clique is formed if *all* pair-scores meet this
; requirement. An in-group is formed, if the majority of the scores to
; other members in the in-group are above the epsilon threshold.

(use-modules (srfi srfi-1))

(define (find-in-group SIMOBJ WA WB EPSILON TIGHTNESS CANDIDATES)
"
  SIMOBJ is an object whose 'pair-count method returns the similarity
  score of the items in WLIST. Similarities are assumed to be symmetric.
  (that is, sim(a,b) == sim(b,a)).
"
	; Given the current ingroup INGRP and the CANDIDATE, return #t
	; if the candidate has a similarity score above MINSCORE to at
	; least TIGHT other members of the ingroup.
	(define (accept INGRP CANDIDATE MINSCORE TIGHT)

		; There can be at most `maxfail` bad scores
		(define maxfail (- (length INGRP) TIGHT))
		(define failcnt 0)
		(every
			(lambda (MEMB)
				(define score (SIMOBJ 'pair-count CANDIDATE MEMB))
				(if (< score MINSCORE) (set! failcnt (+ failcnt 1)))
				(<= failcnt maxfail)
			)
			INGRP))

	(define benchmark (SIMOBJ 'pair-count WA WB))
	(define minscore (- benchmark EPSILON))

	; Convert fractional TIGHTNESS to an integer.
	(define (get-tight INGRP)
		(define insz (length INGRP))
		(if (equal? 2 insz) 2
			(inexact->exact (round (* TIGHTNESS insz)))))

	; Starting with the minimal clique of `(list WA WB)`, create
	; an ingroup by adding members to the ingroup if that candidate
	; has a score no less than `minscore` to at least `TIGHT` members
	; of the group.
	(fold
		(lambda (INGRP CAND)
			(if (accept INGRP CAND minscore (get-tight INGRP))
				(cons CAND INGRP)
				INGRP))
		(list WA WB)
		CANDIDATES)
)

; ---------------------------------------------------------------
; Example usage
; Assumes that a suitable number of word similarities have been
; previously computed.
;
#! ===========
;; General setup of data
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(load-atoms-of-type 'Similarity)
(define sap (add-similarity-api sha #f "shape-mi"))

;; Return a list of all words, ranked by count.
;; If counts are equal, then rank by support.
(define (rank-words LLOBJ)
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

;; Create a list of candidates.
(define ranked-words (rank-words pcs))

; A short list (those that we have similarities for)
(define words-with-sims (take ranked-words 1200))

;; Create a sorted list of ranked pairs.
;; We want to find the top-ranked word-pair.

========== !#
