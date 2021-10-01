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

(define (find-in-group SIMOBJ WA WB EPSILON TIGHTNESS WLIST)
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

	; Given the current ingroup INGRP and a list of candidates CANDLI,
	; find the first member of CANDLI that is accepted as a member of the
	; the clique (i.e. has a score no less than MINSCORE to at least
	; TIGHT members of the INGRP.)
	(define (nominate INGRP CANDLI MINSCORE TIGHT)
		(find
			(lambda (cand) (accept INGRP cand MINSCORE TIGHT))
			CANDLI))

	(define benchmark (SIMOBJ 'pair-count WA WB))
	(define minscore (- benchmark EPSILON))

	; Convert fractional TIGHTNESS to an integer.
	(define (get-tight INGRP)
		(define insz (length INGRP))
		(if (equal? 2 insz) 2
			(inexact->exact (round (* TIGHTNESS insz)))))

	(fold
		(lambda (INGRP CAND)
		)
		(list WA WB)
		WLIST)
)

; ---------------------------------------------------------------
; Example usage
;
; (define pca (make-pseudo-cset-api))
; (define pcs (add-pair-stars pca))
; (define sha (add-covering-sections pcs))
; (sha 'fetch-pairs)
; (sha 'explode-sections)
; (load-atoms-of-type 'Similarity)
; (define sap (add-similarity-api sha #f "shape-mi"))
