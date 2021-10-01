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

(define (find-in-group SIMFUN WA WB EPSILON TIGHTNESS CANDIDATES)
"
  SIMFUN is an function that, given two items, returns a similarity
  score for those items.  Similarities are assumed to be symmetric,
  that is, (SIMFUN a b) == (SIMFUN b a). Usually, the similarity is
  a floating point number, but in fact it can be anything that is
  comparable with greter-than.
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
				(define score (SIMFUN CANDIDATE MEMB))
				(if (< score MINSCORE) (set! failcnt (+ failcnt 1)))
				(<= failcnt maxfail)
			)
			INGRP))

	(define benchmark (SIMFUN WA WB))
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
(define sim (add-pair-stars sap))

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

; Get all of the pairs with similarities on them.
; Same thing as `(cog-get-atoms 'Similarity)`
(define all-sim-pairs (sim 'get-all-elts))
;
; Exlude self-similar pairs.
(define uniq-sims
	(filter (lambda (SIM) (not (equal? (gar SIM) (gdr SIM)))) all-sim-pairs))

; The precomputed scores are word-pair MI's. But what we really want
; are the common-MI's. We'll have to compute that. I guess we could
; have cached that ... but didn't.

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
   (if (< 0 x) (* (log x) ol2) (- (inf))))

(define trp (add-transpose-api sha))
(define tot-mmt (trp 'total-mmt-count))
(define ltot-mmt (log2 tot-mmt))
(define (marg-mmt WRD)
   (- (log2 (trp 'mmt-count WRD)) ltot-mmt))

(define (common-MI WA WB)
   (+ (cog-value-ref (sap 'pair-count WA WB) 0)
      (* 0.5 (+ (marg-mmt WA) (marg-mmt WB) ltot-mmt))))


;; Create a sorted list of ranked pairs.
;; We want to find the top-ranked word-pair.
(define (rank-pairs FUN)
	(sort uniq-sims
		(lambda (ATOM-A ATOM-B)
			(> (FUN ATOM-A) (FUN ATOM-B))))
)

(define sorted-pairs
	(rank-pairs (lambda (SIM) (common-MI (gar SIM) (gdr SIM)))))

;; What's the top-ranked pair?
(car sorted-pairs)

(define (sim-fun WA WB) (cog-value-ref (sap 'pair-count WA WB) 0))


========== !#
