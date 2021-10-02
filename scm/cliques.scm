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
  comparable with greater-than.
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

	; Remove WA, WB from the list of candidates
	(define clean-cand
		(filter (lambda (cand)
			(not (or (equal? cand WA) (equal? cand WB))))
		CANDIDATES))

	; Starting with the minimal clique of `(list WA WB)`, create
	; an ingroup by adding members to the ingroup if that candidate
	; has a score no less than `minscore` to at least `TIGHT` members
	; of the group.
	(fold
		(lambda (CAND INGRP)
			(if (accept INGRP CAND minscore (get-tight INGRP))
				(cons CAND INGRP)
				INGRP))
		(list WA WB)
		clean-cand)
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
(length all-sim-pairs)
;
; Exlude self-similar pairs.
(define uniq-sims
	(filter (lambda (SIM) (not (equal? (gar SIM) (gdr SIM)))) all-sim-pairs))
(length uniq-sims)

; The precomputed scores are word-pair MI's. But what we really want
; are the common-MI's. We'll have to compute that. I guess we could
; have cached that ... but didn't.

(define (mi-sim WA WB)
	(define fmi (sap 'pair-count WA WB))
	(if fmi (cog-value-ref fmi 0) -1e20))

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

(define trp (add-transpose-api sha))
(define tot-mmt (trp 'total-mmt-count))
(define ltot-mmt (log2 tot-mmt))
(define (marg-mmt WRD)
	(- (log2 (trp 'mmt-count WRD)) ltot-mmt))

(define (common-MI WA WB)
	(+ (mi-sim WA WB) (* 0.5 (+ (marg-mmt WA) (marg-mmt WB) ltot-mmt))))

;; Create a sorted list of ranked pairs.
;; We want to find the top-ranked word-pair.
(define (rank-pairs FUN)
	(sort uniq-sims
		(lambda (ATOM-A ATOM-B)
			(> (FUN ATOM-A) (FUN ATOM-B))))
)

(define sorted-pairs
	(rank-pairs (lambda (SIM) (common-MI (gar SIM) (gdr SIM)))))

;; What are the top-ranked pairs?
(take sorted-pairs 10)

; Take a look at what we're dealing with.
(define (prt-sorted-pairs N)
	(for-each
		(lambda (PR)
			(format #t "common-MI= ~6F ~A <<-->> ~A\n"
				(common-MI (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(drop (take sorted-pairs (+ N 20)) N)))

(prt-sorted-pairs 0)

; Go for it
(define in-group (find-in-group common-MI (Word "is") (Word "was")
	0.5  0.7 (take words-with-sims 10)))

; Given a word, what is it's ranking?
(define (rank-of-word WRD)
	(list-index (lambda (RW) (equal? WRD RW)) words-with-sims))

; Graph
(define (in-group-csv FILENAME WA WB TIGHT)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT O_TRUNC)))
	(format csv "#\n# Initial 2-clique: ~A <<>> ~A\n#\n"
		(cog-name WA) (cog-name WB))
	(format csv "# Tightness = ~6F\n" TIGHT)
	(format csv "# This is using common-MI to determine in-group membership.\n")
	; (format csv "# This is using plain MI NOT common-MI\n")
	(format csv "#\n# idx\tepsilon\tsize\tmin-index\tmax-index\twords\n")
	(for-each
		(lambda (N)
			(define epsi (+ (* 0.1 N) -2))
			; (define epsi (* 0.1 N))
			(define in-group (find-in-group common-MI ;;;; mi-sim
				WA WB
				epsi TIGHT words-with-sims))
			(define max-idx
				(fold (lambda (W MAXI) (max MAXI (rank-of-word W))) -1000 in-group))
			(define min-idx
				(fold (lambda (W MINI) (min MINI (rank-of-word W))) 1000 in-group))

			(format csv "~D\t~6F\t~D\t~D\t~D\t{ "
				N epsi (length in-group) min-idx max-idx)
			(for-each (lambda (WRD) 
				(format csv "~A " (cog-name WRD))) in-group)
			(format csv "}\n")
			(force-output csv))
		(iota 100))
	(close csv))

(in-group-csv "/tmp/grp-is-was.dat" (Word "is") (Word "was") 0.7)
(in-group-csv "/tmp/grp-and-but.dat" (Word "and") (Word "but") 0.7)
(in-group-csv "/tmp/grp-in-of.dat" (Word "in") (Word "of") 0.7)
(in-group-csv "/tmp/grp-she-he.dat" (Word "she") (Word "he") 0.7)
(in-group-csv "/tmp/grp-comma-semi.dat" (Word ",") (Word ";") 0.7)
(in-group-csv "/tmp/grp-period-quest.dat" (Word ".") (Word "?") 0.7)
(in-group-csv "/tmp/grp-plus-minus.dat" (Word "+") (Word "—") 0.7)
(in-group-csv "/tmp/grp-roman-i-ii.dat" (Word "i") (Word "ii") 0.7)
(in-group-csv "/tmp/grp-It-There.dat" (Word "It") (Word "There") 0.7)

(in-group-csv "/tmp/grp-spoke-sat.dat" (Word "spoke") (Word "sat") 0.7)
(in-group-csv "/tmp/grp-look-smile.dat" (Word "look") (Word "smile") 0.7)
(in-group-csv "/tmp/grp-town-earth.dat" (Word "town") (Word "earth") 0.7)
(in-group-csv "/tmp/grp-door-house.dat" (Word "door") (Word "house") 0.7)

========== !#
