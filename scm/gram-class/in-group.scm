;
; in-group.scm
;
; Obtain an in-group of similar words. In-groups are those whose members
; have a lot in common with one-another. In-groups can be cliques, and
; more generally are almost-cliques.
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
(use-modules (ice-9 optargs)) ; for define*-public

(define-public (find-in-group SIMFUN WA WB
                              LOWER-BOUND EPSILON TIGHTNESS CANDIDATES)
"
  find-in-group SIMFUN WA WB LOWER-BOUND EPSILON TIGHTNESS CANDIDATES
  Return an in-group of closely related words.

  Given two words WA and WB with a high similarity score, find a clique
  an almost-clique (the in-group), such that all similarity scores in
  that in-group are greater than LOWER-BOUND and are also no less than
  EPSILON below the similarity score of the initial pair.  A clique is
  formed if *all* pair-scores meet this requirement. An in-group is
  formed, if more than a TIGHTNESS fraction of the scores to other
  members in the in-group are above the epsilon threshold. (A TIGHTNESS
  of 0.5 means that a simple majority of the in-group meets the
  requirement; a TIGHTNESS of 1.0 means the in-group is a clique.)

  Arguments:
  WA and WB seed the initial in-group.

  SIMFUN is an function that, given two items, returns a similarity
  score for those items.  Similarities are assumed to be symmetric,
  that is, (SIMFUN a b) == (SIMFUN b a). Usually, the similarity is
  a floating point number, but in fact it can be anything that is
  comparable with greater-than.

  At this time, all experimental results (and thus, recommended
  parameter values) have been done ONLY with ranked-MI. The code
  should still work for other SIMFUN's, but these have not been
  characterized.

  LOWER-BOUND is an absolute lower bound on the in-group similarities.
  All members of the in-group must have similarities that are greater
  than LOWER-BOUND.  Recommended value of 0.0 to 4.0.

  EPSILON is a relative lower bound on the in-group similarities. Most
  members of the in-group must have similarities that are within EPSILON
  of the initial pair.  Pairs that are within EPSILON are termed
  `similar enough`.  Recommended value of 0.5 to 8.0.

  TIGHTNESS is a number between 0 and 1, specifying the fraction of
  the in-group pairs that must be similar enough to one-another. A
  TIGHTNESS of 0.5 means that a majority of the pair-relations must
  be `similar enough`, while a TIGHTNESS of 1.0 means that all of
  them will be. Recommended value of 0.7. Experiments reveal that
  results are relatively insensitive to this value, ranging over 0.3
  to 1.0.

  CANDIDATES is a list of individuals to consider adding to the group.

  Experiments show that the size of the group at first grows slowly as
  a function of increasing EPSILON, followed by a very rapid increase
  after some threshold is passed. Obviously, EPSILON should be set
  below that threshold. Unfortunately, this threshold depends strongly
  on the initial pair, even when working within the same dataset.
"
	; Given the current ingroup INGRP and the CANDIDATE, return #t
	; if the candidate has a similarity score above MINSCORE to at
	; least TIGHT other members of the ingroup. Return #f if the
	; candidate has a score below LOWER-BOUND to any member of the
	; ingroup.
	(define (accept INGRP CANDIDATE MINSCORE TIGHT)

		; There can be at most `maxfail` bad scores
		(define maxfail (- (length INGRP) TIGHT))
		(define failcnt 0)
		(every
			(lambda (MEMB)
				(define score (SIMFUN CANDIDATE MEMB))
				(if (< score LOWER-BOUND) (set! failcnt (+ failcnt maxfail 999)))
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

(define*-public (optimal-in-group SIMFUN WA WB CANDIDATES
	#:key

		; The tightness of the almost-clique. This is the fraction of
		; the in-group members that have a similarity above the the
		; threshold. A tightness of 0.5 means the majority of the in-group
		; members are similar to one-another; a tightness of 1.0 means that
		; they all are (and so form a clique.) See `find-in-group` for
		; more.
		(tightness 0.7)

		; Size of steps (changes in the threshold) that will be taken.
		; The initial threshold is set to the similarity of WA and WB.
		; The threshold is then lowered by steps of epsi-step, until
		; stopping conditions are obtained.
		; For MI-similarity, 0.1 is a good step-size.
		(epsi-step 0.1)

		; The largest change in threshold to consider. The threshold
		; will never go below (similarity of WA and WB) minus this number.
		; For grammatical-MI, experiments suggest this should be 8.5.
		; Specifically, the grouping of roman numerals is still coherent,
		; despite individual pair-wise MI's being 8.5 apart.
		(max-epsi 8.5)

		; Lower bound on the similarity between members of the in-group.
		; All members of the in-group must have a pair-wise similarity
		; greater than this.
		; For MI-similarity, we want similarities greater than about
		; 2.0 or 4.0. Experiments show 1.0 works OK.
		(lower-bound 1.0)

		; The size of the in-group should not jump by more than this over
		; the width of moving window. If it does change by more than this,
		; then we assume that the 'knee' has been found, and the search
		; is halted. This enforces a max linear growth rate in the size of
		; the ingroup. The default value of 2.5 means the ingroup will not
		; add more than two members over the width of the window.
		(max-jump 2.5)

		; The size of the moving window. The size of the in-group at a
		; given threshold is compared to the size of the in-group at
		; (threshold + win-size.) If the change in the in-group size
		; is more than 'max-jump', then search is halted.
		; For grammatical-MI, a delta-MI of 1.0 seems like a reasonable
		; window.
		(win-size 1.0)

		; Hard-coded maximum size of the in-group.  We don't return
		; in-groups larger than this.
		(max-size 12)
	)
"
  optimal-in-group SIMFUN WA WB CANDIDATES

  Return an ingroup of closely related words. The initial members of the
  ingroup are WA and WB. Additional potential members are drawn from
  CANDIDATES if they are similar-enough to the current ingroup, as
  measured by the similarity function SIMFUN.

  This searches for the largest ingroup that is still exclusive. The
  search is performed by admitting individuals from CANDIDATES, one
  at a time, if they are judged similar-enough by SIMFUN. The membership
  requirements are slowly loosened (by dropping the lower bound of what
  is considered 'similar-enough'), until membership explodes. Then
  the lower bound is backed off a bit, just before the explosion.

  Experiments show that as membership requirements are loosened, there
  is a knee in the size of the group: the group size suddenly explodes.
  That is, as the similarity threshold is loosened, the size of the
  group grows slowly at first, and then, at some point, it takes off,
  growing rapidly (growing 'explosively'). This searches for the
  largest group below that inflection point.

  Arguments:
  WA and WB seed the initial in-group.

  SIMFUN is an function that, given two items, returns a similarity
  score for those items.  Similarities are assumed to be symmetric,
  that is, (SIMFUN a b) == (SIMFUN b a). Usually, the similarity is
  a floating point number, but in fact it can be anything that is
  comparable with greater-than. For similarities that are floating point
  numbers, the larger the value, the more similar they are.

  This function has been experimentally tested only for SIMFUN being
  ranked-MI!

  CANDIDATES is a list of individuals to consider adding to the group.
"
	; Loop and try to find the knee. This uses a moving window to
	; try to find the knee. We record all of the in-group sizes in
	; the list called `window`. The length of that list is `win-slots`
	; and is set to `win-size` divided by `epsi-step`. The list is
	; treated as a queue: pop the old size off one end, push the new
	; size onto the other end. The change in group size is just the
	; difference between the two ends of this queue.
	(define epsilon #f)
	(define nsteps (inexact->exact (round (/ max-epsi epsi-step))))
	(define win-slots (inexact->exact (round (/ win-size epsi-step))))
	(define window (make-list win-slots 2))
	(take-while
		(lambda (N)
			(set! epsilon (* N epsi-step))
			(define ing (find-in-group SIMFUN WB WA
				lower-bound epsilon tightness CANDIDATES))
			(define ingsz (length ing))
			(define prevsz (car window))
			; Slide the window over by one slot.
			(set! window (append (drop window 1) (list ingsz)))
			; Compare the two ends of the window.
			(define jump (- ingsz prevsz))
			(and (< jump max-jump) (< ingsz max-size))
		)
		(iota nsteps 1))

	; The above loop halts when we've gone too far. So back off by one
	; step, and return that. (Maybe we should back off two steps, to be
	; conservative?)

	(define in-grp
		(find-in-group SIMFUN WA WB
			lower-bound (- epsilon epsi-step) tightness CANDIDATES))

	; Reverse the list before sending it out. This way, the initial
	; WA and WB appear first in the list, instead of last. This
	; improves readability slightly, with trivial impact to performance.
	(reverse in-grp)
)

; -----
(define-public (optimal-mi-in-group SIMFUN WA WB CANDIDATES)
"
  optimal-mi-in-group - version of optimal-in-group with parameters
  that work for grammatical-MI similarity. See `optimal-in-group`
  for documentation.
"
	(optimal-in-group SIMFUN WA WB CANDIDATES

		; Tightness. Experiments show that the results are insensitive
		; of this, with values ranging from 0.3 to 1.0 giving identical
		; results in many cases.
		#:tightness 0.7

		; Size of steps (changes in similarity) that will be taken
		; in epsilon. For MI-similarity, 0.1 is a good step-size.
		#:epsi-step 0.1

		; The size of the in-group should not jump by more than this
		; over the moving window.
		#:max-jump 2.5

		; The window size
		#:win-size 1.0

		; The largest espsilon to consider.
		; The value of 8.5 comes from experiments: the grouping of roman
		; numerals were still coherent, despite being this far apart.
		#:max-epsi 8.5

		; Lower bound on the similarity between members of the in-group.
		; All members of the in-group must have a pair-wise similarity
		; greater than this. For grammatical-MI, this obviously has to
		; be between 0.0 and 4.0.
		#:lower-bound 1.0

		; Maximum size of the in-group.  We don't return in-groups
		; larger than this.
		#:max-size 12
	))

; ---------------------------------------------------------------
; Example usage.
; Assumes that a suitable number of word similarities have been
; previously computed. Assumes that shapes are enabled and being used.
;
#! ===========
;; General setup of data
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)
(define sap (add-similarity-api sha #f "shape-mi"))
(sap 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

(define sim (add-pair-stars sap))

;; Create a list of candidates.
(define e (make-elapsed-secs))
(define ranked-words (rank-words pcs))
(e)  ;; 19 seconds

; A short list (those that we have similarities for)
(define words-with-sims (take ranked-words 1200))

; Get all of the pairs with similarities on them.
; Same thing as `(cog-get-atoms 'Similarity)`
; Well, not quite ... similarity object fails to get
; the WordClass node similarities!
; Its also slower, because 'get-all-elts does a Pattern search.
(define e (make-elapsed-secs))
(define all-sim-pairs (sim 'get-all-elts))
(e)  ;; 32 seconds
(length all-sim-pairs) ;; 668610
;
; Exclude self-similar pairs.
(define uniq-sims
	(filter (lambda (SIM) (not (equal? (gar SIM) (gdr SIM)))) all-sim-pairs))
(length uniq-sims)

; Use the "ranked-MI" for ranking
(define (ranked-mi-sim WA WB)
	(define miv (sap 'pair-count WA WB))
	(if miv (cog-value-ref miv 1) -inf.0))

;; Discard all similarity pairs with low common-MI.
(define e (make-elapsed-secs))
(define hi-comi-sims
	(filter (lambda (SIM) (< 6.0 (ranked-MI (gar SIM) (gdr SIM))))
		 uniq-sims))
(e) ;; 32 seconds
(length hi-comi-sims)  ;; 1499 pairs in earlier versions

;; Create a sorted list of ranked pairs.
;; We want to find the top-ranked word-pairs
(define (rank-pairs FUN)
	(sort hi-comi-sims
		(lambda (ATOM-A ATOM-B)
			(> (FUN ATOM-A) (FUN ATOM-B))))
)

;; Now sort all of the available pairs. (Sorting all of them takes
;; 15 minutes. So sort only the high common-MI pairs.)
(define sorted-pairs
	(rank-pairs (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM)))))

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
(define in-group (find-in-group ranked-mi-sim (Word "is") (Word "was")
	0.5  0.7 (take words-with-sims 10)))

; Given a word, what is it's ranking?
(define (rank-of-word WRD)
	(list-index (lambda (RW) (equal? WRD RW)) words-with-sims))

; Create graphs for the diary. These appear in Diary Part Four.
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
