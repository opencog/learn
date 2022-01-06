;
; agglo-rank.scm
;
; Loop over all words, merging them into grammatical categories.
; Agglomerative clustering.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file manages the top-most loop for traversing over all words,
; and assigning them to grammatical clusters. This file does not
; provide tools for judging similarity, nor does it provide the
; low-level merge code.  It only manages the top loop.
;
; This is basically the general concept of "agglomerative clustering",
; which is what is (in effect) implemented in this file.
;
; There are other styles of doing agglomerative clustering, implemented
; in `attic/agglo-loops.scm` and `attic/agglo-pairwise.scm`. They work,
; but are more complicated and don't work as well.  (I think they don't
; work as well, but this has not been double-checked experimentally.)
;
; Agglomerative clustering
; ------------------------
; This file implements a form of ranked clustering. It assumes that
; there is a pair-ranking function that will report the next pair to
; be merged together. That pair may be a  pair of words, a word and
; an existing cluster, or a pair of clusters.
;
; This is basic, the `cliques/democratic voting` thing is next, but its
; more complicated, so we do this first.
;
; Assumptions:
; * This assumes that shapes are being used. This is a fundamental
;   requirement for performing connector merges, so we are not going
;   to try to pretend to support anything else.
; * This assumes that support marginals have been computed, and have
;   been loaded into RAM. it will keep support marginals updated, as
;   words are merged.
;
; Notes:
; * make sure WordClassNodes and the MemberLinks are loaded
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; Where the simiarity scores will be stored
(define SIM-ID "shape-mi")

; ---------------------------------------------------------------

(define (rank-words LLOBJ)
"
  rank-words LLOBJ -- Return a list of all words, ranked by count.
  If counts are equal, then rank by support. This may take half-a-
  minute to run.  Assumes that supports have been computed and are
  available.
"
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

; ---------------------------------------------------------------

; XXX FIXME -- Maybe bug (I'm confused about this.) This function
; computes mmt-q aka Q described in the diary Part Four, and uses it
; as a constant offset. The problem is that it changes dramatically
; as clusters are created and counts are moved. This means that
; some of the stored similarity scores have the old Q on them, and
; some have the new Q, and they are no longer comparable (!?) right?
; The point is that the MI and the marginal logP in the common-MI
; definition are unchanged during merging, but mmt-q is. Thus, the
; changing mmt-q unfairly bounces around new and old similarity
; scores in some way that is not obvious and is history dependent. Hmm.
;
; Well, based on practical experience, the Q bounces dramatically at
; first, and then, after the first 10-20 steps, converges to a stable
; value (that is preserved to 3 decimal places.) So maybe it's not a
; problem...
(define (make-simmer LLOBJ)
"
  make-simmer LLOBJ -- return function that computes and stores MI's.

  This computes and stores both the MI and the Ranked-MI scores.
"
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define smi (add-symmetric-mi-compute LLOBJ))

	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (if (< 0 x) (* (log x) ol2) -inf.0))

	(define mmt-q (smi 'mmt-q))
(format #t ">>>> Make-simmer mmt-q = ~9F\n" mmt-q)

	; Compute and save both the fmi and the ranked-MI.
	; The marginal is sum_d P(w,d)P(*,d) / sum_d P(*,d)P(*,d)
	; The mmt-q is sum_d P(*,d)P(*,d) =
	;              sum_d N(*,d)N(*,d) / [ sum_d N(*,d) ]^2
	(define (compute-sim WA WB)
		(define fmi (smi 'mmt-fmi WA WB))
		(define mwa (smi 'mmt-marginal WA))
		(define mwb (smi 'mmt-marginal WB))
		(define qmi (+ fmi (* 0.5 (log2 (* mwa mwb)))))
		(define rmi (+ qmi mmt-q))

		; Print something, so user has something to look at.
		(if (< 4 fmi)
			(format #t "\tMI(`~A`, `~A`) = ~6F  rank-MI = ~6F\n"
				(cog-name WA) (cog-name WB) fmi rmi))
		(store-atom
			(sap 'set-pair-similarity
				(sap 'make-pair WA WB)
				(FloatValue fmi rmi qmi))))

	; Return the function that computes the MI for pairs.
	compute-sim
)

; ---------------------------------------------------------------

(define-public (compute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH)
"
  compute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH - compute MI.

  This will compute the MI similarity of words lying around a diagonal.
  The width of the diagonal is DEPTH. The diagonal is defined by the
  the ranked words. Computations start at START-RANK and proceed to
  DEPTH.  If the Similarity has already been recorded, it will not
  be recomputed.

  Think of a tri-diagonal matrix, but instead of three, its N-diagonal
  with N given by DEPTH.

  WORDLI is a list of word, presumed sorted by rank.

  Examples: If START-RANK is 0 and DEPTH is 200, then the 200x200
  block matrix of similarities will be computed. Since similarities
  are symmetric, this is a symmetric matrix, and so 200 x 201 / 2
  grand total similarities are computed.

  If START-RANK is 300 and DEPTH is 200, then computations start at
  the 300'th ranked word. This results in a total of 200x200
  similarities, as 200 rows are computed, out to 200 places away from
  the diagonal.

"
	; Create a new simmer each time, so we get the updated
	; mmt-q value for this session.
	(define do-compute-sim (make-simmer LLOBJ))

	; Don't recompute similarity, if we've already got it.
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (compute-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if (not miv) (do-compute-sim WA WB)))

	; Perform similarity computations for one row.
	(define (batch-simlist ITEM ITEM-LIST)
		(for-each
			(lambda (item) (compute-sim ITEM item))
			ITEM-LIST))

	; Take the word list and trim it down.
	(define nwords (length WORDLI))
	(define start (min START-RANK nwords))   ; avoid overflow
	(define depth (min DEPTH (- nwords start)))  ; avoid overflow
	(define row-range (take (drop WORDLI start) depth)) ; list of words to do
	(define (col-start off) (max 0 (- (+ start off) depth))) ;  column start
	(define (col-end off) (min (+ start off) depth)) ;  column end
	(define (col-range off)   ; reverse, so we go from diagonal outwards
		(reverse (take (drop WORDLI (col-start off)) (col-end off))))

	(define (do-one-row off)
		(define pone (+ 1 off))
		(batch-simlist (list-ref row-range off) (col-range pone)))

	(define rpt-one-row
		(make-progress-rpt do-one-row 10 #f
			"Diag: Finished ~D rows in ~D secs (~D/sec)\n"
			10))

	; Perform the similarity calculations, looping over the fat diagonal.
	(for-each (lambda (n) (rpt-one-row n)) (iota depth))
)

; ---------------------------------------------------------------

(define (get-ranked-pairs LLOBJ MI-CUTOFF)
"
  get-ranked-pairs LLOBJ MI-CUTOFF - get a ranked list of word pairs

  This returns a list of word-pairs sorted by rank-MI, from greatest
  to least.  All pairs in the list will have an MI of greater than
  MI-CUTOFF.  An MI-CUTOFF of 4 is recommended, maybe down to 2.
  Setting this too low will provide poor merge suggestions, in addition
  to making it take more time (because setting it low will admit more
  pairs, which take more time to sort.)
"
	; General setup of things we need
	(define sap (add-similarity-api LLOBJ #f SIM-ID))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	; Get all the similarities. We're going to just hack this, for
	; now, because we SimilarityLinks with both WordNode and WordClassNode
	; in them.
	(define all-sim-pairs (cog-get-atoms 'SimilarityLink))

	; Exclude pairs with a low MI, and also self-similar pairs.
	(define good-sims
		(filter
			(lambda (SIM)
				(define WA (gar SIM))
				(define WB (gdr SIM))
				(and (< MI-CUTOFF (mi-sim WA WB)) (not (equal? WA WB))))
			all-sim-pairs))

	;; Create a word-pair ranking function
	(define (rank-pairs PRLI FUN)
		(sort PRLI
			(lambda (ATOM-A ATOM-B)
				(> (FUN ATOM-A) (FUN ATOM-B)))))

	;; Now sort all of the available pairs according to ranked-MI
	(rank-pairs good-sims (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM))))
)

; ---------------------------------------------------------------

(define (prt-sorted-pairs LLOBJ LST START N)
"
  prt-sorted-pairs START NUM - print list of word pairs and similarities

  Handy-dandy debug utility.
"
	(define sap (add-similarity-api LLOBJ #f SIM-ID))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	(define len (length LST))
	(define start (min START len))   ; start is just START unless too large.
	(define num (min N (max 0 (- len START))))  ; num is just N unless too large

	(for-each
		(lambda (PR)
			(format #t "ranked-MI = ~6F MI = ~6F (`~A`, `~A`)\n"
				(ranked-mi-sim (gar PR) (gdr PR))
				(mi-sim (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(take (drop LST start) num))
)

; ---------------------------------------------------------------

(define (make-logger LLOBJ)
"
  make-logger LLOBJ -- crate logger to record assorted info in AtomSpace
"
	(define log-anchor (AnchorNode "data logger"))
	(define log-mmt-q (make-data-logger log-anchor (Predicate "mmt-q")))
	(define log-ranked-mi (make-data-logger log-anchor (Predicate "ranked-mi")))
	(define log-sparsity (make-data-logger log-anchor (Predicate "sparsity")))
	(define log-entropy (make-data-logger log-anchor (Predicate "entropy")))
	(define log-left-dim (make-data-logger log-anchor (Predicate "left dim")))
	(define log-right-dim (make-data-logger log-anchor (Predicate "right dim")))
	(define log-left-cnt (make-data-logger log-anchor (Predicate "left-count")))
	(define log-right-cnt (make-data-logger log-anchor (Predicate "right-count")))
	(define log-size (make-data-logger log-anchor (Predicate "total entries")))

	(define (log2 x) (if (< 0 x) (/ (log x) (log 2)) -inf.0))

	(define (get-sparsity)
		(define sup (add-support-api LLOBJ))
		(define nrows (sup 'left-dim))
		(define ncols (sup 'right-dim))
		(define tot (* nrows ncols))
		(define lsize (sup 'total-support-left)) ; equal to total-support-right
		(log2 (/ tot lsize)))

	(define (get-mmt-entropy)
		(define tsr (add-transpose-api LLOBJ))
		(define mmt-support (tsr 'total-mmt-support))
		(define mmt-count (tsr 'total-mmt-count))
		(- (log2 (/ mmt-count (* mmt-support mmt-support)))))

	(define (get-ranked-mi PAIR)
		(define sap (add-similarity-api LLOBJ #f SIM-ID))
		(define miv (sap 'get-count PAIR))
		(if miv (cog-value-ref miv 1) -inf.0))

	; Log some maybe-useful data...
	(lambda (PAIR)
		(log-mmt-q ((add-symmetric-mi-compute LLOBJ) 'mmt-q))
		(log-ranked-mi (get-ranked-mi top-pair))

		(log-sparsity (get-sparsity))
		(log-entropy (get-mmt-entropy))

		; The left and right count should be always equal,
		; and should never change.  This is a sanity check.
		(define sup (add-support-api LLOBJ))
		(log-left-count (sup 'total-count-left))
		(log-right-count (sup 'total-count-right))

		; left and right dimensions (number of rows, columns)
		(log-left-dim (sup 'left-dim))
		(log-right-dim (sup 'right-dim))

		; Total number of non-zero entries
		(log-size (sup 'total-support-left))
	)
)

; ---------------------------------------------------------------

(define (recomp-all-sim LLOBJ WX)
"
  Recompute all existing similarities to word WX
"
	(define e (make-elapsed-secs))
	(define compute-sim (make-simmer LLOBJ))
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define sms (add-pair-stars sap))
	(define wrd-list (sms 'left-duals WX))
	(define existing-list
		(filter (lambda (WRD) (not (nil? (sap 'get-pair WRD WX))))
			wrd-list))

	(for-each (lambda (WRD) (compute-sim WRD WX)) existing-list)

	(format #t "Recomputed ~3D sims for `~A` in ~A secs\n"
		(length existing-list) (cog-name WX) (e))
)

; ---------------------------------------------------------------
(define (setup-initial-similarities LLOBJ NRANK)
"
  Compute a block matrix of similarities between the top-ranked words.
"
	(define e (make-elapsed-secs))

	; Start by getting the ranked words.  Note that this may include
	; WordClass nodes as well as words.
	(define ranked-words (rank-words LLOBJ))
	(format #t "Done ranking words in ~A secs\n" (e))

	; Load similarity-pairs; pointless to recompute if we have them!
	((add-similarity-api LLOBJ #f SIM-ID) 'fetch-pairs)

	; Create similarities for the initial set.
	(compute-diag-mi-sims LLOBJ ranked-words 0 NRANK)
	(format #t "Done computing MI similarity in ~A secs\n" (e))
)

; ---------------------------------------------------------------

(define (main-loop LLOBJ MERGE-FUN NRANK LOOP-CNT)
"
  Unleash the fury. Inside of a loop, apply the MERGE-FUN to the
  top-ranked word-pair, for LOOP-CNT iterations. After each
  iteration, the similarities for a few more words are computed,
  so that, no matte the LOOP-CNT, there is a suitably deep set of
  word-pair similarities to rank and consider.
"
	; Get rid of all MI-similarity scores below this cutoff.
	(define MI-CUTOFF 4.0)

	; Offset on number of simularities to compute
	(define NSIM-OFFSET (length (cog-get-atoms 'WordClassNode)))

	; How many more similarities to compute each step.
	(define GRO-SIZE 2)

	; Range of similarities to compute.
	(define (diag-start N) (+ N NSIM-OFFSET))
	(define (diag-end N) (+ NRANK (* GRO-SIZE (+ N NSIM-OFFSET))))

	(define log-stuff (make-logger LLOBJ))

	(for-each
		(lambda (N)
			(define e (make-elapsed-secs))
			(define sorted-pairs (get-ranked-pairs LLOBJ MI-CUTOFF))
			(format #t "------ Round ~A Next in line:\n" (+ 1 (diag-start N)))
			(prt-sorted-pairs LLOBJ sorted-pairs 0 12)

			(define top-pair (car sorted-pairs))

			; Log some maybe-useful data...
			(log-stuff top-pair)

			; Do the actual merge
			(MERGE-FUN (diag-start N) (gar top-pair) (gdr top-pair))

			(format #t "------ Completed merge in ~A secs\n" (e))

			; Expand the size of the universe
			(define ranked-words (rank-words LLOBJ))

			; (compute-diag-mi-sims LLOBJ ranked-words (diag-start N) (diag-end N))
			(compute-diag-mi-sims LLOBJ ranked-words 0 (diag-end N))
			(format #t "------ Extended the universe in ~A secs\n" (e))
		)
		(iota LOOP-CNT))
)

; ---------------------------------------------------------------

(define (recompute-mmt LLOBJ WRD-LIST)
"
  recompute-mmt LLOBJ WRD-LIST - Recompute MMT for words in WRD-LIST

  This recomputes the marginals for support and counts for the words
  in the WRD-LIST, and also for the disjuncts attached to those words.
  In particular, this recomputes the N(*,d) which is needed by MM^T.
"
	; Gather together all of the DJ's for all the words in the list.
	; These will, in general, be heavily duplicated.
	(define dj-set (make-atom-set))

	; Gather together all affected words.
	(define wrd-set (make-atom-set))
	(for-each wrd-set WRD-LIST)

	; Add pair to the margin-sets.
	(define (pair-margins PAIR)
		(wrd-set (LLOBJ 'left-element PAIR))
		(dj-set (LLOBJ 'right-element PAIR)))

	(define (cross-margins PAIR)
		(for-each pair-margins (LLOBJ 'get-cross-sections PAIR)))

	; Add pair to the margin-sets, and also the matching sections
	; and cross-sections.
	(define (expand-margins PAIR)
		(dj-set (LLOBJ 'right-element PAIR))
		(if (equal? 'Section (cog-type PAIR))
			(cross-margins PAIR)
			(let ((sect (LLOBJ 'get-section PAIR)))
				(pair-margins sect)
				(cross-margins sect))))

	; Populate the margin sets.
	(for-each
		(lambda (WRD) (for-each expand-margins (LLOBJ 'right-stars WRD)))
		WRD-LIST)

	(define psu (add-support-compute LLOBJ))
	(define atc (add-transpose-compute LLOBJ))

	; This for-each loop accounts for 98% of the CPU time in typical cases.
	(for-each
		(lambda (DJ) (store-atom (psu 'set-left-marginals DJ)))
		(dj-set #f))

	(for-each
		(lambda (WRD) (store-atom (psu 'set-right-marginals WRD)))
		(wrd-set #f))

	(for-each
		(lambda (WRD) (store-atom (atc 'set-mmt-marginals WRD)))
		(wrd-set #f))
)

(define (recompute-mmt-final LLOBJ)
"
  recompute-mmt-final LLOBJ -- recompute grand totals for the MM^T case
"
	(define asc (add-support-compute LLOBJ))
	(define atc (add-transpose-compute LLOBJ))

	; Computing the 'set-left-totals takes about 97% of the total
	; time in this function, and about 8% of the grand-total time
	; (of merging words). Yet I suspect that it is not needed...
	; If the totals are not recomputed, then the matrix-summary-report
	; is borken.
	(store-atom (asc 'set-left-totals))   ;; is this needed? Its slow.
	(store-atom (asc 'set-right-totals))  ;; is this needed?
	(store-atom (atc 'set-mmt-totals))
)

; ---------------------------------------------------------------

(define-public (in-group-cluster LLOBJ QUORUM COMMONALITY NOISE NRANK LOOP-CNT)
"
  in-group-cluster LLOBJ QUORUM NRANK LOOP-CNT - perform clustering.

  Loops over a list of the most similar words, and unifies them into a
  cluster. Multiple words are selected at the same time to create a
  cluster.  The selection of words is done by selecting an 'in-group'
  of words that are all similar to one-another. The selection of
  ConnectorSeq's to be merged is done by majority voting to determine
  those ConnectorSeq's that the majority of the in-group have in common.
  The size of the in-group is adjusted to maximize commonality.

  There are three important parameters that determine the operation, and
  two more that control the overall loop.

  The QUORUM parameter is a floating point number, between 0.0 and 1.0
  that determines how many of the in-group members must share a
  ConnectorSeq for it to be considered to be held 'in common'.  (Think
  of a group of individuals having some trait in common.)

  Recommended values for QUORUM are in the 0.4 to 0.7 range. At the
  moment, 0.5 or 0.6 seem to work well.

  The algo begins by selecting the two words that are deemed to be the
  most similar to one-another, as reported by the `add-similarity` API.
  These two are the initial members of an 'in-group'. Other similar
  words are added as members, to create the largest possible in-group
  that is still exclusionary. The members of the in-group must have
  large pair-wise similarity.  It must also be exclusive, in that if the
  similarity threshold was reduced, membership would become explosively
  large.  (From experiments, it can be seen that as the similarity
  threshold is lowered, the group stays small, growing slowly, or not
  at all. Then there is an inflection point, where the group suddenly
  grows explosively large, gaining many members despite a small change
  in the similarity threshold. The in-group is selected to be the
  largest group below that inflection.)

  After the formation of the in-group, a poll is taken to see how many
  ConnectorSeq's the group has in common (as controlled by QUORUM,
  described above.) The 'commonality' is this frction. If the
  commonality is less than the COMMONALITY parameter, then the size of
  the in-group is reduced, by removing the least-similar member, and
  a poll is taken again. This continues until either the commonality is
  greater than the COMMONALITY parameter, or until the commonality
  drops, as compared to the previous group. (The commonality can drop,
  because in a smaller group, it can be harder to have a quorum.)

  Recommended values for COMMONALITY are in the 0.05 to 0.25 range.
  At the moment 0.2 seems to work well.

  NOISE is a noise-floor threshold. If a given section has a count equal
  or less than the NOISE parameter, it gets no vote in determining the
  commonality.  (Think of a group of individuals, one of whom has a
  minor quirky trait. One does not wish to have that minor trait to
  interfer with the group as a whole, thus it is ignored.)

  The NOISE parameter also plays a second role (perhaps it should be
  split out into a distinct parameter?) All sections with a count equal
  or below the noise floor are unconditionally merged into the cluster.

  Recommended value for NOISE is 0 to 4.

  All ConnectorSeq's that have been determined to be held in common by
  the in-group are then merged into the cluster. Note that the process
  of voting has both a narrowing and a broadening effect. Narrowing, in
  that once a group of similar words have been selected, not all
  ConSeq's are added to the cluster. The goal of this narrowing is to
  explcitly factor out distinct word-senses. Thus, a word like 'saw',
  which can be both noun and verb, will have it's noun-like ConSeq's
  merged with other nouns, while the verb-like ConSeq's are left behind,
  to be merged with other verbs.

  This algo also has a broadening effect: By majority vote, once a
  ConSeq is accepted into the cluster, all of those words will now share
  that ConSeq, even if some of them had not previously. The goal of this
  broadening is to generalize from particulars to generalities.

  There are two control parameters, NRANK and LOOP-COUNT.

  LOOP-COUNT is the number of times to run the loop, performing a
  select-and-merge step each time around.

  NRANK is the number of words to rank, before similarity computations
  are performed. The words are ranked according to the grand-total
  observation count on them, most frequent words first. Then the
  pair-wise similarities are computed for the top NRANK words (thus,
  a total of NRANK * (NRANK - 1) / 2 similarities are computed.) The
  goal here is to avoid having to compute simiarities between all words,
  which is computationaly infeasible. Experimentally, it is unlikely
  that frequent words are similar to infrequent ones, except in
  pathological cases. The word-pair with the highest similarity is then
  used to seed the in-group at the start of each loop.

  As the loop runs, additional similarities are computed each step. The
  number of words with similarity scores on them is kept at NRANK plus
  twice the number of loops that have been run. This provides for a
  deeper buffer, the rarer the words get. That is, there are many
  less-common words that are similar to one-another, and these have
  widly-varying rank; the size of the band of similarities must increase
  to capture these.

  Recommended value for NRANK is between 100 and 200.

  Status: mostly finished, fairly well-tested. Seems to work as
  designed.

  TODO: cannot merge two clusters together. (or add words to an existing
  cluster???) Code will throw an exception if this case is hit.
"
	(setup-initial-similarities LLOBJ NRANK)

	; The ranked MI similarity of two words
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	(define merge-majority (make-merge-majority LLOBJ QUORUM NOISE #t))

	; ------------------------------
	; Find the largest in-group that also shares more than a
	; fraction COMMONALITY of disjuncts among a QUORUM of members.
	; The returned group will always have at least two members,
	; the initial two proposed.
	(define (get-merg-grp WA WB CANDIDATES)
		(define initial-in-grp
			(optimal-in-group ranked-mi-sim WA WB CANDIDATES))
		(format #t "Initial in-group size=~D:" (length initial-in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
			initial-in-grp)
		(format #t "\n")

		; Tail-recursive trimmer; rejects large groups with little
		; commonality. Accepts first grouping with commonality above
		; the threshold COMMONALITY, or the last grouping before the
		; commonality decreases.
		(define (trim-group GRP prev-com prev-grp)
			(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
			(define comality (/ (car ovlp) (cadr ovlp)))
			(format #t "In-group size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
				(length GRP) (car ovlp) (cadr ovlp) (* comality 100))

			; In plain English:
			; If comality is above threshold, accept.
			; If comality dropped, compared to the previous,
			;    accept the previous.
			; If we are down to two, accept. Do this check last.
			; Else trim one word from the end, and try again.
			(cond
				((< COMMONALITY comality) GRP)
				((< comality prev-com) prev-grp)
				((= (length GRP) 2) GRP)
				(else (trim-group (drop-right GRP 1) comality GRP))))

		(trim-group initial-in-grp -1.0 initial-in-grp)
	)

	; ------------------------------
	; Main workhorse function
	(define (do-merge N WA WB)
		(define e (make-elapsed-secs))
		(format #t "------ Start merge ~D with seed pair `~A` and `~A`\n"
			(+ N 1) (cog-name WA) (cog-name WB))

		(define ranked-words (rank-words LLOBJ))
		; Approximation to number of words with sims.
		; This is overkill; NRANK is more than enough!
		(define n-to-take
			(min (length ranked-words) (+ NRANK (* 3 N))))
		(define words-with-sims (take ranked-words n-to-take))
		(define in-grp (get-merg-grp WA WB words-with-sims))
		(format #t "In-group size=~A:" (length in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD))) in-grp)
		(format #t "\n")

		(define wclass (make-class-node LLOBJ in-grp))
		(merge-majority wclass in-grp)

		(format #t "------ Merged into `~A` in ~A secs\n"
			(cog-name wclass) (e))

		; Recompute marginals after merge. Clobber first; else the
		; duals and stars are wrong, which ruins the support calculations.
		(LLOBJ 'clobber)
		(recompute-mmt LLOBJ (cons wclass in-grp))
		(recompute-mmt-final LLOBJ)

		(format #t "------ Recomputed MMT marginals in ~A secs\n" (e))

		; After merging, recompute similarities for the words
		; that were touched.
		(for-each (lambda (WRD) (recomp-all-sim LLOBJ WRD)) in-grp)
		(recomp-all-sim LLOBJ wclass)

		(format #t "------ Recomputed MI in ~A secs\n" (e))
	)

	; --------------------------------------------
	; Unleash the fury
	(main-loop LLOBJ do-merge NRANK LOOP-CNT)
)

; ---------------------------------------------------------------
#! ========
;
; Example usage

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

; If this hasn't been done, then it needs to be!
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

(define sap (add-similarity-api sha #f "shape-mi"))
(define asm (add-symmetric-mi-compute sha))

==== !#
