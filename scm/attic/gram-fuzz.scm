;
; gram-fuzz.scm
;
; Fuzzy wrappers for merging words into word-classes by grammatical
; similarity. Deprecated. Use the stuff in `agglo-rank.scm` instead.
; This file has been taken out of service, and lives in the attic now.
;
; Copyright (c) 2017, 2018, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` for an overview.
;
; This file implements the orthogonal/union/overlap type merging
; described in `gram-classification.scm`. The issue is that union
; merging works poorly.
;
; merge-project
; -------------
; The above merge methods are implemented in the `merge-project`
; function. It takes, as an argument, a fractional weight which is
; used when the disjunct isn't shared between both words. Setting
; the weight to zero gives overlap merging; setting it to one gives
; union merging. Setting it to fractional values provides a merge
; that is intermediate between the two: an overlap, plus a bit more,
; viz some of the union.  A second parameter serves as a cutoff, so
; that any observation counts below the cutoff are always merged.
;
; That is, the merger is given by the vector
;
;   v_merged = v_overlap + FRAC * (v_union - v_overlap)
;
; for those vector components in v_union that have been observed more
; than the minimum cutoff; else all of the small v_union components
; are merged.
;
; If v_a and v_b are both words, then the counts on v_a and v_b are
; adjusted to remove the counts that were added into v_merged. If one
; of the two is already a word-class, then the counts are simply moved
; from the word to the class.
;
; merge-discrim
; -------------
; Built on the merge-project method, the FRAC is a sigmoid function,
; ranging from 0.0 to 1.0, depending on the cosine between the vectors.
; The idea is that, if two words are already extremely similar, we may
; as well assume they really are in the same class, and so do a union
; merge. But if the words are only kind-of similar, but not a lot, then
; assume that the are both linear combinations of several word senses,
; and do only the minimal overlap merge, so as to avoid damaging the
; other senses.
;
; A reasonable strategy would seem to bee to take
;
;   FRAC = (cos - cos_min) / (1.0 - cos_min)
;
; where `cos_min` is the minimum cosine acceptable, for any kind of
; merging to be performed.
; Implemented in the `make-discrim` call.
;
; merge-disinfo
; -------------
; Like `merge-discrim` but using mutual information instead of cosines.
; Implemented in the `make-disinfo` call.
;
; Parameter choices
; -----------------
; Gut-sense intuition suggests that `merge-mifuzz` with a min acceptable
; MI of about 5 works best. The union fraction should be set to zero.
;
; Earlier work is summarized in `grammar-report/grammar-report.pdf`.
; Pretty much everything there used a union-merge fraction of 0.3,
; which, in reprospect, may have been much too large. Certainly,
; if the goal is to maximize entropy, then any value greater than zero
; will fail to do that.  Thus, the only reason to usae a union fraction
; greater than zero is if one suspects one is trapped in a local
; maximum, and needs to hop out.  Practical experience shows that this
; can be a bit risky, and easily corrupts clustering.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------

(define-public (make-merger STARS MPRED FRAC-FN NOISE MIN-CNT STORE FIN MRG-CON)
"
  make-merger STARS MPRED FRAC-FN NOISE MIN-CNT STORE FIN MRG-CON --
  Return object that implements the `merge-project` merge style
  (as described at the top of this file).

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  MPRED is a predicate that takes two rows in STARS (two Atoms that are
  left-elements, i.e. row-indicators, in STARS) and returns #t/#f i.e.
  a yes/no value as to whether the corresponding rows in STARS should
  be merged or not.

  FRAC-FUN is a function that takes two rows in STARS and returns a
  number between 0.0 and 1.0 indicating what fraction of a row to merge,
  when the corresponding matrix element in the other row is null.

  NOISE is the smallest observation count, below which counts will
  not be divided up, when the merge is performed. (All of the count
  will be merged, when it is less than NOISE)

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a row is allowed to have, to even be considered for
  merging.

  STORE is an extra function called, after the merge is to completed,
  and may be used to compute and store additional needed data that
  the algo here is unaware of. This include computation of supports,
  marginal MI and similar. It is called with an argument of the altered
  row.

  FIN is an extra function called, after the merge is to completed.
  It is called without an argument.

  MRG-CON is #t if Connectors should also be merged.  This requires
  that the STARS object have shapes on it.

  This object provides the following methods:

  'merge-predicate -- a wrapper around MPRED above.
  'merge-function -- the function that actually performs the merge.
  'discard-margin? -- Return #t if count on word is below MIN-CNT.
                      Uses the marginal counts for this decision.
                      Used by `trim-and-rank` to ignore this word.
                      (`trim-and-rank` prepares the list of words to
                      cluster.)
  'discard? --        Same as above, but  count is recomputed, instead
                      of being pulled from the margin. This is required
                      when a word has been merged, as then the margin
                      count will be stale (aka wrong, invalid). Used
                      by `greedy-grow` to ignore the stub of a word
                      after merging. That is, if all that remains in a
                      word after merging is some cruft with a count less
                      than MIN-CNT, it won't be further merged into
                      anything; it will be ignored.
"
	(define pss (add-support-api STARS))
	(define psu (add-support-compute STARS))

	(define (ACCUMULATE LLOBJ CLUST SECT WEIGHT)
		(accumulate-count CLUST SECT WEIGHT NOISE))

	; Return a WordClassNode that is the result of the merge.
	(define (merge WA WB)
		(define wa-is-cls (equal? (STARS 'cluster-type) (Type (cog-type WA))))
		(define wb-is-cls (equal? (STARS 'cluster-type) (Type (cog-type WB))))
		(define cls (STARS 'make-cluster WA WB))

		; Cluster - either create a new cluster, or add to an existing
		; one. Afterwards, need to recompute selected marginals. This
		; is required so that future similarity judgements work correctly.
		; The mergers altered the counts, and so the marginals on
		; those words and disjuncts are wrong. Specifically, they're
		; wrong only for WA, WB and cls. Here, we'll just recompute the
		; most basic support for WA, WB and cls and thier disjuncts.
		; The MI similarity also needs MM^T to be recomputed; the STORE
		; callback provides an opporunity to do that.
		; The results are stored, so that everything is on disk in
		; case of a restart.
		; Clobber first, since Sections were probably deleted.
		(cond
			((and wa-is-cls wb-is-cls)
				(merge-clusters STARS WA WB ACCUMULATE MRG-CON))
			((and (not wa-is-cls) (not wb-is-cls))
				(begin
					(start-cluster STARS cls WA WB FRAC-FN ACCUMULATE MRG-CON)
					(STORE cls)))
			(wa-is-cls
				(merge-into-cluster STARS WA WB FRAC-FN ACCUMULATE MRG-CON))
			(wb-is-cls
				(merge-into-cluster STARS WB WA FRAC-FN ACCUMULATE MRG-CON))
		)

		(STORE WA)
		(STORE WB)
		(FIN)
		cls
	)

	(define (is-small-margin? WORD)
		(< (pss 'right-count WORD) MIN-CNT))

	(define (is-small? WORD)
		(< (psu 'right-count WORD) MIN-CNT))

	; ------------------
	; Methods on this class.

	(lambda (message . args)
		(case message
			((merge-predicate)  (apply MPRED args))
			((merge-function)   (apply merge args))
			((discard-margin?)  (apply is-small-margin? args))
			((discard?)         (apply is-small? args))
			(else               (apply STARS (cons message args)))
		))
)

; ---------------------------------------------------------------
; Is it OK to merge WORD-A and WORD-B into a common vector?
;
; Return #t if the two should be merged, else return #f
; WORD-A might be a WordClassNode or a WordNode.
; WORD-B should be a WordNode.
;
; SIM-FUNC must be a function that takes two words (or word-classes)
; and returns the similarity between them.
;
; The CUTOFF is used to make the ok-to-merge decision; if the similarity
; is greater than CUTOFF, then this returns #t else it returns #f.
;
; The is effectively the same as saying
;    (< CUTOFF (SIM-FUNC WORD-A WORD-B))
; which is only a single trivial line of code ... but ...
; The below is a mass of print statements to show forward progress.
; The current infrastructure is sufficiently slow, that the prints are
; reassuring that the system is not hung.
;
(define (is-similar? SIM-FUNC CUTOFF WORD-A WORD-B)

	(define (report-progress)
		(let* (
				(start-time (get-internal-real-time))
				(sim (SIM-FUNC WORD-A WORD-B))
				(now (get-internal-real-time))
				(elapsed-time (* 1.0e-9 (- now start-time))))

			; Only print if its time-consuming.
			(if (< 2.0 elapsed-time)
				(format #t "Dist=~6F for ~A \"~A\" -- \"~A\" in ~5F secs\n"
					sim
					(if (eq? 'WordNode (cog-type WORD-A)) "word" "class")
					(cog-name WORD-A) (cog-name WORD-B)
					elapsed-time))

			; Print mergers.
			(if (< CUTOFF sim)
				(format #t "---------Bingo! Dist=~6F for ~A \"~A\" -- \"~A\"\n"
					sim
					(if (eq? 'WordNode (cog-type WORD-A)) "word" "class")
					(cog-name WORD-A) (cog-name WORD-B)
					))
			sim))

	; True, if similarity is larger than the cutoff.
	(< CUTOFF (report-progress))
)

; ---------------------------------------------------------------

(define (recompute-support LLOBJ WRD)
"
  recompute-support LLOBJ WRD - Recompute support marginals for WRD

  This recomputes the marginals for support and counts, which is
  what coine distance and Jaccard overlap need to do thier stuff.
  It is NOT enough for MI/MMT calculations!
"
	(define psu (add-support-compute LLOBJ))
	(store-atom (psu 'set-right-marginals WRD))
)

; ---------------------------------------------------------------

(define-public (make-fuzz STARS CUTOFF UNION-FRAC NOISE MIN-CNT)
"
  make-fuzz -- Return an object that can perform a cosine-distance
               projection-merge, with a fixed union-merge fraction.

  Uses the `merge-project` merge style. This implements a fixed
  linear interpolation between overlap-merge and union merge. Recall
  that the overlap-merge merges all disjuncts that the two parts have
  in common, while the union merge merges all disjuncts.

  Deprecated, because cosine doesn't work well, and the projection
  merge to a non-zero union fraction also gives poor results.

  Caution: this has been hacked to assume shapes (the #t flag is
  passed) and so this is not backwards compat with earlier behavior!

  See `make-merger` for the methods supplied by this object.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  UNION-FRAC is the fixed fraction of the union-set of the disjuncts
  that will be merged.

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pcos (add-similarity-compute STARS))
	(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-cosine CUTOFF WORD-A WORD-B))

	(define (fixed-frac WA WB) UNION-FRAC)
	(define (recomp W) (recompute-support STARS W))
	(define (noop) #f)

	(make-merger STARS mpred fixed-frac NOISE MIN-CNT recomp noop #t)
)

; ---------------------------------------------------------------

(define-public (make-discrim STARS CUTOFF NOISE MIN-CNT)
"
  make-discrim -- Return an object that can perform a \"discriminating\"
  merge. When a word is to be merged into a word class, the fraction
  to be merged will depend on the cosine angle between the two.
  Effectively, there is a sigmoid taper between the union-merge and
  the intersection-merge. The more similar they are, the more of a
  union merge; the less similar the more of an intersection merge.

  The idea is that if two words are highly similar, they really should
  be taken together. If they are only kind-of similar, then maybe one
  word has multiple senses, and we only want to merge the fraction that
  shares a common word-sense, and leave the other word-sense out of it.

  Uses the `merge-discrim` merge style; the merge fraction is a sigmoid
  taper.

  Deprecated, because cosine doesn't work well, and the projection
  merge to a non-zero union fraction also gives poor results.

  Caution: this has been hacked to assume shapes (the #t flag is
  passed) and so this is not backwards compat with earlier behavior!

  See `make-merger` for the methods supplied by this object.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pcos (add-similarity-compute STARS))
	(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-cosine CUTOFF WORD-A WORD-B))

	; The fractional amount to merge will be proportional
	; to the cosine between them. The more similar they are,
	; the more they are merged together.
	(define (cos-fraction WA WB)
		(define cosi (pcos 'right-cosine WA WB))
		(/ (- cosi CUTOFF)  (- 1.0 CUTOFF)))

	(define (recomp W) (recompute-support STARS W))
	(define (noop) #f)

	(make-merger STARS mpred cos-fraction NOISE MIN-CNT recomp noop #t)
)

; ---------------------------------------------------------------

(define-public (make-mifuzz STARS CUTOFF UNION-FRAC NOISE MIN-CNT)
"
  make-mifuzz -- Return an object that can perform a mutual-information
                 projection-merge, with a fixed union-merge fraction.

  Uses the `merge-project` merge style. This implements a fixed
  linear interpolation between overlap-merge and union merge. Recall
  that the overlap-merge merges all disjuncts that the two parts have
  in common, while the union merge merges all disjuncts.

  Deprecated, because the projection merge to a non-zero union
  fraction gives poor results.

  Caution: this has been hacked to assume shapes (the #t flag is
  passed) and so this is not backwards compat with earlier behavior!

  See `make-merger` for the methods supplied by this object.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable MI, for words to be considered
  mergable.

  UNION-FRAC is the fixed fraction of the union-set of the disjuncts
  that will be merged.

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pmi (add-symmetric-mi-compute STARS))

	(define (get-mi wa wb) (pmi 'mmt-fmi wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-mi CUTOFF WORD-A WORD-B))

	; The fraction to merge is fixed.
	(define (mi-fract WA WB) UNION-FRAC)
	(define (redo-mmt WRD) (recompute-mmt STARS WRD))
	(define (finish)
		(define ptc (add-transpose-compute STARS))
		(store-atom (ptc 'set-mmt-totals)))

	(make-merger pmi mpred mi-fract NOISE MIN-CNT redo-mmt finish #t)
)

; ---------------------------------------------------------------

(define-public (make-midisc STARS CUTOFF NOISE MIN-CNT)
"
  make-midisc -- Return an object that can perform a mutual-information
                 projection-merge, with a tapered union-merge fraction.

  Uses the `merge-project` merge style. This adds a very small amount
  of the union-merge to the overlap-merge.  Recall that the
  overlap-merge merges all disjuncts that the two parts have in
  common, while the union merge merges all disjuncts.

  The tapering uses a merge fraction of
      1/2**(max(MI(a,a), MI(b,b))-CUTOFF)
  where MI(a,a) and MI(b,b) is the self-mutual information of the two
  items a and b to be merged.  This merge fraction is chosen such that,
  very roughly, the MI between the cluster, and the remainder of a and b
  after merging will be less than CUTOFF ... roughly. The formula is
  an inexact guesstimate. This could be improved.

  Deprecated, because the projection merge to a non-zero union
  fraction gives poor results.

  Caution: this has been hacked to assume shapes (the #t flag is
  passed) and so this is not backwards compat with earlier behavior!

  See `make-merger` for the methods supplied by this object.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable MI, for words to be considered
  mergable.

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pss (add-support-api STARS))
	(define pmi (add-symmetric-mi-compute STARS))
	(define pti (add-transpose-api STARS))

	(define (get-mi wa wb) (pmi 'mmt-fmi wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-mi CUTOFF WORD-A WORD-B))

	(define total-mmt-count (pti 'total-mmt-count))
	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (* (log x) ol2))

	; The self-MI is just the same as (get-mi wrd wrd).
	; The below is faster than calling `get-mi`; it uses
	; cached values. Still, it would be better if we
	; stored a cached self-mi value.
	(define (get-self-mi wrd)
		(define len (pss 'right-length wrd))
		(define mmt (pti 'mmt-count wrd))
		(log2 (/ (* len len total-mmt-count) (* mmt mmt))))

	; The fraction to merge is a ballpark estimate that attempts
	; to make sure that the MI between the new cluster and the
	; excluded bits is less than the cutoff.
	(define (mi-fraction WA WB)
		(define mihi (max (get-self-mi WA) (get-self-mi WB)))
		(expt 2.0 (- CUTOFF mihi)))

	(define (redo-mmt WRD) (recompute-mmt STARS WRD))
	(define (finish)
		(define ptc (add-transpose-compute STARS))
		(store-atom (ptc 'set-mmt-totals)))

	(make-merger pmi mpred mi-fraction NOISE MIN-CNT redo-mmt finish #t)
)

; ---------------------------------------------------------------

(define-public (make-disinfo STARS CUTOFF NOISE MIN-CNT)
"
  make-disinfo -- Return an object that can perform a \"discriminating\"
                  merge, using MI for similarity.

  Deprecated. Based on diary results, this appears to give poor results.
  Suggest using either `make-mifuzz` with a zero or a very small union
  frac, or to use  `make-midisc`.

  Use `merge-project` style merging, with linear taper of the union-merge.
  This is the same as `merge-discrim` above, but using MI instead
  of cosine similarity.

  Caution: this has been hacked to assume shapes (the #t flag is
  passed) and so this is not backwards compat with earlier behavior!

  See `make-merger` for the methods supplied by this object.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable MI, for words to be considered
  mergable.

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pss (add-support-api STARS))
	(define pmi (add-symmetric-mi-compute STARS))
	(define pti (add-transpose-api STARS))

	(define (get-mi wa wb) (pmi 'mmt-fmi wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-mi CUTOFF WORD-A WORD-B))

	(define total-mmt-count (pti 'total-mmt-count))
	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (* (log x) ol2))

	; The self-MI is just the same as (get-mi wrd wrd).
	; The below is faster than calling `get-mi`; it uses
	; cached values. Still, it would be better if we
	; stored a cached self-mi value.
	(define (get-self-mi wrd)
		(define len (pss 'right-length wrd))
		(define mmt (pti 'mmt-count wrd))
		(log2 (/ (* len len total-mmt-count) (* mmt mmt))))

	; The fraction to merge is a linear ramp, starting at zero
	; at the cutoff, and ramping up to one when these are very
	; similar.
	(define (mi-fraction WA WB)
		(define milo (min (get-self-mi WA) (get-self-mi WB)))
		(define fmi (get-mi WA WB))
		(/ (- fmi CUTOFF) (- milo CUTOFF)))

	(define (redo-mmt WRD) (recompute-mmt STARS WRD))
	(define (finish)
		(define ptc (add-transpose-compute STARS))
		(store-atom (ptc 'set-mmt-totals)))

	(make-merger pmi mpred mi-fraction NOISE MIN-CNT redo-mmt finish #t)
)

; ---------------------------------------------------------------
; Example usage
; (None)
