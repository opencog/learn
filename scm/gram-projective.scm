;
; gram-projective.scm
;
; Merge words into word-classes by grammatical similarity.
; Projective merge strategies.
;
; Copyright (c) 2017, 2018, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` for an overview.
;
; This file implements the orthogonal/union/overlap type merging
; described in `gram-classification.scm`. See the `gram-optim.scm` file
; for the entropy-maximizing merge implementation.
;
; Orthogonal merging
; ------------------
; In this merge strategy, `w` is decomposed into `s` and `t` by
; orthogonal decomposition, up to a clamping constraint, so as to keep
; all counts non-negative. That is, start by taking `s` as the component
; of `w` that is parallel to `g`, and `t` as the orthogonal complement.
; In general, this will result in `t` having negative components; this
; is clearly not allowed in a probability space. Thus, those counts are
; clamped to zero, and the excess is transferred back to `s` so that the
; total `w = s + t` is preserved.
;
; Note the following properties of this algo:
; a) The combined vector `g_new` has exactly the same support as `g_old`.
;    That is, any disjuncts in `w` that are not in `g_old` are already
;    orthogonal. This may be undesirable, as it prevents the broadening
;    of the support of `g`, i.e. the learning of new, but compatible
;    grammatical usage. See discussion of "broadening" below.
;
; b) The process is not quite linear, as the final `s` is not actually
;    parallel to `g_old`.
;
;
; Union merging
; -------------
; Here, one decomposes `w` into components that are parallel and
; perpendicular to `g + w`, instead of `g` as above.  Otherwise, one
; proceeds as above.
;
; Note that the support of `g + w` is the union of the support of `g`
; and of `w`, whence the name.  This appears to provide a simple
; solution to the broadening problem, mentioned above.  Conversely, by
; taking the union of support, the new support may contain elements
; from `w` that belong to other word-senses, and do NOT belong to `g`
; (do not belong to the word sense associate with `g`).
;
; Initial cluster formation
; -------------------------
; The above described what to do to extend an existing grammatical class
; with a new candidate word.  It does not describe how to form the
; initial grammatical class, out of the merger of two words. Several
; strategies are possible. Given two words `u` and `v`, These are:
;
; * Simple sum: let `g=u+v`. That's it; nothing more.
; * Overlap and union merge, given below.
;
; Overlap merge
; -------------
; A formal (i.e. mathematically dense) description of overlap merging is
; given here. One wishes to compute the intersection of basis elements
; (the intersection of "disjuncts" aka "sections") of the two words, and
; then sum the counts only on this intersected set. Let
;
;   {e_a} = set of basis elements in v_a with non-zero coefficients
;   {e_b} = set of basis elements in v_b with non-zero coefficients
;   {e_overlap} = {e_a} set-intersection {e_b}
;   pi_overlap = unit on diagonal for each e in {e_overlap}
;              == projection matrix onto the subspace {e_overlap}
;   v_a^pi = pi_overlap . v_a == projection of v_a onto {e_overlap}
;   v_b^pi = pi_overlap . v_b == projection of v_b onto {e_overlap}
;
;   v_cluster = v_a^pi + v_b^pi
;   v_a^new = v_a - v_a^pi
;   v_b^new = v_b - v_b^pi
;
; The idea here is that the vector subspace {e_overlap} consists of
; those grammatical usages that are common for both words a and b,
; and thus hopefully correspond to how words a and b are used in a
; common sense. Thus v_cluster is the common word-sense, while v_a^new
; and v_b^new are everything else, everything left-over.  Note that
; v_a^new and v_b^new are orthogonal to v_cluster. Note that v_a^new
; and v_b^new are both exactly zero on {e_overlap} -- the subtraction
; wipes out those coefficients. Note that the total number of counts
; is preserved.  That is,
;
;   ||v_a|| + ||v_b|| = ||v_cluster|| + ||v_a^new|| + ||v_b^new||
;
; where ||v|| == ||v||_1 the l_1 norm aka count aka Manhattan-distance.
;
; If v_a and v_b have several word-senses in common, then so will
; v_cluster.  Since there is no a priori way to force v_a and v_b to
; encode only one common word sense, there needs to be some distinct
; mechanism to split v_cluster into multiple word senses, if that is
; needed.
;
; Union merging can be described using almost the same formulas, except
; that one takes
;
;   {e_union} = {e_a} set-union {e_b}
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
; where cos_min is the minimum cosine acceptable, for any kind of
; merging to be performed.
;
; merge-frac
; ----------
; Abstraction of the above two merge styles, using a callback function
; to obtain the merge fraction.
;
; Parameter choices
; -----------------
; Gut-sense intuition suggests these possible experiments:
;
; * Fuzz: use `merge-project` with hard-coded frac=0.3 and cosine
;   distance with min acceptable cosine=0.65
;
; * Discrim: use `merge-discrim` with min acceptable cosine = 0.5
;
; * Info: use `merge-project` with hard-coded frac=0.3 and information
;   distance with min acceptable MI=3
;
; Actual measurements (see `grammar-report/grammar-report.pdf`) indicate
; that these are actually rather good parameter choices; and surprisingly,
; the `merge-discrim` works better than `merge-project`.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

(define (merge-row-pairs LLOBJ COLS SING-A SING-B FRAC ZIPF)
"
  merge-row-pairs LLOBJ COLS FRAC ZIPF -- Merge two rows into a third row.

  COLS should be a list of three items (three pairs) in the same column
  of the matrix LLOBJ. The counts on the first two items in the list
  will be totalled and assigned to the third item; the counts on the
  first two items will be decremented by the amount each contributed.

  The first or the second item are allowed to be null. If not null,
  these are assumed to all be in the same column, in that they all
  return the same value for (LLOBJ 'right-element item))

  SING-A and SING-B should be #t or #f.

  The updated count is stored to the database.

  The prototypical use-case has PAIR-A and PAIR-B being two Sections
  of (word, disjunct) pairs, having the same disjunct but two different
  words. The goal is to merge the two words together into a single
  word-class.
"
	(define PAIR-A (first COLS))
	(define PAIR-B (second COLS))
	(define PR-ACC (third COLS))

	; The counts on each, or zero.
	(define a-cnt (if (null? PAIR-A) 0 (LLOBJ 'get-count PAIR-A)))
	(define b-cnt (if (null? PAIR-B) 0 (LLOBJ 'get-count PAIR-B)))

	; If the other count is zero, take only a FRAC of the count.
	; But only if we are merging in a word, not a word-class;
	; we never want to shrink the support of a word-class, here.
	(define wac (if
			(and SING-A (null? PAIR-B) (< ZIPF a-cnt))
			(* FRAC a-cnt) a-cnt))
	(define wbc (if
			(and SING-B (null? PAIR-A) (< ZIPF b-cnt))
			(* FRAC b-cnt) b-cnt))

	; Sum them.
	(define cnt (+ wac wbc))

	; Update the count on the section.
	; If the count is zero or less, delete the section.
	(define (update-section-count SECT CNT)
		(if (< 1.0e-10 CNT)
			(begin (set-count SECT CNT) (store-atom SECT))
			(begin (set-count SECT 0) (cog-delete! SECT))))

	; The cnt can be zero, if FRAC is zero.  Do nothing in this case.
	(if (< 1.0e-10 cnt)
		(begin

			; The summed counts
			(set-count PR-ACC cnt)
			(store-atom PR-ACC) ; save to the database.

			; Now subtract the counts from the words.
			; Left side is either a word or a word-class.
			; If its a word-class, we've already updated
			; the count.
			(if (and SING-A (not (null? PAIR-A)))
				(update-section-count PAIR-A (- a-cnt wac)))

			; Right side is WB and is always a WordNode
			; In principle, its always a singleton, so the test
			; is superfluous.
			(if (and SING-B (not (null? PAIR-B)))
				(update-section-count PAIR-B (- b-cnt wbc)))
		))

	; Return the pair of counts.
	(cons wac wbc)
)

; ---------------------------------------------------------------------

(define* (merge-frac LLOBJ FRAC-FN ZIPF WA WB CLS SING-A
	#:optional (MRG-CON #t))
"
  merge-frac LLOBJ FRAC-FN ZIPF WA WB CLS SING-A MRG-CON --
     merge the rows WA and WB of LLOBJ into a combined row.
     Returns the merged class.

  In the prototypical use case, each row corresponds to a WordNode,
  and the result of summing them results in a WordClassNode. Thus,
  by convention, it is assumed that the pairs are (word, disjunct)
  pairs, and LLOBJ was made by `make-gram-class-api` or by
  `add-shape-vec-api`. The code itself is generic, and may work on
  other kinds of LLOBJ's too.

  This assumes that storage is connected; the updated counts for the
  rows are written to storage.

  XXX At this time, this does not merge connectors within shapes.

  LLOBJ is used to access pairs.
  WA should be of `(LLOBJ 'left-type)` or be of CLASS-TYPE (that is,
     either a WordNode or a WordClassNode.)
  WB is expected to be `(LLOBJ 'left-type)` (a WordNode).
  FRAC-FN should be a function taking WA and WB as arguments, and
     returning a floating point number between zero and one, indicating
     the fraction of a non-shared count to be used.
     Returning 1.0 gives the sum of the union of supports;
     Returning 0.0 gives the sum of the intersection of supports.
  ZIPF is the smallest observation count, below which counts
     will not be divided up, if a merge is performed. (All of the
     count will be merged, when it is less than ZIPF)
  SING-A indicates how merging is to be done.
     XXX FIXME describe what is done.

  The merger of WA and WB are performed, using the 'projection
  merge' strategy described above. To recap, this is done as follows.
  If WA and WB are both `(LLOBJ 'left-type)` (i.e. are WordNodes),
  then a CLASS-TYPE (a WordClass) is created, having both WA and WB
  as members (via MemberLink).  Counts are then transferred from WA
  and WB to the class; subtotal contributions to the count are stored
  on the MemberLink.

  The counts are summed only if both counts are non-zero. Otherwise,
  only a FRAC fraction of a single, unmatched count is transferred.

  If the result leaves a pair with a count of zero, that pair
  is deleted.  This assumes that storage is connected; the updated
  pair is written to storage.

  If WA is of CLASS-TYPE (e.g. a WordClassNode), and WB is not, then
  WB is merged into WA. That is, the counts on WA are adjusted only
  upwards, and those on WB only downwards.
"
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	; Accumulated counts for the two.
	(define accum-lcnt 0)
	(define accum-rcnt 0)

	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN WA WB))

	; Use the tuple-math object to provide a pair of rows that
	; are aligned with one-another.
	(define (bogus a b) (format #t "Its ~A and ~A\n" a b))
	(define ptu (add-tuple-math LLOBJ bogus))

	; A list of pairs of sections to merge.
	(define perls (ptu 'right-stars (list WA WB)))

	(define trips
		(map
			(lambda (PRL)
				(define PAIR-A (first PRL))
				(define PAIR-B (second PRL))

				; The column (the right side). Both PAIR-A and
				; PAIR-B are in the same column. Just get it.
				(define col (if (null? PAIR-A)
						(LLOBJ 'right-element PAIR-B)
						(LLOBJ 'right-element PAIR-A)))

				; The place where the merge counts should be written
				(define mrg (LLOBJ 'make-pair CLS col))

				; Create a triple.
				(list PAIR-A PAIR-B mrg)
			)
		perls))

	; Perform the merge.
	(define monitor-rate (make-rate-monitor))
	(for-each
		(lambda (ITL)
			(define counts
				(merge-row-pairs LLOBJ ITL SING-A #t frac-to-merge ZIPF))
			; Accumulate the counts, handy for tracking membership fraction
			(set! accum-lcnt (+ accum-lcnt (car counts)))
			(set! accum-rcnt (+ accum-rcnt (cdr counts)))
			(monitor-rate #f))
		trips)

	(monitor-rate
		"---------Merged ~A sections in ~5F secs; ~6F scts/sec\n")

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	; Create and store MemberLinks.
	(if SING-A
		(let ((ma (MemberLink WA CLS))
				(mb (MemberLink WB CLS)))
			; Track the number of word-observations moved from
			; the words, to the class. This is how much the words
			; contributed to the class.
			(set-count ma accum-lcnt)
			(set-count mb accum-rcnt)
			; Put the two words into the new word-class.
			(store-atom ma)
			(store-atom mb))

		; If WA is not a WordNode, assume its a WordClassNode.
		; The process is similar, but slightly altered.
		; We assume that WB is a WordNode, but perform no safety
		; checking to verify this.
		(let ((mb (MemberLink WB CLS)))
			(set-count mb accum-rcnt)
			; Add WB to the mrg-class (which is WA already)
			(store-atom mb))
	)

	; Return the word-class
	CLS
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
; (foo (format #t "Start distance ~A \"~A\" -- \"~A\"\n"
; (if (eq? 'WordNode (cog-type WORD-A)) "word" "class")
; (cog-name WORD-A) (cog-name WORD-B)))
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

; Create a new word-class out of the two words.
; Concatenate the string names to get the class name.
; If WA is already a word-class, just use it as-is.
(define (make-word-class WA WB SING)
	(if SING
		(cog-new-node 'WordClass (string-concatenate
					(list (cog-name WA) " " (cog-name WB))))
		WA)
)

(define (make-fuzz STARS CUTOFF UNION-FRAC ZIPF MIN-CNT)
"
  make-fuzz -- Do projection-merge, with a fixed merge fraction.

  Uses the `merge-project` merge style.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  UNION-FRAC is the fixed fraction of the union-set of the disjuncts
  that will be merged.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((psa STARS)
			(pss (add-support-api psa))
			(psu (add-support-compute psa))
			(pcos (add-pair-cosine-compute psa))
		)
		(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
		(define (mpred WORD-A WORD-B)
			(is-similar? get-cosine CUTOFF WORD-A WORD-B))

		(define (fixed-frac WA WB) UNION-FRAC)

		; Return a WordClassNode that is the result of the merge.
		(define (merge WORD-A WORD-B)
			(define single (not (eq? 'WordClass (cog-type WORD-A))))
			(define cls (make-word-class WORD-A WORD-B single))
			(merge-frac pcos fixed-frac ZIPF WORD-A WORD-B cls single)

			; Need to recompute the marginals, in order for future
			; cosine evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-frac also updates storage...
			; Clobber first, since Sections were probably deleted.
			(psa 'clobber)
			(store-atom (psu 'set-right-marginals WORD-A))
			(store-atom (psu 'set-right-marginals WORD-B))
			(store-atom (psu 'set-right-marginals cls))
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
				((merge-predicate)  (apply mpred args))
				((merge-function)   (apply merge args))
				((discard-margin?)  (apply is-small-margin? args))
				((discard?)         (apply is-small? args))
				((clobber)          (begin (psa 'clobber) (psu 'clobber)))
				(else               (apply psa (cons message args)))
			)))
)

; ---------------------------------------------------------------

(define (make-discrim STARS CUTOFF ZIPF MIN-CNT)
"
  make-discrim -- Do a \"discriminating\" merge. When a word is to be
  merged into a word class, the fraction to be merged will depend on
  the cosine angle between the two. Effectively, there is a sigmoid
  taper between the union-merge and the intersection-merge. The more
  similar they are, the more of a union merge; the less similar the
  more of an intersection merge.

  The idea is that if two words are highly similar, they really should
  be taken together. If they are only kind-of similar, then maybe one
  word has multiple senses, and we only want to merge the fraction that
  shares a common word-sense, and leave the other word-sense out of it.

  Uses the `merge-discrim` merge style; the merge fraction is a sigmoid
  taper.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((psa STARS)
			(pss (add-support-api psa))
			(psu (add-support-compute psa))
			(pcos (add-pair-cosine-compute psa))
		)
		(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
		(define (mpred WORD-A WORD-B)
			(is-similar? get-cosine CUTOFF WORD-A WORD-B))

		; The fractional amount to merge will be proportional
		; to the cosine between them. The more similar they are,
		; the more they are merged together.
		(define (cos-fraction WA WB)
			(define cosi (pcos 'right-cosine WA WB))
			(/ (- cosi CUTOFF)  (- 1.0 CUTOFF)))

		; Return a WordClassNode that is the result of the merge.
		(define (merge WORD-A WORD-B)
			(define single (not (eq? 'WordClass (cog-type WORD-A))))
			(define cls (make-word-class WORD-A WORD-B single))
			(merge-frac pcos cos-fraction ZIPF WORD-A WORD-B cls single)
			; Need to recompute the marginals, in order for future
			; cosine evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-frac also updates storage...
			; Clobber first, since Sections were probably deleted.
			(psa 'clobber)
			(store-atom (psu 'set-right-marginals WORD-A))
			(store-atom (psu 'set-right-marginals WORD-B))
			(store-atom (psu 'set-right-marginals cls))
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
				((merge-predicate)  (apply mpred args))
				((merge-function)   (apply merge args))
				((discard-margin?)  (apply is-small-margin? args))
				((discard?)         (apply is-small? args))
				((clobber)          (begin (psa 'clobber) (psu 'clobber)))
				(else               (apply psa (cons message args)))
			)))
)

; ---------------------------------------------------------------

(define (make-disinfo STARS CUTOFF ZIPF MIN-CNT)
"
  make-disinfo -- Do a \"discriminating\" merge, using MI for
  similarity.

  Use `merge-project` style merging, with linear taper of the union-merge.
  This is the same as `merge-discrim` above, but using MI instead
  of cosine similarity.

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  CUTOFF is the min acceptable MI, for words to be considered
  mergable.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((dsa STARS)
			(pss (add-support-api dsa))
			(psu (add-support-compute dsa))
			(pmi (add-symmetric-mi-compute dsa))
			(pti (add-transpose-api dsa))
			(ptc (add-transpose-compute dsa))
		)

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

		; Return a WordClassNode that is the result of the merge.
		(define (merge WORD-A WORD-B)
			(define single (not (eq? 'WordClass (cog-type WORD-A))))
			(define cls (make-word-class WORD-A WORD-B single))
			(merge-frac pmi mi-fraction ZIPF WORD-A WORD-B cls single)
			; Need to recompute the marginals, in order for future
			; MI evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-frac also updates storage...
			; Clobber first, since Sections were probably deleted.
			(dsa 'clobber)
			(psu 'set-right-marginals WORD-A)
			(psu 'set-right-marginals WORD-B)
			(psu 'set-right-marginals cls)
			(store-atom (ptc 'set-mmt-marginals WORD-A))
			(store-atom (ptc 'set-mmt-marginals WORD-B))
			(store-atom (ptc 'set-mmt-marginals cls))
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
				((merge-predicate)  (apply mpred args))
				((merge-function)   (apply merge args))
				((discard-margin?)  (apply is-small-margin? args))
				((discard?)         (apply is-small? args))
				((clobber)          (begin (dsa 'clobber) (psu 'clobber)))
				(else               (apply dsa (cons message args)))
			)))
)

; ---------------------------------------------------------------
; Example usage
;
; (load-atoms-of-type 'WordNode)          ; Typically about 80 seconds
; (define pca (make-pseudo-cset-api))
; (define psa (add-dynamic-stars pca))
;
; Verify that support is correctly computed.
; cit-vil is a vector of pairs for matching sections for "city" "village".
; Note that the null list '() means 'no such section'
;
; (define (bogus a b) (format #t "Its ~A and ~A\n" a b))
; (define ptu (add-tuple-math psa bogus))
; (define cit-vil (ptu 'right-stars (list (Word "city") (Word "village"))))
; (length cit-vil)
;
; Show the first three values of the vector:
; (ptu 'get-count (car cit-vil))
; (ptu 'get-count (cadr cit-vil))
; (ptu 'get-count (caddr cit-vil))
;
; print the whole vector:
; (for-each (lambda (pr) (ptu 'get-count pr)) cit-vil)
;
; Is it OK to merge?
; (define pcos (add-pair-cosine-compute psa))
; (is-cosine-similar? pcos (Word "run") (Word "jump"))
; (is-cosine-similar? pcos (Word "city") (Word "village"))
;
; Perform the actual merge
; (define (frac WA WB) 0.3)
; (define cls (WordClass "city-village"))
; (merge-frac pcos frac 4 (Word "city") (Word "village") cls #t)
;
; Verify presence in the database:
; select count(*) from atoms where type=22;
