;
; gram-projective.scm
;
; Merge words into word-classes by grammatical similarity.
; Projective merge strategies.
;
; Copyright (c) 2017, 2018, 2019 Linas Vepstas
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

(define (merge-project LLOBJ FRAC-FN ZIPF WA WB)
"
  merge-project LLOBJ FRAC-FN ZIPF WA WB - merge WA and WB into a
  grammatical class.  Returns the merged class (a WordClassNode with
  Sections on it, contaiing the merged counts). This merges the
  Sections; this does not merge connectors, nor does it merge shapes.
  See `cset-class.scm` for connector merging.

  WA should be a WordNode or a WordClassNode.
  WB is expected to be a WordNode.
  FRAC-FN should be a function taking WA and WB as arguments, and
     returning a floating point number between zero and one, indicating
     the fraction of a non-shared count to be used.
     Returning 1.0 gives the sum of the union of supports;
     Returning 0.0 gives the sum of the intersection of supports.
  ZIPF is the smallest observation count, below which counts
     will not be divided up, if a marge is performed.
  LLOBJ is used to access counts on pairs.  Pairs are SectionLinks,
     that is, are (word,disjunct) pairs wrapped in a SectionLink.

  The merger of WA and WB are performed, using the 'projection
  merge' strategy. This is done like so. If WA and WB are both
  WordNodes, then a WordClass is created, having both WA and WB as
  members.  Counts are then transferred from WA and WB to the class.

  The counts are summed only if both counts are non-zero. Otherwise,
  only a FRAC fraction of a single, unmatched count is transferred.

  If WA is a WordClassNode, and WB is not, then WB is merged into
  WA. That is, the counts on WA are adjusted only upwards, and those
  on WB only downwards.
"
	(define (bogus a b) (format #t "Its ~A and ~A\n" a b))
	(define ptu (add-tuple-math LLOBJ bogus))

	; set-count ATOM CNT - Set the raw observational count on ATOM.
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (cog-new-ctv 1 0 CNT)))

	; Create a new word-class out of the two words.
	; Concatenate the string names to get the class name.
	; If WA is already a word-class, just use it as-is.
	(define wrd-class
		(if (eq? 'WordClassNode (cog-type WA)) WA
			(WordClassNode (string-concatenate
					(list (cog-name WA) " " (cog-name WB))))))

	; Accumulated counts for the two.
	(define accum-lcnt 0)
	(define accum-rcnt 0)

	; Merge two sections into one, placing the result on the word-class.
	; Given a pair of sections, sum the counts from each, and then place
	; that count on a corresponding section on the word-class.  Store the
	; updated section to the database.
	;
	; One or the other sections can be null. If both sections are not
	; null, then both are assumed to have exactly the same disjunct.
	;
	; This works fine for merging two words, or for merging
	; a word and a word-class.  It even works for merging
	; two word-classes.
	;
	(define (merge-section-pair SECT-PAIR)
		; The two word-sections to merge
		(define lsec (first SECT-PAIR))
		(define rsec (second SECT-PAIR))

		; The counts on each, or zero.
		(define lcnt (if (null? lsec) 0 (LLOBJ 'get-count lsec)))
		(define rcnt (if (null? rsec) 0 (LLOBJ 'get-count rsec)))

		; Return #t if sect is a Word section, not a word-class section.
		(define (is-word-sect? sect)
			(eq? 'WordNode (cog-type (cog-outgoing-atom sect 0))))

		; If the other count is zero, take only a FRAC of the count.
		; But only if we are merging in a word, not a word-class;
		; we never want to shrink the support of a word-class, here.
		(define frac (FRAC-FN WA WB))
		(define wlc (if
				(and (null? rsec) (is-word-sect? lsec) (< ZIPF lcnt))
				(* frac lcnt) lcnt))
		(define wrc (if
				(and (null? lsec) (is-word-sect? rsec) (< ZIPF rcnt))
				(* frac rcnt) rcnt))

		; Sum them.
		(define cnt (+ wlc wrc))

		; Compute what's left on each.
		(define lrem (- lcnt wlc))

		; Update the count on the section.
		; If the count is zero or less, delete the section.
		(define (update-section-count SECT CNT)
			(if (< 1.0e-10 CNT)
				(begin (set-count SECT CNT) (store-atom SECT))
				(begin (set-count SECT 0) (cog-delete SECT))))

		; The cnt can be zero, if FRAC is zero.  Do nothing in this case.
		(if (< 1.0e-10 cnt)
			(let* (
					; The disjunct. Both lsec and rsec have the same disjunct.
					(seq (if (null? lsec) (cog-outgoing-atom rsec 1)
							(cog-outgoing-atom lsec 1)))
					; The merged word-class
					(mrg (Section wrd-class seq))
				)

				; The summed counts
				(set-count mrg cnt)
				(store-atom mrg) ; save to the database.

				; Now subtract the counts from the words.
				; Left side is either a word or a word-class.
				; If its a word-class, we've already updated
				; the count.
				(if (and (not (null? lsec)) (is-word-sect? lsec))
					(update-section-count lsec (- lcnt wlc)))

				; Right side is WB and is always a WordNode
				(if (not (null? rsec))
					(update-section-count rsec (- rcnt wrc)))
			))

		; Accumulate the counts, handy for tracking membership fraction
		(set! accum-lcnt (+ accum-lcnt wlc))
		(set! accum-rcnt (+ accum-rcnt wrc))
	)

	(for-each merge-section-pair (ptu 'right-stars (list WA WB)))

	(if (eq? 'WordNode (cog-type WA))
		(let ((ma (MemberLink WA wrd-class))
				(mb (MemberLink WB wrd-class)))
			; Track the number of word-observations moved from
			; the words, the the class. This is how much the words
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
		(let ((mb (MemberLink WB wrd-class)))
			(set-count mb accum-rcnt)
			; Add WB to the mrg-class (which is WA already)
			(store-atom mb))
	)
	wrd-class
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

(define (make-fuzz CUTOFF UNION-FRAC ZIPF MIN-CNT)
"
  make-fuzz -- Do projection-merge, with a fixed merge fraction.

  Uses `merge-project`.

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  UNION-FRAC is the fixed fraction of the union-set of the disjuncts
  that will be merged.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((pca (make-pseudo-cset-api))
			(psa (add-dynamic-stars pca))
			(pss (add-support-api psa))
			(psu (add-support-compute psa))
			(pcos (add-pair-cosine-compute psa))
		)
		(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
		(define (mpred WORD-A WORD-B)
			(is-similar? get-cosine CUTOFF WORD-A WORD-B))

		(define (fixed-frac WA WB) UNION-FRAC)
		(define (merge WORD-A WORD-B)
			(define cls (merge-project pcos fixed-frac ZIPF WORD-A WORD-B))
			; Need to recompute the marginals, in order for future
			; cosine evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-project also updates storage...
			(store-atom (psu 'set-right-marginals WORD-A))
			(store-atom (psu 'set-right-marginals WORD-B))
			(store-atom (psu 'set-right-marginals cls))
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

(define (make-discrim CUTOFF ZIPF MIN-CNT)
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

  Built on top of `merge-project`, using a sigmoid taper.

  CUTOFF is the min acceptable cosine, for words to be considered
  mergable.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((pca (make-pseudo-cset-api))
			(psa (add-dynamic-stars pca))
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

		(define (merge WORD-A WORD-B)
			(define cls (merge-project pcos cos-fraction ZIPF WORD-A WORD-B))
			; Need to recompute the marginals, in order for future
			; cosine evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-project also updates storage...
			(store-atom (psu 'set-right-marginals WORD-A))
			(store-atom (psu 'set-right-marginals WORD-B))
			(store-atom (psu 'set-right-marginals cls))
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

(define (make-disinfo CUTOFF ZIPF MIN-CNT)
"
  make-disinfo -- Do a \"discriminating\" merge, using MI for
  similarity.

  Use `merge-project` with linear taper of the union-merge.
  This is the same as `merge-discrim` above, but using MI instead
  of cosine similarity.

  CUTOFF is the min acceptable MI, for words to be considered
  mergable.

  ZIPF is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(let* ((pca (make-pseudo-cset-api))
			(psa (add-dynamic-stars pca))
			(pss (add-support-api psa))
			(psu (add-support-compute psa))
			(pmi (add-symmetric-mi-compute psa))
			(ptc (add-transpose-compute psa))
		)
		(define (get-mi wa wb) (pmi 'mmt-fmi wa wb))
		(define (mpred WORD-A WORD-B)
			(is-similar? get-mi CUTOFF WORD-A WORD-B))

		(define (mi-fraction WA WB)
			0.5)

		(define (merge WORD-A WORD-B)
			(define cls (merge-project pmi mi-fraction ZIPF WORD-A WORD-B))
			; Need to recompute the marginals, in order for future
			; MI evaluations to work correctly.  We also store this,
			; so that restarts can see the correct values.  Recall
			; that merge-project also updates storage...
			(store-atom (ptc 'set-mmt-marginals WORD-A))
			(store-atom (ptc 'set-mmt-marginals WORD-B))
			(store-atom (ptc 'set-mmt-marginals cls))
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
; (merge-project pcos frac 4 (Word "city") (Word "village"))
;
; Verify presence in the database:
; select count(*) from atoms where type=22;
