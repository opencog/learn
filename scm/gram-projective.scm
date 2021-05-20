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
; Although the code keeps talking about words and word-classes, it is
; (almost) entirely generic, and can merge (cluster) anything. The only
; place(s) where its not generic is in some progress-report printing,
; and in the general discussion of what this code does. Otherwise, what
; to merge, and where to put the merger results are defined by LLOBJ.
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
; where `cos_min` is the minimum cosine acceptable, for any kind of
; merging to be performed.
; Implemented in the `make-discrim` call.
;
; merge-disinfo
; -------------
; Like `merge-discrim` but using mutual information instead of cosines.
; Implemented in the `make-disinfo` call.
;
; start-cluster, merge-into-cluster
; ---------------------------------
; Implementation of the common parts of the above merge styles,
; using callbacks and parameters to obtain the merge fraction.
; Calls `accumulate-count` to do the column-by-column summing.
;
; make-merger
; -----------
; High-level wrapper for above. Provides a generic API.
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
; TODO
; ----
; It might be useful to move the management of the MemberLink's to
; the `add-cluster-gram` object.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
; Return #t if the count is effectively zero.
; Use an epsilon for rounding errors.
(define (is-zero? cnt) (< cnt 1.0e-10))

(define (accumulate-count LLOBJ ACC PAIR FRAC NOISE)
"
  accumulate-count LLOBJ ACC PAIR FRAC NOISE -- Accumulate count
    from PAIR into ACC.

  ACC and PAIR should be two pairs in the matrix LLOBJ. (Usually,
  they will be in the same row or column, although this code does not
  assume this.)

  The count on PAIR will be transfered to ACC, with some caveats:
  If the count on ACC is non-zero, then *all* of the count on PAIR
  will be transfered (and PAIR will be removed from the database).

  If the count on ACC is zero, and the count on PAIR is greater than
  NOISE (floating-point noise-floor), then only a FRAC of the count
  will get transfered to ACC. If the count is below the noise floor,
  then all of it will be transfered over.

  Both Atoms, with updated counts, are stored to the database.

  The prototypical use-case has ACC and PAIR being two Sections
  of (word, disjunct) pairs, having the same disjunct but two different
  words. The goal is to merge the two words together into a single
  word-class.
"

	; The counts on the accumulator and the pair to merge.
	(define mcnt (LLOBJ 'get-count PAIR))
	(define acnt (LLOBJ 'get-count ACC))

	; If the accumulator count is zero, transfer only a FRAC of
	; the count into the accumulator.
	(define taper-cnt (if
			(and (is-zero? acnt) (< NOISE mcnt))
			(* FRAC mcnt) mcnt))

	; Update the count on the donor pair.
	; If the count is zero or less, delete the donor pair.
	; (Actually, it should never be less than zero!)
	(define (update-donor-count SECT CNT)
		(set-count SECT CNT)
		(unless (is-zero? CNT) (store-atom SECT)))

	; If there is nothing to transfer over, do nothing.
	(if (not (is-zero? taper-cnt))
		(begin

			; The accumulated count
			(set-count ACC (+ acnt taper-cnt))
			(store-atom ACC) ; save to the database.

			; Decrement the equivalent amount from the donor pair.
			(update-donor-count PAIR (- mcnt taper-cnt))
		))

	; Return how much was transfered over.
	taper-cnt
)

; ---------------------------------------------------------------------

(define (start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON)
"
  start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON --
     Start a new cluster by merging rows WA and WB of LLOBJ into a
     combined row CLS.

  In the prototypical use case, each row corresponds to a WordNode,
  and the result of summing them results in a WordClassNode. Thus,
  by convention, it is assumed that the pairs are (word, disjunct)
  pairs, and LLOBJ was made by `make-pseudo-cset-api` or by
  `add-shape-vec-api`. The code itself is generic, and may work on
  other kinds of LLOBJ's too.

  LLOBJ is used to access pairs.
  WA and WB should both be of `(LLOBJ 'left-type)`. They should
     designate two different rows in LLOBJ that will be merged,
     column-by-column.
  CLS denotes a new row in LLOBJ, that will contain the merged counts.
     MemberLinks will be created from WA and WB to CLS.
  FRAC-FN should be a function taking WA and WB as arguments, and
     returning a floating point number between zero and one, indicating
     the fraction of a non-shared count to be used.
     Returning 1.0 gives the sum of the union of supports;
     Returning 0.0 gives the sum of the intersection of supports.
  NOISE is the smallest observation count, below which counts will
     not be divided up, when the merge is performed. (All of the
     count will be merged, when it is less than NOISE)
  MRG-CON boolean flag; if #t then connectors will be merged.

  The merger of rows WA and WB are performed, using the 'projection
  merge' strategy described above. To recap, this is done as follows.
  If counts on a given column of both WA and WB are non-zero, they are
  summed, and the total is placed on the matching column of CLS. The
  contributing columns are removed (as thier count is now zero).
  If one is zero, and the other is not, then only a FRAC of the count
  is transfered.

  Accumulated row totals are stored in the two MemberLinks that attach
  WA and WB to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	(define monitor-rate (make-rate-monitor))

	; Create MemberLinks. We need these early, for decision-making
	; during the merge.
	(define ma (MemberLink WA CLS))
	(define mb (MemberLink WB CLS))

	; Accumulated counts for the two MemberLinks.
	(define accum-acnt 0)
	(define accum-bcnt 0)

	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN WA WB))

	; Use the tuple-math object to provide a pair of rows that
	; are aligned with one-another.
	(define (bogus a b) (format #t "Its ~A and ~A\n" a b))
	(define ptu (add-tuple-math LLOBJ bogus))

	; A list of pairs of sections to merge.
	; This is a list of pairs of columns from LLOBJ, where either
	; one or the other or both rows have non-zero elements in them.
	(define perls (ptu 'right-stars (list WA WB)))

	; Loop over the sections above, merging them into one cluster.
	(for-each
		(lambda (PRL)
			(define PAIR-A (first PRL))
			(define PAIR-B (second PRL))

			(define null-a (null? PAIR-A))
			(define null-b (null? PAIR-B))

			; The target into which to accumulate counts. This is
			; an entry in the same column that PAIR-A and PAIR-B
			; are in. (TODO maybe we could check that both PAIR-A
			; and PAIR-B are in the same column.)
			(define col (if null-a
					(LLOBJ 'right-element PAIR-B)
					(LLOBJ 'right-element PAIR-A)))

			; The place where the merge counts should be written
			(define mrg (LLOBJ 'make-pair CLS col))

			(define (do-acc CNT W PR WEI)
				(set! CNT (+ CNT
					(accumulate-count LLOBJ mrg PR WEI NOISE))))

			; Now perform the merge. Overlapping entries are
			; completely merged (frac=1.0). Non-overlapping ones
			; contribute only FRAC.
			(monitor-rate #f)
			(cond
				(null-a (do-acc accum-bcnt WB PAIR-B frac-to-merge))
				(null-b (do-acc accum-acnt WA PAIR-A frac-to-merge))
				(else ; AKA (not (or null-a null-b))
					(begin
						(do-acc accum-acnt WA PAIR-A 1.0)
						(do-acc accum-bcnt WB PAIR-B 1.0)))))
		perls)

	(monitor-rate
		"------ Create: Merged ~A sections in ~5F secs; ~6F scts/sec\n")

	; If merging connectors, then make a second pass. We can't do this
	; in the first pass, because the connector-merge logic needs to
	; manipulate the merged Sections. (There's no obvious way to do
	; this in a single pass; I tried.)
	(when MRG-CON

	(set! monitor-rate (make-rate-monitor))
	(for-each
		(lambda (PRL)
			(define PAIR-A (first PRL))
			(define PAIR-B (second PRL))

			(define null-a (null? PAIR-A))
			(define null-b (null? PAIR-B))

			; The target into which to accumulate counts. This is
			; an entry in the same column that PAIR-A and PAIR-B
			; are in. (TODO maybe we could check that both PAIR-A
			; and PAIR-B are in the same column.)
			(define col (if null-a
					(LLOBJ 'right-element PAIR-B)
					(LLOBJ 'right-element PAIR-A)))

			; The place where the merge counts should be written
			(define mrg (LLOBJ 'make-pair CLS col))

			(define (do-acc CNT W PR WEI)
				(reshape-merge LLOBJ CLS mrg W PR WEI NOISE))

			; Now perform the merge. Overlapping entries are
			; completely merged (frac=1.0). Non-overlapping ones
			; contribute only FRAC.
			(monitor-rate #f)
			(cond
				(null-a (do-acc accum-bcnt WB PAIR-B frac-to-merge))
				(null-b (do-acc accum-acnt WA PAIR-A frac-to-merge))
				(else ; AKA (not (or null-a null-b))
					(begin
						(do-acc accum-acnt WA PAIR-A 1.0)
						(do-acc accum-bcnt WB PAIR-B 1.0)))))
		perls)
	(monitor-rate
		"------ Create: Revised ~A shapes in ~5F secs; ~6F scts/sec\n")
	)

	(set! monitor-rate (make-rate-monitor))
	(monitor-rate #f)

	; Track the number of observations moved from the two items
	; into the combined class. This tracks the individual
	; contributions.
	(set-count ma accum-acnt)
	(set-count mb accum-bcnt)

	; Store the counts on the MemberLinks.
	(store-atom ma)
	(store-atom mb)

	; Cleanup after merging.
	(LLOBJ 'clobber)
	(remove-empty-sections LLOBJ WA)
	(remove-empty-sections LLOBJ WB)
	(remove-empty-sections LLOBJ CLS)

	; cog-extract! only removes them from the AtomSpace;
	; cog-delete removes them from the database.
	; (for-each cog-extract! (cog-get-atoms 'ShapeLink))
	(for-each cog-extract! (cog-get-atoms 'ConnectorSeq))
	(for-each cog-delete! (cog-get-atoms 'ShapeLink))
	; (for-each cog-delete! (cog-get-atoms 'ConnectorSeq))

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	(monitor-rate
		"------ Create: cleanup ~A in ~5F secs; ~6F ops/sec\n")
)

; ---------------------------------------------------------------------

(define (merge-into-cluster LLOBJ CLS WA FRAC-FN NOISE MRG-CON)
"
  merge-into-cluster LLOBJ CLS WA FRAC-FN NOISE MRG-CON --
     Merge WA into cluster CLS. These are two rows in LLOBJ,
     the merge is done column-by-column. A memberLink from
     WA to CLS will be created.

  See start-cluster for addtional details.

  LLOBJ is used to access pairs.
  WA should be of `(LLOBJ 'left-type)`
  CLS should be interpretable as a row in LLOBJ.

  FRAC-FN should be a function taking CLS and WA as arguments, and
     returning a floating point number between zero and one, indicating
     the fraction of a non-shared count to be used.
     Returning 1.0 gives the sum of the union of supports;
     Returning 0.0 gives the sum of the intersection of supports.
  NOISE is the smallest observation count, below which counts will
     not be divided up, when the merge is performed. (All of the
     count will be merged, when it is less than NOISE)
  MRG-CON boolean flag; if #t then connectors will be merged.

  The merger of row WA into CLS is performed, using the 'projection
  merge' strategy described above. To recap, this is done as follows.
  If counts on a given column of both CLS and WA are non-zero, then
  all of the count from WA is transfered to CLS. That column in WA
  is removed (as it's count is now zero). If the count on CLS is zero,
  then only a FRAC of WA's count is transfered.

  Accumulated row totals are stored in the MemberLink that attaches
  WA to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	; Create the MemberLink. We need this early, for decision-making
	; during the merge.
	(define ma (MemberLink WA CLS))

	; Accumulated count on the MemberLink.
	(define accum-cnt 0)

	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN CLS WA))

	; Use the tuple-math object to provide a pair of rows that
	; are aligned with one-another.
	(define (bogus a b) (format #t "Its ~A and ~A\n" a b))
	(define ptu (add-tuple-math LLOBJ bogus))

	(define monitor-rate (make-rate-monitor))

	; A list of pairs of sections to merge.
	; This is a list of pairs of columns from LLOBJ, where either
	; one or the other or both rows have non-zero elements in them.
	(define perls (ptu 'right-stars (list CLS WA)))

	; Caution: there's a "feature" bug in projection merging when used
	; with connector merging. The code below will create sections with
	; dangling connectors that may be unwanted. Easiest to explain by
	; example. Consider a section (f, abe) being merged into a cluster
	; {e,j} to form a cluster {e,j,f}. The code below will create a
	; section ({ej}, abe) as the C-section, and transfer some counts
	; to it. But, when connector merging is desired, it should have gone
	; to ({ej}, ab{ej}). There are two possible solutions: have the
	; connector merging try to detect this, and clean it up, or have
	; the tuple object pair up (f, abe) to ({ej}, ab{ej}). There is no
	; "natural" way for the tuple object to create this pairing (it is
	; "naturally" linear, by design) so we must clean up during connector
	; merging.
	(for-each
		(lambda (PRL)
			(define PAIR-C (first PRL))
			(define PAIR-A (second PRL))

			(define (do-acc PRC WEI)
				(monitor-rate #f)
				(set! accum-cnt (+ accum-cnt
					(accumulate-count LLOBJ PRC PAIR-A WEI NOISE))))

			; There's nothing to do if A is empty.
			(when (not (null? PAIR-A))

				; Two different tasks, depending on whether PAIR-C
				; exists or not - we merge all, or just some.
				(if (null? PAIR-C)

					; pare-c is the non-null version of PAIR-C
					; We accumulate a fraction of PAIR-A into it.
					(let* ((col (LLOBJ 'right-element PAIR-A))
							(pare-c (LLOBJ 'make-pair CLS col)))
						(do-acc pare-c frac-to-merge))

					; PAIR-C exists already. Merge 100% of A into it.
					(do-acc PAIR-C 1.0))
			))
		perls)

	(monitor-rate
		"------ Extend: Merged ~A sections in ~5F secs; ~6F scts/sec\n")

	(when MRG-CON
	(set! monitor-rate (make-rate-monitor))
	(for-each
		(lambda (PRL)
			(define PAIR-C (first PRL))
			(define PAIR-A (second PRL))

			(define (do-acc PRC WEI)
				(monitor-rate #f)
				(reshape-merge LLOBJ CLS PRC WA PAIR-A WEI NOISE))

			; There's nothing to do if A is empty.
			(when (not (null? PAIR-A))

				; Two different tasks, depending on whether PAIR-C
				; exists or not - we merge all, or just some.
				(if (null? PAIR-C)

					; pare-c is the non-null version of PAIR-C
					; We accumulate a fraction of PAIR-A into it.
					(let* ((col (LLOBJ 'right-element PAIR-A))
							(pare-c (LLOBJ 'make-pair CLS col)))
						(do-acc pare-c frac-to-merge))

					; PAIR-C exists already. Merge 100% of A into it.
					(do-acc PAIR-C 1.0))
			))
		perls)

	(monitor-rate
		"------ Extend: Revised ~A shapes in ~5F secs; ~6F scts/sec\n")
	)

	(set! monitor-rate (make-rate-monitor))
	(monitor-rate #f)

	; Track the number of observations moved from WA to the class.
	; Store the updated count.
	(set-count ma accum-cnt)
	(store-atom ma)

	; Cleanup after merging.
	(LLOBJ 'clobber)
	(remove-empty-sections LLOBJ WA)
	(remove-empty-sections LLOBJ CLS)

	; cog-extract! only removes them from the AtomSpace;
	; cog-delete removes them from the database.
	; (for-each cog-extract! (cog-get-atoms 'ShapeLink))
	(for-each cog-extract! (cog-get-atoms 'ConnectorSeq))
	(for-each cog-delete! (cog-get-atoms 'ShapeLink))
	; (for-each cog-delete! (cog-get-atoms 'ConnectorSeq))

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	(monitor-rate
		"------ Extend: Cleanup ~A in ~5F secs; ~6F ops/sec\n")
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

(define-public (make-merger STARS MPRED FRAC-FN NOISE MIN-CNT STORE MRG-CON)
"
  make-merger STARS MPRED FRAC-FN NOISE MIN-CNT STORE MRG-CON --
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

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a marge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a row is allowed to have, to even be considered for
  merging.

  STORE is an extra function called, after the merge is to completed,
  and may be used to store additional needed data that the algo here is
  unaware of.

  MRG-CON is #t if Connectors should also be merged.

  This object provides the following methods:

  'merge-predicate -- a wrapper around MPRED above.
  'merge-function -- the function that actuall performs the merge.
  'discard-margin? --
  'discard? --
  'clobber -- invalidate all caches.
"
	(define pss (add-support-api STARS))
	(define psu (add-support-compute STARS))

	; Return a WordClassNode that is the result of the merge.
	(define (merge WA WB)
		(define single (not (eq? (STARS 'cluster-type) (cog-type WA))))
		(define cls (STARS 'make-cluster WA WB))

		; Cluster - either create a new cluster, or add to an existing
		; one. Afterwards, need to recompute the marginals. This is so
		; that future similarity judgements work correctly. This is
		; because the mergers altered the counts, and so the marginals
		; are wrong. (Well, they're wrong only for WA, WB and cls,
		; whereas clobber clobbers everything. Oh well. Its hard.)
		; The results are stored, so that everything is on disk in
		; of a restart.
		; Clobber first, since Sections were probably deleted.
		(if single
			(begin
				(start-cluster psu cls WA WB FRAC-FN NOISE MRG-CON)
				(psu 'clobber)
				(store-atom (psu 'set-right-marginals cls))
				(STORE cls)
			)
			(begin
				(merge-into-cluster psu WA WB FRAC-FN NOISE MRG-CON)
				(psu 'clobber)
			))

		(store-atom (psu 'set-right-marginals WA))
		(store-atom (psu 'set-right-marginals WB))

		(STORE WA)
		(STORE WB)

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
			((clobber)          (begin (STARS 'clobber) (psu 'clobber)))
			(else               (apply STARS (cons message args)))
		))
)

; ---------------------------------------------------------------

(define-public (make-fuzz STARS CUTOFF UNION-FRAC NOISE MIN-CNT)
"
  make-fuzz -- Do projection-merge, with a fixed merge fraction.

  Uses the `merge-project` merge style.

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
	(define pcos (add-pair-cosine-compute STARS))
	(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-cosine CUTOFF WORD-A WORD-B))

	(define (fixed-frac WA WB) UNION-FRAC)

	(make-merger STARS mpred fixed-frac NOISE MIN-CNT (lambda (x) #f) #t)
)

; ---------------------------------------------------------------

(define-public (make-discrim STARS CUTOFF NOISE MIN-CNT)
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

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pcos (add-pair-cosine-compute STARS))
	(define (get-cosine wa wb) (pcos 'right-cosine wa wb))
	(define (mpred WORD-A WORD-B)
		(is-similar? get-cosine CUTOFF WORD-A WORD-B))

	; The fractional amount to merge will be proportional
	; to the cosine between them. The more similar they are,
	; the more they are merged together.
	(define (cos-fraction WA WB)
		(define cosi (pcos 'right-cosine WA WB))
		(/ (- cosi CUTOFF)  (- 1.0 CUTOFF)))

	(make-merger STARS mpred cos-fraction NOISE MIN-CNT (lambda (x) #f) #t)
)

; ---------------------------------------------------------------

(define-public (make-disinfo STARS CUTOFF NOISE MIN-CNT)
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

  NOISE is the smallest observation count, below which counts
  will not be divided up, if a merge is performed.

  MIN-CNT is the minimum count (l1-norm) of the observations of
  disjuncts that a word is allowed to have, to even be considered.
"
	(define pss (add-support-api STARS))
	(define pmi (add-symmetric-mi-compute STARS))
	(define pti (add-transpose-api STARS))
	(define ptc (add-transpose-compute STARS))

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

	(define (store-mmt row)
		(store-atom (ptc 'set-mmt-marginals row)))

	(make-merger pmi mpred mi-fraction NOISE MIN-CNT store-mmt #t)
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
; (start-cluster psa cls (Word "city") (Word "village") frac 4.0 #t)
;
; Verify presence in the database:
; select count(*) from atoms where type=22;
