;
; gram-pairwise.scm
;
; Create clusters by merging two items at a time.
;
; Copyright (c) 2017, 2018, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` and `gram-projective.scm` for an overview.
;
; start-cluster
; -------------
; Create a brand new cluster out of two items.
;
; merge-into-cluster
; ------------------
; Merge item into an existing cluster.
;
; merge-clusters
; --------------
; Merge two clusters into one.
;
; make-merge-pair
; ---------------
; Wrapper for above. Returns a function that will merge a pair or
; things, whatever they may be.
;
; This is a kind-of special case of `make-merge-majority`, which will
; merge N items into a cluster, in one go. However, it's a bit different
; in that it does support the union-fractional merge (fuzzy merge)
; described in `gram-projective.scm` and elsewhere.  The fuzzy merge
; idea doesn't seem to be that great, and so is abandoned in the
; majority-vote code base. However, the unit tests explicitly test it,
; so we're not going to wreck that. It was hard-won functionality.
; (Hmm, We could add it easily to the majority-vote code ...)
;
; ---------------------------------------------------------------------
;
; gram-projective.scm
;
; Merge vectors into clusters. Basic core tools.
; Also: detailed description of projective merge.
;
; Copyright (c) 2017, 2018, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` for an overview.
;
; The notes below descrbe orthogonal/union/overlap type merging in
; greater detail than what's in `gram-classification.scm`.
;
; The tools provided in this file are used by several different merge
; algorithms. The provided tools, listed in order:
;
; * Some debug printers for compact printing of Sections.
; * `accumulate-count`, for transferring counts from one basis elt to
;   another.
; * `assign-to-cluster`, which loops over a vector, and calls a callback
;   on each basis elt. In most cases, this callback then will
;   eventually call `accumulate-count` to perform the actual count
;   transfer.
; * `rebalance-shapes`, which loops over a vector, and makes sure that
;   counts are on CrossSections are consistent with the counts on Sections.
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
; initial grammatical class, out of the merger of N words. Several
; strategies are possible. Given words `u`, `v`, `w`, ... one may:
;
; * Simple sum: let `g=u+v+w+...`. That's it; nothing more.
; * Overlap and union merge, described below.
; * Democratic voting: merge those basis elements shared by a majority.
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
; accumulate-count, assign-to-cluster
; -----------------------------------
; The above merge methods are implemented in the `accumulate-count`
; and `assign-to-cluster` functions. The first does the math for
; one basis element, the second loops over the basis elts in a vector.
;
; The first takes, as an argument, a fractional weight which is
; used when the disjunct isn't shared between both words. Setting
; the weight to zero gives overlap merging; setting it to one gives
; union merging. Setting it to fractional values provides a merge
; that is intermediate between the two: an overlap, plus a bit more,
; viz some of the union.  This is sometimes called "fuzzy merging"
; in other places.
;
; That is, the merger is given by the vector
;
;   v_merged = v_overlap + FRAC * (v_union - v_overlap)
;
; If v_a and v_b are both words, then the counts on v_a and v_b are
; adjusted to remove the counts that were added into v_merged. If one
; of the two is already a word-class, then the counts are simply moved
; from the word to the class.
;
; TODO
; ----
; It might be useful to move the management of the MemberLink's to
; the `add-gram-class-api` object.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

(define-public (accumulate-count LLOBJ ACC DONOR FRAC)
"
  accumulate-count LLOBJ ACC DONOR FRAC -- Accumulate a fraction
    FRAC of the count from DONOR into ACC.

  ACC and DONOR should be two pairs in the matrix LLOBJ.

  FRAC should be a numeric fraction, between 0.0 and 1.0.

  A fraction FRAC of the count on DONOR will be transferred to ACC.
"
	; Return #t if the count is effectively zero.
	; Use an epsilon for rounding errors.
	(define (is-zero? cnt) (< cnt 1.0e-10))

	(define moved (LLOBJ 'move-count ACC DONOR FRAC))

	; If something was transfered, save the updated counts.
	(when (not (is-zero? moved))
		(rebalance-count LLOBJ ACC (get-count ACC))
		(rebalance-count LLOBJ DONOR (get-count DONOR))
	)

	; Return how much was transferred over.
	moved
)

; ---------------------------------------------------------------------

(define (assign-to-cluster LLOBJ CLS WA CLIQUE)
"
  assign-to-cluster LLOBJ CLS WA CLIQUE --

  Loop over the disjuncts on WA, and call CLIQUE on each,
  passing CLS and the disjunct to it.

  A MemberLink from WA to CLS will be created, holding the
  accumulated count returned by CLIQUE.

  LLOBJ is used to access pairs.
  WA should be of `(LLOBJ 'left-type)`
  CLS should be interpretable as a row in LLOBJ.

  CLIQUE is a function that returns how much of a given disjunct
     is merged.

  The merger of row WA into CLS is performed, using the CLIQUE
  function to make disjunct-by-disjunct merge decisions.

  This assumes that storage is connected; the updated counts are
  written to storage.
"
	(define monitor-count (make-count-monitor))

	; Accumulated count on the MemberLink.
	(define accum-cnt 0)

	(for-each
		(lambda (PAIR-A)
			(define cnt (CLIQUE CLS PAIR-A accumulate-count))
			(monitor-count #f)
			(when (< 0 cnt)
				(monitor-count #t) ; increment only if counted!
				(set! accum-cnt (+ accum-cnt cnt))))
		(LLOBJ 'right-stars WA))

	; Track the number of observations moved from WA to the class.
	(define memb-a (MemberLink WA CLS))
	(cog-inc-count! memb-a accum-cnt)
	(store-atom memb-a)

	(monitor-count
		(string-append
			"------ Assign: Merged ~D of ~D sections on `"
			(cog-name WA)
			"` in ~6,1F secs\n"))
)

; ---------------------------------------------------------------------

(define (rebalance-shapes LLOBJ CLS WA CLIQUE)
"
  rebalance-shapes LLOBJ CLS WA CLIQUE --

  Loop over the pairs having WA on the left, and call CLIQUE on each,
  passing CLS and the pair to it.

  LLOBJ is used to access pairs.  WA and CLS should both be 'left-types
  of `LLOBJ`

  CLIQUE should be a function that takes the given CLS and donor pair,
  and uses those to deduce the merge-into pair. The merge-into pair,
  and the donor pair are then handed to `rebalance-merge`, which makes
  the counts consistent on both pairs.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	(define monitor-rate (make-rate-monitor))

	(for-each
		(lambda (PAIR-A)
			(monitor-rate #f)
			(CLIQUE CLS PAIR-A rebalance-merge)
		)
		(LLOBJ 'right-stars WA))

	(monitor-rate
			"------ Assign: Revised ~A shapes in ~5F secs; ~6F scts/sec\n")
)

; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON)
"
  start-cluster LLOBJ CLS WA WB FRAC-FN NOISE MRG-CON --
     Start a new cluster by merging rows WA and WB of LLOBJ into a
     combined row CLS.

  In the prototypical use case, each row corresponds to a WordNode,
  and the result of summing them results in a WordClassNode. Thus,
  by convention, it is assumed that the pairs are (word, disjunct)
  pairs, and LLOBJ was made by `make-pseudo-cset-api` or by
  `add-shape-vec-api`. The code itself is generic, and might work on
  other kinds of LLOBJ's too. (It might work, but has not been tested.)

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

  MRG-CON boolean flag; if #t then connectors will be merged.

  The merger of rows WA and WB are performed, using the 'projection
  merge' strategy described above. To recap, this is done as follows.
  If counts on a given column of both WA and WB are non-zero, they are
  summed, and the total is placed on the matching column of CLS. The
  contributing columns are removed (as their count is now zero).
  If one is zero, and the other is not, then only a FRAC of the count
  is transferred.

  Accumulated row totals are stored in the two MemberLinks that attach
  WA and WB to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN WA WB))

	(define (make-flat CLUST SECT)
		(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))

	; CLUST is identical to CLS, always.
	(define (clique CLUST SECT ACC-FUN)
		(define WRD (LLOBJ 'left-element SECT))
		(define DJ (LLOBJ 'right-element SECT))
		(define WOTHER (if (equal? WRD WA) WB WA))
		(define OTHSEC (LLOBJ 'get-pair WOTHER DJ))

		; If OTHSEC is nil, then it is the non-overlap case, and
		; only a fraction is merged.  Otherwise, all is merged.
		(if (nil? OTHSEC)
			(if (<= (LLOBJ 'get-count SECT) NOISE)
				(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
				(if (< 0 frac-to-merge)
					(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT frac-to-merge)))
			(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
		)
	)

	; Create MemberLinks *before* starting merge!
	(MemberLink WA CLS)
	(MemberLink WB CLS)
	(assign-to-cluster LLOBJ CLS WA clique)
	(assign-to-cluster LLOBJ CLS WB clique)

	(when MRG-CON
		(rebalance-shapes LLOBJ CLS WA clique)
		(rebalance-shapes LLOBJ CLS WB clique)
	)

	; Cleanup after merging.
	; The LLOBJ is assumed to be just a stars object, and so the
	; intent of this clobber is to force it to recompute it's left
	; and right basis.
	(define e (make-elapsed-secs))
	(LLOBJ 'clobber)
	(remove-empty-sections LLOBJ WA MRG-CON)
	(remove-empty-sections LLOBJ WB MRG-CON)
	(remove-empty-sections LLOBJ CLS MRG-CON)

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	(format #t "------ StartCluster: Cleanup ~A in ~5F secs\n"
		(cog-name CLS) (e))
)

; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (merge-into-cluster LLOBJ CLS WA FRAC-FN NOISE MRG-CON)
"
  merge-into-cluster LLOBJ CLS WA FRAC-FN MRG-CON --
     Merge WA into cluster CLS. These are two rows in LLOBJ,
     the merge is done column-by-column. A MemberLink from
     WA to CLS will be created.

  See start-cluster for additional details.

  LLOBJ is used to access pairs.
  WA should be of `(LLOBJ 'left-type)`
  CLS should be interpretable as a row in LLOBJ.

  FRAC-FN should be a function taking CLS and WA as arguments, and
     returning a floating point number between zero and one, indicating
     the fraction of a non-shared count to be used.
     Returning 1.0 gives the sum of the union of supports;
     Returning 0.0 gives the sum of the intersection of supports.
  MRG-CON boolean flag; if #t then connectors will be merged.

  The merger of row WA into CLS is performed, using the 'projection
  merge' strategy described above. To recap, this is done as follows.
  If counts on a given column of both CLS and WA are non-zero, then
  all of the count from WA is transferred to CLS. That column in WA
  is removed (as it's count is now zero). If the count on CLS is zero,
  then only a FRAC of WA's count is transferred.

  Accumulated row totals are stored in the MemberLink that attaches
  WA to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN CLS WA))

	(define (make-flat CLUST SECT)
		(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))

	; CLUST is identical to CLS, always.
	(define (clique CLUST SECT ACC-FUN)
		(define DJ (LLOBJ 'right-element SECT))
		(define CLS-SECT (LLOBJ 'get-pair CLUST DJ))

		; What a mess. If the corresponding cluster section exists,
		; then merge all of the section into it. Else, create the
		; flattened cluster, and merge all if the given section is
		; iteself non-flat, or if its below the noise threshold.
		(if (nil? CLS-SECT)
			(if (or
					(<= (LLOBJ 'get-count SECT) NOISE)
					(LLOBJ 'is-nonflat? CLUST SECT))
				(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT 1.0)
				(if (< 0 frac-to-merge)
					(ACC-FUN LLOBJ (make-flat CLUST SECT) SECT frac-to-merge)))
			(ACC-FUN LLOBJ CLS-SECT SECT 1.0))
	)

	; Create MemberLink *before* starting merge!
	(MemberLink WA CLS)
	(assign-to-cluster LLOBJ CLS WA clique)

	(when MRG-CON
		(rebalance-shapes LLOBJ CLS WA clique)
	)

	; Cleanup after merging.
	; The LLOBJ is assumed to be just a stars object, and so the
	; intent of this clobber is to force it to recompute it's left
	; and right basis.
	(define e (make-elapsed-secs))
	(LLOBJ 'clobber)
	(remove-empty-sections LLOBJ WA MRG-CON)
	(remove-empty-sections LLOBJ CLS MRG-CON)

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	(format #t "------ Merge-Into-Cluster: Cleanup ~A in ~5F secs\n"
		(cog-name CLS) (e))
)

; ---------------------------------------------------------------------

; XXX TODO this should not be exported, not really.
(define-public (merge-clusters LLOBJ CLA CLB MRG-CON)
"
  merge-clusters LLOBJ CLA CLB FRAC-FN MRG-CON --
     Merge clusters CLA and CLB. These are two rows in LLOBJ,
     the merge is done column-by-column.

  This will perform a \"union merge\" -- all disjuncts on CLB will
  be transferred to CLA, and CLB will be removed.

  See start-cluster for additional details.
"
	(define (make-flat CLUST SECT)
		(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))

	; CLUST is identical to CLA, always.
	(define (clique CLUST SECT ACC-FUN)
		(define DJ (LLOBJ 'right-element SECT))
		(define MSECT (LLOBJ 'make-pair CLUST DJ))
		(define CLS-SECT (make-flat CLUST MSECT))
		(ACC-FUN LLOBJ CLS-SECT SECT 1.0)
	)

	; Create MemberLink *before* starting merge!
	(for-each
		(lambda (MEMB-B) (MemberLink (gar MEMB-B) CLA))
		(cog-incoming-by-type CLB 'MemberLink))

	; Merge the two clusters. Delete the spurious MemberLink!
	(assign-to-cluster LLOBJ CLA CLB clique)
	(cog-delete! (Member CLB CLA))

	; Copy all counts from MemberLinks on CLB to CLA.
	; Delete MemberLinks on CLB.
	(for-each
		(lambda (MEMB-B)
			; Get the word
			(define WRD (gar MEMB-B))

			; Get the count
			(define CNT-A 0)
			(define CNT-B (LLOBJ 'get-count MEMB-B))

			; Does a corresponding word exist on class A?
			(define MEMB-A (cog-link 'MemberLink WRD CLA))

			(if (not (nil? MEMB-A))
				(set! CNT-A (LLOBJ 'get-count MEMB-A)))

			; Create the MmeberLink on A, and update the count.
			(define MBA (MemberLink WRD CLA))
			(set-count MBA (+ CNT-A CNT-B))
			(store-atom MBA)

			; Delete the B-MemberLink.  If its not deleteable,
			; then wipe out the count on it.
			(if (not (cog-delete! MEMB-B))
				(set-count MEMB-B 0))
		)
		(cog-incoming-by-type CLB 'MemberLink))

	; Now merge connectors, if that was asked for.
	(when MRG-CON
		(rebalance-shapes LLOBJ CLA CLB clique)
	)

	; Cleanup after merging.
	; The LLOBJ is assumed to be just a stars object, and so the
	; intent of this clobber is to force it to recompute it's left
	; and right basis.
	(define e (make-elapsed-secs))
	(LLOBJ 'clobber)
	(remove-empty-sections LLOBJ CLA MRG-CON)
	(remove-empty-sections LLOBJ CLB MRG-CON) ; This should remove ALL of them!

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)

	; Delete the old class... But first, let's make sure it is
	; really is empty!  It should not appear in any sections or
	; cross-sections!  It might appear in Connectors that are in
	; ConnectorSeqs that are in marginals, and we cannot control
	; that. XXX FIXME These need to be cleaned up!
	; So check the types we can control.
	(if (or
			(not (equal? 0 (cog-incoming-size-by-type CLB 'Section)))
			(not (equal? 0 (cog-incoming-size-by-type CLB 'CrossSection)))
			(not (equal? 0 (cog-incoming-size-by-type CLB 'Shape))))
		(throw 'non-empy-class 'merge-clusters "we expect it to be empty!"))

	(cog-delete! CLB)

	(format #t "------ Merge-Clusters: Cleanup ~A in ~5F secs\n"
		(cog-name CLA) (e))
)

; ---------------------------------------------------------------

; XXX TODO once make-merge-majority is done, this can be reimplemented
; as a special case of that. That means that the above three functions
; can be discarded. It also means that `gram-class-api` 'make-cluster
; method can be discarded or refactored.
;
; This requires that `make-merge-majority` be modified to support
; FRAC-FUN.
(define-public (make-merge-pair STARS FRAC-FN NOISE STORE FIN MRG-CON)
"
  make-merger-pair STARS FRAC-FN NOISE STORE FIN MRG-CON --
  Return object that implements the `merge-project` merge style
  (as described at the top of this file).

  STARS is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  FRAC-FUN is a function that takes two rows in STARS and returns a
  number between 0.0 and 1.0 indicating what fraction of a row to merge,
  when the corresponding matrix element in the other row is null.

  NOISE is a floating-point numeric value indicating a count, below
  which a merge will always be made. That is, if the count on the
  donating section is less than this value, then that section will
  be merged in it's entirety (ignoring the value returned by FRAC-FUN.)

  STORE is an extra function called, after the merge is to completed,
  and may be used to compute and store additional needed data that
  the algo here is unaware of. This include computation of supports,
  marginal MI and similar. It is called with an argument of the altered
  row.

  FIN is an extra function called, after the merge is to completed.
  It is called without an argument.

  MRG-CON is #t if Connectors should also be merged.  This requires
  that the STARS object have shapes on it.
"
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
		; most basic support for WA, WB and cls and their disjuncts.
		; The MI similarity also needs MM^T to be recomputed; the STORE
		; callback provides an opporunity to do that.
		; The results are stored, so that everything is on disk in
		; case of a restart.
		;
		; Clobber first, since the STORE callback need correct stars
		; and duals in order to recompute marginals correctly.
		(STARS 'clobber)
		(cond
			((and wa-is-cls wb-is-cls)
				(merge-clusters STARS WA WB MRG-CON))
			((and (not wa-is-cls) (not wb-is-cls))
				(begin
					(start-cluster STARS cls WA WB FRAC-FN NOISE MRG-CON)
					(STORE cls)))
			(wa-is-cls
				(merge-into-cluster STARS WA WB FRAC-FN NOISE MRG-CON))
			(wb-is-cls
				(merge-into-cluster STARS WB WA FRAC-FN NOISE MRG-CON))
		)

		(STORE WA)
		(STORE WB)
		(FIN)
		cls
	)

	; Return the above function
	merge
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
; (define pcos (add-similarity-compute psa))
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
