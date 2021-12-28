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

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

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

	; CLUST is identical to CLS, always.
	(define (clique xLLOBJ CLUST SECT ACC-FUN)
		(define WRD (LLOBJ 'left-element SECT))
		(define DJ (LLOBJ 'right-element SECT))
		(define WOTHER (if (equal? WRD WA) WB WA))
		(define OTHSEC (LLOBJ 'get-pair WOTHER DJ))

		; If OTHSEC is nill, then it is the non-overlap case, and
		; only a fraction is merged.  Otherwise, all is merged.
		(if (nil? OTHSEC)
			(if (<= (LLOBJ 'get-count SECT) NOISE)
				(ACC-FUN LLOBJ (LLOBJ 'make-flat CLUST SECT MRG-CON) SECT 1.0)
				(if (< 0 frac-to-merge)
					(ACC-FUN LLOBJ (LLOBJ 'make-flat CLUST SECT MRG-CON) SECT frac-to-merge)))
			(ACC-FUN LLOBJ (LLOBJ 'make-flat CLUST SECT MRG-CON) SECT 1.0)
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
	(remove-empty-sections LLOBJ WA)
	(remove-empty-sections LLOBJ WB)
	(remove-empty-sections LLOBJ CLS)

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
	(define frac-m (FRAC-FN CLS WA))

	; CLUST is identical to CLS, always.
	(define (clique xLLOBJ CLUST SECT ACC-FUN)
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
				(ACC-FUN LLOBJ (LLOBJ 'make-flat CLUST SECT MRG-CON) SECT 1.0)
				(if (< 0 frac-m)
					(ACC-FUN LLOBJ (LLOBJ 'make-flat CLUST SECT MRG-CON) SECT frac-m)))
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
	(remove-empty-sections LLOBJ WA)
	(remove-empty-sections LLOBJ CLS)

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
	; CLUST is identical to CLA, always.
	(define (clique xLLOBJ CLUST SECT ACC-FUN)
		(define DJ (LLOBJ 'right-element SECT))
		(define MSECT (LLOBJ 'make-pair CLUST DJ))
		(define CLS-SECT (LLOBJ 'make-flat CLUST MSECT MRG-CON))
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
	(remove-empty-sections LLOBJ CLA)
	(remove-empty-sections LLOBJ CLB) ; This should remove ALL of them!

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
