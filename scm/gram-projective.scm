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
; TODO: This code implements the "projection merge" strategy, but the
; latest results indicate that this does not improve quality all that
; much, if at all. So the latest merge style `comi` just disables
; the projection merge by setting the union-frac to zero.  Thus, the
; code in this file could be simplified by ripping out all the
; union-merge stuff.  Anyway, the democratic-vote idea will require
; explicit lists of disjuncts to merge, so this needs a rewrite, anyway.
;
; XXX Unless we decide to union-in anything below the noise-threshold
; cutoff, in which case we want to continue doing unions, just to handle
; this particular case.
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
; TODO
; ----
; It might be useful to move the management of the MemberLink's to
; the `add-gram-class-api` object.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
; Debugging prints which print Section Cross-Sections in a short-hand.
; This short-hand is used extensively in the unit tests.

(define (prt-word W)
	(define t (cog-type W))
	(cond
		((equal? t 'WordClassNode) (format #f "{~A}" (cog-name W)))
		((equal? t 'WordNode) (cog-name W))
		((equal? t 'VariableNode) "$")
	)
)

(define (prt-conseq LST)
	(string-concatenate
		(map (lambda (CON)
			(format #f " ~A~A" (prt-word (gar CON)) (cog-name (gdr CON))))
			LST))
)

(define (prt-section SECT)
	(format #f "~6,3F * (~A, ~A)"
		(cog-count SECT)
		(prt-word (gar SECT))
		(prt-conseq (cog-outgoing-set (gdr SECT))))
)

(define (prt-cross-section XSECT)
	(format #f "~6,3F * [~A, <~A, ~A>]"
		(cog-count XSECT)
		(prt-word (gar XSECT))
		(prt-word (gar (gdr XSECT)))
		(prt-conseq (cdr (cog-outgoing-set (gdr XSECT)))))
)

(define-public (prt-element ELT)
	(if (equal? (cog-type ELT) 'Section)
		(prt-section ELT)
		(prt-cross-section ELT))
)

(define-public (prt-element-list LST)
	(string-concatenate
		(map (lambda (ELT)
			(format #f "~A\n" (prt-element ELT)))
			LST))
)

; ---------------------------------------------------------------------

;; XXX This should not be public/exported, except that the unit tests
;; need this.
(define-public (accumulate-count LLOBJ ACC PAIR FRAC)
"
  accumulate-count LLOBJ ACC PAIR FRAC -- Accumulate count
    from PAIR into ACC.

  ACC and PAIR should be two pairs in the matrix LLOBJ. (Usually,
  they will be in the same row or column, although this code does not
  assume this.)

  The count on PAIR will be transfered to ACC, with some caveats:
  If the count on ACC is non-zero, then *all* of the count on PAIR
  will be transfered (and PAIR will be removed from the database).

  If the count on ACC is zero, then only a FRAC of the count will
  get transfered to ACC.

  Both Atoms, with updated counts, are stored to the database.

  The prototypical use-case has ACC and PAIR being two Sections
  of (word, disjunct) pairs, having the same disjunct but two different
  words. The goal is to merge the two words together into a single
  word-class.
"

	; The counts on the accumulator and the pair to merge.
	(define mcnt (LLOBJ 'get-count PAIR))
	(define acnt (LLOBJ 'get-count ACC))

	; Return #t if the count is effectively zero.
	; Use an epsilon for rounding errors.
	(define (is-zero? cnt) (< cnt 1.0e-10))

	; If the accumulator count is zero, transfer only a FRAC of
	; the count into the accumulator.
	(define taper-cnt (* FRAC mcnt))

	; Update the count on the donor pair.
	; Avoid touching the database if the count is zero;
	; the donor will be deleted later on.
	(define (update-donor-count SECT CNT)
		(set-count SECT CNT)
		(unless (is-zero? CNT) (store-atom SECT)))

	; If there is nothing to transfer over, do nothing.
	(when (not (is-zero? taper-cnt))

		; The accumulated count
		(set-count ACC (+ acnt taper-cnt))
		(store-atom ACC) ; save to the database.

		; Decrement the equivalent amount from the donor pair.
		(update-donor-count PAIR (- mcnt taper-cnt))
	)

	; Return how much was transfered over.
	taper-cnt
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
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	(define monitor-rate (make-rate-monitor))

	; Accumulated count on the MemberLink.
	(define accum-cnt 0)

	; Caution: there's a "feature" bug in assignment when used with
	; connector merging. The code below will create sections with
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
		(lambda (PAIR-A)
			(monitor-rate #f)
			(set! accum-cnt (+ accum-cnt
				(CLIQUE LLOBJ CLS PAIR-A accumulate-count)))
		)
		(LLOBJ 'right-stars WA))

	; Create MemberLinks. Do this before the connector-merge step,
	; as they are examined during that phase.
	(define memb-a (MemberLink WA CLS))

	; Track the number of observations moved from WA to the class.
	(define old-cnt (get-count memb-a))
	(set-count memb-a (+ old-cnt accum-cnt))
	(store-atom memb-a)

	(monitor-rate
		"------ Assign: Merged ~A sections in ~5F secs; ~6F scts/sec\n")
)

; ---------------------------------------------------------------------

(define (merge-connectors LLOBJ CLS WA CLIQUE)
"
  merge-connectors LLOBJ CLS WA CLIQUE --

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

  Accumulated row totals are stored in the MemberLink that attaches
  WA to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	(define monitor-rate (make-rate-monitor))

	; XXX FIXME Can we get rid of the FRAC?

	; SECT ends up being PAIR-A in the loop below.
	; MRGECT is the matching merger section
	(define (reshape OBJ MRGECT SECT FRAC)
		(reshape-merge OBJ CLS MRGECT WA SECT FRAC)
	)

	(for-each
		(lambda (PAIR-A)
			(monitor-rate #f)
			(CLIQUE LLOBJ CLS PAIR-A reshape)
		)
		(LLOBJ 'right-stars WA))

	(monitor-rate
			"------ Assign: Revised ~A shapes in ~5F secs; ~6F scts/sec\n")

	(set! monitor-rate (make-rate-monitor))
	(monitor-rate #f)
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
  contributing columns are removed (as thier count is now zero).
  If one is zero, and the other is not, then only a FRAC of the count
  is transfered.

  Accumulated row totals are stored in the two MemberLinks that attach
  WA and WB to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN WA WB))

	; CLUST is identical to CLS, always.
	(define (clique LLOBJ CLUST SECT ACC-FUN)
		(define WRD (LLOBJ 'left-element SECT))
		(define DJ (LLOBJ 'right-element SECT))
		(define WOTHER (if (equal? WRD WA) WB WA))
		(define OTHSEC (LLOBJ 'get-pair WOTHER DJ))
		(if (nil? OTHSEC)
			(if (<= (LLOBJ 'get-count SECT) NOISE)
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT 1.0)
				(if (< 0 frac-to-merge)
					(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT frac-to-merge)))
			(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT 1.0)
		)
	)

	(assign-to-cluster LLOBJ CLS WA clique)
	(assign-to-cluster LLOBJ CLS WB clique)

	(when MRG-CON
		(merge-connectors LLOBJ CLS WA clique)
		(merge-connectors LLOBJ CLS WB clique)
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
  all of the count from WA is transfered to CLS. That column in WA
  is removed (as it's count is now zero). If the count on CLS is zero,
  then only a FRAC of WA's count is transfered.

  Accumulated row totals are stored in the MemberLink that attaches
  WA to CLS.

  This assumes that storage is connected; the updated counts are written
  to storage.
"
	; Fraction of non-overlapping disjuncts to merge
	(define frac-to-merge (FRAC-FN CLS WA))

	; CLUST is identical to CLS, always.
	(define (clique LLOBJ CLUST SECT ACC-FUN)
		(define WRD (LLOBJ 'left-element SECT))
		(define DJ (LLOBJ 'right-element SECT))
		(define CLS-SECT (LLOBJ 'get-pair CLUST DJ))
		(if (nil? CLS-SECT)
			(if (<= (LLOBJ 'get-count SECT) NOISE)
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT 1.0)
				(if (< 0 frac-to-merge)
					(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT frac-to-merge)))
			(ACC-FUN LLOBJ CLS-SECT SECT 1.0)
		)
	)

	(assign-to-cluster LLOBJ CLS WA clique)

	(when MRG-CON
		(merge-connectors LLOBJ CLS WA clique)
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
  be transfered to CLA, and CLB will be removed.

  See start-cluster for additional details.
"
	; CLUST is identical to CLA, always.
	(define (clique LLOBJ CLUST SECT ACC-FUN)
		(define WRD (LLOBJ 'left-element SECT))
		(define DJ (LLOBJ 'right-element SECT))
		(define CLS-SECT (LLOBJ 'make-pair CLUST DJ))
		(ACC-FUN LLOBJ CLS-SECT SECT 1.0)
	)

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
		(merge-connectors LLOBJ CLA CLB clique)
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

; XXX TODO once make-merge-ingroup is done, this can be reimplemented
; as a special case of that. That means that the above three functions
; can be discarded. It also means that `gram-class-api` 'make-cluster
; method can be discarded or refactored.
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
		; most basic support for WA, WB and cls and thier disjuncts.
		; The MI similarity also needs MM^T to be recomputed; the STORE
		; callback provides an opporunity to do that.
		; The results are stored, so that everything is on disk in
		; case of a restart.
		; Clobber first, since Sections were probably deleted.
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

(define-public (make-merge-ingroup LLOBJ QUORUM MRG-CON)
"
  make-merger-ingroup LLOBJ QUORUM MRG-CON --
  Return a function that will merge a list of words into one class.
  The disjuncts that are selected to be merged are those shared by
  the majority of the given words, where `majority` is defined as
  a fraction that is greater or equal to QUORUM.

  LLOBJ is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  QUORUM is a floating point number indicating the fraction of
  sections that must share a given disjunct, before that disjunct is
  merged into the cluster.

  MRG-CON is #t if Connectors should also be merged.  This requires
  that the LLOBJ object have shapes on it.
"
	; WLIST is a list of WordNodes and/or WordClassNodes
	; Return a WordClassNode that is the result of the merge.
	(define (merge WLIST)
		(for-each
			(lambda (WRD)
				(if (equal? (cog-type WRD) 'WordClassNode)
					(throw 'not-implemented 'make-merge-ingroup
						"Not done yet")))
			WLIST)

		; The minimum number of sections that must exist for
		; a given disjunct.
		(define vote-thresh
			(inexact->exact (round (* QUORUM (length WLIST)))))

		; Return #t if the DJ is shared by the majority of the
		; sections. Does the count exceed the threshold?
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						(if (nil? (LLOBJ 'get-section WRD DJ)) 0 1))
					0
					WLIST)))

		; Merge the particular DJ, if it is shared by the majority.
		; CLUST is identical to cls, defined below.
		(define (clique LLOBJ CLUST SECT ACC-FUN)
			(define DJ (LLOBJ 'right-element SECT))

			(if (vote-to-accept? DJ)
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ)  SECT 1.0)))

		; We are going to control the name we give it. We could also
		; delegate this to `add-gram-class-api`, but for now, we're
		; going to punt and do it here. Some day, in a generic framework,
		; this will need to be cleaned up.
		(define cls-name (string-join (map cog-name WLIST)))
		(define cls-type (cog-type (LLOBJ 'cluster-type)))
		(define cls (cog-new-node cls-type cls-name))

		(for-each
			(lambda (WRD) (assign-to-cluster LLOBJ cls WRD clique))
			WLIST)

		(when MRG-CON
			(for-each
				(lambda (WRD) (merge-connectors LLOBJ cls WRD clique))
				WLIST)
		)

		; Cleanup after merging.
		; The LLOBJ is assumed to be just a stars object, and so the
		; intent of this clobber is to force it to recompute it's left
		; and right basis.
		(define e (make-elapsed-secs))
		(LLOBJ 'clobber)
		(for-each
			(lambda (WRD) (remove-empty-sections LLOBJ WRD))
			WLIST)
		(remove-empty-sections LLOBJ cls)

		; Clobber the left and right caches; the cog-delete! changed things.
		(LLOBJ 'clobber)

		(format #t "------ merge-ingroup: Cleanup ~A in ~5F secs\n"
			(cog-name cls) (e))

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
