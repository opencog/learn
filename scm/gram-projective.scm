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
; Debugging prints which print Section Cross-Sections in a short-hand.
; This short-hand is used extensively in the unit tests.

(define-public (prt-word W)
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

(define (prt-shape SHAPE)
	(format #f "<~A, ~A>"
		(prt-word (gar SHAPE))
		(prt-conseq (cdr (cog-outgoing-set SHAPE))))
)

(define-public (prt-dj DJ)
	(if (equal? (cog-type DJ) 'ShapeLink)
		(prt-shape DJ)
		(prt-conseq (cog-outgoing-set DJ)))
)

(define (prt-section SECT)
	(format #f "~6,3F * (~A, ~A)"
		(cog-count SECT)
		(prt-word (gar SECT))
		(prt-conseq (cog-outgoing-set (gdr SECT))))
)

(define (prt-cross-section XSECT)
	(format #f "~6,3F * [~A, ~A]"
		(cog-count XSECT)
		(prt-word (gar XSECT))
		(prt-shape (gdr XSECT)))
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
	; set-count ATOM CNT - Set the raw observational count on ATOM.
	; XXX FIXME there should be a set-count on the LLOBJ...
	; Strange but true, there is no setter, currently!
	(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

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
	; XXX FIXME Use atomic accumulate-count here.
	(define memb-a (MemberLink WA CLS))
	(define old-cnt (get-count memb-a))
	(set-count memb-a (+ old-cnt accum-cnt))
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
; Example usage (none)
