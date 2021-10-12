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
; * `accumulate-count`, for transfering counts from one basis elt to
;   another.
; * `assign-to-cluster`, which loops over a vector, and calls a callback
;   on each basis elt. In most cases, this callback then will
;   eventually call `accumulate-count` to perform the actual count
;   transer.
; * `merge-connectors`, which loops over a vector, and calls the
;   reshape function on the basis elements, so that the connectors
;   therein can be adjusted and merged as appropriate.
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

; XXX This is a generic function. It should probably be moved over
; to the core matrix code-base. Along with this should be a creation
; of an object-specific 'set-count method.
;
; XXX TODO: a default 'get-count could be provided by the stars
; object, so that the base object does not need to provide it!
;
(define-public (accumulate-count LLOBJ ACC DONOR FRAC)
"
  accumulate-count LLOBJ ACC DONOR FRAC -- Accumulate a fraction
    FRAC of the count from DONOR into ACC.

  ACC and DONOR should be two pairs in the matrix LLOBJ.

  FRAC should be a numeric fraction, between 0.0 and 1.0.

  A fraction FRAC of the count on DONOR will be transfered to ACC.
  Both Atoms, with updated counts, are stored to the database, with
  one exception: if the final DONOR count is zero, it is NOT stored
  in the database. It is assumed that some later step will be deleting
  it, so we avoid a pointless store.
"
	; Return #t if the count is effectively zero.
	; Use an epsilon for rounding errors.
	(define (is-zero? cnt) (< cnt 1.0e-10))

	; The counts on the accumulator and the pair to merge.
	(define donor-cnt (LLOBJ 'get-count DONOR))
	(define frac-cnt (* FRAC donor-cnt))
	(define rem-cnt (- donor-cnt frac-cnt))

	; If there is nothing to transfer over, do nothing.
	(when (not (is-zero? frac-cnt))

		; The accumulated count
		(set-count ACC (+ frac-cnt (LLOBJ 'get-count ACC)))
		(store-atom ACC) ; save to the database.

		; Update the count on the donor pair.
		; Avoid touching the database if the count is zero;
		; the donor will be deleted later on.
		(set-count DONOR rem-cnt)
		(unless (is-zero? rem-cnt) (store-atom DONOR))
	)

	; Return how much was transfered over.
	frac-cnt
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
; Example usage (none)
