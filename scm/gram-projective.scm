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
						(if (nil? (LLOBJ 'get-pair WRD DJ)) 0 1))
					0
					WLIST)))

		; Merge the particular DJ, if it is shared by the majority.
		; CLUST is identical to cls, defined below. Return zero if
		; there is no merge.
		(define (clique LLOBJ CLUST SECT ACC-FUN)
			(define DJ (LLOBJ 'right-element SECT))

			(if (vote-to-accept? DJ)
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ)  SECT 1.0)
				0))

		; We are going to control the name we give it. We could also
		; delegate this to `add-gram-class-api`, but for now, we're
		; going to punt and do it here. Some day, in a generic framework,
		; this will need to be cleaned up.
		(define cls-name (string-join (map cog-name WLIST)))
		(define cls-type (LLOBJ 'cluster-type))
		(define cls-typname
			(if (cog-atom? cls-type) (cog-name cls-type) cls-type))
		(define cls (cog-new-node cls-typname cls-name))

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

		(format #t "------ merge-ingroup: Cleanup `~A` in ~A secs\n"
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
