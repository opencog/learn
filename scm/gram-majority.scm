;
; gram-majority.scm
;
; Merge N vectors at a time into a new cluster. Merge basis elements by
; majority democratic vote.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` and `gram-projective.scm` for an overview.
;
; Given N vectors that have been selected to form a cluster, one can
; determine what basis elements should be a part of that cluster by
; looking to see what all the vectors have in common. If the majority
; of the vectors share a particular basis element, then all of them
; should contribute that element to the cluster.
;
; This is termed "democratic voting" since a majority concept is used,
; and each vector gets one vote. (Future extensions might consider
; proportional votes?) This idea really only works if N>2 as voting
; between two contributors does not make really make sense.
;
; make-merge-majority
; -------------------
; Merge N items into a brand new cluster.
;
; An older implementation which merged items together in a pairwise
; fashion can be found in the file `attic/gram-pairwise.scm`. That code
; provided three distinct merge methods: (a) merge two items into a brand
; new class; (b) merge one item into an existing class (c) merge two
; existing classes.  That code has been retired because the code below
; accomplishes the above.  Note, however, that the handling of case
; (b) and (c) below is a bit ad hoc, and perhaps could be ... modified,
; or reverted.  All of these cases are tested in the unit tests, so any
; changes would need to be done so as to not break the unit tests.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

(define-public (count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST)
"
  count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST -- Return a count
  of the number of connector sequences which are shared by a majority
  of the words in WORD-LIST. Actually, return a list of two numbers:
  this count and the total connector sequences appearing in common in
  all of the words.  Dividing these two numbers gives a generalized form
  of the Jaccard similarity between all of the words in the WORD-LIST.

  The majority is determined by QUORUM, which should be a floating-
  point number between 0.0 and 1.0.

  ConnectorSeq's with a count of less than NOISE are ignored.

  This function is a kind-of Jaccard distance between multiple words
  (two or more).  The conventional (unweighted) Jaccard distance is
  defined only for pairs of items. The generalization is done by
  counting to see if a fraction QUORUM is shared. Setting QUORUM to
  1.0, and applying the function to two items returns the conventional
  Jaccard distance.
"
	; The threshold, the minimum number of sections that must exist
	; for a given disjunct. For a list of length two, both must share
	; that disjunct (thus giving the traditional overlap merge).
	(define wlen (length WORD-LIST))
	(define vote-thresh
		(if (equal? wlen 2) 2
			(inexact->exact (round (* QUORUM wlen)))))

	; The group-similarity function uses a strictly-less-than compare.
	; So, to be shared by two, its enough to set low-bnd to one.
	(define low-bnd (- vote-thresh 1))

	; ((make-group-similarity LLOBJ) 'mutual-col-supp low-bnd WORD-LIST)
	((make-group-similarity LLOBJ) 'noise-col-supp low-bnd NOISE WORD-LIST)
)

(define-public (make-class-node LLOBJ WLIST)
"
  make-class-node LLOBJ WLIST - create a node suitable for merging WLIST

  The WLIST is assumed to be a list of Nodes (i.e. Atoms having string
  names) which will be merged to form a class. This function creates a
  unique name for that merge class, and then uses the LLOBJ to create
  that class node.

  XXX TODO: move this so its a method on `add-gram-class-api`.
"
	(define cls-name (string-join (map cog-name WLIST)))
	(define cls-type (LLOBJ 'cluster-type))
	(define cls-typname
		(if (cog-atom? cls-type) (cog-name cls-type) cls-type))

	; Make sure we've never created this name before.
	(define (mknode cname)
		(if (nil? (cog-node cls-typname cname))
			(cog-new-node cls-typname cname)
			(mknode (string-append cname ".i"))
		))
	(mknode cls-name)
)

(define*-public (make-merge-majority LLOBJ QUORUM NOISE
	#:optional (MRG-CON #t) (FRAC 0))
"
  make-merger-majority LLOBJ QUORUM NOISE [MRG-CON FRAC] --
  Return a function that will merge a list of words into one class.
  The disjuncts that are selected to be merged are those shared by
  the majority of the given words, where `majority` is defined as
  a fraction that is greater or equal to QUORUM.

  LLOBJ is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  QUORUM is a floating point number indicating the fraction of members
  that must share a given disjunct, before that disjunct is merged into
  the cluster.

  NOISE is a count, such that if a ConnectorSeq has a count less
  than or equal to this, it will always be merged, irrespective of
  the majority vote. In short, small differences between members
  are ignored, and lumped up into the quirkness of the group.

  MRG-CON is an optional argument, defaulting to #t if not specified.
  Set this to #t to indicate that Connectors should also be merged.
  For this to work, the LLOBJ object must have shapes on it.

  FRAC is an optional argument, defaulting to 0. A non-zero argument
  is the fraction of of a disjunct to merge, if the quorum election
  fails.  This is used primarily in the unit tests, only.

  The returned function has the following signature:
     (merge CLASS WORD-LIST)
  where the items in WORD-LIST will be merged into the CLASS. The CLASS
  should be an Atom of type ItemClassNode (or WordClassNode). In the
  usual case, the WORD-LIST is a list of ItemNodes (or WordNodes). The
  merge decision is made on a disjunct-by-disjunct basis, using a
  majority vote mechanism, as described elsewhere.

  In addition to the above case, there are two rather ad hoc special
  cases handled here. One special cases is the merge of a single item
  into an existing class. The other special cases is the merge of two
  classes into one.  These cases are tested in unit tests originally
  developed for pair-wise merge. The handling of these two cases seems
  to be appropriate; however, they're not well-motivated. That is, maybe
  they could be changed?

  Anyway, the new clustering code, as currently written, does not attempt
  to merge two existing classes together, nor does it attempt to merge a
  single item into an existing class. So ... these policies can be changed.
  Perhaps we need to separate this ad hoc policy from the mechanism.
"
	; WLIST is a list of ItemNodes and/or ItemClassNodes that will be
	; merged into CLASS.
	(define (merge CLASS WLIST)

		; We need to distinguish individual items, from item classes.
		; Item classes should be of type ItemClassNode, but its easier
		; to just do the below.
		(define class-type (cog-type CLASS))

		; If two classes are being merged, then the counts from one class
		; must be moved to the other. Thiis utility copies those counts.
		(define (move-count FROM-CLASS)
			(for-each (lambda (MEMB)
				(define new-memb (MemberLink (gar MEMB) CLASS))
				(cog-inc-count! new-memb (get-count MEMB))
				(cog-delete! MEMB))
				(cog-incoming-by-type FROM-CLASS 'MemberLink)))

		; Add words to cluster *before* starting merge!
		; This is needed, as the merge will look for these links.
		(for-each (lambda (WRD)
			(if (equal? class-type (cog-type? WRD))
				(move-count WRD)
				(MemberLink WRD CLASS)))
			WLIST)

		; ---------------------------------------------------
		; The minimum number of sections that must exist for
		; a given disjunct. For a list of length two, both
		; must share that disjunct (thus giving the traditional
		; overlap merge).
		(define wlen (length WLIST))
		(define vote-thresh
			(if (<= wlen 2) 2
				(inexact->exact (round (* QUORUM wlen)))))

		; If there is only one word in the word-list, then assume that
		; the user wants to merge this one word into an existing class.
		; In this case, the class itself gets a vote as to inclusion.
		; As of right now, this is used only in the unit tests, and it
		; is used only with a non-zero FRAC value.
		(define voter-list
			(if (equal? 1 wlen) (cons CLASS WLIST) WLIST))

		; Return #t if the DJ is shared by the majority of the
		; sections. Does the count exceed the threshold?
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						; XXX TODO this should be either
						; (if (< 0 (LLOBJ 'pair-count WRD DJ)) ...)
						; or it should be
						; (if (< NOISE (LLOBJ 'pair-count WRD DJ)) ...)
						; to be fully compatible with `count-shared-conseq`
						; However, simply checking for presence is the most
						; tolerant, accepting way of voting, so we allow that.
						(if (nil? (LLOBJ 'get-pair WRD DJ)) CNT (+ 1 CNT)))
					0
					voter-list)))

		; ---------------------------------------------------
		; Collect up all disjuncts into one place.
		; The main loop will loop over these.
		(define dj-set (make-atom-set))
		(for-each
			(lambda (WRD)
				(for-each
					(lambda (PAIR) (dj-set (LLOBJ 'right-element PAIR)))
					(LLOBJ 'right-stars WRD)))
			WLIST)
		(define dj-list (dj-set #f))

		; ---------------------------------------------------
		(define (make-flat CLUST SECT)
			(if MRG-CON (LLOBJ 'make-flat CLUST SECT) SECT))

		(define (update-memb-count WRD CLS CNT)
			(cog-inc-count! (MemberLink WRD CLS) CNT))

		; Merge the particular DJ, if it is shared by the majority,
		; or if the count on it is below the noise floor.
		; Return zero if there is no merge.
		;
		; The `is-nonflat?` test is perhaps funny-looking. It returns #t if
		; any connector in SECT uses CLASS. If so, then CLASS will be used
		; consistently. This is not obviously "correct", but does seem to
		; make sense in a way. The unit test `connector-merge-triconind.scm`
		; does check this with 0 < FRAC < 1.
		;
		; When merging two classes into one, just accept all disjuncts on
		; the class to be merged. This is a kind-of ad-hoc, unmotivated action
		; that seems to be an OK thing to do, for now. Cause why not? This is
		; tested in the `class-merge-basic.scm` unit test.
		(define (do-merge WRD DJ ACCEPT)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(when (not (nil? SECT))
				(let* ((merge-full
							(or ACCEPT
								(<= (LLOBJ 'get-count SECT) NOISE)
								(LLOBJ 'is-nonflat? CLASS SECT)
								(equal? class-type (cog-type WRD))))
						(frakm (if merge-full 1.0 FRAC)))
					(when (< 0 frakm)
						(update-memb-count WRD CLASS
							(accumulate-count LLOBJ (make-flat CLASS SECT) SECT frakm))))))

		; Perform the merge a given disjunct, or not
		(define (merge-dj DJ)
			(define have-majority (vote-to-accept? DJ))
			(for-each
				(lambda (WRD) (do-merge WRD DJ have-majority))
				WLIST))

		; Given a specific Section, transport the counts on it to
		; the CrossSections that can be derived from it.
		(define (do-rebalance WRD DJ)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(when (not (nil? SECT))
				(rebalance-merge LLOBJ (make-flat CLASS SECT) SECT)))

		(define (rebalance-dj DJ)
			(for-each (lambda (WRD) (do-rebalance WRD DJ)) WLIST))

		; Loop over disjuncts, handling the Sections only.
		; Any remaining CrossSections not handled in this loop
		; are handled out-of-line, below.
		(for-each
			(lambda (DJ)
				(when (equal? 'ConnectorSeq (cog-type DJ))
					(merge-dj DJ)
					(if MRG-CON (rebalance-dj DJ))))
			dj-list)

		; ---------------------------------------------------
		; After the above loop has run, all of the Sections have been
		; merged, as well as all of the CrossSections that can be derived
		; from them. Now, we want to handle all the remaining CrossSections
		; that were not handled above.
		;
		; To do this, we need to perform a set-subtraction: figure out
		; what's been done, and what remains.

		; First, figure out what DJ's have been done already.
		(define done-djs (make-atom-set))
		(define (do-record WRD DJ)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(when (not (nil? SECT))
				(for-each (lambda (XRS)
					(done-djs (LLOBJ 'right-element XRS)))
					(LLOBJ 'get-cross-sections SECT))))

		(define (record-cross DJ)
			(for-each
				(lambda (WRD) (do-record WRD DJ))
				WLIST))

		; Main loop, to record all the DJ's that have already
		; been handled.
		(for-each
			(lambda (DJ)
				(when (equal? 'ConnectorSeq (cog-type DJ))
					(done-djs DJ)
					(record-cross DJ)))
			dj-list)

		; Subtract the ones that are done. The list of left-overs
		; should be a list consisting entirely of CrossSections.
		(define left-overs (atoms-subtract
			dj-list (done-djs #f)))

		; Loop over the remaining CrossSections, and merge them.
		; The loop must be run twice, since there may be multiple
		; Crosses from a given Section, and rebalancing too soon
		; throws off the merging.
		(for-each merge-dj left-overs)
		(for-each rebalance-dj left-overs)

		; ---------------------------------------------------
		; Cleanup after merging.
		; The LLOBJ is assumed to be just a stars object, and so the
		; intent of this clobber is to force it to recompute it's left
		; and right basis.
		(define e (make-elapsed-secs))
		(LLOBJ 'clobber)
		(for-each
			(lambda (WRD) (remove-empty-sections LLOBJ WRD MRG-CON))
			WLIST)
		(remove-empty-sections LLOBJ CLASS MRG-CON)

		; Clobber the left and right caches; the cog-delete! changed things.
		(LLOBJ 'clobber)

		(format #t "------ merge-majority: Cleanup `~A` in ~A secs\n"
			(cog-name CLASS) (e))
	)

	; Return the above function
	merge
)

; ---------------------------------------------------------------
; Example usage  (none)
