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
		(if (<= wlen 2) 2
			(inexact->exact (round (* QUORUM wlen)))))

	; The group-similarity function uses a strictly-less-than compare.
	; So, to be shared by two, its enough to set low-bnd to one.
	(define low-bnd (- vote-thresh 1))

	; ((make-group-similarity LLOBJ) 'mutual-col-supp low-bnd WORD-LIST)
	((make-group-similarity LLOBJ) 'noise-col-supp low-bnd NOISE WORD-LIST)
)

; ---------------------------------------------------------------------

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

; ---------------------------------------------------------------------

(define*-public (make-merge-majority LLOBJ QUORUM NOISE
	#:optional (MRG-CON #t) (FRAC 0))
"
  make-merge-majority LLOBJ QUORUM NOISE [MRG-CON FRAC] --
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
  XXX At this time, setting this to #f does not work.

  FRAC is an optional argument, defaulting to 0. A non-zero argument
  is the fraction of of a disjunct to merge, if the quorum election
  fails.  This is currently used only in the unit tests, but is
  extensively tested there.

  The returned function has the following signature:
     (merge CLASS WORD-LIST)
  where the items in WORD-LIST will be merged into the CLASS. The CLASS
  should be an Atom of type ItemClassNode (or WordClassNode). In the
  usual case, the WORD-LIST is a list of ItemNodes (or WordNodes). The
  merge decision is made on a disjunct-by-disjunct basis, using a
  majority vote mechanism, as described elsewhere.

  In addition to the above case, there are two rather ad hoc special
  cases handled here. One special case is the merge of a single item
  into an existing class. The other special case is the merge of two
  classes into one.  These cases are tested in unit tests originally
  developed for pair-wise merge. The handling of these two cases seems
  to be appropriate; however, they're not well-motivated. That is, maybe
  they could be changed?

  Anyway, the new clustering code (i.e. the code that calls this sub-
  routine), as currently written, does not attempt to merge two existing
  classes together, nor does it attempt to merge a single item into an
  existing class. Thus, the above polcies are tested only in the unit
  tests, and these policies can be changed.  Perhaps we need to separate
  this ad hoc policy from the mechanism.
"
	; WLIST is a list of ItemNodes and/or ItemClassNodes that will be
	; merged into CLASS.
	(define (merge CLASS WLIST)

		; We need to distinguish individual items, from item classes.
		; Item classes should be of type ItemClassNode, but its easier
		; to just do the below.
		(define class-type (cog-type CLASS))

		; Add words to cluster *before* starting merge! This is needed,
		; as the merge will look for these links. The case where one of
		; the words is a class is handled at the very end.
		(for-each (lambda (WRD) (MemberLink WRD CLASS)) WLIST)

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
		(define (get-all-djs)
			(define dj-set (make-atom-set))
			(for-each
				(lambda (WRD)
					(for-each
						(lambda (PAIR) (dj-set (LLOBJ 'right-element PAIR)))
						(LLOBJ 'right-stars WRD)))
				WLIST)
			(dj-set #f))

		(define dj-list (get-all-djs))

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
		;
		; This returns the fraction actually merged, so that the caller can
		; find out if anything was actually done.
		(define (do-merge WRD DJ ACCEPT)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(if (not (nil? SECT))
				(let* ((merge-full
							(or ACCEPT
								(<= (LLOBJ 'get-count SECT) NOISE)
								(LLOBJ 'is-nonflat? CLASS SECT)
								(equal? class-type (cog-type WRD))))
						(frakm (if merge-full 1.0 FRAC)))
					(when (< 0 frakm)
						(update-memb-count WRD CLASS
							(accumulate-count LLOBJ (make-flat CLASS SECT) SECT frakm)))
					; Return fraction actually merged.
					frakm)
				; Return zero if no section.
				0))

		; Perform the merge a given disjunct, or not
		; Return a total count of how much was merged.
		(define (merge-dj DJ)
			(define have-majority (vote-to-accept? DJ))
			(fold
				(lambda (WRD SUM) (+ SUM (do-merge WRD DJ have-majority)))
				0 WLIST))

		; Given a specific Section, transport the counts on it to
		; the CrossSections that can be derived from it.
		(define (do-rebalance WRD DJ)
			(define SECT (LLOBJ 'get-pair WRD DJ))
			(when (not (nil? SECT))
				(rebalance-merge LLOBJ (make-flat CLASS SECT) SECT)))

		(define (rebalance-dj DJ)
			(for-each (lambda (WRD) (do-rebalance WRD DJ)) WLIST))

		; Keep track of what DJ's have been handled already.
		(define done-djs (make-atom-set))
		(define (record-done DJ)

			; Record the shapes built from the crosses
			(define (do-record WRD)
				(define SECT (LLOBJ 'get-pair WRD DJ))
				(when (not (nil? SECT))
					(for-each (lambda (XRS)
						(done-djs (LLOBJ 'right-element XRS)))
						(LLOBJ 'make-cross-sections SECT))))

			(done-djs DJ)
			(for-each do-record WLIST))

		; Loop over disjuncts, handling the Sections only.
		; Any remaining CrossSections not handled in this loop
		; are handled out-of-line, below.
		(define e (make-elapsed-secs))
		(define scnt 0)
		(define mscnt 0)
		(for-each
			(lambda (DJ)
				(when (equal? 'ConnectorSeq (cog-type DJ))
					(when (< 0 (merge-dj DJ))
						(record-done DJ)
						(set! mscnt (+ 1 mscnt)))
					(if MRG-CON (rebalance-dj DJ))
					(done-djs DJ) ; Record unconditionaly.
					(set! scnt (+ 1 scnt))))
			dj-list)

		; At the conclusion, store the counts on the MemberLinks
		(for-each (lambda (WRD) (store-atom (MemberLink WRD CLASS))) WLIST)

		(format #t "------ merge-majority: Merge ~D of ~D sections in ~A secs\n"
			mscnt scnt (e))

		; ---------------------------------------------------
		; After the above loop has run, all of the Sections have been
		; merged, as well as all of the CrossSections that can be derived
		; from them. Now, we want to handle all the remaining CrossSections
		; that were not handled above.
		(define d (make-elapsed-secs))
		;
		; To do this, we need to perform a set-subtraction. The list of
		; left-overs should consist entirely of CrossSections.
		(define left-overs (atoms-subtract
			dj-list (done-djs #f)))

		; Loop over the remaining CrossSections, and merge them.
		; We would like to do this:
		;    (for-each merge-dj left-overs)
		;    (for-each rebalance-dj left-overs)
		; but the above will mess up detailed balance. Getting this to
		; work right requires two tricks. The first trick is easy: do one
		; merge and balance at a time, and then scrub the left-over list
		; to remove the ones that have been handled already.
		;
		; The second trick is harder, because it involves order-dependence.
		; Also, there is no unit test for the second trick, because we
		; can't control the order dependence in the unit test.
		; It goes like this: Start with
		;    N (f, a- a+)  and  M (f, b- a+)
		; and merge a & b. The crosses are:
		;    N [a, <f, $- a+>] + M [b, <f, $- a+>] -- Yes, merge!
		;    N [a, <f, a- $+>] + 0 [b, <f, a- $+>] -- oh no!
		;    M [a, <f, b- $+>] + 0 [b, <f, b- $+>] -- oh no!
		; If we hit the first one, we merge and rebalance, and the merge
		; knocks out the next two (since the merge rebuilds the sections
		; and the cross-sections). But if either of the "oh no!"'s are
		; hit first, the merge is rejected, and the "Yes, merge!" case
		; is never considered, and so never happens. Hmm.  To fix this,
		; we reorder shapes so that the mergeable ones are always performed
		; first (via the `get-alt-shapes` function below).

		(define shape-done? (make-once-predicate))

		; Get all of the other shapes derviable from SHP.
		(define (get-alt-shapes SHP)
			(define alt-shp (make-atom-set))
			(for-each
				(lambda (WRD)
					(define XROS (LLOBJ 'get-pair WRD SHP))
					(if (not (nil? XROS))
						(let ((SECT (LLOBJ 'get-section XROS)))
							; Usually SECT should never be nil, but,
							; apparently, after some crashes & restarts,
							; it can be. Boooo.
							(if (not (nil? SECT))
								(let ((ALL-X (LLOBJ 'make-cross-sections SECT)))
									(for-each (lambda (CRS)
										(alt-shp (LLOBJ 'right-element CRS))) ALL-X))))))
				WLIST)

			; Maybe it's been done already?
			(filter (lambda (SH) (not (shape-done? SH))) (alt-shp #f)))

		; Given SHP, return a related shape, if that related shape is mergable.
		(define (mergable-shape? SHP)
			(if (vote-to-accept? SHP) SHP
				(find vote-to-accept? (get-alt-shapes SHP))))

		; Given SHP, merge it, or one of its related shapes, if any one
		; of them is mergable. Return count of how much was merged. The
		; return value is currently used only for stats reporting.
		(define (merge-shape SHP)
			(define alt-shp (mergable-shape? SHP))
			(define have-majority (not (nil? alt-shp)))
			(define mrg-shp (if (nil? alt-shp) SHP alt-shp))
			(define mcnt (fold
				(lambda (WRD SUM) (+ SUM (do-merge WRD mrg-shp have-majority)))
				0 WLIST))
			(rebalance-dj mrg-shp)
			mcnt)

		; Tail-recursive merge of a list of shapes.
		(define mshcnt 0)
		(define (merge-shapes SHL)

			; Perform the merge (if it hasn't been done yet)
			(define shape (car SHL))
			(if (not (shape-done? shape))
				(if (< 0 (merge-shape shape)) (set! mshcnt (+ 1 mshcnt))))

			(define rest (cdr SHL))
			(if (not (nil? rest)) (merge-shapes rest)))

		; Now actually do the merge.
		(if (not (nil? left-overs)) (merge-shapes left-overs))

		(format #t "------ merge-majority: Remaining ~A of ~A cross in ~A secs\n"
			mshcnt (length left-overs) (d))

		; If any of the merged items was a class, then transfer a
		; proportionate amount of the counts to new class. We do this
		; now, and not later, because the merged class may change later
		; on (it may grow or shrink). Unfortunately, this erases the
		; history of the merge.
		(define (move-count FROM-CLASS)

			; A list of all members of FROM-CLASS.
			(define sublist
				(filter (lambda (MEMB) (equal? (gdr MEMB) FROM-CLASS))
					(cog-incoming-by-type FROM-CLASS 'MemberLink)))

			(if (nil? sublist)
				(throw 'bad-membership 'merge-majority "Empty word class!"))

			; Get the total count on FROM-CLASS. This should be equal to
			; the marginal count. That is, it should equal
			;   ((add-support-api LLOBJ) 'right-count FROM-CLASS)
			(define old-count
				(fold (lambda (MEMB SUM) (+ SUM (cog-count MEMB)))
					0 sublist))

			(if (not (< 0 old-count))
				(throw 'bad-membership 'merge-majority "No counts on word class!"))

			; Get the total count transfered.
			(define dmemb (Member FROM-CLASS CLASS))
			(define new-count (cog-count dmemb))

			; How much of the old count was transfered.
			(define fract (/ new-count old-count))

			; Move a proportion of the counts from old to new.
			(for-each (lambda (FMEMB)
					(define fcnt (cog-count FMEMB))
					(define xfer (* fcnt fract))
					(store-atom (cog-inc-count! (MemberLink (gar FMEMB) CLASS) xfer))
					(store-atom (cog-inc-count! FMEMB (- xfer))))
				sublist)

			; Get rid of the class-membership.  This erases the history.
			; XXX It would be nice to preserve history somehow, but how?
			(cog-delete! dmemb))

		(for-each (lambda (WRD)
				(if (equal? class-type (cog-type WRD)) (move-count WRD)))
			WLIST)

		*unspecified*
	)

	; Return the above function
	merge
)

; ---------------------------------------------------------------
; Example usage:
; Merge two words into one class:
;
;    (define wclass (WordClass "foo"))
;    (define in-grp (list (Word "As") (Word "as")))
;    (define merge-majority (make-merge-majority star-obj 0.5 4 #t))
;    (merge-majority wclass in-grp)
;    (length (star-obj 'right-stars (WordClass "foo")))
;
; where star-obj includes shapes.
