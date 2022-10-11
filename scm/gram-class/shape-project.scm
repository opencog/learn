;
; shape-project.scm
;
; Merge connectors based on linear shape merges.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The creation of clusters is done via a "linear" projective merge of
; vectors that encode Sections. See top of `gram-classification.scm` for
; an overview of the general idea of linear projective merging.
;
; Recall that the "germ" of a Section is the first element of the
; section, the "vertex". The rest of the Section is a ConnectorSeq.
; A "vector" is a single germ, and all of the Sections on it.
; Linear (projective) merging refers to the idea that, to merge two
; vectors, one need only merge their germs into a common class
; (cluster). A new vector is created, having that cluster as the germ,
; and including parts of the vectors of the two donor germs. The
; mechanical process of creating the new vector is implemented in
; `gram-majority.scm`. An older variant is in `attic/gram-pairwise.scm`.
;
; In more abstract, formal mathematical terms, projective merging is
; the projection of a sheaf to a base-space; or rather, the projection
; of a local collection of sheaf sections down to a single base-point.
; Informally, it is a kind of "flattening" of the covering space to a
; linearized vector space at the base point. Rather than attempting to
; develop a formal definition of this flattening, the rest of the
; documentation tries to stay informal and hand-wavey. However, if
; you know what a fiber bundle is, or if you know what a covering space
; is, try to keep that in mind: the projection described here is a kind
; of oddball variation on that. It would be an excellent exercise to
; write down the formalities for this, some rainy day.
;
; A problem arises when Sections contain Connectors in which the
; donating germs appear. How should this be handled? There is certainly
; a rich variety of choices, which then easily lead to confusing
; questions. To minimize the confusion, a principle of linearity is
; invoked.  This principle leads to the idea of Shapes and
; CrossSections. These last two are described in `shape-vec.scm`
;
; Thus, the concept of a vector of Sections sitting on a germ is
; extended to a vector of Sections and CrossSections on that germ.
; This is a kind of "direct sum" of the two vectors. The goal is
; to continue to do merging as before, a linear projection of two
; vectors onto a common cluster, making use of the CrossSections to
; provide a suggestion of how germs in Connectors should be merged.
;
; Basic Merging Example
; ---------------------
; Consider the following projective merge, taken from the
; `connector-merge-basic.scm` unit test:
;
;    (e, abc) + (j, abc) -> ({ej}, abc)
;    (e, dgh) + (j, dgh) -> ({ej}, dgh)
;    (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
; "p" is the fraction of the count on the original donor section
; that should be moved to the merged vector. Note that this is where
; the "linearity" comes in: counts are additive, and p + (1-p) = 1
; i.e. the total count is preserved; it is only redistributed between
; the vectors.
;
; The above has NO germs in any of the connectors, and so merging the
; corresponding sections is straight-forward.
;
; CrossSection Merging Example
; ----------------------------
; Given the above 5 Sections (3 for "e" and 2 for "j"), they may be
; exploded into 15 CrossSections, 3 each for the 5 total Sections.
;
; For example, (e, abc) explodes to
;
;    [a, <e, vbc>]   and  [b, <e, avc>]  and  [c, <e, abv>]
;
; where [] denotes the CrossSection, and <> denotes the Shape. The "v"
; is the variable node in the Shape (that the germ of the cross-section
; plugs into).
;
; Of the 15 CrossSections, none of them have "e" or "j" as a germ,
; and therefore, none of them contribute to the vectors to be merged.
; However, they all have Shapes whose point is a germ.
;
; Insofar as CrossSections are secondary and are derived from the
; Sections, self-consistency suggests that they should stay consistent
; with the results of the merger of the Sections. Thus, post-merger,
; the number of Sections should reduce to 12 = 3x4, with 9 of them
; having {ej} as the point, and 3 more having just {e} as the point.
; Counts should update as well.
;
; Connector Merging Example
; -------------------------
; Consider the following projective merge, taken from the
; `connector-merge-cons.scm` unit test:
;
;    (e, abc) + (j, abc) -> ({ej}, abc)
;    (e, dgh) + (j, dgh) -> ({ej}, dgh)
;    (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;     none    + (j, abe) -> p * ({ej}, abx) + (1-p) * (j, aby)
;     none    + (j, egh) -> p * ({ej}, zgh) + (1-p) * (j, wgh)
;
; Here, the germ "j" has two sections with contain "e" as a connector.
; After merging, it is less than clear as to what x,y,z,w should be.
; There are several possibilities:
;
; 1)   x and  y could both be "e"
; 2)   x and  y could both be {ej}
; 3)   x could be {ej} and y could be just "e"
;
; In addition, although "p" was written as the merge fraction on the
; last two, it is not obviously the correct fraction.
;
; For guidance, lets look at what is happening with the cross-sections.
; The Section (j, abe) has three cross-sections on it:
;
;    [a, <j, vbe>]   and  [b, <j, ave>]  and  [e, <j, abv>]
;
; We are interested only in the last cross-section, as it is the only
; one on the vector for the germ "e". The merge for this looks like:
;
;     none + [e, <j, abv>] -> p * [{ej}, <j, abv>] + (1-p) * [e, <j, abv>]
;
; Given the above post-merge form, we can now reconstruct the
; corresponding post-merge Sections. The are
;
;     [{ej}, <j, abv>]  => (j, ab{ej})
;     [e, <j, abv>]  => (j, abe)
;
; and so this reconstruction recommends option 3:
;
;     none + (j, abe) -> p * (j, ab{ej}) + (1-p) * (j, abe)
;
; Compare to the earlier direct merge of the Sections:
;
;     none + (j, abe) -> p * ({ej}, abe) + (1-p) * (j, abe)
;
; Ugh. Clearly, the merge proceedre is non-commutative. Now what?  Well...
;
; Diary entry for "April-May 20201 ...Non-Commutivity, Again... Case B"
; discusses what to do, and why to do it that way. The conclusion is
; that the right answer, here is to create a section
;    p * ({ej}, ab{ej})
; and to zero out the other two.
;
; The code below implements the above. It also handles the more complex
; cases, where ConnectorSeqs may have multiple connectors in them
; needing to be merged (to be "flattened"). The code below works, and
; is tested by seven unit tests. My gut feeling is that the code can be
; simplified, but I haven't found this yet. It's as complicated as it
; is, because the handling of shapes/cross-sections is "non-linear" in
; a hard-to-define way. It does not feel like the concept is clearly
; expressed, just yet...
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

(define-public (prt-dj-list LST)
	(string-concatenate
		(map (lambda (ELT)
			(format #f "~A\n" (prt-dj ELT)))
			LST))
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

(define (rebalance-count LLOBJ SECTION CNT)
"
  rebalance-count LLOBJ SECTION CNT - set count on section and crosses.

  The SECTION is presumed to be some section on which the observation
  count was adjusted (possibly even set to zero.) This function
  enforces 'detailed balance', making sure that the CrossSections
  corresponding to SECTION have the same count.

  If the count isn't zero, then the SECTION is store to the database.
  If it is zero, then it's likely that a later stage will delete it,
  so a pointless store is avoided.
"
	(define (is-zero? cnt) (< cnt 1.0e-10))

	(set-count SECTION CNT)
	(if (not (is-zero? CNT)) (store-atom SECTION))
	(for-each
		(lambda (XST) (set-count XST CNT))
		(LLOBJ 'make-cross-sections SECTION))
)

; ---------------------------------------------------------------------

(define (rebalance-merge LLOBJ MRG DONOR)
"
  rebalance-merge LLOBJ MRG DONOR - Readjust counts on CrossSections

  After a DONOR section has been merged into the MRG section, assorted
  CrossSections may be left in inconsistent states. This rebalances all
  counts on both Sections and CrossSections.

  FRAC is ignored; its for API compatilibility.
"
	(define is-sect (equal? 'Section (cog-type DONOR)))

	(define mrg (if is-sect MRG (LLOBJ 'make-section MRG)))
	(define don (if is-sect DONOR (LLOBJ 'make-section DONOR)))

	(LLOBJ 'make-cross-sections mrg)
	(rebalance-count LLOBJ mrg (LLOBJ 'get-count MRG))
	(rebalance-count LLOBJ don (LLOBJ 'get-count DONOR))
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
		(rebalance-merge LLOBJ ACC DONOR)
	)

	; Return how much was transferred over.
	moved
)

; ---------------------------------------------------------------

; XXX TODO -- generic deletion should be moved to a method
; on the base object -- probably to add-pair-stars. The extra
; stuff like deleting crosses belongs in the shape-vec API.
(define (remove-empty-sections LLOBJ ROW RMX)
"
  remove-empty-sections LLOBJ ROW RMX -- scan the ROW for Sections and
  call cog-delete! on those that have an zero count. If RMX is #t, then
  the corresponding CrossSections will also be deleted.
"
	(define ns 0)
	(define nx 0)

	(define (is-zero? cnt) (< cnt 1.0e-10))

	; If the count in Section is zero, delete it.
	; Also scan all of it's crosses. Crosses aren't normally stored in
	; the DB, so we just extract them.
	(define (del-sect SEC)
		; Cleanup cross sections, if they are provided.
		(when RMX
			(for-each (lambda (xst)
				(define shp (LLOBJ 'right-element xst))
				; Cross-sections shouldn't have been stored ... and
				; yet they often are. So cog-extract! is NOT enough.
				(cog-delete! xst)
				; Shapes store marginals, so if they are deleteable, delete them.
				(cog-delete! shp)  ;; Safe, its not recursive.
				(set! nx (+ 1 nx)))
			(LLOBJ 'get-cross-sections SEC)))
		(define csq (LLOBJ 'right-element SEC))
		(cog-delete! SEC)
		(cog-delete! csq)  ;; Safe; because its not recursive.
		(set! ns (+ 1 ns)))

	(define (del-xes XST)
		; Perhaps its been deleted already? This will happen if a
		; connector occurs twice in a section: it will then have two
		; cross-sections. The first deletes the section, the second
		; finds the section is already deleted.
		(define sect (LLOBJ 'get-section XST))
		(if (not (nil? sect)) (del-sect sect))
		(set! nx (+ 1 nx)))

	; Cleanup after merging.
	(for-each
		(lambda (ITEM)
			(if (and (cog-atom? ITEM) (is-zero? (LLOBJ 'get-count ITEM)))
				(cond
					((eq? 'Section (cog-type ITEM)) (del-sect ITEM))
					((eq? 'CrossSection (cog-type ITEM)) (del-xes ITEM))
					(else
						(throw 'assert 'remove-empty-sections "Its broken")))
				))
		(LLOBJ 'right-stars ROW))

	; After doing the above, we might find that ROW appears in
	; Connectors that are not used anywhere.  Delete those connectors.
	(for-each cog-delete! (cog-incoming-by-type ROW 'Connector))

	; After doing the above, we may find that ROW has no more users,
	; anywhere, except in the marginals. We should clean those up
	; too, except we don't know what the marginals are. Alas!
	; FIXME but how?

	; (format #t "Deleted ~A secs, ~A crosses for ~A" ns nx ROW)
)

(define-public (remove-all-empty-sections LLOBJ WRD-LIST)
"
  remove-all-empty-sections LLOBJ WRD-LIST -- Cleanup after merging.

  Remove all Sections and CrossSections with a zero count.
"
	(define MRG-CON #t)
	(for-each
		(lambda (WRD) (remove-empty-sections LLOBJ WRD MRG-CON))
		WRD-LIST)

	; Clobber the left and right caches; the cog-delete! changed things.
	(LLOBJ 'clobber)
)

; ---------------------------------------------------------------------
; Example usage (none)
; ---------------------------------------------------------------------
