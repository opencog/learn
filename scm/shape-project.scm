;
; shape-project.scm
;
; Merge connectors based on linea shape merges.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The creation of clusters is done via a "linear" projective merge of
; vectors that encode Sections. See top of `gram-projective.scm` for
; an overview of the general idea of linear projective merging.
;
; Recall that the "germ" of a Section is the first element of the
; section, the "vertex". The rest of the Section is a ConnectorSeq.
; A "vector" is a single germ, and all of the Sections on it.
; Linear (projective) merging refers to the idea that, to merge two
; vectors, one need only merge thier germs into a common class
; (cluster). A new vector is created, having that cluster as the germ,
; and including parts of the vectors of the two donor germs. The
; mechanical process of creating the new vector is implemented in
; `gram-projective.scm`.
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
; the "linearily" comes in: counts are additive, and p + (1-p) = 1
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

(define (rebalance-count LLOBJ SECTION CNT)
"
  rebalance-count LLOBJ SECTION CNT - set count on section and crosses.

  The SECTION is presumed to be some section on which the observation
  count was adjusted (possibly even set to zero.) This function
  enforces 'detailed balance', making sure that the CrossSections
  corresponding to SECTION have the same count.
"
	(set-count SECTION CNT)
	(store-atom SECTION)
	(for-each
		(lambda (XST) (set-count XST CNT))
		(LLOBJ 'get-cross-sections SECTION))
)

; ---------------------------------------------------------------------

(define (merge-resects LLOBJ GLS W XMR XDON)
"
  merge-resects - Merge Sections corresponding to CrossSection XMR

  XMR is assumed to be a cross-section having GLS as it's germ,
  XDON is assumed to be a cross-section having W as it's germ, and
  counts have already been transfered approprately from XDON to XMR.
  That is, its assumed the node W has been merged into the cluster
  node GLS already.

  This function adjusts counts on the corresponding sections that
  arise from XDON and XMR. If the germ of the Section arising from
  XMR belongs to GLS, then a revised Section is created, having GLS
  as the germ.
"
	(define resect (LLOBJ 'make-section XMR))
	(define germ (LLOBJ 'left-element resect))
	(define mgsf (LLOBJ 'flatten GLS resect))

	; This is confusing ... can't we just call accumulate-count?
	; (accumulate-count LLOBJ mgs donor FRAC NOISE)
	; ???
	(if (nil? (cog-link 'MemberLink germ GLS))
		(let ((donor (LLOBJ 'make-section XDON))
				(mgs (if mgsf mgsf resect))
				(d-cnt (LLOBJ 'get-count XDON))
				(x-cnt (LLOBJ 'get-count XMR)))

			(when mgsf
				(rebalance-count LLOBJ resect 0)
				(LLOBJ 'make-cross-sections mgs))
			(rebalance-count LLOBJ mgs x-cnt)
			(rebalance-count LLOBJ donor d-cnt)
		)
		(let* ((reg (if mgsf mgsf
					(LLOBJ 'make-pair GLS (LLOBJ 'right-element resect))))
				(r-cnt (LLOBJ 'get-count reg)))

			(set-count XMR 0)
			; Create the cross-sections corresponding to `regs`
			(for-each
				(lambda (xfin) (set-count xfin r-cnt))
				(LLOBJ 'make-cross-sections reg))
	))
)

; ---------------------------------------------------------------------

(define (reshape-merge LLOBJ GLS MRG W DONOR FRAC NOISE)
"
  reshape-merge LLOBJ GLS MRG W DONOR FRAC NOISE

  Merge connectors on the Section/CrossSection MRG.
  This creates a set of 'consistent' Sections/CrossSections such that
  these contain the merged cluster node GLS in the appropriate places
  as germs, points or in connectors. The problem addressed here is that
  the ordinary linear projective merge does correctly create the merged
  Sections/CrossSections with GLS as the germ, but leaves behind a
  mish-mash of the raw unmerged word W to appear in various Shapes (as
  the point) and in various Connectors. These need to be harmonzed so
  that W is replaced by GLS in all the right places, with counts updated
  appropriately.

  When this completes, the resulting Sections/CrossSections are fully
  self-consistent. The inconsistent Sections/CrossSections will have
  thier counts set to zero, and need to be deleted in a subsequent pass.
"
	(define donor-type (cog-type DONOR))

	(when (equal? 'Section donor-type)
		(let ((flat (LLOBJ 'flatten GLS MRG)))
			(if flat
				; If MRG can be flattened, then transfer all counts
				; from MRG to the flattened variant thereof.
				(begin
					(rebalance-count LLOBJ flat
						(+ (LLOBJ 'get-count flat) (LLOBJ 'get-count MRG)))
					(rebalance-count LLOBJ MRG 0)
					(rebalance-count LLOBJ DONOR (LLOBJ 'get-count DONOR))
				)

				; If MRG does not need flattening, then ...
				; Loop over donating cross-sections.
				(begin
					(for-each
						(lambda (XST)
							(define xmr (LLOBJ 're-cross GLS XST))
							(accumulate-count LLOBJ xmr XST FRAC NOISE))
						(LLOBJ 'make-cross-sections DONOR))

						; We can rebalance here, but it does not seem to
						; affect anything.
						; (rebalance-count LLOBJ DONOR (LLOBJ 'get-count DONOR))

						; Special case: donor was already non-flat. We need
						; to transfer all of the counts over.
						(when (LLOBJ 'is-nonflat? GLS MRG)
							(set-count MRG
								(+ (LLOBJ 'get-count MRG) (LLOBJ 'get-count DONOR)))
							(rebalance-count LLOBJ DONOR 0)))))

		; Always rebalance the merged section.
		(rebalance-count LLOBJ MRG (LLOBJ 'get-count MRG))
	)

	(when (equal? 'CrossSection donor-type)
		(merge-resects LLOBJ GLS W MRG DONOR))
)

; ---------------------------------------------------------------

(define (remove-empty-sections LLOBJ ROW)
"
  remove-empty-sections LLOBJ ROW -- scan the ROW for Sections &
  CrossSections and call cog-delete! on those that have an zero count.
  This will also delete the corresponding CrossSections.
"
	(define ns 0)
	(define nx 0)

	; If the count in Section is zero, delete it.
	; Also scan all of it's crosses. Crosses aren't normally stored in
	; the DB, so we just extract them.
	(define (del-sect SEC)
		(for-each (lambda (xst)
			(when (and (cog-atom? xst) (is-zero? (LLOBJ 'get-count xst)))
				(let ((shp (gdr xst)))
					(cog-extract! xst)
					(cog-extract! shp)
					(set! nx (+ 1 nx)))))
			(LLOBJ 'get-cross-sections SEC))
		(when (is-zero? (LLOBJ 'get-count SEC))
			(let ((csq (gdr SEC)))
				(cog-delete! SEC)
				(cog-delete! csq)
				(set! ns (+ 1 ns)))))

	(define (del-xes XST)
		(define sct (LLOBJ 'get-section XST))
		(when (and (cog-atom? sct) (is-zero? (LLOBJ 'get-count sct)))
			(del-sect sct))
		(when (and (cog-atom? XST) (is-zero? (LLOBJ 'get-count XST)))
			(let ((shp (gdr XST)))
				(cog-extract! XST)
				(cog-extract! shp)
				(set! nx (+ 1 nx)))))

	; Cleanup after merging.
	(for-each
		(lambda (ITEM)
			(if (cog-atom? ITEM)
				(cond
					((eq? 'Section (cog-type ITEM)) (del-sect ITEM))
					((eq? 'CrossSection (cog-type ITEM)) (del-xes ITEM))
					(else
						(throw 'remove-empty-sections 'assert "Its broken")))
				))
		(LLOBJ 'right-stars ROW))
	; (format #t "Deleted ~A secs, ~A crosses for ~A" ns nx ROW)
)

; ---------------------------------------------------------------------
