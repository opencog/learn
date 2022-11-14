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
; the number of Sections should reduce to 12 = 3x4, wiht 9 of them
; having {ej} as the point, and 3 more having just {e} as the point.
; Counts should update as well.
;
; The function `merge-crosses` below maintains this correspondence, for
; the simple case.
;
; Connector Merging Example
; -------------------------
; Consider the following projective merge, taken from the
; `connector-merge-full.scm` unit test:
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
; and so this reconstruction recommends:
;
;     none + (j, abe) -> p * (j, ab{ej}) + (1-p) * (j, abe)
;
; Compare to the earlier direct merge of the Sections:
;
;     none + (j, abe) -> p * ({ej}, abe) + (1-p) * (j, abe)
;
; Ugh. Non-commutative. Now what???
;
; Diary entry for "April-May 20201 ...Non-Commutivity, Again... Case B"
; discusses what to do, and why to do it that way. The conclusion is
; that the right answer, here is to create a section
;    p * ({ej}, ab{ej})
; and to zero out the other two.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

(define (merge-connectors LLOBJ CLS WRD)
"
unfinished prototype

  Get the row for CLS, walk the row, merge connectors on it.
"

(define nsec 0)
(define msec 0)
(format #t "\n")
(format #t "=============================\n")
(format #t "merge ~A into ~A\n" WRD CLS)
	; The entire vector associated with the cluster CLS
	(define all-stars (LLOBJ 'right-stars CLS))

(format #t "there are ~A sections and ~A cross of ~A\n"
(length (filter (lambda (ITEM) (eq? 'Section (cog-type ITEM))) all-stars))
(length (filter (lambda (ITEM) (eq? 'CrossSection (cog-type ITEM))) all-stars))
(length all-stars))

	; Does the word appear in the connector CON?
	(define (word-in-connector? CON)
		(equal? (gar CON) WRD))

	; ------------------------------------------------------------------
	; Create a list of CrossSections, appearing in the all-stars vector,
	; that donated WRD to the cluster. These CrossSections corespond to
	; Sections (that are typically not in the stars) having WRD in a
	; Connector.
	(define donor-xes
		(filter-map (lambda (ITEM)
			(if (eq? 'CrossSection (cog-type ITEM))
				(let ((donor (cog-link 'CrossSection WRD (gdr ITEM))))
					(if (nil? donor) #f donor))
				#f))
			all-stars))

(format #t "WRD appears as connector in ~A crosses\n"
(length donor-xes))

	; Create a predicate to test if a CrossSection for WRD
	; was merged into the CLS cluster.
	; Usage: (is-merged-xsect? some-sect)
	(define is-merged-xsect?  (make-aset-predicate donor-xes))

	; Although the Section SEC may contain WRD in one of it's connectors,
	; that does NOT mean that WRD should be replace by CLS. That
	; replacement is to be performed only if the corresponding
	; CrossSection contributed to CLS. The code below searches for
	; those CrossSections, and if found, performs the substitution.
	; It returns the updated section.
	(define (revise-section SEC DONOR)

		; List of donating cross-sections.
		(define mumble
			(filter is-merged-xsect? (LLOBJ 'get-cross-sections DONOR)))

		; A list of all the locations in the Sections ConnectorSeq
		; that need to be replaced with the merged class.
		(define location-list
			(map
				(lambda (XSECT)
					; The list of connectors in the shape.
					(define conli (cdr (cog-outgoing-set (gdr XSECT))))
					(list-index
						(lambda (CON)
							(eq? 'VariableNode (cog-type (gar CON))))
						conli))
				mumble))

(set! msec (+ 1 msec))
		; Are there any substitutions to be made? If so, then substitute.
		(if (null? location-list) '()
			(let* (
					; The list of connectors in the Section SEC
					(conli (cog-outgoing-set (gdr SEC)))
					(idx 0)
					(next (car location-list))
					(rest (cdr location-list))

					; The revised list, after substitution.
					(newli
						(map
							(lambda (CON)
								(define jdx idx)
								(set! idx (+ 1 idx))
								(if (eq? jdx next)
									(begin
										(when (not (null? rest))
											(set! next (car rest))
											(set! rest (cdr rest)))
										(Connector CLS (gdr CON)))
									CON))
							conli))

					; A copy of the Section SEC with substituted connectors.
					(newsec (Section (gar SEC) (ConnectorSeq newli))))

				; Transfer the counts over to the new Section.
				(set-count newsec (LLOBJ 'get-count SEC))
				(set-count SEC 0)

				; Return the new section.
				newsec))
	)

	; ------------------------------------------------------------------
	; Similar to `revise-section`, for a CrossSection.
	; the given CrossSection. More precisely:
	; Although the CrossSection XST may contain WRD in one of it's
	; connectors, that does NOT mean that WRD should be replaced by CLS.
	; That replacement is to be performed only if the corresponding
	; Section contributed to CLS. The code below searches for that
	; Section, and if found, performs the substitution. It returns the
	; updated CrossSection.
	(define (revise-xsect XST)
; xxxxx
; todo
		#f
	)

	; ------------------------------------------------------------------

	; Create a list of all CrossSections, obtainable from Sections in
	; the all-stars vector, that donated WRD to the cluster. These
	; CrossSections (obviously coresponding to Sections in the stars)
	; have WRD as the point of the Shape.
	(define donor-sex-shape
		(append-map (lambda (ITEM)
			(if (eq? 'Section (cog-type ITEM))
				(let ((donor (cog-link 'Section WRD (gdr ITEM))))
					(if (nil? donor) '()
						(LLOBJ 'get-cross-sections donor)))
				'()))
			all-stars))

	; Given a CrossSection XST having WRD as the point of the Shape,
	; create a new CrossSection having CLS as the point of the Shape.
	; Transfer all counts from the old CrossSection to the new one.
	(define (revise-shape XST)
		(define newx (CrossSection (gar XST)
				(Shape CLS (cdr (cog-outgoing-set (gdr XST))))))

		; Transfer the counts over to the new CrossSection.
		(set-count newx (LLOBJ 'get-count XST))
		(set-count XST 0))

	(for-each revise-shape donor-sex-shape)

	; ------------------------------------------------------------------
	; Revise the Section that is obtained from the given CrossSection.
	; The given CrossSection should be a merged CrossSection (i.e. having
	; CLS as its point). If WRD was a donor to it, then there is some
	; Section where WRD appears in a Connector, and that WRD should be
	; replaced by CLS in that Section. Update that Section.
	(define (revise-sect-from-xsect XST)
		; Create the donating CrossSection; we need this,
		; so as to find the Section it came from.
		(define donor (cog-link 'CrossSection WRD (gdr XST)))

		; The Section from whence it came, or null.
		(when (not (nil? donor))
			(let ((donor-sect (LLOBJ 'get-section donor)))
				(when (not (nil? donor-sect))
					(revise-section donor-sect donor-sect)
; (format #t "duuude XST=~A revised=~A\n" XST mrg-sect)
; (throw 'need-merge 'merge-connectors "working on it")
			)))
	)

	; Handle the simple case, where the CrossSection does not
	; have WRD in it. For this case, just transfer the count
	; from the originating Section to the Section that has
	; CLS in place of WRD.  Note that XST already has CLS as
	; it's germ.
	(define (transfer-xsect XST)
		; Create the donating CrossSection; we need this,
		; so as to find the Section it came from.
		(define donor (cog-link 'CrossSection WRD (gdr XST)))

		; The Section from whence it came, or null.
		(when (not (nil? donor))
			(let ((orig-sect (LLOBJ 'get-section donor))
					(new-sect (LLOBJ 'make-section XST)))
				(if (nil? orig-sect)
					(throw 'bug 'merge-connectors "donor section is missing"))
				(set-count new-sect (LLOBJ 'get-count orig-sect))
				(set-count orig-sect 0))))

	; Merge the connectors in Section Sec, if needed.
	; A merge is needed if WRD appears in any connector in SEC.
	; A merge is possible only if the WRD donated counts to SEC.
	(define (do-merge-sectn SEC)
		(define conseq (gdr SEC))
		(define conli (cog-outgoing-set conseq))
		(define need-merge (any word-in-connector? conli))
(set! nsec (+ 1 nsec))
		(define donor (cog-link 'Section WRD conseq))
		(when (and (not (nil? donor)) need-merge)
			(revise-section SEC donor))
	)

	; Same as above, but for cross-sections.
	(define (do-merge-xsect XST)
		(define shape (gdr XST))
		(define allseq (cog-outgoing-set shape))
		(define conli (cdr allseq))
		(define need-merge (any word-in-connector? conli))
		(if need-merge
			(revise-sect-from-xsect XST)
			(transfer-xsect XST)
		)
	)

	; Same as above, dispatching on the type.
	(define (do-merge-cons ITEM)
		(let ((ptype (cog-type ITEM)))
			(cond
				((eq? ptype 'Section) (do-merge-sectn ITEM))
				((eq? ptype 'CrossSection) (do-merge-xsect ITEM))
				(else (throw 'bad-pair-type 'merge-connectors
						"Unexpected pair type for merging!")))))

	; Loop over all sections and cross-sections associated with
	; the newly created/expanded cluster.
	(for-each do-merge-cons all-stars)

(format #t "in conclusion sections handled ~A of ~A for ~A in ~A\n" msec nsec WRD CLS)
)

; ---------------------------------------------------------------------
