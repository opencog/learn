;
; class-merge-basic.scm
; Unit test for merging of Connectors - merge two classes.
;
; Tests merging of two word classes, one into the other. This is
; a basic test of the simplest case.
;
; Created October 2021

(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "class-data.scm")

; ---------------------------------------------------------------
;
; This diagram explains what is being tested here:
;
;    ({ej}, abc) + ({rs}, abc) -> ({ej}, abc)
;    ({ej}, klm) +     none    -> ({ej}, klm)
;        none    + ({rs}, dgh) -> ({ej}, dgh)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
;
; What about the CrossSections?
; We expect 12 to be created, 3 each for the 4 total Sections.
; For example, ({ej}, abc) explodes to
;    [a, <{ej}, vbc>]   and  [b, <{ej}, avc>]  and  [c, <{ej}, abv>]
; where [] denotes the CrossSection, and <> denotes the Shape. The "v"
; is the variable node in the Shape (that the germ of the cross-section
; plugs into).
;
; These should stay consistent with the merged sections. i.e. these
; should reduce to 9=12-6+3 grand-total, with appropriate counts.
; All 9 of them will have {ej} as the point, none will have {rs} as
; the point.

(define t-class-merge "simple two-cluster merge test")
(test-begin t-class-merge)

; Open the database
(setup-database)

; Load some data
(setup-ej-sections)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define gsc (add-covering-sections pca))

; Verify that the data loaded correctly
; We expect 2 sections on "ej" and two on "rs"
(test-equal 2 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 2 (length (gsc 'right-stars (WordClass "r s"))))

; Get the total count on all Sections
(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

; Create CrossSections and verify that they got created
(gsc 'explode-sections)
(test-equal 12 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 1 (length (gsc 'right-stars (Word "g"))))
(test-equal 1 (length (gsc 'right-stars (Word "h"))))

; Should not be any CrossSections on e,j; should be same as before.
(test-equal 2 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 2 (length (gsc 'right-stars (WordClass "r s"))))

(test-equal 2 (length (gsc 'right-stars (Word "a"))))
(test-equal 2 (length (gsc 'right-stars (Word "b"))))

; We expect a total of 2+2=5 Sections
(test-equal 4 (length (cog-get-atoms 'Section)))

; --------------------------
; Merge two sections together.
(merge gsc (WordClass "e j") (WordClass "r s") 0)

; We expect three sections on "e", the abc, dgh and klm sections.
(test-equal 3 (length (gsc 'right-stars (WordClass "e j"))))

; We expect no sections remaining on rs
(test-equal 0 (length (gsc 'right-stars (WordClass "r s"))))


; Of the 4 original Sections, 2 are deleted, and 1 is created,
; leaving a grand total of 3. The new one is (ej,dgh).
; The deleted ones are the two rs sections.
(test-equal 3 (length (cog-get-atoms 'Section)))

; Of the 12 original CrossSections, 6 are deleted outright, three
; get their counts incremented, and three get created.
(test-equal 9 (length (cog-get-atoms 'CrossSection)))

; --------------
; Validate counts.
; For example:
(define epsilon 1.0e-8)
(test-approximate cnt-ej-klm
	(cog-count (car (gsc 'right-stars (WordClass "e j")))) epsilon)

; To gain access to the counts, load them by name.
(expected-ej-sections)
(test-approximate (+ cnt-ej-abc cnt-rs-abc) (cog-count sec-ej-abc) epsilon)
(test-approximate cnt-rs-dgh (cog-count sec-ej-dgh) epsilon)
(test-approximate cnt-ej-klm (cog-count sec-ej-klm) epsilon)

; Validate counts on select CrossSections...
(test-approximate (+ cnt-ej-abc cnt-rs-abc) (cog-count xes-b-ej-avc) epsilon)
(test-approximate cnt-ej-klm (cog-count xes-k-ej-vlm) epsilon)
(test-approximate cnt-rs-dgh (cog-count xes-d-ej-vgh) epsilon)

; -----------------------
; Verify detailed balance
(test-assert (check-sections gsc epsilon))
(test-assert (check-crosses gsc epsilon))
(test-assert (check-shapes gsc epsilon))

; Verify no change in totals
(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
	epsilon)

(test-end t-class-merge)

; ---------------------------------------------------------------
(opencog-test-end)
