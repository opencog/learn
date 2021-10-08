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
(use-modules (opencog nlp learn))

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
;xxxx
; We expect 15 to be created, 3 each for the 5 total Sections.
; For example, (e, abc) explodes to
;    [a, <e, vbc>]   and  [b, <e, avc>]  and  [c, <e, abv>]
; where [] denotes the CrossSection, and <> denotes the Shape. The "v"
; is the variable node in the Shape (that the germ of the cross-section
; plugs into).
;
; These should stay consistent with the merged sections. i.e. these
; should reduce to 12=9+3 grand-total, with appropriate counts.
; 9 of them will have {ej} as the point, and 3 will have "e" as the
; point.

(define t-start-cluster "simple two-cluster merge test")
(test-begin t-start-cluster)

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

#! ================
; Create CrossSections and verify that they got created
(gsc 'explode-sections)
(test-equal 15 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 2 (length (gsc 'right-stars (Word "g"))))
(test-equal 2 (length (gsc 'right-stars (Word "h"))))

; Should not be any CrossSections on e,j; should be same as before.
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))

; We expect a total of 3+2=5 Sections
(test-equal 5 (length (cog-get-atoms 'Section)))

; --------------------------
; Merge two sections together.
(define frac 0.25)
(define disc (make-fuzz gsc 0 frac 4 0))
(disc 'merge-function (Word "e") (Word "j"))

; We expect just one section remaining on "e", the klm section.
(test-equal 1 (length (gsc 'right-stars (Word "e"))))

; We expect no sections remaining on j
(test-equal 0 (length (gsc 'right-stars (Word "j"))))

; We expect three merged sections
(test-equal 3 (length (gsc 'right-stars (WordClassNode "e j"))))

; Of the 5 original Sections, 4 are deleted, and 3 are created,
; leaving a grand total of 4. The 3 new ones are all e-j, the
; remaining old one is an "e" with a reduced count.  This is just
; the sum of the above.
(test-equal 4 (length (cog-get-atoms 'Section)))

; Of the 15 original CrossSections, 12 are deleted outright, and three
; get thier counts reduced (the e-klm crosses). A total of 3x3=9 new
; crosses get created, leaving a grand-total of 12.
(test-equal 12 (length (cog-get-atoms 'CrossSection)))

; --------------
; Validate counts.
; For example:
(define epsilon 1.0e-8)
(test-approximate (* cnt-e-klm (- 1.0 frac))
	(cog-count (car (gsc 'right-stars (Word "e")))) epsilon)

; To gain access to the counts, load them by name.
(expected-e-j-sections)
(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count sec-ej-abc) epsilon)
(test-approximate (+ cnt-e-dgh cnt-j-dgh) (cog-count sec-ej-dgh) epsilon)
(test-approximate (* frac cnt-e-klm) (cog-count sec-ej-klm) epsilon)
(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count sec-e-klm) epsilon)

; Validate counts on select CrossSections...
(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count xes-b-ej-avc) epsilon)
(test-approximate (* frac cnt-e-klm) (cog-count xes-k-ej-vlm) epsilon)
(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

; -----------------------
; Verify detailed balance
(test-assert (check-sections gsc epsilon))
(test-assert (check-crosses gsc epsilon))

; Verify no change in totals
(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
	epsilon)
========== !#

(test-end t-start-cluster)

; ---------------------------------------------------------------
