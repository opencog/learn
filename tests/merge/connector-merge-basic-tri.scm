;
; connector-merge-basic-tri.scm
; Unit test for merging of Connectors - basic, simple case.
;
; Tests merging of three words into a single word-class.
; The focus here is to make sure that the CrossSections are
; merged correctly, and specifically, that the "point" of the
; CrossSection has the cluster in it.
;
; None of the sections have the words to be merged as connectors;
; this makes it "basic", since the complex connector merging logic
; is not triggered.
;
; Created May 2021

(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "connector-data.scm")

; ---------------------------------------------------------------
;
; This diagram explains what is being tested here:
;
; The first two words are merged as before (in the basic test):
;    (e, abc) + (j, abc) -> ({ej}, abc)
;    (e, dgh) + (j, dgh) -> ({ej}, dgh)
;    (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;
; and then "f" is added:
;    ({ej}, abc) + (f, abc) -> ({ej}, abc)
;    ({ej}, dgh) + (f, dgh) -> ({ej}, dgh)
;    ({ej}, klm) + (f, klm) -> ({ej}, klm)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
; "p" is the fraction to merge == 0.25 hard-coded, below.
;
; What about the CrossSections?
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

; ---------------------------------------------------------------
;
; Test is more or less the same as the basic test, except that now
; a third word is added, to extend an existing cluster. Specifically,
; word "f" to the existing cluster "ej".
;
; This minor variation tests a different code branch than the
; initial cluster formation code.
;

(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-f-sections)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; Verify that the data loaded correctly
	; We expect 3 sections on "e" and two on "j"
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))
	(test-equal 3 (length (gsc 'right-stars (Word "f"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	(gsc 'explode-sections)
	(test-equal 24 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 3 (length (gsc 'right-stars (Word "g"))))
	(test-equal 3 (length (gsc 'right-stars (Word "h"))))

	; Should not be any CrossSections on e,j; should be same as before.
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))
	(test-equal 3 (length (gsc 'right-stars (Word "f"))))

	; We expect a total of 3+2+3=8 Sections
	(test-equal 8 (length (cog-get-atoms 'Section)))

	; --------------
	; Merge the first two sections together.
	(define frac 0.25)
	(merge gsc WA WB frac)
	(define WC-EJ (WordClassNode WC-NAME))

	; Verify detailed balance
	(define epsilon 1.0e-8)
	(test-equal 7 (length (cog-get-atoms 'Section)))
	(test-equal 21 (length (cog-get-atoms 'CrossSection)))
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))

	; Merge the third section.
	(merge gsc WC-EJ (Word "f") frac)

	; We expect just one section remaining on "e", the klm section.
	; We expect no sections remaining on j
	(test-equal 1 (length (gsc 'right-stars (Word "e"))))
	(test-equal 0 (length (gsc 'right-stars (Word "j"))))
	(test-equal 0 (length (gsc 'right-stars (Word "f"))))

	; We expect three merged sections
	(test-equal 3 (length (gsc 'right-stars WC-EJ)))

	; Of the 8 original Sections, 7 are deleted, and 3 are created,
	; leaving a grand total of 4. The 3 new ones are all e-j, the
	; remaining old one is an "e" with a reduced count.  This is just
	; the sum of the above.
	(test-equal 4 (length (cog-get-atoms 'Section)))

	; Of the 15 original CrossSections, 12 are deleted outright, and three
	; get their counts reduced (the e-klm crosses). A total of 3x3=9 new
	; crosses get created, leaving a grand-total of 12.
	(test-equal 12 (length (cog-get-atoms 'CrossSection)))

	; --------------
	; Validate counts.
	(test-approximate (* cnt-e-klm (- 1.0 frac))
		(cog-count (car (gsc 'right-stars (Word "e")))) epsilon)

	; Validate counts on the Sections...
	(expected-e-j-sections WC-EJ)
	(test-approximate (+ cnt-e-abc cnt-j-abc cnt-f-abc)
		(cog-count sec-ej-abc) epsilon)
	(test-approximate (+ cnt-e-dgh cnt-j-dgh cnt-f-dgh)
		(cog-count sec-ej-dgh) epsilon)
	(test-approximate (+ cnt-f-klm (* frac cnt-e-klm))
		(cog-count sec-ej-klm) epsilon)
	(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count sec-e-klm) epsilon)

	; Validate counts on select CrossSections...
	(test-approximate (+ cnt-e-abc cnt-j-abc cnt-f-abc)
		(cog-count xes-b-ej-avc) epsilon)
	(test-approximate (+ cnt-f-klm (* frac cnt-e-klm))
		 (cog-count xes-k-ej-vlm) epsilon)
	(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

	; -----------------------
	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-merge-into-cluster "simple merge-into-cluster test")
(test-begin t-merge-into-cluster)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-merge-into-cluster)

; ---------------------------------------------------------------
(opencog-test-end)
