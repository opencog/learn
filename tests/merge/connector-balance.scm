;
; connector-balance.scm
; Unit test for merging of Connectors - detailed balance.
;
; Tests merging of several words into a single word-class.
; The focus here is to make sure that detailed balance of the counts
; across the merged Sections and CrossSections are preserved (obeyed).
;
; Created Dec 2021

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "balance-data.scm")

; ---------------------------------------------------------------
;
; This diagram explains what is being tested here:
;
;    (a, gh) + (b, gh) -> ({ab}, gh)
;    (c, kaaam)        -> (c, k{ab}{ab}{ab}m)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
;
; What about the CrossSections?
; We expect 9 to be created, 2 each for the first two, and 5 for the last.
; For example, (a, gh) explodes to
;    [g, <a, vh>] and  [h, <a, gv>]
; where [] denotes the CrossSection, and <> denotes the Shape. The "v"
; is the variable node in the Shape (that the germ of the cross-section
; plugs into).
;
; The (c, kaaam) explodes to
;    [k, <c, vaaam>]
;    [a, <c, kvaam>]
;    [a, <c, kavam>]
;    [a, <c, kaavm>]
;    [m, <c, kaaav>]
;
; These should stay consistent with the merged sections. i.e. these
; should reduce to 2 grand-total sections, with appropriate counts.

(define (run-balance WA WB WC-NAME)

	; Load some data
	(setup-a-b-sections)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(define gsc (add-covering-sections pca))

	; Verify that the data loaded correctly
	; We expect 1 sections each on "a" and "b"
	(test-equal 1 (length (gsc 'right-stars (Word "a"))))
	(test-equal 1 (length (gsc 'right-stars (Word "b"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	(gsc 'explode-sections)
	(test-equal 9 (length (cog-get-atoms 'CrossSection)))

#! ===========
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
	(merge gsc WA WB frac)
	(define WC-EJ (WordClassNode WC-NAME))

	; We expect just one section remaining on "e", the klm section.
	(test-equal 1 (length (gsc 'right-stars (Word "e"))))

	; We expect no sections remaining on j
	(test-equal 0 (length (gsc 'right-stars (Word "j"))))

	; We expect three merged sections
	(test-equal 3 (length (gsc 'right-stars WC-EJ)))

	; Of the 5 original Sections, 4 are deleted, and 3 are created,
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
	; For example:
	(define epsilon 1.0e-8)
	(test-approximate (* cnt-e-klm (- 1.0 frac))
		(cog-count (car (gsc 'right-stars (Word "e")))) epsilon)

	; To gain access to the counts, load them by name.
	(expected-e-j-sections WC-EJ)
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
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
============ !#
)

(define t-start-cluster "connector balance test")
(test-begin t-start-cluster)

	; Check both merge orders. Results should be independent of the order.
	(setup-database)
	(run-balance (Word "a") (Word "b") "a b")

	(setup-database)
	(run-balance (Word "b") (Word "a") "b a")
(test-end t-start-cluster)

; ---------------------------------------------------------------
