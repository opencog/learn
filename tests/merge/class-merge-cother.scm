;
; class-merge-cother.scm
; Unit test for merging of Connectors - unexpected conditions.
;
; Tests merging of two word-classes into a single word-class.
; The focus here is to make sure that that when the classes to
; be merged also appear in Connectors, that those are merged
; correctly, too. This triggers some extra merge logic, beyond
; the basic case.
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
; This is similar to `class-merg-cons.scm` except assorted
; other cases are explored.
;
; This diagram explains what is being tested here:
;
; From basic section merge:
;    ({ej}, abc) + ({rs}, abc)    -> ({ej}, abc)
;    ({ej}, klm) +     none       -> ({ej}, klm)
;        none    + ({rs}, dgh)    -> ({ej}, dgh)
;        none    + (f, kl{rs})    -> (f, kl{ej})
;    (g, kl{ej}) + (g, kl{rs})    -> (g, kl{ej})
;
(define t-class-cother "class other connector merge test")
(test-begin t-class-cother)

; Open the database
(setup-database)

; Load some data
(setup-ej-sections)
(setup-fg-extra)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define gsc (add-covering-sections pca))

; Verify that the data loaded correctly
; We expect 2 sections on "e j" and 3 on "r s"
(test-equal 2 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 2 (length (gsc 'right-stars (WordClass "r s"))))

; Get the total count on all Sections
(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

; Create CrossSections and verify that they got created
; We expect 3 x 7 = 21 of them.
(gsc 'explode-sections)
(test-equal 21 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 1 (length (gsc 'right-stars (Word "f"))))
(test-equal 3 (length (gsc 'right-stars (Word "g"))))

; Expect one CrossSection on ej two and rs.
(test-equal 3 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 4 (length (gsc 'right-stars (WordClass "r s"))))

(test-equal 2 (len-type (WordClass "e j") 'Section))
(test-equal 1 (len-type (WordClass "e j") 'CrossSection))
(test-equal 2 (len-type (WordClass "r s") 'Section))
(test-equal 2 (len-type (WordClass "r s") 'CrossSection))

; We expect a total of 2+2=4 Sections and 2+1 CrossSections
(test-equal 7 (length (cog-get-atoms 'Section)))

; --------------------------
; Merge two sections together.
(merge gsc (WordClass "e j") (WordClass "r s") 0)

; We expect three sections "ej", and zero on rs.
(test-equal 3 (len-type (WordClass "e j") 'Section))
(test-equal 2 (len-type (WordClass "e j") 'CrossSection))
(test-equal 0 (len-type (WordClass "r s") 'Section))
(test-equal 5 (length (gsc 'right-stars (WordClass "e j"))))

; Of the 7 original Sections, 4 are deleted, and 2 are created,
; leaving a grand total of 5.
(test-equal 5 (length (cog-get-atoms 'Section)))
(test-equal 15 (length (cog-get-atoms 'CrossSection)))

; ----------------------------
; Validate counts.
(define epsilon 1.0e-8)
(test-approximate cnt-ej-klm
	(cog-count (car (gsc 'right-stars (WordClass "e j")))) epsilon)

; We expect abc, dgh and klm sections to behave exactly as they do for
; the basic test case, and so cut-n-paste that unit test code.
(expected-ej-sections)

(test-approximate (+ cnt-ej-abc cnt-rs-abc) (cog-count sec-ej-abc) epsilon)
(test-approximate cnt-rs-dgh (cog-count sec-ej-dgh) epsilon)
(test-approximate cnt-ej-klm (cog-count sec-ej-klm) epsilon)

; Validate counts on select CrossSections...
(test-approximate (+ cnt-ej-abc cnt-rs-abc) (cog-count xes-b-ej-avc) epsilon)
(test-approximate cnt-ej-klm (cog-count xes-k-ej-vlm) epsilon)
(test-approximate cnt-rs-dgh (cog-count xes-d-ej-vgh) epsilon)

; --------------------------
; Expect 15 CrossSections as described above.
(test-equal 15 (length (cog-get-atoms 'CrossSection)))

; Validate counts on various Sections and CrossSections...
(expected-fg-extra-sections)
(test-approximate cnt-f-klrs (cog-count sec-f-klej) epsilon)
(test-approximate (+ cnt-g-klrs cnt-g-klej) (cog-count sec-g-klej) epsilon)

(test-approximate cnt-f-klrs (cog-count xes-ej-f-klv) epsilon)
(test-approximate (+ cnt-g-klrs cnt-g-klej) (cog-count xes-ej-g-klv) epsilon)

; -----------------------
; Verify detailed balance
(test-assert (check-sections gsc epsilon))
(test-assert (check-crosses gsc epsilon))
(test-assert (check-shapes gsc epsilon))

; Verify no change in totals
(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
	epsilon)

(test-end t-class-cother)

; ---------------------------------------------------------------
(opencog-test-end)
