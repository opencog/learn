;
; class-merge-cons.scm
; Unit test for merging of Connectors - single connector; 2-cluster
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
(use-modules (opencog nlp learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "class-data.scm")

; ---------------------------------------------------------------
;
; This is similar to `class-merg-basic.scm` except that the classes
; appear both as germs, and in connectors.
;
; This diagram explains what is being tested here:
;
; From basic section merge:
;    ({ej}, abc) + ({rs}, abc)    -> ({ej}, abc)
;    ({ej}, klm) +     none       -> ({ej}, klm)
;        none    + ({rs}, dgh)    -> ({ej}, dgh)
;        none    + ({rs}, ab{ej}) -> ({ej}, ab{ej})
; ({ej}, kl{rs}) +     none       -> ({ej}, kl{ej})
;
(define t-class-connector "class connector merge test")
(test-begin t-class-connector)

; Open the database
(setup-database)

; Load some data
(setup-ej-sections)
(setup-ej-extra)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define gsc (add-covering-sections pca))

; Verify that the data loaded correctly
; We expect 3 sections on "e j" and 3 on "r s"
(test-equal 3 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 3 (length (gsc 'right-stars (WordClass "r s"))))

; Get the total count on all Sections
(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

; Create CrossSections and verify that they got created
; We expect 3 x (3+3) = 18 of them.
(gsc 'explode-sections)
(test-equal 18 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 1 (length (gsc 'right-stars (Word "g"))))
(test-equal 1 (length (gsc 'right-stars (Word "h"))))

; Expect one CrossSection each on ej and rs.
(test-equal 4 (length (gsc 'right-stars (WordClass "e j"))))
(test-equal 4 (length (gsc 'right-stars (WordClass "r s"))))

(test-equal 3 (len-type (WordClass "e j") 'Section))
(test-equal 1 (len-type (WordClass "e j") 'CrossSection))
(test-equal 3 (len-type (WordClass "r s") 'Section))
(test-equal 1 (len-type (WordClass "r s") 'CrossSection))

; We expect a total of 3+3=6 Sections
(test-equal 6 (length (cog-get-atoms 'Section)))

; --------------------------
; Merge two sections together.
(define disc (make-fuzz gsc 0 0 4 0))
(disc 'merge-function (WordClass "e j") (WordClass "r s"))

#! =====================
; We expect one section left on "e", the klm section, and two
; cross-sections. The two cross-sections should correspond
; to the sections (1-p) * (j, abe) and (1-p) * (j, egh)
; that is, to the "orthogonal"  word-sense.
(test-equal 1 (len-type (Word "e") 'Section))
(test-equal 2 (len-type (Word "e") 'CrossSection))
(test-equal 3 (length (gsc 'right-stars (Word "e"))))

; We expect two sections remaining on j
(test-equal 2 (len-type (Word "j") 'Section))
(test-equal 0 (len-type (Word "j") 'CrossSection))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))

; We expect five merged sections
(test-equal 5 (len-type (WordClass "e j") 'Section))
(test-equal 2 (len-type (WordClass "e j") 'CrossSection))
(test-equal 7 (length (gsc 'right-stars (WordClass "e j"))))

; Of the 7=3+4 original Sections, 4 are deleted, and 5 are created,
; leaving a grand total of 8. The 5 new ones are all e-j, the
; remaining three ones are "e" or "j" with reduced counts.
; This is just a total over everything above.
(test-equal 8 (length (cog-get-atoms 'Section)))

; ----------------------------
; Validate counts.
(define epsilon 1.0e-8)
(test-approximate (* cnt-e-klm (- 1.0 frac))
	(cog-count (car (filter-type (Word "e") 'Section))) epsilon)

; We expect abc, dgh and klm sections to behave exactly as they do for
; the basic test case, and so cut-n-paste that unit test code.
(expected-e-j-sections)
(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count sec-ej-abc) epsilon)
(test-approximate (+ cnt-e-dgh cnt-j-dgh) (cog-count sec-ej-dgh) epsilon)
(test-approximate (* frac cnt-e-klm) (cog-count sec-ej-klm) epsilon)
(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count sec-e-klm) epsilon)

; Validate counts on select CrossSections...
(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count xes-b-ej-avc) epsilon)
(test-approximate (* frac cnt-e-klm) (cog-count xes-k-ej-vlm) epsilon)
(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

; --------------------------
; Expect 24 CrossSections as described above.
(test-equal 24 (length (cog-get-atoms 'CrossSection)))

; Validate counts on various Sections and CrossSections...
(expected-j-extra-sections)
(test-approximate (* (- 1 frac) cnt-j-abe) (cog-count sec-j-abe) epsilon)
(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count sec-j-egh) epsilon)

(test-approximate (* frac cnt-j-abe) (cog-count sec-ej-abv) epsilon)
(test-approximate (* frac cnt-j-egh) (cog-count sec-ej-vgh) epsilon)

(test-approximate (* (- 1 frac) cnt-j-abe) (cog-count xes-e-j-abv) epsilon)
(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count xes-e-j-vgh) epsilon)

(test-approximate (* frac cnt-j-abe) (cog-count xes-ej-ej-abv) epsilon)
(test-approximate (* frac cnt-j-egh) (cog-count xes-ej-ej-vgh) epsilon)

; -----------------------
; Verify detailed balance
(test-assert (check-sections gsc epsilon))
(test-assert (check-crosses gsc epsilon))

; Verify no change in totals
(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
	epsilon)
======= !#

(test-end t-class-connector)

; ---------------------------------------------------------------
