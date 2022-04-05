;
; connector-merge-conind.scm
; Unit test for indirect merge of Connectors - one connector; 2-cluster
;
; Tests merging of two words into a single word-class. Similar to
; `connector-merge-cons.scm`, but with merging via cross-sections.
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
; This is similar to `connector-merge-cons.scm` except that one more
; Section is added into the mix.
;
; This diagram explains what is being tested here:
;
; From basic section merge:
;    (e, abc) + (j, abc) -> ({ej}, abc)
;    (e, dgh) + (j, dgh) -> ({ej}, dgh)
;    (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;     none    + (j, abe) -> p * ({ej}, ab{ej}) + (1-p) * (j, abe)
;     none    + (j, egh) -> p * ({ej}, {ej}gh) + (1-p) * (j, egh)
;
; The last two give cross-sections:
;    [e, <j, abv>] + none -> p * [{ej}, <j, abv>] + (1-p) * [e, <j, abv>]
;    [e, <j, vgh>] + none -> p * [{ej}, <j, vgh>] + (1-p) * [e, <j, vgh>]
;
; All of the above is tested in `connector-merge-cons.scm`. This test
; also includes two more Sections (a, kle) and (f, kle) which should
; lead to two more cross-section merges:
;
;    [e, <a, klv>] + none -> p * [{ej}, <a, klv>] + (1-p) * [e, <a, klv>]
;    [e, <f, klv>] + none -> p * [{ej}, <f, klv>] + (1-p) * [e, <f, klv>]
;
; Reshaping these gives two new Sections, and modified counts on the
; existing sections:
;
;    p * (a, kl{ej}) + (1-p) * (a, kle)
;    p * (f, kl{ej}) + (1-p) * (f, kle)
;
; These two new Sections give four new CrossSections:
;
;   p * [k, <a,  $- l- {e j}+>]
;   p * [l, <a,  k- $- {e j}+>]
;   p * [k, <f,  $- l- {e j}+>]
;   p * [l, <f,  k- $- {e j}+>]
;

(define (run-test WA WB WC-NAME)

	; Load some data
	(setup-e-j-sections)
	(setup-j-extra)
	(setup-indirect-sections)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; Verify that the data loaded correctly
	; We expect 3 sections on "e" and four on "j"
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	; We expect 3 x (3+4+2) = 27 of them.
	(gsc 'explode-sections)
	(test-equal 27 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 3 (length (gsc 'right-stars (Word "g"))))
	(test-equal 3 (length (gsc 'right-stars (Word "h"))))

	; Expect 3 Sections and four CrossSections on e.
	(test-equal 7 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))

	(test-equal 3 (len-type (Word "e") 'Section))
	(test-equal 4 (len-type (Word "e") 'CrossSection))
	(test-equal 4 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))

	; We expect a total of 3+4+2=9 Sections
	(test-equal 9 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge two sections together.
	(define frac 0.25)
	(merge gsc WA WB frac)
	(define WC-EJ (WordClassNode WC-NAME))

	; We expect one section left on "e", the klm section, and two
	; cross-sections. The two cross-sections should correspond
	; to the sections (1-p) * (j, abe) and (1-p) * (j, egh)
	; that is, to the "orthogonal"  word-sense.
	(test-equal 1 (len-type (Word "e") 'Section))
	(test-equal 4 (len-type (Word "e") 'CrossSection))
	(test-equal 5 (length (gsc 'right-stars (Word "e"))))

	; We expect two sections remaining on j
	(test-equal 2 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))

	; We expect five merged sections
	(test-equal 5 (len-type WC-EJ 'Section))
	(test-equal 4 (len-type WC-EJ 'CrossSection))
	(test-equal 9 (length (gsc 'right-stars WC-EJ)))

	; Of the 9=3+4+2 original Sections, 4 are deleted, and 7 are created,
	; leaving a grand total of 12. Five of the new ones have {ej} as germ,
	; and two with {ej} as connectors.
	; This is just a total over everything above.
	(test-equal 12 (length (cog-get-atoms 'Section)))

	; ----------------------------
	; Validate counts.
	(define epsilon 1.0e-8)
	(test-approximate (* cnt-e-klm (- 1.0 frac))
		(cog-count (car (filter-type (Word "e") 'Section))) epsilon)

	; We expect abc, dgh and klm sections to behave exactly as they do for
	; the basic test case, and so cut-n-paste that unit test code.
	(expected-e-j-sections WC-EJ)
	(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count sec-ej-abc) epsilon)
	(test-approximate (+ cnt-e-dgh cnt-j-dgh) (cog-count sec-ej-dgh) epsilon)
	(test-approximate (* frac cnt-e-klm) (cog-count sec-ej-klm) epsilon)
	(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count sec-e-klm) epsilon)

	; Validate counts on select CrossSections...
	(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count xes-b-ej-avc) epsilon)
	(test-approximate (* frac cnt-e-klm) (cog-count xes-k-ej-vlm) epsilon)
	(test-approximate (* (- 1 frac) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

	; --------------------------
	; Expect 36 CrossSections as described above.
	(test-equal 36 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various Sections and CrossSections...
	; This is identical to the tests in `connector-merge-cons.scm` and so
	; is just a cut-n-paste of that test.
	(expected-j-extra-sections WC-EJ)
	(test-approximate (* (- 1 frac) cnt-j-abe) (cog-count sec-j-abe) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count sec-j-egh) epsilon)

	(test-approximate (* frac cnt-j-abe) (cog-count sec-ej-abv) epsilon)
	(test-approximate (* frac cnt-j-egh) (cog-count sec-ej-vgh) epsilon)

	(test-approximate (* (- 1 frac) cnt-j-abe) (cog-count xes-e-j-abv) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count xes-e-j-vgh) epsilon)

	(test-approximate (* frac cnt-j-abe) (cog-count xes-ej-ej-abv) epsilon)
	(test-approximate (* frac cnt-j-egh) (cog-count xes-ej-ej-vgh) epsilon)

	; -----------------------
	; Now test the new sections, not seen in earlier tests.
	(expected-indirect-sections WC-EJ)

	(test-approximate (* (- 1 frac) cnt-a-kle) (cog-count sec-a-kle) epsilon)
	(test-approximate (* (- 1 frac) cnt-f-kle) (cog-count sec-f-kle) epsilon)

	(test-approximate (* frac cnt-a-kle) (cog-count sec-a-klv) epsilon)
	(test-approximate (* frac cnt-f-kle) (cog-count sec-f-klv) epsilon)

	; -----------------------
	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-cone-cluster "connector 2-cluster extra test")
(test-begin t-cone-cluster)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-cone-cluster)

; ---------------------------------------------------------------
(opencog-test-end)
