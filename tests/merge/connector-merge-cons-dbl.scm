;
; connector-merge-cons-dbl.scm
; Unit test for merging of Connectors - two connectors; 2-cluster
;
; Like connector-merge-cons.scm, except there are two connectors
; per section that need a merge.
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
; This is similar to the "simple start-cluster merge test" except
; that the word "e" appears both as germ, and in two connectors.
;
; This diagram explains what is being tested here:
;
; From basic section merge:
;    (e, abc) + (j, abc) -> ({ej}, abc)
;    (e, dgh) + (j, dgh) -> ({ej}, dgh)
;    (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;     none    + (j, ebe) -> p * ({ej}, ebe) + (1-p) * (j, ebe)
;     none    + (j, eeh) -> p * ({ej}, eeh) + (1-p) * (j, eeh)
;
; However, the last two are not the final form. From the cross-section
; merge, one has
;    [e, <j, vbe>] + none -> p * [{ej}, <j, vbe>] + (1-p) * [e, <j, vbe>]
;    [e, <j, ebv>] + none -> p * [{ej}, <j, ebv>] + (1-p) * [e, <j, ebv>]
;    [e, <j, veh>] + none -> p * [{ej}, <j, veh>] + (1-p) * [e, <j, veh>]
;    [e, <j, evh>] + none -> p * [{ej}, <j, evh>] + (1-p) * [e, <j, evh>]
;
; which reshapes into
;     p * (j, {ej}be) + (1-p) * (j, ebe)
;     p * (j, eb{ej}) + (1-p) * (j, ebe)
;     p * (j, {ej}eh) + (1-p) * (j, eeh)
;     p * (j, e{ej}h) + (1-p) * (j, eeh)
;
; The four reshapes are combined with the two merged sections, to yield
; as the final form
;     p * ({ej}, {ej}b{ej}) + (1-p) * (j, ebe)
;     p * ({ej}, {ej}{ej}h) + (1-p) * (j, eeh)
;
; The cross-sections on e should be:
;     (1-p) * [e, <j, vbe>]
;     (1-p) * [e, <j, ebv>]
;     (1-p) * [e, <j, veh>]
;     (1-p) * [e, <j, evh>]
; and nothing more. The motivation for this is described in the diary
; entry "April-May 2021 ...Non-Commutivity, Again... Case B".
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
; "p" is the fraction to merge == 0.25, hard-coded below.
;
; What should the cross-sections be?
; The should track the above sections, with count as appropriate.
; to recap, expect:
;     ({ej}, abc)    * 1.0
;     ({ej}, dgh)    * 1.0
;     ({ej}, klm)    * p
;     (e, klm)       * 1-p
;     (j, ebe)       * 1-p
;     (j, eeh)       * 1-p
;     ({ej}, {ej}b{ej}) * p
;     ({ej}, {ej}{ej}h) * p
; There are 8 of these, so expect 24=8*3 CrossSections

(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-j-double-e)

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
	; We expect 3 x (3+4) = 21 of them.
	(gsc 'explode-sections)
	(test-equal 21 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 2 (length (gsc 'right-stars (Word "g"))))
	(test-equal 3 (length (gsc 'right-stars (Word "h"))))

	; Expect 3 Sections and four CrossSections on e.
	(test-equal 7 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))

	(test-equal 3 (len-type (Word "e") 'Section))
	(test-equal 4 (len-type (Word "e") 'CrossSection))
	(test-equal 4 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))

	; We expect a total of 3+4=7 Sections
	(test-equal 7 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge two sections together.
	(define frac 0.25)
	(merge gsc WA WB frac)
	(define WC-EJ (WordClassNode WC-NAME))

	; We expect one section left on "e", the klm section, and 4
	; cross-sections. The four cross-sections should correspond
	; to the sections (1-p) * (j, ebe) and (1-p) * (j, eeh)
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
	; Expect 24 CrossSections as described above.
	(test-equal 24 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various Sections and CrossSections...
	(expected-j-double-e WC-EJ)
	(test-approximate (* (- 1 frac) cnt-j-ebe) (cog-count sec-j-ebe) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-eeh) (cog-count sec-j-eeh) epsilon)

	(test-approximate (* frac cnt-j-ebe) (cog-count sec-ej-vbv) epsilon)
	(test-approximate (* frac cnt-j-eeh) (cog-count sec-ej-vvh) epsilon)

	(test-approximate (* (- 1 frac) cnt-j-ebe) (cog-count xes-e-j-ebv) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-eeh) (cog-count xes-e-j-veh) epsilon)

	(test-approximate (* frac cnt-j-ebe) (cog-count xes-ej-ej-ebv) epsilon)
	(test-approximate (* frac cnt-j-eeh) (cog-count xes-ej-ej-veh) epsilon)

	; -----------------------
	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-two-connector-two-cluster "2-connector 2-cluster merge test")
(test-begin t-two-connector-two-cluster)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-two-connector-two-cluster)

; ---------------------------------------------------------------
(opencog-test-end)
