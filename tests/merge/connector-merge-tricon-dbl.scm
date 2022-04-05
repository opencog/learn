;
; connector-merge-tricon-dbl.scm
; Unit test for merging of Connectors - two double connectors; 3-cluster
;
; Mashup of two earlier test cases: connector-merge-cons-dbl.scm
; and connector-merge-tricon.scm
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
; This is a mashup of two earlier test cases: one for merging three
; words (with connectors in them): `connector-merge-tricon.scm` and one
; that has multiple connectors in the merge cluster:
; `connector-merge-cons-dbl.scm`
;
; When the first two words {ej} are merged, we expect some
; cross-sections from "f" to contribute:
;    [e, <f, vbe>] + none -> p * [{ej}, <f, vbe>] + (1-p) * [e, <f, vbe>]
;    [e, <f, ebv>] + none -> p * [{ej}, <f, ebv>] + (1-p) * [e, <f, ebv>]
;    [e, <f, veh>] + none -> p * [{ej}, <f, veh>] + (1-p) * [e, <f, veh>]
;    [e, <f, evh>] + none -> p * [{ej}, <f, evh>] + (1-p) * [e, <f, evh>]
;
; which reshapes into
;     p * (f, {ej}be) + (1-p) * (f, ebe)
;     p * (f, eb{ej}) + (1-p) * (f, ebe)
;     p * (f, {ej}eh) + (1-p) * (f, eeh)
;     p * (f, e{ej}h) + (1-p) * (f, eeh)
;
; and then flattens to
;     p * (f, {ej}b{ej}) + (1-p) * (f, ebe)
;     p * (f, {ej}{ej}h) + (1-p) * (f, eeh)
;
; This is explicitly tested (see "TEST F1" below).
;
; Next, "f" is merged into {ej}.
;
; The earlier ej merge reduced the count on (f, ebe) and (f, eeh).
; The "f" merge reduces it some more:
;    none + (f, ebe) -> p * ({ej}, ebe) + (1-p) * (f, ebe)
;    none + (f, eeh) -> p * ({ej}, eeh) + (1-p) * (f, eeh)
;
; The counts on ({ej}, ebe) and ({ej}, eeh) are accumulated into
; ({ej}, {ej}b{ej}) and ({ej}, {ej}{ej}h) respectively. It is NOT
; immediately obvious that these should be accumulated, instead of
; zeroed out. However, the initial projective cluster formation won't
; work right if these are zeroed out, so we avoid further special
; cases, and accumulate here.  These is tested in "TEST F2" below.

; But also, we have the earlier fraction that gets merged:
;    ({ej}, {ej}b{ej}) + p * (f, {ej}b{ej}) -> ({ej}, {ej}b{ej})
;    ({ej}, {ej}{ej}h) + p * (f, {ej}{ej}h) -> ({ej}, {ej}{ej}h)
;
; To recap, we expect 10 sections
;     ({ej}, abc)    * 1.0
;     ({ej}, dgh)    * 1.0
;     ({ej}, klm)    * p
;     (e, klm)       * 1-p
;     (j, ebe)       * 1-p
;     (j, eeh)       * 1-p
;     (f, ebe)       * (1-p)(1-p)
;     (f, eeh)       * (1-p)(1-p)
;     ({ej}, {ej}b{ej}) * p (j + f)
;     ({ej}, {ej}{ej}h) * p (j + f)
;
; There are 10 of these, so expect 30=10*3 CrossSections

(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-f-sections)
	(setup-j-double-e)
	(setup-f-double-e)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; ------------------
	; Verify that the data loaded correctly
	; We expect 3 sections on "e" and four on "j"
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))
	(test-equal 5 (length (gsc 'right-stars (Word "f"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	; We expect 3 x (3+4+5) = 36 of them.
	(gsc 'explode-sections)
	(test-equal 36 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 3 (length (gsc 'right-stars (Word "g"))))
	(test-equal 5 (length (gsc 'right-stars (Word "h"))))

	; Expect 3 Sections and 8 CrossSections on e.
	(test-equal 11 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))
	(test-equal 5 (length (gsc 'right-stars (Word "f"))))

	(test-equal 3 (len-type (Word "e") 'Section))
	(test-equal 8 (len-type (Word "e") 'CrossSection))
	(test-equal 4 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 5 (len-type (Word "f") 'Section))
	(test-equal 0 (len-type (Word "f") 'CrossSection))

	; We expect a total of 3+4+5=12 Sections
	(test-equal 12 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge the first two sections together.
	(define frac1 0.25)
	(merge gsc WA WB frac1)
	(define WC-EJ (WordClassNode WC-NAME))

	; 12 sections as before plus 3 more.
	(test-equal 15 (length (cog-get-atoms 'Section)))

	(test-equal 1 (length (filter-type (Word "e") 'Section)))
	(test-equal 5 (length (filter-type WC-EJ 'Section)))

	(test-equal 8 (length (filter-type (Word "e") 'CrossSection)))

	; Expect 8 cross-sections. These are:
	;    [{ej}, <{ej}, {ej}bv>]
	;    [{ej}, <{ej}, vb{ej}>]
	;    [{ej}, <{ej}, v{ej}h>]
	;    [{ej}, <{ej}, {ej}vh>]
	;    [{ej}, <f, {ej}bv>]  -- flattening of  [{ej}, <f, ebv>]
	;    [{ej}, <f, vb{ej}>]
	;    [{ej}, <f, v{ej}h>]
	;    [{ej}, <f, {ej}vh>]
	;
	(test-equal 8 (length (filter-type WC-EJ 'CrossSection)))

	; Validate counts.
	(define epsilon 1.0e-8)
	(expected-e-j-sections WC-EJ)
	(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count sec-ej-abc) epsilon)
	(test-approximate (+ cnt-e-dgh cnt-j-dgh) (cog-count sec-ej-dgh) epsilon)
	(test-approximate (* frac1 cnt-e-klm) (cog-count sec-ej-klm) epsilon)
	(test-approximate (* (- 1 frac1) cnt-e-klm) (cog-count sec-e-klm) epsilon)

	; Validate counts on select CrossSections...
	(test-approximate (+ cnt-e-abc cnt-j-abc) (cog-count xes-b-ej-avc) epsilon)
	(test-approximate (* frac1 cnt-e-klm) (cog-count xes-k-ej-vlm) epsilon)
	(test-approximate (* (- 1 frac1) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

	(expected-j-double-e WC-EJ)
	; Next four tests are "TEST F1" described up top.
	(test-approximate (* frac1 cnt-j-ebe) (cog-count sec-ej-vbv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-ebe) (cog-count sec-j-ebe) epsilon)
	(test-approximate (* frac1 cnt-j-eeh) (cog-count sec-ej-vvh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-eeh) (cog-count sec-j-eeh) epsilon)

	(expected-f-double-e WC-EJ)
	(test-approximate (* frac1 cnt-f-ebe) (cog-count sec-f-ejbej) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-ebe) (cog-count sec-f-ebe) epsilon)
	(test-approximate (* frac1 cnt-f-eeh) (cog-count sec-f-ejejh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-eeh) (cog-count sec-f-eeh) epsilon)

	(test-approximate (* frac1 cnt-f-ebe) (cog-count xes-ej-f-ejbv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-ebe) (cog-count xes-e-f-ebv) epsilon)
	(test-approximate (* frac1 cnt-f-eeh) (cog-count xes-ej-f-vejh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-eeh) (cog-count xes-e-f-veh) epsilon)

	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)

	; -------------------------------
	(format #t "Now merging 'f' into 'ej'\n")
	(define frac2 0.35)
	(merge gsc WC-EJ (Word "f") frac2)

	(test-equal 4 (length (filter-type WC-EJ 'CrossSection)))

	; We expect one section left on "e", the klm section, and four
	; cross-sections. The four cross-sections should correspond
	; to the sections (1-p) * (j, ebe) and (1-p) * (j, eeh)
	; that is, to the "orthogonal"  word-sense.
	(test-equal 1 (len-type (Word "e") 'Section))
	(test-equal 8 (len-type (Word "e") 'CrossSection))
	(test-equal 9 (length (gsc 'right-stars (Word "e"))))

	; We expect two sections remaining on j, and on f
	(test-equal 2 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))

	(test-equal 2 (len-type (Word "f") 'Section))
	(test-equal 0 (len-type (Word "f") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "f"))))

	; We expect five merged sections
	(test-equal 5 (len-type WC-EJ 'Section))
	(test-equal 4 (len-type WC-EJ 'CrossSection))
	(test-equal 9 (length (gsc 'right-stars WC-EJ)))

	; Of the 10=3+4+3 original Sections, 5 are deleted, and 5 are created,
	; leaving a grand total of 10. The 5 new ones are all e-j, the
	; remaining three ones are "e" or "j" with reduced counts.
	; This is just a total over everything above.
	(test-equal 10 (length (cog-get-atoms 'Section)))

	; ----------------------------
	; Validate counts.
	(test-approximate (* cnt-e-klm (- 1.0 frac1))
		(cog-count (car (filter-type (Word "e") 'Section))) epsilon)

	; We expect abc, dgh and klm sections to behave exactly as they do for
	; the basic test case, and so cut-n-paste that unit test code.
	(test-approximate (+ cnt-e-abc cnt-j-abc cnt-f-abc)
		(cog-count sec-ej-abc) epsilon)
	(test-approximate (+ cnt-e-dgh cnt-j-dgh cnt-f-dgh)
		(cog-count sec-ej-dgh) epsilon)
	(test-approximate (+ (* frac1 cnt-e-klm) cnt-f-klm)
		(cog-count sec-ej-klm) epsilon)
	(test-approximate (* (- 1 frac1) cnt-e-klm)
		(cog-count sec-e-klm) epsilon)

	; Validate counts on select CrossSections...
	(test-approximate (+ cnt-e-abc cnt-j-abc cnt-f-abc)
		(cog-count xes-b-ej-avc) epsilon)
	(test-approximate (+ (* frac1 cnt-e-klm) cnt-f-klm)
		(cog-count xes-k-ej-vlm) epsilon)
	(test-approximate (* (- 1 frac1) cnt-e-klm) (cog-count xes-k-e-vlm) epsilon)

	; The j counts should be untouched from before.
	(test-approximate (* (- 1 frac1) cnt-j-ebe) (cog-count sec-j-ebe) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-eeh) (cog-count sec-j-eeh) epsilon)

	; Now, for some of the more complex cases.
	; The (f,ebe) and (f,eeh) sections are twice-reduced, as explained above.
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-ebe)
		(cog-count sec-f-ebe) epsilon)
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-eeh)
		(cog-count sec-f-eeh) epsilon)

	; The remainder got transferred ... these two tests are "TEST F2"
	; as described up top.
	(define expected-sec-ej-vbv-count
		(+ (* frac1 (+ cnt-j-ebe cnt-f-ebe))  ; from linear merge
			(* frac2 (- 1 frac1) cnt-f-ebe)))  ; from connector merge
	(test-approximate expected-sec-ej-vbv-count
		(cog-count sec-ej-vbv) epsilon)

	(define expected-sec-ej-vvh-count
		(+ (* frac1 (+ cnt-j-eeh cnt-f-eeh))
			(* frac2 (- 1 frac1) cnt-f-eeh)))
	(test-approximate expected-sec-ej-vvh-count
		(cog-count sec-ej-vvh) epsilon)


	; --------------------------
	; Expect 30 CrossSections as described above.
	(test-equal 30 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various CrossSections...
	(test-approximate (* (- 1 frac1) cnt-j-ebe) (cog-count xes-e-j-ebv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-eeh) (cog-count xes-e-j-veh) epsilon)

	(test-approximate expected-sec-ej-vbv-count
		(cog-count xes-ej-ej-ebv) epsilon)
	(test-approximate expected-sec-ej-vvh-count
		(cog-count xes-ej-ej-veh) epsilon)

	; ----------------------
	; Check detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-two-three-cluster "two-connector-2-word 3-cluster merge test")
(test-begin t-two-three-cluster)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-two-three-cluster)

; ---------------------------------------------------------------
(opencog-test-end)
