;
; connector-merge-tricon.scm
; Unit test for merging of Connectors - two single connectors; 3-cluster
;
; Tests merging of three words into a single word-class.
; The focus here is to make sure that that when the words to
; be merged also appear in Connectors, that those are merged
; correctly, too. This triggers some extra merge logic, beyond
; the basic case.
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
; This is similar to the "connector 3-cluster merge test" except
; that the third word is "non-flat" and has connectors that belong
; to the cluster. See the explanation in the test
; `connector-merge-cons.scm` for the general overview of merging, and
; `connector-merge-tri.scm` for the "flat" three-connector merge.
;
; When the first two words {ej} are merged, we expect some
; cross-sections from "f" to contribute:
;    [e, <f, abv>] + none -> p * [{ej}, <f, abv>] + (1-p) * [e, <f, abv>]
;    [e, <f, vgh>] + none -> p * [{ej}, <f, vgh>] + (1-p) * [e, <f, vgh>]
;
; which reshapes into
;     p * (f, ab{ej}) + (1-p) * (f, abe)
;     p * (f, {ej}gh) + (1-p) * (f, egh)
;
; This is explicitly tested (see "TEST F1" below).
;
; Next, "f" is merged into {ej}. This gives a "flat" merge:
;    (f, klm) +  ({ej}, klm) -> ({ej}, klm)
; which will transfer all of the count from f to {ej}.
;
; The earlier ej merge reduced the count on (f, abe) and (f, egh).
; The "f" merge reduces it some more:
;    none + (f, abe) -> p * ({ej}, abe) + (1-p) * (f, abe)
;    none + (f, egh) -> p * ({ej}, egh) + (1-p) * (f, egh)
;
; The counts on ({ej}, abe) and ({ej}, egh) are accumulated into
; ({ej}, ab{ej}) and ({ej}, {ej}gh) respectively. It is NOT immediately
; obvious that these should be accumulated, instead of zeroed out.
; However, the initial projective cluster formation won't work right
; if these are zeroed out, so we avoid further special cases, and
; accumulate here.  These are tested in "TEST F2" below.

; But also, we have the earlier fraction that gets merged:
;    ({ej}, ab{ej}) + p * (f, ab{ej}) -> ({ej}, ab{ej})
;    ({ej}, {ej}gh) + p * (f, {ej}gh) -> ({ej}, {ej}gh)
;
; To recap, we expect 10 sections
;     ({ej}, abc)    * 1.0
;     ({ej}, dgh)    * 1.0
;     ({ej}, klm)    * p
;     (e, klm)       * 1-p
;     (j, abe)       * 1-p
;     (j, egh)       * 1-p
;     (f, abe)       * (1-p)(1-p)
;     (f, egh)       * (1-p)(1-p)
;     ({ej}, ab{ej}) * p (j + f)
;     ({ej}, {ej}gh) * p (j + f)
;
; There are 10 of these, so expect 30=10*3 CrossSections

(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-f-sections)
	(setup-j-extra)
	(setup-f-extra)

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
	(test-equal 5 (length (gsc 'right-stars (Word "g"))))
	(test-equal 5 (length (gsc 'right-stars (Word "h"))))

	; Expect 3 Sections and 4 CrossSections on e.
	(test-equal 7 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))
	(test-equal 5 (length (gsc 'right-stars (Word "f"))))

	(test-equal 3 (len-type (Word "e") 'Section))
	(test-equal 4 (len-type (Word "e") 'CrossSection))
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

	; 10 sections as before plus 5 more.
	(test-equal 15 (length (cog-get-atoms 'Section)))

	(test-equal 1 (length (filter-type (Word "e") 'Section)))
	(test-equal 5 (length (filter-type WC-EJ 'Section)))

	(test-equal 4 (length (filter-type (Word "e") 'CrossSection)))
	(test-equal 4 (length (filter-type WC-EJ 'CrossSection)))

	; Validate counts.
	(define epsilon 1.0e-8)
	(expected-e-j-sections WC-EJ)
	(test-approximate (* frac1 cnt-e-klm) (cog-count sec-ej-klm) epsilon)
	(test-approximate (* (- 1 frac1) cnt-e-klm) (cog-count sec-e-klm) epsilon)

	(expected-j-extra-sections WC-EJ)
	; Next four tests are "TEST F1" described up top.
	(test-approximate (* frac1 cnt-j-abe) (cog-count sec-ej-abv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-abe) (cog-count sec-j-abe) epsilon)
	(test-approximate (* frac1 cnt-j-egh) (cog-count sec-ej-vgh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-egh) (cog-count sec-j-egh) epsilon)

	(expected-f-extra-sections WC-EJ)
	(test-approximate (* frac1 cnt-f-abe) (cog-count sec-f-abej) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-abe) (cog-count sec-f-abe) epsilon)
	(test-approximate (* frac1 cnt-f-egh) (cog-count sec-f-ejgh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-egh) (cog-count sec-f-egh) epsilon)

	(test-approximate (* frac1 cnt-f-abe) (cog-count xes-ej-f-abv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-abe) (cog-count xes-e-f-abv) epsilon)
	(test-approximate (* frac1 cnt-f-egh) (cog-count xes-ej-f-vgh) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-egh) (cog-count xes-e-f-vgh) epsilon)

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

	(test-equal 2 (length (filter-type WC-EJ 'CrossSection)))

	; We expect one section left on "e", the klm section, and four
	; cross-sections. The four cross-sections should correspond
	; to the sections (1-p) * (j, abe) and (1-p) * (j, egh) and
	; (1-p) * (f, abe) and (1-p) * (f, egh)
	; that is, to the "orthogonal"  word-sense.
	(test-equal 1 (len-type (Word "e") 'Section))
	(test-equal 4 (len-type (Word "e") 'CrossSection))
	(test-equal 5 (length (gsc 'right-stars (Word "e"))))

	; We expect two sections remaining on j, and on f
	(test-equal 2 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))

	(test-equal 2 (len-type (Word "f") 'Section))
	(test-equal 0 (len-type (Word "f") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "f"))))

	; We expect five merged sections
	(test-equal 5 (len-type WC-EJ 'Section))
	(test-equal 2 (len-type WC-EJ 'CrossSection))
	(test-equal 7 (length (gsc 'right-stars WC-EJ)))

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
	(expected-e-j-sections WC-EJ)
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
	(test-approximate (* (- 1 frac1) cnt-j-abe) (cog-count sec-j-abe) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-egh) (cog-count sec-j-egh) epsilon)

	; Now, for some of the more complex cases.
	; The (f,abe) and (f,egh) sections are twice-reduced, as explained above.
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-abe)
		(cog-count sec-f-abe) epsilon)
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-egh)
		(cog-count sec-f-egh) epsilon)

	; The remainder got transferred ... these two tests are "TEST F2"
	; as described up top.
	(define expected-sec-ej-abv-count
		(+ (* frac1 (+ cnt-j-abe cnt-f-abe))  ; from linear merge
			(* frac2 (- 1 frac1) cnt-f-abe)))  ; from connector merge
	(test-approximate expected-sec-ej-abv-count
		(cog-count sec-ej-abv) epsilon)

	(define expected-sec-ej-vgh-count
		(+ (* frac1 (+ cnt-j-egh cnt-f-egh))
			(* frac2 (- 1 frac1) cnt-f-egh)))
	(test-approximate expected-sec-ej-vgh-count
		(cog-count sec-ej-vgh) epsilon)


	; --------------------------
	; Expect 30 CrossSections as described above.
	(test-equal 30 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various CrossSections...
	(test-approximate (* (- 1 frac1) cnt-j-abe) (cog-count xes-e-j-abv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-egh) (cog-count xes-e-j-vgh) epsilon)

	(test-approximate expected-sec-ej-abv-count
		(cog-count xes-ej-ej-abv) epsilon)
	(test-approximate expected-sec-ej-vgh-count
		(cog-count xes-ej-ej-vgh) epsilon)

	; ----------------------
	; Check detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-three-cluster "one-connector-2-word 3-cluster merge test")
(test-begin t-three-cluster)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-three-cluster)

; ---------------------------------------------------------------
(opencog-test-end)
