;
; connector-merge-triconind.scm
; Unit test for indirect merging of Connectors - 3-cluster
;
; This is a mashup between `connector-merge-conind.scm` and
; `connector-merge-tricon.scm`, with indirectly generated sections.
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
; This is a mashup between `connector-merge-conind.scm` and
; `connector-merge-tricon.scm`, taking the idea of indirect merges
; from the first, and an additional third member to the cluster from
; the second unit test. Most of the unit test is a cut-n-paste of the
; the first two, with tweaks to handle the differences.
;
; After the first merge, which is identical to `connector-merge-conind.scm`
; one obtains new cross-sections
;
;    [e, <a, klv>] + none -> p * [{ej}, <a, klv>] + (1-p) * [e, <a, klv>]
;    [e, <f, klv>] + none -> p * [{ej}, <f, klv>] + (1-p) * [e, <f, klv>]
;
; which reshape to
;
;    p * (a, kl{ej}) + (1-p) * (a, kle)
;    p * (f, kl{ej}) + (1-p) * (f, kle)
;
; This is explicitly tested (see "TEST F1" below).
;
; Next, "f" is merged into {ej}. There is a Section merge:
;
;   none + (f, kle) -> q * ({ej}, kle) + (1-q) * (f, kle)
;
; which is obviously non-flat, and must flatten out to
;
;   none + (f, kle) -> q * ({ej}, kl{ej}) + (1-q) * (f, kle)
;
; There is also a cross-section which is similarly flattened:
;
;    [e, <f, klv>] + none -> q * [{ej}, <{ej}, klv>] + (1-q) * [e, <f, klv>]
;
; These are tested in "TEST F3" below.
;

(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-f-sections)
	(setup-j-extra)
	(setup-f-extra)
	(setup-indirect-sections)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; ------------------
	; Verify that the data loaded correctly
	; We expect 3 sections on "e" and four on "j"
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))
	(test-equal 6 (length (gsc 'right-stars (Word "f"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	; We expect 3 x (3+4+5+2) = 42 of them.
	(gsc 'explode-sections)
	(test-equal 42 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 5 (length (gsc 'right-stars (Word "g"))))
	(test-equal 5 (length (gsc 'right-stars (Word "h"))))

	; Expect 3 Sections and 4 CrossSections on e.
	(test-equal 9 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))
	(test-equal 6 (length (gsc 'right-stars (Word "f"))))

	(test-equal 3 (len-type (Word "e") 'Section))
	(test-equal 6 (len-type (Word "e") 'CrossSection))
	(test-equal 4 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 6 (len-type (Word "f") 'Section))
	(test-equal 0 (len-type (Word "f") 'CrossSection))

	; We expect a total of 3+4+5+2=14 Sections
	(test-equal 14 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge the first two sections together.
	(define frac1 0.25)
	(merge gsc WA WB frac1)
	(define WC-EJ (WordClassNode WC-NAME))

	; 10 sections as before plus 9 more.
	(test-equal 19 (length (cog-get-atoms 'Section)))

	(test-equal 1 (length (filter-type (Word "e") 'Section)))
	(test-equal 5 (length (filter-type WC-EJ 'Section)))

	(test-equal 6 (length (filter-type (Word "e") 'CrossSection)))
	(test-equal 6 (length (filter-type WC-EJ 'CrossSection)))

	; Validate counts.
	; This part is *identical* to the tests in `connector-merge-tricon.scm`
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

	; The next section is *identical* to that of `connector-merge-conind.scm`
	; (except that frac -> frac1)
	(expected-indirect-sections WC-EJ)
	(test-approximate (* (- 1 frac1) cnt-a-kle) (cog-count sec-a-kle) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-kle) (cog-count sec-f-kle) epsilon)
	(test-approximate (* frac1 cnt-a-kle) (cog-count sec-a-klv) epsilon)
	(test-approximate (* frac1 cnt-f-kle) (cog-count sec-f-klv) epsilon)

	; The next four tests are unique to this unit test.
	(test-approximate (* (- 1 frac1) cnt-a-kle) (cog-count sec-a-kle) epsilon)
	(test-approximate (* frac1 cnt-a-kle) (cog-count sec-a-klv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-f-kle) (cog-count sec-f-kle) epsilon)
	(test-approximate (* frac1 cnt-f-kle) (cog-count sec-f-klv) epsilon)

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

	; 2 from before, plus 2 more
	(test-equal 4 (length (filter-type WC-EJ 'CrossSection)))

	; We expect one section left on "e", the klm section, and six
	; cross-sections. The six cross-sections should correspond
	; to the sections (1-p) * (j, abe) and (1-p) * (j, egh) and
	; (1-p) * (f, abe) and (1-p) * (f, egh) and
	; (1-p) * (a, kle) and (1-p) * (f, kle)
	; that is, to the "orthogonal"  word-sense.
	(test-equal 1 (len-type (Word "e") 'Section))
	(test-equal 6 (len-type (Word "e") 'CrossSection))
	(test-equal 7 (length (gsc 'right-stars (Word "e"))))

	; We expect two sections remaining on j, and three on f
	(test-equal 2 (len-type (Word "j") 'Section))
	(test-equal 0 (len-type (Word "j") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))

	(test-equal 3 (len-type (Word "f") 'Section))
	(test-equal 0 (len-type (Word "f") 'CrossSection))
	(test-equal 3 (length (gsc 'right-stars (Word "f"))))

	; We expect five merged sections
	(test-equal 6 (len-type WC-EJ 'Section))
	(test-equal 4 (len-type WC-EJ 'CrossSection))
	(test-equal 10 (length (gsc 'right-stars WC-EJ)))

	; Of the 12=3+4+3+2 original Sections, 5 are deleted, and 7 are created,
	; leaving a grand total of 14. Five of the new ones have {ej} as germ,
	; and two with {ej} as connectors.
	; This is just a total over everything above.
	(test-equal 14 (length (cog-get-atoms 'Section)))

	; ----------------------------
	; Validate counts.
	; The first part of this is *identical* to the tests in
	; `connector-merge-tricon.scm`
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
	; The (f,abe) and (f,egh) sections are twice-reduced, as explained
	; in `connector-merge-tricon.scm`.
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-abe)
		(cog-count sec-f-abe) epsilon)
	(test-approximate (* (- 1 frac1) (- 1 frac2) cnt-f-egh)
		(cog-count sec-f-egh) epsilon)

	; The remainder got transferred ... these two tests are "TEST F2"
	; as described in `connector-merge-tricon.scm`.
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
	; Expect 42 CrossSections.
	(test-equal 42 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various CrossSections...
	(test-approximate (* (- 1 frac1) cnt-j-abe) (cog-count xes-e-j-abv) epsilon)
	(test-approximate (* (- 1 frac1) cnt-j-egh) (cog-count xes-e-j-vgh) epsilon)

	(test-approximate expected-sec-ej-abv-count
		(cog-count xes-ej-ej-abv) epsilon)
	(test-approximate expected-sec-ej-vgh-count
		(cog-count xes-ej-ej-vgh) epsilon)

	; ----------------------
	; Section "TEST F3" -- test the sections unique to this test, not
	; appearing in the other unit tests. Gonna skip testing the
	; cross-sections, as the `check-sections` and `check-crosses` should
	; be adequate for that.

	; Reload, because the merge wiped out one of them.
	(expected-indirect-sections WC-EJ)

	; These two are same as before.
	(test-approximate (* (- 1 frac1) cnt-a-kle) (cog-count sec-a-kle) epsilon)
	(test-approximate (* frac1 cnt-a-kle) (cog-count sec-a-klv) epsilon)

	; One is reduced, the other enlarged.
	(test-approximate 0.0 (cog-count sec-f-klv) epsilon)
	(test-approximate (* (- 1 frac2) (- 1 frac1) cnt-f-kle)
		(cog-count sec-f-kle) epsilon)

	; ----------------------
	; Check detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-three-indirect "one-connector-2-word 3-cluster indirect test")
(test-begin t-three-indirect)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-three-indirect)

; ---------------------------------------------------------------
(opencog-test-end)
