;
; connector-merge-conext.scm
; Unit test for merging of Connectors - single connector; 2-cluster
;
; Similar to `connector-merge-cons.scm` except that it adds two more
; Sections that merge down into a common section ({ej}, ab{ej})
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
; This is similar to the `connector-merge-cons.scm` except that it adds
; two more Sections: (e, abe) and (e, abj) which will merge into the
; Section (j, abe) from the earlier test, to create ({ej}, ab{ej}).
; This further stresses the flattening and connector merging code.
;
; These two new sections alter the processing, as follows:
; From basic section merge:
;    (e, abe) + (j, abe) -> ({ej}, abe)
;    (e, abj) +   none   -> p * ({ej}, abj) + (1-p) * (e, abj)
;
; The last two are not the final form. From the cross-section merge,
; one has
;    [e, <e, abv>] + [j, <e, abv>] -> [{ej}, <e, abv>]
;    [e, <j, abv>] + none -> p * [{ej}, <j, abv>] + (1-p) * [e, <j, abv>]
;
; which reshapes into
;     (e, ab{ej})
;     p * (j, ab{ej}) + (1-p) * (j, abe)
;
; The two reshapes are combined with the two merged sections, to yield
; as the final form
;     ({ej}, ab{ej})  + (1-p) * (j, abe)
;
; One of the cross-sections on e should be as before:
;     (1-p) * [e, <j, abv>]
;
; -----
; Untouched from the earlier `connector-merge-cons.scm` logic is
;     none    + (j, egh) -> p * ({ej}, egh) + (1-p) * (j, egh)  ; setup-j-extra
;
; which just as before should reshape into
;     p * ({ej}, {ej}gh) + (1-p) * (j, egh)
;
; and have cross-sections
;     (1-p) * [e, <j, vgh>]
;
(define (run-test WA WB WC-NAME)
	; Load some data
	(setup-e-j-sections)
	(setup-j-extra)
	(setup-e-extra)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; Verify that the data loaded correctly
	; We expect 5 sections on "e" and four on "j"
	(test-equal 5 (length (gsc 'right-stars (Word "e"))))
	(test-equal 4 (length (gsc 'right-stars (Word "j"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	; We expect 3 x (5+4) = 27 of them.
	(gsc 'explode-sections)
	(test-equal 27 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 3 (length (gsc 'right-stars (Word "g"))))
	(test-equal 3 (length (gsc 'right-stars (Word "h"))))

	; Expect 5 Sections and 3 CrossSections on e.
	(test-equal 8 (length (gsc 'right-stars (Word "e"))))
	(test-equal 5 (length (gsc 'right-stars (Word "j"))))

	(test-equal 5 (len-type (Word "e") 'Section))
	(test-equal 3 (len-type (Word "e") 'CrossSection))
	(test-equal 4 (len-type (Word "j") 'Section))
	(test-equal 1 (len-type (Word "j") 'CrossSection))

	; We expect a total of 5+4=9 Sections
	(test-equal 9 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge two sections together.
	(define frac 0.25)
	(merge gsc WA WB frac)
	(define WC-EJ (WordClassNode WC-NAME))

	; We expect two section left on "e", the klm section, and
	; a fraction of the (e, abj) section.
	; We expect only one cross-section: [e, <j,vgh>] as all others are
	; absorbed. This cross-section corresponds to (1-p) * (j, egh)
	(test-equal 2 (len-type (Word "e") 'Section))
	(test-equal 1 (len-type (Word "e") 'CrossSection))
	(test-equal 3 (length (gsc 'right-stars (Word "e"))))

	; We expect one sections remaining on j -- (j, egh)
	; We expect one cross-section: a fraction of (e, abj)
	(test-equal 1 (len-type (Word "j") 'Section))
	(test-equal 1 (len-type (Word "j") 'CrossSection))
	(test-equal 2 (length (gsc 'right-stars (Word "j"))))

	; We expect five merged sections, just like in `merge-cons.scm`
	(test-equal 5 (len-type WC-EJ 'Section))
	(test-equal 2 (len-type WC-EJ 'CrossSection))
	(test-equal 7 (length (gsc 'right-stars WC-EJ)))

	; Of the 9=5+4 original Sections, 6 are deleted, and 5 are created,
	; leaving a grand total of 8. The 5 new ones are all e-j, the
	; remaining three ones are "e" or "j" with reduced counts.
	; This is just a total over everything above.
	(test-equal 8 (length (cog-get-atoms 'Section)))

	; ----------------------------
	; Validate counts.  The tests immediatey below are identical to,
	; unchanged from `connector-merge-cons.scm`
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
	; Counts that are unique to this particular unit test.
	;
	; Expect 24 CrossSections
	(test-equal 24 (length (cog-get-atoms 'CrossSection)))

	; Validate counts on various Sections and CrossSections...
	(expected-j-extra-sections WC-EJ)
	(test-approximate 0 (cog-count sec-j-abe) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count sec-j-egh) epsilon)

	(define tot-ej-abv (+ cnt-j-abe cnt-e-abe (* frac cnt-e-abj)))
	(test-approximate tot-ej-abv (cog-count sec-ej-abv) epsilon)
	(test-approximate (* frac cnt-j-egh) (cog-count sec-ej-vgh) epsilon)

	(test-approximate 0 (cog-count xes-e-j-abv) epsilon)
	(test-approximate (* (- 1 frac) cnt-j-egh) (cog-count xes-e-j-vgh) epsilon)

	(test-approximate tot-ej-abv (cog-count xes-ej-ej-abv) epsilon)
	(test-approximate (* frac cnt-j-egh) (cog-count xes-ej-ej-vgh) epsilon)

	; -----------------------
	; (cog-delete! sec-j-abe)
	; (cog-delete! xes-e-j-abv)

	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-two-cluster-extend "extended 2-cluster test")
(test-begin t-two-cluster-extend)

	; Open the database
	(setup-database)
	(run-test (Word "e") (Word "j") "e j")

	(setup-database)
	(run-test (Word "j") (Word "e") "j e")

(test-end t-two-cluster-extend)

; ---------------------------------------------------------------
(opencog-test-end)
