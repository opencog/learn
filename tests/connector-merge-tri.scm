;
; connector-merge-tri.scm
; Unit test for merging of Connectors - single connector; 3-cluster
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
(use-modules (opencog nlp learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "connector-data.scm")

; ---------------------------------------------------------------
;
; This is similar to the "connector 2-cluster merge test" except
; that three words form the cluster. See the explanation in the test
; `connector-merge-cons.scm` for the general overview.
;
; To recap, we expect 11 sections
;     ({ej}, abc)    * 1.0
;     ({ej}, dgh)    * 1.0
;     ({ej}, klm)    * p
;     (e, klm)       * 1-p
;     (j, abe)       * 1-p
;     (j, egh)       * 1-p
;     ({ej}, ab{ej}) * p
;     ({ej}, {ej}gh) * p
;
; There are 11 of these, so expect 33=11*3 CrossSections

(define t-three-cluster "connector 3-cluster merge test")
(test-begin t-three-cluster)

; Open the database
(setup-database)

; Load some data
(setup-e-j-sections)
(setup-f-sections)
(setup-j-extra)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(define gsc (add-cluster-gram csc))

; Verify that the data loaded correctly
; We expect 3 sections on "e" and four on "j"
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 4 (length (gsc 'right-stars (Word "j"))))
(test-equal 3 (length (gsc 'right-stars (Word "f"))))

; Create CrossSections and verify that they got created
; We expect 3 x (3+4+3) = 30 of them.
(csc 'explode-sections)
(test-equal 30 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 4 (length (gsc 'right-stars (Word "g"))))
(test-equal 4 (length (gsc 'right-stars (Word "h"))))

(define (filter-type wrd atype)
	(filter
		(lambda (atom) (equal? (cog-type atom) atype))
		(gsc 'right-stars wrd)))

(define (len-type wrd atype)
	(length (filter-type wrd atype)))

; Expect 3 Sections and two CrossSections on e.
(test-equal 5 (length (gsc 'right-stars (Word "e"))))
(test-equal 4 (length (gsc 'right-stars (Word "j"))))
(test-equal 3 (length (gsc 'right-stars (Word "f"))))

(test-equal 3 (len-type (Word "e") 'Section))
(test-equal 2 (len-type (Word "e") 'CrossSection))
(test-equal 4 (len-type (Word "j") 'Section))
(test-equal 0 (len-type (Word "j") 'CrossSection))
(test-equal 3 (len-type (Word "f") 'Section))
(test-equal 0 (len-type (Word "f") 'CrossSection))

; We expect a total of 3+4+3=10 Sections
(test-equal 10 (length (cog-get-atoms 'Section)))

; --------------------------
; Merge two sections together.
(define frac 0.25)
(define disc (make-fuzz gsc 0 frac 4 0))
(disc 'merge-function (Word "e") (Word "j"))

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

; Of the 10=3+4+3 original Sections, 4 are deleted, and 5 are created,
; leaving a grand total of 11. The 5 new ones are all e-j, the
; remaining three ones are "e" or "j" with reduced counts.
; This is just a total over everything above.
(test-equal 11 (length (cog-get-atoms 'Section)))

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
; Expect 33 CrossSections as described above.
(test-equal 33 (length (cog-get-atoms 'CrossSection)))

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

; (test-end t-three-cluster)

; ---------------------------------------------------------------
