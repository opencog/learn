;
; connector-merge-basic.scm
; Unit test for merging of Connectors - basic, simple case.
;
; Tests merging of several words into a single word-class.
; The focus here is to make sure that the CrossSections are
; merged correctly, and specifically, that the "point" of the
; CrossSection has the cluster in it.
;
; None of the sections have the words to be merged as connectors;
; this makes it "basic", since the complex connector merging logic
; is not triggered.
;
; Created May 2021

(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog nlp learn))

(use-modules (opencog test-runner))

(opencog-test-runner)

(load "connector-setup.scm")
(load "connector-data.scm")

; ---------------------------------------------------------------
;
; This diagram explains what is being tested here:
;
; (e, abc) + (j, abc) -> ([ej], abc)
; (e, dgh) + (j, dgh) -> ([ej], dgh)
; (e, klm) +  none    -> frac([ej], klm) + (1-frac)(e, klm)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; [ej] is short for (WordClassNode "e j")
; "frac" is the fraction to merge == 0.357 a magic number coming
; from the cosine angle between the vectors.
;

(define t-start-cluster "simple start-cluster merge test")
(test-begin t-start-cluster)

; Open the database
(setup-database)

; Load some data
(setup-e-j-sections)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(define gsc (add-cluster-gram csc))

; Verify that the data loaded correctly
; We expect 3 sections on "e" and two on "j"
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))

; Create CrossSections and verify that they got created
(csc 'explode-sections)
(test-equal 15 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 2 (length (gsc 'right-stars (Word "g"))))
(test-equal 2 (length (gsc 'right-stars (Word "h"))))

; Should not be any CrossSections on e,j; should be same as before.
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))

; We expect a total of 3+2=5 Sections
(test-equal 5 (length (cog-get-atoms 'Section)))

; Merge two sections together.
(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))

; We expect just one section left on "e", the klm section.
(test-equal 1 (length (gsc 'right-stars (Word "e"))))

; We expect no sections left on j
(test-equal 0 (length (gsc 'right-stars (Word "j"))))

; We expect three merged sections
(test-equal 3 (length (gsc 'right-stars (WordClassNode "e j"))))

; Of the 5 original Sections, 4 are deleted, and 3 are created,
; leaving a grand total of 4. The 3 new ones are all e-j, the
; remaining old one is an "e" with a reduced count.  This is just
; the sum of the above.
(test-equal 4 (length (cog-get-atoms 'Section)))

; Validate counts.
(define angl 0.35718064330452926) ; magic value from make-discrim
(test-approximate (* cnt-e-klm (- 1.0 angl))
	(cog-count (car (gsc 'right-stars (Word "e")))) 0.001)

; TODO: validate counts on the other Sections...

; Of the 15 original CrossSections, 12 are deleted outright, and three
; get thier counts reduced (the e-klm crosses). A total of 3x3=9 new
; crosses get created, leaving a grand-total of 12.
(test-equal 12 (length (cog-get-atoms 'CrossSection)))

; TODO: validate counts on the CrossSections...

(test-end t-start-cluster)

; ---------------------------------------------------------------
;
; Test much as above, but now adding "f" to the existing cluster "ej".
; This is a minor variation on the above, but tests a diffferent code
; branch.
(define t-merge-into-cluster "simple merge-into-cluster test")
(test-begin t-merge-into-cluster)

; Open the database
(setup-database)

; Load some data
(setup-e-j-sections)
(setup-f-sections)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(define gsc (add-cluster-gram csc))

; Verify that the data loaded correctly
; We expect 3 sections on "e" and two on "j"
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))
(test-equal 3 (length (gsc 'right-stars (Word "f"))))

; Create CrossSections and verify that they got created
(csc 'explode-sections)
(test-equal 24 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 3 (length (gsc 'right-stars (Word "g"))))
(test-equal 3 (length (gsc 'right-stars (Word "h"))))

; Should not be any CrossSections on e,j; should be same as before.
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 2 (length (gsc 'right-stars (Word "j"))))
(test-equal 3 (length (gsc 'right-stars (Word "f"))))

; We expect a total of 3+2+3=8 Sections
(test-equal 8 (length (cog-get-atoms 'Section)))

; Merge three sections together.
(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))
(disc 'merge-function (WordClassNode "e j") (Word "f"))

; We expect just one section left on "e", the klm section.
; We expect no sections left on j
(test-equal 1 (length (gsc 'right-stars (Word "e"))))
(test-equal 0 (length (gsc 'right-stars (Word "j"))))
(test-equal 0 (length (gsc 'right-stars (Word "f"))))

; We expect three merged sections
(test-equal 3 (length (gsc 'right-stars (WordClassNode "e j"))))

; Of the 8 original Sections, 7 are deleted, and 3 are created,
; leaving a grand total of 4. The 3 new ones are all e-j, the
; remaining old one is an "e" with a reduced count.  This is just
; the sum of the above.
(test-equal 4 (length (cog-get-atoms 'Section)))

; Validate counts.
(define angl 0.35718064330452926) ; magic value from make-discrim
(test-approximate (* cnt-e-klm (- 1.0 angl))
	(cog-count (car (gsc 'right-stars (Word "e")))) 0.001)

; TODO: validate counts on the other Sections...

; Of the 15 original CrossSections, 12 are deleted outright, and three
; get thier counts reduced (the e-klm crosses). A total of 3x3=9 new
; crosses get created, leaving a grand-total of 12.
(test-equal 12 (length (cog-get-atoms 'CrossSection)))

; TODO: validate counts on the CrossSections...

(test-end t-merge-into-cluster)

; ---------------------------------------------------------------
