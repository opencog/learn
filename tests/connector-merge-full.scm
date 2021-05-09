;
; connector-merge-full.scm
; Unit test for merging of Connectors - full case.
;
; Tests merging of several words into a single word-class.
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

(opencog-test-runner)

(load "connector-setup.scm")
(load "connector-data.scm")

; ---------------------------------------------------------------
;
; This diagram explains what is being tested here:
;
; (e, abc) + (j, abc) -> ({ej}, abc)
; (e, dgh) + (j, dgh) -> ({ej}, dgh)
; (e, klm) +  none    -> p * ({ej}, klm) + (1-p) * (e, klm)
;  none    + (j, abe) -> p * ({ej}, abx) + (1-p) * (j, aby)
;  none    + (j, egh) -> p * ({ej}, zgh) + (1-p) * (j, wgh)
;
; In this diagram, (e,abc) is abbreviated notation for
; (Section (Word e) (ConnectorList (Connector a) (Connector b) (Connector c)))
; and so on.
; {ej} is short for (WordClassNode "e j") (a set of two words)
; "p" is the fraction to merge == 0.25, hard-coded below.
;
; This is similar to the "simple start-cluster merge test" except
; that now, "j" has two more sections. The merge follows the general
; pattern as before, but with a twist: what should x,y,z,w be in the
; above? There are several choices:
;
; 1)   x and  y could both be "e"
; 2)   x and  y could both be "ej"
; 3)   x could be "ej" and y could be just "e"
;

(define t-start-cluster "full start-cluster merge test")
(test-begin t-start-cluster)

; Open the database
(setup-database)

; Load some data
(setup-e-j-sections)
(setup-j-extra)

; Define matrix API to the data
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(define gsc (add-cluster-gram csc))

; Verify that the data loaded correctly
; We expect 3 sections on "e" and four on "j"
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 4 (length (gsc 'right-stars (Word "j"))))

; Create CrossSections and verify that they got created
; We expect 3 x (3+4) = 21 of them.
(csc 'explode-sections)
(test-equal 21 (length (cog-get-atoms 'CrossSection)))

; Verify that direct-sum object is accessing shapes correctly
; i.e. the 'explode should have created some CrossSections
(test-equal 3 (length (gsc 'right-stars (Word "g"))))
(test-equal 3 (length (gsc 'right-stars (Word "h"))))

(define (len-type wrd atype)
	(length (filter
		(lambda (atom) (equal? (cog-type atom) atype))
		(gsc 'right-stars wrd))))

; Expect 3 Sections and two CrossSections on e.
(test-equal 5 (length (gsc 'right-stars (Word "e"))))
(test-equal 4 (length (gsc 'right-stars (Word "j"))))

(test-equal 3 (len-type (Word "e") 'Section))
(test-equal 2 (len-type (Word "e") 'CrossSection))
(test-equal 4 (len-type (Word "j") 'Section))
(test-equal 0 (len-type (Word "j") 'CrossSection))

; We expect a total of 3+4=7 Sections
(test-equal 7 (length (cog-get-atoms 'Section)))

; Merge two sections together.
(define frac 0.25)
(define disc (make-fuzz gsc 0 frac 4 0))
(disc 'merge-function (Word "e") (Word "j"))

#! =====================
; We expect one section left on "e", the klm section, and two
; cross-sections. The two cross-sections should correspond
; to the sections (e, ab{ej}) and (e, {ej}gh).
; Why? Because fractional pick-up means these two sections get created,
; and we expect matching cross-sections to be populated as well.
(test-equal 3 (length (gsc 'right-stars (Word "e"))))
(test-equal 1 (len-type (Word "e") 'Section))
(test-equal 2 (len-type (Word "e") 'CrossSection))

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
(test-approximate (* cnt-e-klm (- 1.0 frac))
	(cog-count (car (gsc 'right-stars (Word "e")))) 0.001)

; TODO: validate counts on the other Sections...

; Of the 15 original CrossSections, 12 are deleted outright, and three
; get thier counts reduced (the e-klm crosses). A total of 3x3=9 new
; crosses get created, leaving a grand-total of 12.
(test-equal 12 (length (cog-get-atoms 'CrossSection)))

; TODO: validate counts on the CrossSections...
============ !#

(test-end t-start-cluster)

; ---------------------------------------------------------------
