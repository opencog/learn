;
; connector-merge.scm
;

(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog nlp learn))
(use-modules (opencog persist) (opencog persist-rocks))

(use-modules (opencog test-runner))

(opencog-test-runner)

; ---------------------------------------------------------------
; Database management
(define (setup-database)

	(define dbdir "/tmp/test-merge")
	(cog-atomspace-clear)

	; Create directory if needed.
	(if (not (access? dbdir F_OK)) (mkdir dbdir))
	(define storage-node (RocksStorageNode
		(string-append "rocks://" dbdir)))
	(cog-open storage-node)
)

; ---------------------------------------------------------------
(define t-simple "simplest merge test")
(test-begin t-simple)

; Open the database
(setup-database)

; Load some data
(load "connector-data.scm")
(setup-basic-sections)

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
(test-equal 2 (length (gsc 'right-stars (Word "g"))))
(test-equal 2 (length (gsc 'right-stars (Word "h"))))

; Merge two sections together.
(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))

; We expect just one section left on "e", the klm section.
(test-equal 1 (length (gsc 'right-stars (Word "e"))))

; We expect no sections left on j
(test-equal 0 (length (gsc 'right-stars (Word "j"))))

; We expect three merged sections
(test-equal 3 (length (gsc 'right-stars (WordClassNode "e j"))))

; 15 CrossSections should have been deleted; 9 should have
; been created. All nine should have the WordClass as the point of the
; Shape... XXX hang on, that's not right...
(test-equal 9 (length (cog-get-atoms 'CrossSection)))

(test-end t-simple)

; ---------------------------------------------------------------
