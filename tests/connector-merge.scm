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

(load "connector-data.scm")

(setup-database)
(setup-basic-sections)

(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(csc 'explode-sections)
(define gsc (add-cluster-gram csc))

(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))


(test-equal "foo" #t (not #f))
(test-end t-simple)

; ---------------------------------------------------------------
