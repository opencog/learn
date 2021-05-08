;
; connector-setup.scm
; Database setup common to all tests.

(use-modules (opencog))
(use-modules (opencog persist) (opencog persist-rocks))

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
