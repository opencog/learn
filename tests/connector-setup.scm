;
; connector-setup.scm
; Database setup common to all tests.

(use-modules (opencog))
(use-modules (opencog persist) (opencog persist-rocks))

(define storage-node #f)

; ---------------------------------------------------------------
; Database management
(define (setup-database)

	; If already open, close it.
	(if (cog-atom? storage-node) (cog-close storage-node))

	(define dbdir "/tmp/test-merge")
	(cog-atomspace-clear)

	; Create directory if needed.
	(if (not (access? dbdir F_OK)) (mkdir dbdir))
	(set! storage-node (RocksStorageNode
		(string-append "rocks://" dbdir)))
	(cog-open storage-node)
)

; ---------------------------------------------------------------
