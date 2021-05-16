;
; connector-setup.scm
; Database setup common to all tests.

(use-modules (srfi srfi-1))

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
; Detailed balance

(define (check-sections LLOBJ EPSILON)
"
  check-sections -- Loop over Sections, verify counts match Crosses

  Self-consistent detailed balance requires that counts on CrossSections
  should be equal to the counts on the Sections from which they came.
  Return #t if everything balances, else return #f and print the
  imbalance.
"
	(every
		(lambda (sect)
			(define scnt (cog-count sect))
			(every
				(lambda (cross)
					(define diff (- scnt (cog-count cross)))
					(if (< (abs diff) EPSILON) #t
						(begin
							(format #t "Error: Unbalanced at\n~A~A" sect cross)
							#f)))
				(LLOBJ 'get-cross-sections sect)))
		(cog-get-atoms 'Section))
)

(define (check-crosses LLOBJ EPSILON)
"
  check-crosses -- Loop over CrossSections, verify counts match Sections

  Self-consistent detailed balance requires that counts on CrossSections
  should be equal to the counts on the Sections from which they came.
  Return #t if everything balances, else return #f and print the
  imbalance.

  This performs exactly the same checks as `check-sections`, but in a
  different order.
"
	(every
		(lambda (cross)
			(define sect (LLOBJ 'get-section cross))
			(define diff (- (cog-count sect) (cog-count cross)))
			(if (< (abs diff) EPSILON) #t
				(begin
					(format #t "Error: Unbalanced at\n~A~A" sect cross)
					#f)))
		(cog-get-atoms 'CrossSection))
)

; ---------------------------------------------------------------
