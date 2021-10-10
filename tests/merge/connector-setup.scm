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
; Utilities

(define (filter-type wrd atype)
	(filter
		(lambda (atom) (equal? (cog-type atom) atype))
		(gsc 'right-stars wrd)))

(define (len-type wrd atype)
	(length (filter-type wrd atype)))

; ---------------------------------------------------------------

; Merge words/word-classes WA WB into a cluster.
(define (merge LLOBJ WA WB FRAC)
	(define (frac WA WB) FRAC)
	(define (always WA WB) #t)
	(define (noop W) #f)
	(define (final) #f)
	(define mrg (make-merger LLOBJ always frac 0 0 noop final #t))
	(mrg 'merge-function WA WB)
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

(define (check-one-cross LLOBJ CROSS EPSILON)
"
  check-one-cross -- Check one CrossSection, verify that the counts
  on it matches it's corresponding Section.

  Return #t if it matches, else print an error message and return #f.
"
	(define sect (LLOBJ 'get-section CROSS))
	(if (nil? sect)
		(begin
			(format #t "Error: no Section for ~A" CROSS)
			#f)
		(let ((diff (- (cog-count sect) (cog-count CROSS))))
			(if (< (abs diff) EPSILON) #t
				(begin
					(format #t "Error: Unbalanced at\n~A~A" sect CROSS)
					#f))))
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
		(lambda (cross) (check-one-cross LLOBJ cross EPSILON))
		(cog-get-atoms 'CrossSection))
)

(define (check-shapes LLOBJ EPSILON)
"
  check-shapes -- Loop over Shapes, verify counts match Sections

  Self-consistent detailed balance requires that counts on CrossSections
  should be equal to the counts on the Sections from which they came.
  Return #t if everything balances, else return #f and print the
  imbalance.

  This performs exactly the same checks as `check-crosses`, but in a
  different order.
"
	(every
		(lambda (SHAPE)
			(every
				(lambda (cross) (check-one-cross LLOBJ cross EPSILON))
				(cog-incoming-by-type SHAPE 'CrossSection)))
		(cog-get-atoms 'ShapeLink))
)

; ---------------------------------------------------------------
