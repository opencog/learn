; Perform unit tests for the MST-parsing part of the ULL pipeline.

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "MST-parse-test")

; Setup
(load "setup.scm") ; custom unit-test utilities

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

; Sets MI value for word-pair atom
(define (set-MI-value PAIR-ATOM MI-VALUE)
	(cog-set-value! PAIR-ATOM mi-key (FloatValue 0 MI-VALUE))
)

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

; Open the database.
(sql-open "postgres:///MST-parse-test")

; First mode to check: clique, without mst-distance
(define cnt-mode "clique")
(define mst-dist #f)

; Only create word-pairs that were observed. Counts are
; irrelevant, as we'll attach the MI-values next
(define word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "first" cnt-mode 0)
		(make-word-pair "The" "first" cnt-mode 0)
		(make-word-pair "The" "test-sentence." cnt-mode 0)
		(make-word-pair "first" "test-sentence." cnt-mode 0)
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second" cnt-mode 0)
		(make-word-pair "The" "second" cnt-mode 0)
		(make-word-pair "The" "one" cnt-mode 0)
		(make-word-pair "second" "one" cnt-mode 0)
	)
)

; manually calculated, arbitrary values for each pair in the list
; suited to test parses for the mst-parser
(define MI-list
	(list (- (log2 5) 1) (- (log2 5) 2) (- (log2 5) 2) (- (log2 5) 2) (log2 5)
	(1) (1) (1) (1))
)

; Assign mi-values to each atom
(for-each
	(lambda (pair-atom mi-value)
		(set-MI-value pair-atom mi-value)
	)
	word-pair-atoms MI-list
)

; Parse the sentences
(define parse-1 (observe-mst-mode test-str-1 cnt-mode mst-dist #f))
(define parse-2 (observe-mst-mode test-str-2 cnt-mode mst-dist #f))

; manually calculated, expected parses
(define w1 (cons 1 (WordNode "###LEFT-WALL###")))
(define w2 (cons 2 (WordNode "The")))
(define w3 (cons 3 (WordNode "first")))
(define w4 (cons 4 (WordNode "test-sentence.")))
(define expected-parse-1
	(append 
		(append 
			(list (cons (cons w3 w4) (log2 5)))
			(list (cons (cons w2 w4) (- (log2 5) 2)))
		)
		(list (cons (cons w1 w2) (- (log2 5) 1)))
	)
)

(set! w3 (cons 3 (WordNode "second")))
(set! w4 (cons 4 (WordNode "one")))
(define expected-parse-2
	(append 
		(append 
			(list (cons (cons w3 w4) (log2 5)))
			(list (cons (cons w2 w4) (- (log2 5) 2)))
		)
		(list (cons (cons w1 w2) (- (log2 5) 1)))
	)
)

; Test that MST-parses are as expected
(define check-MST-text "Checking MST-parses clique no mst-dist")
(test-assert check-MST-text (equal? parse-1 expected-parse-1))
(test-assert check-MST-text (equal? parse-2 expected-parse-2))

; -------------------------------------------------
; Close testing database and suite
(sql-close)

(test-end suite-name)
