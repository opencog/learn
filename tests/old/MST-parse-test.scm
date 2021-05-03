; Perform unit tests for the MST-parsing part of the ULL pipeline.

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "MST-parse-test")

;-------------------------------------------------------
; Setup file contains unit-test utility functions
(load "setup.scm")

; Open the database.
(sql-open "postgres:///ULL_tests")

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

; Sets MI value for word-pair atom
(define (set-MI-value PAIR-ATOM MI-VALUE)
	(cog-set-value! PAIR-ATOM mi-key (FloatValue 0 MI-VALUE))
)

; Only create word-pairs that were observed. Counts are
; irrelevant, as we'll attach the MI-values next
(define (make-word-pair-list CNT-MODE)
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The" CNT-MODE 0)
		(make-word-pair "###LEFT-WALL###" "first" CNT-MODE 0)
		(make-word-pair "The" "first" CNT-MODE 0)
		(make-word-pair "The" "test-sentence." CNT-MODE 0)
		(make-word-pair "first" "test-sentence." CNT-MODE 0)
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second" CNT-MODE 0)
		(make-word-pair "The" "second" CNT-MODE 0)
		(make-word-pair "The" "one" CNT-MODE 0)
		(make-word-pair "second" "one" CNT-MODE 0)
	)
)

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

; First mode to check: clique, without mst distance multipliers
(define cnt-mode "clique")
(define dist-mult '(1))

(define word-pair-atoms (make-word-pair-list cnt-mode))

; manually calculated, arbitrary values for each pair in the list
; suited to test parses for the mst-parser
(define MI-list
	(list (- (log2 5) 1) (- (log2 5) 2) (- (log2 5) 2) (- (log2 5) 2) (log2 5)
	1 1.1 1.4 0.8)
)

; Assign mi-values to each atom
(for-each
	(lambda (pair-atom mi-value)
		(set-MI-value pair-atom mi-value)
	)
	word-pair-atoms MI-list
)

; Parse the sentences
(define parse-1 (observe-mst-mode test-str-1 cnt-mode dist-mult "NONE"))
(define parse-2 (observe-mst-mode test-str-2 cnt-mode dist-mult "NONE"))

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
			(list (cons (cons w2 w4) 1.4))
			(list (cons (cons w1 w2) (- (log2 5) 1)))
		)
		(list (cons (cons w2 w3) 1.1))
	)
)

; Test that MST-parses are as expected
(define check-MST-text "Checking MST-parses: 'clique' mode, no dist-mult")
(test-assert check-MST-text (equal? parse-1 expected-parse-1))
(test-assert check-MST-text (equal? parse-2 expected-parse-2))

; -------------------------------------------------
; Second mode to check: any, with dist-mult
; any doesn't change the MST-parser, just testing option works
(set! cnt-mode "any")
(set! dist-mult '(1 0.5 0.1))

(set! word-pair-atoms (make-word-pair-list cnt-mode))

; Assign mi-values to each atom
(for-each
	(lambda (pair-atom mi-value)
		(set-MI-value pair-atom mi-value)
	)
	word-pair-atoms MI-list
)

; Parse the sentences
(set! parse-1 (observe-mst-mode test-str-1 cnt-mode dist-mult "NONE"))
(set! parse-2 (observe-mst-mode test-str-2 cnt-mode dist-mult "NONE"))

; manually calculated, expected parses
(set! w3 (cons 3 (WordNode "first")))
(set! w4 (cons 4 (WordNode "test-sentence.")))
(define expected-parse-1
	(append 
		(append 
			(list (cons (cons w3 w4) (log2 5)))
			(list (cons (cons w2 w3) (- (log2 5) 2)))
		)
		(list (cons (cons w1 w2) (- (log2 5) 1)))
	)
)

(set! w3 (cons 3 (WordNode "second")))
(set! w4 (cons 4 (WordNode "one")))
(define expected-parse-2
	(append 
		(append 
			(list (cons (cons w1 w2) (- (log2 5) 1)))
			(list (cons (cons w2 w3) 1.1))
		)
		(list (cons (cons w3 w4) 0.8))
	)
)

; Test that MST-parses are as expected
(define check-MST-text "Checking MST-parses: 'any' mode with dist-mult")
(test-assert check-MST-text (equal? parse-1 expected-parse-1))
(test-assert check-MST-text (equal? parse-2 expected-parse-2))

; -------------------------------------------------
; Third mode to check: file 
; First test: no mst distance multipliers
(set! cnt-mode "file")
(set! dist-mult '(1 1))

(define text-block
"Test in file mode\n\
0 ###LEFT-WALL### 1 Test 2.1\n\
0 ###LEFT-WALL### 2 in 4.1\n\
0 ###LEFT-WALL### 3 file 4\n\
0 ###LEFT-WALL### 4 mode 1\n\
1 Test 2 in 2.05\n\
1 Test 3 file 1\n\
1 Test 4 mode 5\n\
2 in 3 file 1\n\
2 in 4 mode 1\n\
3 file 4 mode 2")

; Parse the sentences
(set! parse-1 (observe-mst-mode text-block cnt-mode dist-mult "NONE"))

; Manually calculated, expected parses (note that current heuristic doesn't
; give us the actual MST parse, but a close one).
; Atoms in "file" mode mst-parser have a different structure from other modes,
; needed to retain the word positions in the sentece.
(set! w1 (cons 1 (WordSequenceLink (WordNode "###LEFT-WALL###") (NumberNode 0))))
(set! w2 (cons 2 (WordSequenceLink (WordNode "Test") (NumberNode 1))))
(set! w3 (cons 3 (WordSequenceLink (WordNode "in") (NumberNode 2))))
(set! w4 (cons 4 (WordSequenceLink (WordNode "file") (NumberNode 3))))
(define w5 (cons 5 (WordSequenceLink (WordNode "mode") (NumberNode 4))))
(define expected-parse-1
	(append
		(append 
			(append 
				(list (cons (cons w2 w5) 5))
				(list (cons (cons w1 w2) 2.1))
			)
			(list (cons (cons w2 w3) 2.05))
		)
		(list (cons (cons w4 w5) 2))
	)
)

; Test that MST-parses are as expected
(define check-MST-text "Checking MST-parses: 'file' mode, no dist-mult")
(test-assert check-MST-text (equal? parse-1 expected-parse-1))

; Second test: mst-distance multipliers
(set! dist-mult '(1 0.5))

; Parse the sentences
(set! parse-2 (observe-mst-mode text-block cnt-mode dist-mult "NONE"))

; Manually calculated, expected parses
(define expected-parse-2
	(append
		(append 
			(append 
				(list (cons (cons w2 w5) 2.5))
				(list (cons (cons w1 w2) 2.1))
			)
			(list (cons (cons w2 w3) 2.05))
		)
		(list (cons (cons w4 w5) 2))
	)
)

; Test that MST-parses are as expected
(define check-MST-text "Checking MST-parses: 'file' mode, w/dist-mult")
(test-assert check-MST-text (equal? parse-2 expected-parse-2))

; -------------------------------------------------
; Close testing database and suite
(sql-close)

(test-end suite-name)
