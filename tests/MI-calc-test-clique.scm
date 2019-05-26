; Perform unit tests for the Mutual Information calculation
; in clique modes, part of the ULL pipeline.

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "MI-calc-test-clique")

;-------------------------------------------------------
; Setup file contains unit-test utility functions
(load "setup.scm")

; Open the database.
(sql-open "postgres:///ULL_calcMI_clique_test")

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

; First mode to check: clique and clique-dist, they use same api
(define cnt-mode "clique")

; Only create word-pairs that have positive counts
; otherwise there's a problem when calculating MI
; Counts are assigned by manually estimating how many
; times each pair should have been observed, considering a
; clique window of 2. First pair appears on both sentences
(define word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The" cnt-mode 2)
		(make-word-pair "###LEFT-WALL###" "first" cnt-mode 1)
		(make-word-pair "The" "first" cnt-mode 1)
		(make-word-pair "The" "test-sentence." cnt-mode 1)
		(make-word-pair "first" "test-sentence." cnt-mode 1)
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second" cnt-mode 1)
		(make-word-pair "The" "second" cnt-mode 1)
		(make-word-pair "The" "one" cnt-mode 1)
		(make-word-pair "second" "one" cnt-mode 1)
	)
)

; Run ULL pipeline to calculate MI
(comp-mi cnt-mode)

; manually calculated, expected MI values for each pair in the list
(define MI-list
	(list (- (log2 5) 1) (- (log2 5) 2) (- (log2 5) 2) (- (log2 5) 2) (log2 5)
	(- (log2 5) 2) (- (log2 5) 2) (- (log2 5) 2) (log2 5))
)
(define tolerance 0.000001) ; tolerated diff between MI-values

; Test that the MI values were calculated correctly by pipeline
(define check-MI-text "Checking correct MI values clique")

(for-each
	(lambda (atom expected-MI)
		(define diff (- (get-MI-value atom) expected-MI))
		(test-assert check-MI-text (< (abs diff) tolerance))
	)
	word-pair-atoms MI-list
)

; -------------------------------------------------
; Close testing database and suite
(sql-close)

(test-end suite-name)
