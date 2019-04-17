; Perform unit tests for the pair-counting part of the ULL pipeline

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "pair-count-tests")

;------------------------------------------------------------------------------
; Setup file contains unit-test utility functions
(load "setup.scm") 

; Open the database.
(sql-open "postgres:///ULL_tests")

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

(define cnt-mode "clique")

(define word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "first" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "test-sentence." cnt-mode 0)
		(make-word-pair "The" "first" cnt-mode 0)
		(make-word-pair "The" "test-sentence." cnt-mode 0)
		(make-word-pair "first" "test-sentence." cnt-mode 0)
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "one" cnt-mode 0)
		(make-word-pair "The" "second" cnt-mode 0)
		(make-word-pair "The" "one" cnt-mode 0)
		(make-word-pair "second" "one" cnt-mode 0)
	)
)

; -------------------------------------------------
; First mode to check: clique
(define window 2)
(observe-text-mode test-str-1 cnt-mode window)
(observe-text-mode test-str-2 cnt-mode window)

; Counts that each pair should have been observed
; First pair appears on both sentences
; 3rd and 8th pairs are out of window
(define counts-list
	(list 2 1 0 1 1 1 1 0 1 1 1)
)

; Test that tokenization is done by simple space splitting
(test-assert "Space-based tokenization" 
	(= (get-counts (list-ref word-pair-atoms 4)) (list-ref counts-list 4)))

; Test that LEFT-WALL is included
(test-assert "LEFT-WALL added to parse" 
	(= (get-counts (list-ref word-pair-atoms 1)) (list-ref counts-list 1)))

; Test that the rest of the pairs were counted correctly
(define check-cnts-text "Checking correct counts clique")
(for-each
	(lambda (atom count)
		(test-assert check-cnts-text (= (get-counts atom) count))
		(set-atom-count atom 0)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Second mode to check: clique-dist
(define cnt-mode "clique-dist")
(define window 2)
(observe-text-mode test-str-1 cnt-mode window)
(observe-text-mode test-str-2 cnt-mode window)

; Counts that each pair should have been observed, w/distance modifier
; First pair appears on both sentences
; 3rd and 8th pairs are out of window
(set! counts-list
	(list 4 1 0 2 1 2 1 0 2 1 2)
)

; Test that pairs were counted correctly
(define check-cnts-text "Checking correct counts clique-dist")
(for-each
	(lambda (atom count)
		(test-assert check-cnts-text (= (get-counts atom) count))
		(set-atom-count atom 0)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Third mode to check: any
(define cnt-mode "any")

(set! word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "first" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "test-sentence." cnt-mode 0)
		(make-word-pair "The" "first" cnt-mode 0)
		(make-word-pair "The" "test-sentence." cnt-mode 0)
		(make-word-pair "first" "test-sentence." cnt-mode 0)
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second" cnt-mode 0)
		(make-word-pair "###LEFT-WALL###" "one" cnt-mode 0)
		(make-word-pair "The" "second" cnt-mode 0)
		(make-word-pair "The" "one" cnt-mode 0)
		(make-word-pair "second" "one" cnt-mode 0)
	)
)

; Just a minumum of times each pair should have been observed
; because "any" mode is random
(set! counts-list
	(list 1 1 1 1 1 1 1 1 1 1 1)
)

(define window 0) ; all parses returned by LG "any"
(observe-text-mode test-str-1 cnt-mode window)
(observe-text-mode test-str-2 cnt-mode window)

; Test that pairs were counted, meaning parsing took effect
; This test is less strict than prvious, because there is
; a random component in `any` mode
(define check-cnts-text "Checking minimum counts in 'any' mode")
(for-each
	(lambda (atom count)
		(test-assert check-cnts-text (>= (get-counts atom) count))
		(set-atom-count atom 0)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Close testing database and suite
(sql-close)

(test-end suite-name)
