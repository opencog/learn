; Perform unit tests for the pair-counting part of the ULL pipeline

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "pair-count-tests")

;------------------------------------------------------------------------------
; Setup

(use-modules
  (opencog)
  (opencog nlp)
  (opencog persist)
  (opencog persist-sql)
  (opencog cogserver))

; NOTE
; 1. The files are loaded in pipeline order. In general, the later files
;  depend on definitions contained
; in the earlier files.
; 2. Poc changes are loaded after files of the same name are loaded so as to
; redfefine the functions.
; 3. load-from-path is used so as to be able to redfine some functions. If
; (opencog nlp learn) that will not be possilbe as some of the functions
; are not exported.
(load-from-path "opencog/nlp/learn/common.scm")
(load "../run-poc/redefine-common.scm")
(load-from-path "opencog/nlp/learn/utilities.scm")
(load-from-path "opencog/nlp/learn/link-pipeline.scm")
(load "../run-poc/redefine-link-pipeline.scm")
(load-from-path "opencog/nlp/learn/singletons.scm")
(load-from-path "opencog/nlp/learn/batch-word-pair.scm")
(load-from-path "opencog/nlp/learn/mst-parser.scm")
(load "../run-poc/redefine-mst-parser.scm")
(load-from-path "opencog/nlp/learn/pseudo-csets.scm")
(load-from-path "opencog/nlp/learn/shape-vec.scm")
(load-from-path "opencog/nlp/learn/summary.scm")
(load-from-path "opencog/nlp/learn/gram-class.scm")
(load-from-path "opencog/nlp/learn/gram-agglo.scm")

; Resets atom count to zero
(define (reset-atom-count ATOM)
	(cog-set-tv! ATOM (cog-new-ctv 0 0 0))
	(store-atom ATOM)
)

; Generator of word-pair atoms for testing
(define (make-word-pair word1 word2)
	(define pare (ListLink (WordNode word1) (WordNode word2)))
	(define pair-atom (EvaluationLink pair-pred pare))
	(reset-atom-count pair-atom) ; avoid interference if database is pre-used
	pair-atom ; return the atom for the word-pair
)

; Gets count value from word-pair atom
(define (get-counts PAIR-ATOM)
	;(fetch-atom PAIR-ATOM)
	(cog-value-ref (cog-tv PAIR-ATOM) 2)
)

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

; Open the database.
(sql-open "postgres:///pair-count-test")

; First sentence possible pairs
(define tok-pair-1 (make-word-pair "###LEFT-WALL###" "The"))
(define tok-pair-2 (make-word-pair "###LEFT-WALL###" "first"))
(define tok-pair-3 (make-word-pair "###LEFT-WALL###" "test-sentence."))
(define tok-pair-4 (make-word-pair "The" "first"))
(define tok-pair-5 (make-word-pair "The" "test-sentence."))
(define tok-pair-6 (make-word-pair "first" "test-sentence."))

; First sentence possible pairs
(define tok-pair-7 (make-word-pair "###LEFT-WALL###" "second"))
(define tok-pair-8 (make-word-pair "###LEFT-WALL###" "one"))
(define tok-pair-9 (make-word-pair "The" "second"))
(define tok-pair-10 (make-word-pair "The" "one"))
(define tok-pair-11 (make-word-pair "second" "one"))

; First mode to check: clique
(define mode "clique")
(define window 2)
(observe-text-mode test-str-1 mode window)
(observe-text-mode test-str-2 mode window)

; Test that tokenization is done by simple space splitting
(test-assert "Space-based tokenization" (= (get-counts tok-pair-5) 1))

; Test that LEFT-WALL is included
(test-assert "LEFT-WALL added to parse" (= (get-counts tok-pair-2) 1))

; Test that the rest of the pairs were counted correctly
(define check-cnts-text "Checking correct counts clique")
(test-assert check-cnts-text (= (get-counts tok-pair-1) 2)) ; Two occurences
(test-assert check-cnts-text (= (get-counts tok-pair-3) 0)) ; Out of window
(test-assert check-cnts-text (= (get-counts tok-pair-4) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-6) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-7) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-8) 0)) ; Out of window
(test-assert check-cnts-text (= (get-counts tok-pair-9) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-10) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-11) 1))

(reset-atom-count tok-pair-1)
(reset-atom-count tok-pair-2)
(reset-atom-count tok-pair-3)
(reset-atom-count tok-pair-4)
(reset-atom-count tok-pair-5)
(reset-atom-count tok-pair-6)
(reset-atom-count tok-pair-7)
(reset-atom-count tok-pair-8)
(reset-atom-count tok-pair-9)
(reset-atom-count tok-pair-10)
(reset-atom-count tok-pair-11)

; Second mode to check: clique-dist
(define mode "clique-dist")
(define window 2)
(observe-text-mode test-str-1 mode window)
(observe-text-mode test-str-2 mode window)

; Test that pairs were counted correctly
(define check-cnts-text "Checking correct counts clique-dist")
(test-assert check-cnts-text (= (get-counts tok-pair-1) 4)) ; Two occurences
(test-assert check-cnts-text (= (get-counts tok-pair-2) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-3) 0)) ; Out of window
(test-assert check-cnts-text (= (get-counts tok-pair-4) 2))
(test-assert check-cnts-text (= (get-counts tok-pair-5) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-6) 2))
(test-assert check-cnts-text (= (get-counts tok-pair-7) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-8) 0)) ; Out of window
(test-assert check-cnts-text (= (get-counts tok-pair-9) 2))
(test-assert check-cnts-text (= (get-counts tok-pair-10) 1))
(test-assert check-cnts-text (= (get-counts tok-pair-11) 2))

(sql-close)

(test-end suite-name)
