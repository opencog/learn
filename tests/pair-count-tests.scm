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

(define word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The")
		(make-word-pair "###LEFT-WALL###" "first")
		(make-word-pair "###LEFT-WALL###" "test-sentence.")
		(make-word-pair "The" "first")
		(make-word-pair "The" "test-sentence.")
		(make-word-pair "first" "test-sentence.")
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second")
		(make-word-pair "###LEFT-WALL###" "one")
		(make-word-pair "The" "second")
		(make-word-pair "The" "one")
		(make-word-pair "second" "one")
	)
)

; -------------------------------------------------
; First mode to check: clique
(define mode "clique")
(define window 2)
(observe-text-mode test-str-1 mode window)
(observe-text-mode test-str-2 mode window)

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
		(reset-atom-count atom)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Second mode to check: clique-dist
(define mode "clique-dist")
(define window 2)
(observe-text-mode test-str-1 mode window)
(observe-text-mode test-str-2 mode window)

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
		(reset-atom-count atom)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Third mode to check: any

; Generator of word-pair atoms for testing
(define (make-word-pair word1 word2)
	(define pare (ListLink (WordNode word1) (WordNode word2)))
	(define pair-atom (EvaluationLink (LinkGrammarRelationshipNode "ANY") pare))
	(reset-atom-count pair-atom) ; avoid interference if database is pre-used
	pair-atom ; return the atom for the word-pair
)

(set! word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The")
		(make-word-pair "###LEFT-WALL###" "first")
		(make-word-pair "###LEFT-WALL###" "test-sentence.")
		(make-word-pair "The" "first")
		(make-word-pair "The" "test-sentence.")
		(make-word-pair "first" "test-sentence.")
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second")
		(make-word-pair "###LEFT-WALL###" "one")
		(make-word-pair "The" "second")
		(make-word-pair "The" "one")
		(make-word-pair "second" "one")
	)
)

; Just a minumum of times each pair should have been observed
; because "any" mode is random
(set! counts-list
	(list 1 1 1 1 1 1 1 1 1 1 1)
)

(define mode "any")
(define window 0)
(observe-text-mode test-str-1 mode window)
(observe-text-mode test-str-2 mode window)

; Test that pairs were counted, meaning parsing took effect
; This test is less strict than prvious, because there is
; a random component in `any` mode
(define check-cnts-text "Checking minimum counts in 'any' mode")
(for-each
	(lambda (atom count)
		(test-assert check-cnts-text (>= (get-counts atom) count))
		(reset-atom-count atom)
	)
	word-pair-atoms counts-list
)

; -------------------------------------------------
; Close testing database and suite
(sql-close)

(test-end suite-name)
