; Perform unit tests for the Mutual Information calculation
; in clique modes, part of the ULL pipeline.

(use-modules (opencog test-runner))

(opencog-test-runner)
; Name of test-suite
(define suite-name "MI-calc-test-clique")

;------------------------------------------------------------------------------
; Setup

(use-modules
  (opencog)
  (opencog nlp)
  (opencog persist)
  (opencog persist-sql)
  (opencog cogserver)
  (opencog matrix))

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
(load "../run-poc/compute-mi.scm")

; Define log2 function
(define (log2 x) (/ (log x) (log 2)))

; Sets ATOM count to desired value
(define (set-atom-count ATOM value)
	(cog-set-tv! ATOM (cog-new-ctv 0 0 value))
	(store-atom ATOM)
)

; Generator of word-pair atoms for testing
(define (make-word-pair word1 word2)
	(define pare (ListLink (WordNode word1) (WordNode word2)))
	(define pair-atom (EvaluationLink pair-pred pare))
	(set-atom-count pair-atom 0) ; avoid interference if database is pre-used
	pair-atom ; return the atom for the word-pair
)

; Gets MI value from word-pair atom
(define mi-key (Predicate "*-Mutual Info Key-*"))
(define (get-MI-value PAIR-ATOM)
	(cog-value-ref (cog-value PAIR-ATOM mi-key) 1)
)

(define test-str-1 "The first test-sentence.")
(define test-str-2 "The second one")

;-------------------------------------------------------
; Begin test
(test-begin suite-name)

; Open the database.
(sql-open "postgres:///MI-calc-test-clique")

; Only create word-pairs that have positive counts
; otherwise there's a problem when calculating MI
(define word-pair-atoms
	(list
		; First sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "The")
		(make-word-pair "###LEFT-WALL###" "first")
		(make-word-pair "The" "first")
		(make-word-pair "The" "test-sentence.")
		(make-word-pair "first" "test-sentence.")
		; Second sentence possible pairs
		(make-word-pair "###LEFT-WALL###" "second")
		(make-word-pair "The" "second")
		(make-word-pair "The" "one")
		(make-word-pair "second" "one")
	)
)

; -------------------------------------------------
; First mode to check: clique and clique-dist, they use same api
(define cnt-mode "clique")

; Counts that each pair should have been observed, considering a
; clique window of 2
; First pair appears on both sentences
(define counts-list
	(list 2 1 1 1 1 1 1 1 1)
)

; Set the counts for each pair as if it was observed
(for-each
	(lambda (atom count)
		(set-atom-count atom count)
	)
	word-pair-atoms counts-list
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
