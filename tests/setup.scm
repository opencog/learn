; Loads required modules and defines common utilities
; for ULL unit tests

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
(define (make-word-pair word1 word2 MODE CNT)
	(define pare (ListLink (WordNode word1) (WordNode word2)))
	(define pair-atom
		(if (equal? MODE "any") 
			(EvaluationLink (LinkGrammarRelationshipNode "ANY") pare)
			(EvaluationLink pair-pred pare)
		)
	)
	(set-atom-count pair-atom CNT) ; avoid interference if database is pre-used
	pair-atom ; return the atom for the word-pair
)

; Gets MI value from word-pair atom
(define mi-key (Predicate "*-Mutual Info Key-*"))
(define (get-MI-value PAIR-ATOM)
	(cog-value-ref (cog-value PAIR-ATOM mi-key) 1)
)

; Gets count value from word-pair atom
(define (get-counts PAIR-ATOM)
	;(fetch-atom PAIR-ATOM)
	(cog-value-ref (cog-tv PAIR-ATOM) 2)
)
