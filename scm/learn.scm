;
; Language learning module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog nlp learn))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(include-from-path "opencog/nlp/learn/common.scm")
(include-from-path "opencog/nlp/learn/utilities.scm")
(include-from-path "opencog/nlp/learn/link-pipeline.scm")
(include-from-path "opencog/nlp/learn/singletons.scm")
(include-from-path "opencog/nlp/learn/batch-word-pair.scm")
(include-from-path "opencog/nlp/learn/mst-parser.scm")
(include-from-path "opencog/nlp/learn/pseudo-csets.scm")
(include-from-path "opencog/nlp/learn/shape-vec.scm")
(include-from-path "opencog/nlp/learn/summary.scm")
(include-from-path "opencog/nlp/learn/vectors.scm")
(include-from-path "opencog/nlp/learn/gram-classification.scm")
(include-from-path "opencog/nlp/learn/cset-merge.scm")
(include-from-path "opencog/nlp/learn/gram-projective.scm")
(include-from-path "opencog/nlp/learn/gram-optim.scm")
(include-from-path "opencog/nlp/learn/gram-agglo.scm")
(include-from-path "opencog/nlp/learn/gram-class-api.scm")
; (include-from-path "opencog/nlp/learn/link-class.scm")
(include-from-path "opencog/nlp/learn/lg-compare.scm")
