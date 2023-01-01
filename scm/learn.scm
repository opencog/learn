;
; Language learning module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog learn))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(include-from-path "opencog/learn/common.scm")
(include-from-path "opencog/learn/utilities.scm")
(include-from-path "opencog/learn/pair-count-window.scm")
(include-from-path "opencog/learn/word-pair-count.scm")
(include-from-path "opencog/learn/sliding-block.scm")
(include-from-path "opencog/learn/batch-pair.scm")
(include-from-path "opencog/learn/pseudo-csets.scm")
(include-from-path "opencog/learn/lg-parser.scm")
(include-from-path "opencog/learn/shape-vec.scm")
(include-from-path "opencog/learn/shape-project.scm")
(include-from-path "opencog/learn/gram-majority.scm")
(include-from-path "opencog/learn/gram-optim.scm")
(include-from-path "opencog/learn/in-group.scm")
(include-from-path "opencog/learn/similarity.scm")
(include-from-path "opencog/learn/mi-similarity.scm")
(include-from-path "opencog/learn/jaccard.scm")
(include-from-path "opencog/learn/log-merge.scm")
(include-from-path "opencog/learn/gram-class-api.scm")
(include-from-path "opencog/learn/agglo-mi-rank.scm")
(include-from-path "opencog/learn/agglo-goe.scm")
(include-from-path "opencog/learn/singletons.scm")
(include-from-path "opencog/learn/gram-filters.scm")
(include-from-path "opencog/learn/lg-compare.scm")
