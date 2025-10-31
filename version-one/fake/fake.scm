;
; Artificial grammar module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog nlp fake))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
;
; The zipf files are stolen from srfi-194 and can be removed when
; guile implemments srfi-194.
(include-from-path "opencog/nlp/fake/zipf.scm")
(include-from-path "opencog/nlp/fake/zipf-zri.scm")
(include-from-path "opencog/nlp/fake/random-dict.scm")
(include-from-path "opencog/nlp/fake/print-dict.scm")
