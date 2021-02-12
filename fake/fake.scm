;
; Artificial grammar module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog nlp fake))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(load "fake/zipf.scm")
(load "fake/zipf-zri.scm")
(load "fake/random-dict.scm")
(load "fake/print-dict.scm")
