;
; Export-to-Link-Grammar module.
;
; This is distinct from (opencog learn) because it depends on `dbi`
; which not everyone has installed, and so loading this module might
; trigger an error.
;
(define-module (opencog nlp lg-export))

; Just one file for now.
(include-from-path "opencog/nlp/lg-export/export-disjuncts.scm")
