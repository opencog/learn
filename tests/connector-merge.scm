;
; connector-merge.scm
;

(use-modules (opencog) (opencog nlp))
(use-modules (opencog nlp learn))

(use-modules (opencog test-runner))

(opencog-test-runner)

; ---------------------------------------------------------------
(define tgra "sniff test")
(test-begin tgra)

(define x #t)
(define y (not #f))

(test-equal "foo" x y)
(test-end tgra)

; ---------------------------------------------------------------
