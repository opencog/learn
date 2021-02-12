;
; zipf.scm
;
; Zipf random distribution in scheme.
;
; This provides guile-specific scaffolding to enable using the
; srfi-194 random number generator. This is a hack. This is only
; needed because guile does not currently support srfi-194.
;

; Fake make-random-real-generator for guile.
(define (make-random-real-generator lo hi)
	(lambda ()
		(+ (* (- hi lo) (random:uniform)) lo))
)

; Convert float to exact integer.
(define (exact FLOAT) (inexact->exact (floor FLOAT)))

; Load the actual implementation.
; (load "zipf-zri.scm")
