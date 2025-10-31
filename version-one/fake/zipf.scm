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
; This causes truly bizarro error in the guile compiler.
; Calling floor a second time really messes it up!
; (define (exact FLOAT) (inexact->exact (floor FLOAT)))
(define (exact FLOAT) (inexact->exact FLOAT))

; Load the actual implementation.
; (load "zipf-zri.scm")
