;
; Create a Zipf random distribution.
;
; Created by Linas Vepstas 24 June 2020
; Nominated for inclusion in srfi-194
; Not yet unit-tested, may be buggy!
;
; Implementation taken from drobilla's May 24, 2017 answer to
; https://stackoverflow.com/questions/9983239/how-to-generate-zipf-distributed-numbers-efficiently
;
; That code is referenced with this:
; "Rejection-inversion to generate variates from monotone discrete
; distributions", Wolfgang Hörmann and Gerhard Derflinger
; ACM TOMACS 6.3 (1996): 169-184
;
; This code works best for large-N distributions; for small-N one
; can precompute an array and cache it.  For an example, see
; https://github.com/opencog/cogutil/blob/master/opencog/util/zipf.h
; The cached version takes longer to create, but once created, it runs
; 2x or 3x faster ... for small N (say, less than 300).  But for large
; N, the cache is blown (literally, L1 D-cache misses on the CPU core).
;
; Example usage:
;    (define zgen (make-zipf-generator 50 1.01))
;    (generator->list zgen 10)
;
;
; ------------------------------------------------------------
; Begin with private functions, not to be exported.
; Public API at bottom.

; Epsilon to avoid convergence issues with divide-by-zero.
; This is to avoid explosions due to scheme not having many
; common elementary functions (that I know of, at any rate).
(define epsilon 1.0e-8)

; (exp(x) - 1) / x 
; This partly works around the issue of scheme not having a native
; high-precision exp(x)-1 function that is accurate for small x.
; (that I know of). Uses the epsilon above.
(define (expxm1bx x)
	(if (< epsilon (abs x))
		(/ (- (exp x) 1) x)
		(+ 1 (* (/ x 2) (+ 1 (* (/ x 3) (+ 1 (/ x 4)))))))
)

; log(1 + x) / x 
; As before, uses the epsilon to work around the missing high-precision
; log(1+x) function. Bummer.
(define (log1pxbx x)
	(if (< epsilon x)
		(/ (log (+ 1 x)) x)
		(- 1 (* x (- (/ 1 2) (* x (- (/ 1 3) (* x (/ 1 4))))))))
)

; The hat function h(x) = 1 / (x ^ q) 
(define (hat x q) 
	(expt x (- q))
)

; The integral of hat(x)
; H(x) = log(x) if q == 1, (x^(1-q) - 1)/(1 - q) otherwise.
; Note the numerator is one less than in the paper order to
; work with all positive q.
(define (big-h x q)
	(define logx (log x))
	(* (expxm1bx (* (- 1 q) logx)) logx)
)

; The inverse function of H(x)
(define (big-h-inv x q)
	(define t (max -1 (* x (- 1 q))))
	(exp (* x (log1pxbx t)))
)

; Clamp x to [lo, hi].
(define (clamp x lo hi)
	(max lo (min x hi))
)

; ------------------------------------------------------------
; iota
(define (generator->list f n)
	(map (lambda (junk) (f)) (make-list n)))

; The public API
; Defaults should be n==int-max and q==1
(define (make-zipf-generator n q)
	(define big-h-x1 (- 1 (big-h 1.5 q)))
	(define big-h-n (big-h (+ n 0.5) q))
	(define ivl (- big-h-n big-h-x1))

	; random:uniform is a guile-specific uniform distribution over 0 to 1
	(define (dist) (+ big-h-x1 (* ivl (random:uniform))))

	; Attempt to hit the dartboard. Return #f if we fail,
	; otherwise return an integer between 1 and n.
	(define (try)
		(define u (dist))
		(define x (big-h-inv u q))
		(define flt-k (clamp (round x) 1 n))
		; Convert to integer. This is guile-specific, I don't know
		; generic scheme for this.
		(define k (inexact->exact (floor flt-k)))

		(if (>= u (- (big-h (+ k 0.5) q) (hat k q))) k #f))

	; Did we hit the dartboard? If not, try again.
	(define (loop-until)
		(define k (try))
		(if k k (loop-until)))

	; Return the generator.
	loop-until
)

*unspecified*
