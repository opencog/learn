;
; sphere.scm
; Uniform distributions on a sphere, and a ball.
; Submitted for inclusion in srfi-194
;
; Algorithm based on BoxMeuller as described in
; http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
;

(use-modules (srfi srfi-1))

; A srfi-158 pseudo-compatible shim. (Guile-3.0 does not have srfi-158
; for some reason). I'm going to work with plain lists, as that seems
; easiest...  But this really should create a vector, not a list ...
(define (generator->vector f n)
	(map (lambda (junk) (f)) (make-list n)))


(define (make-sphere-generator N)
"
  make-sphere-generator N - return a generator of points uniformly
  distributed on an N-dimensional sphere.

  This implements the BoxMeuller algorithm, that is, of normalizing
  Gaussian random  variables.
"
	; Work-around for guile
	; (define gaussg (make-normal-generator))
	(define gaussg random:normal)

	; Banach l2-norm aka root-mean-square distance.
	(define (l2-norm VEC)
		(sqrt (fold (lambda (x sum) (+ sum (* x x))) 0 VEC)))

	(define np1 (+ N 1))

	; Create a vector of N+1 values.
	(lambda ()
		(define vect (generator->vector gaussg np1))
		(define norm (/ 1 (l2-norm vect)))
		(map (lambda (x) (* x norm)) vect)
	)
)

(define (make-ball-generator N)
"
  make-ball-generator N - return a generator of points uniformly
  distributed inside an N-dimensional ball.

  This implements the Harman-Lacko-Voelker Dropped Coordinate method.
"

	(define sphereg (make-sphere-generator (+ N 1)))

	; Create a vector of N+2 values, and drop the last two.
	; (The sphere already added one, so we only add one more)
	(lambda ()
		(take (sphereg) N)
	)
)

; -------------------------------------
; pseudo unit-test junk

(define (test-sphere N REPS)
"
  Take REPS samples from unit sphere, verify random distribution.
"
	(define sphereg (make-sphere-generator N))

	; Fix list of samples
	(define samples
		(map (lambda (junk) (sphereg)) (make-list REPS)))

	(define (l2-norm VEC)
		(sqrt (fold (lambda (x sum) (+ sum (* x x))) 0 VEC)))

	; Expect a vector approaching zero.
	(define converge-to-zero
		(fold (lambda (samp acc) (map + samp acc))
			(make-list REPS 0) samples))

	(define should-be-zero (l2-norm converge-to-zero))
	(define norm-should-be-zero (/ should-be-zero (* 1.57 (sqrt REPS))))

	; maximum allowed tolerance for radius deviation
	(define EPS (* 2e-16 (sqrt N)))

	; Each individual sphere radius should be 1.0 to within float
	; tolerance.
	(for-each (lambda (SAMP)
		(define diff (abs (- 1 (l2-norm SAMP))))
		(if (< EPS diff)
			(format #t "Error: Sphere radius-test FAIL: ~A\n" diff)
			; (format #t "Sphere radius-test pass: ~A\n" diff)
		))
		samples)

	; The distribution should be zero
	(if (< 1 norm-should-be-zero)
		(format #t "Error: Sphere distribution test FAIL: ~A\n"
			norm-should-be-zero))

	norm-should-be-zero
)
