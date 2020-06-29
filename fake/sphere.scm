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
  N+1 Gaussian random variables.
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
	(lambda () (take (sphereg) N))
)

; -------------------------------------
; pseudo unit-test junk

(define (test-sphere N REPS)
"
  Take REPS samples from unit sphere, verify random distribution.

  This test checks that:
  * Every sample has unit length, within numerical tolerance.
  * The REPS samples are uniformly distributed.
  * Rotations of the REPS samples are uniformly distributed.
"
	(define sphereg (make-sphere-generator N))

	; Fix list of samples
	(define samples
		(map (lambda (junk) (sphereg)) (make-list REPS)))

	(define (l2-norm VEC)
		(sqrt (fold (lambda (x sum) (+ sum (* x x))) 0 VEC)))

	; Rotate the j'th amnd k'th coordinates of a vector VEC
	; by cosine co and sine si
	(define (pair-rot VEC j k co si)
		(define oj (list-ref VEC j))
		(define ok (list-ref VEC k))
		(define nj (+ (* co oj) (* si ok)))
		(define nk (+ (* (- si) oj) (* co ok)))
		(define beg (take VEC j))
		(define mid (take (drop VEC (+ j 1)) (- k j 1)))
		(define end (drop VEC (+ k 1)))
		(append beg (list nj) mid (list nk) end))

	; Randomly rotate a single vector in some plane.
	(define (rand-pair-rot VEC)
		(define j (random N))
		(define k (+ j 1 (random (- N j))))
		(define theta (* 3.14 (random:uniform)))
		(define co (cos theta))
		(define si (sin theta))
		(pair-rot VEC j k co si))

	; Apply a random rotation to a collection of vectors
	(define how-many-rots (if (< 10 N) 10 N))
	(define (arb-rot VEC-LIST)
		(define j (random N))
		(define k (+ j 1 (random (- N j))))
		(define theta (* 3.14 (random:uniform)))
		(define co (cos theta))
		(define si (sin theta))
		(define rvl (map (lambda (vec) (pair-rot vec j k co si)) VEC-LIST))
		(if (not (= 0 (random how-many-rots))) (arb-rot rvl) rvl))

	; Expect a vector approaching zero. That is, each individual
	; coordinate should be uniformly randomly distributed in the
	; interval [-1,1]. The sum of REPS samples of these should
	; converge to zero, within pi/2 sqrt(REPS).
	(define (converge-to-zero VEC)
		(fold (lambda (samp acc) (map + samp acc))
			(make-list REPS 0) VEC))

	(define (should-be-zero VEC) (l2-norm (converge-to-zero VEC)))
	(define (norm-should-be-zero VEC)
		(/ (should-be-zero VEC) (* 1.57 (sqrt REPS))))

	(define FAIL #f)
	(define (check-zero VEC)
		(define zz (norm-should-be-zero VEC))
		(if (< 1 zz)
			(begin (set! FAIL #t)
			(format #t "Error: Sphere distribution test FAIL: ~A\n" zz))))

	; maximum allowed tolerance for radius deviation
	(define EPS (* 4e-16 (sqrt N)))

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
	(check-zero samples)

	; Rotate wildly. Should still be uniform.
	(for-each
		(lambda (junk) (check-zero (arb-rot samples)))
		(make-list 12))

	(if (not FAIL)
		(format #t "Sphere test pass!\n"))
)

; ----------------------------------------------------
; Eyeballed sanity checking...

; For the histogramming tools
(use-modules (opencog) (opencog matrix))

(define (histogram samples nbins filename)
"
  Take `samples`, histogram them into nbins, write to a file.
  It is assumed `samples` is a list of floats, distributed
  over -1 to 1 with some distribution.
"
	(define nsamp (length samples))
	(define norm (lambda (VEC) (exact->inexact (/ nbins nsamp))))

	(define bins (bin-count samples nbins (lambda (x) x) norm -1 1))

	(let ((outport (open-file filename "w")))
		(print-bincounts-tsv bins outport)
		(close outport))
)

(define (mk-coord N REPS selector)
"
  Generate `REPS` points uniformly distributed over an `N` dimensional
  sphere. Apply the function `selector` to each point, with `selector`
  returning a single floating point value. Histogram these, and write
  the historgram out to file `foo.dat` for graphing.
"
	(define sphereg (make-sphere-generator N))
	(define points (map (lambda (junk) (sphereg)) (make-list REPS)))

	(define samples (map selector points))
	(histogram samples 100 "foo.dat")
)

; (mk-coord 2 50000 first)
; (mk-coord 2 50000 second)

; Obtain the angular coordinate from a vector VEC, by taking the
; arctan, dividing by pi, to get an angle in the range -1 to 1.
(define (theta VEC) (/ (atan (first VEC) (second VEC)) 3.14159265358979))

; (mk-coord 2 50000 theta)

; Rotate the point by an angle, returning the rotated first coord.
(define (rot VEC)
	(define phi 0.7854)
	; (define phi 1.5707963268)
	(define co (cos phi))
	(define si (sin phi))
	(+ (* co (first VEC)) (* si (second VEC))))

; (mk-coord 2 50000 rot)

; Return a list of `LEN` floating point numbers randomly distributed
; over -1 to 1, using ordinary random number generators.
(define (unifomy LEN)
	(map (lambda (r) (* 2 (- r 0.5)))
		(map (lambda (n) (random:uniform)) (iota LEN))))

; (histogram (unifomy 50000) 100 "foo.dat")

(define (rehisto samples nbins nrebins filename)
"
  Take `samples`, histogram them then re-histogram them.
"
	(define nsamp (length samples))
	(define norm (lambda (VEC) (exact->inexact (/ nbins nsamp))))
	(define bins (bin-count samples nbins (lambda (x) x) norm -1 1))

	(define vals (array->list (second bins)))

	(define rebins (bin-count-scores vals nrebins))

	(let ((outport (open-file filename "w")))
		(print-bincounts-tsv rebins outport)
		(close outport))
)
