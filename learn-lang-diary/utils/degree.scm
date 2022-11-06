;
; degree.scm
;
; Scripts for graphing the degree.
; Used in Diary Part Six.
;
; Loop over left-basis, and bincount:
; support (this is the degree)
; count (this is the "weighted" degree, the degree "with multiplicity")
;

(define (degree-plot LLOBJ NBINS LO HI FILE VALFN WEIFN)

	(define lwords (LLOBJ 'left-basis))

	(define sup-obj (add-support-api LLOBJ))

	(define bins (bin-count lwords NBINS VALFN WEIFN LO HI))

	(define oport (open-file FILE "w"))
	(print-bincounts-tsv bins oport)
	(close oport)
)

(define (log2 x)
	(if (< 0 x) (/ (log x) (log 2)) 0.0))

; For each left-word, the right-support is the "degree" of that node.
(define (pval WRD) (sup-obj 'right-support WRD))
(define (pcnt WRD) 1)
(degree-plot star-obj 200 0 50000 "/tmp/degree.dat" pval pcnt)
(degree-plot star-obj 1200 0 1200 "/tmp/degree-fine.dat" pval pcnt)

; Same as above, but uniform x-scale
(define (pval WRD) (log2 (sup-obj 'right-support WRD)))
(define (pcnt WRD) 1)
(degree-plot star-obj 200 0 20 "/tmp/log2-degree.dat" pval pcnt)

; Same as above, but the counted degree (i.e. "with multiplicity")
(define (pval WRD) (sup-obj 'right-count WRD))
(define (pcnt WRD) 1)
(degree-plot star-obj 5000 0 5000 "/tmp/degree-m-fine.dat" pval pcnt)

; Same as above, but the weighted degree
; This produces a strange and confusing graph of uncertain significance.
(define (pval WRD) (sup-obj 'right-support WRD))
(define (pcnt WRD) (sup-obj 'right-count WRD))
(degree-plot star-obj 5000 0 5000 "/tmp/degree-w-fine.dat" pval pcnt)

;----------
(define lwords (star-obj 'left-basis))
(define (wdegree-plot NBINS LO HI FILE VALFN WEIFN)

	(define (catwei WRD) (catch #t
		(lambda () (WEIFN WRD))
		(lambda (key . args)  0.0)))

	(define bins (bin-count lwords NBINS VALFN catwei LO HI))

	(define oport (open-file FILE "w"))
	(print-bincounts-tsv bins oport)
	(close oport)
)

; Same as above, but rescaled
(define freq-obj (add-pair-freq-api star-obj))
(define (pcnt WRD) (freq-obj 'right-wild-freq WRD))
(wdegree-plot 5000 0 5000 "/tmp/degree-lp-fine.dat" pval pcnt)

(define (pcnt WRD) (freq-obj 'left-wild-freq WRD))
(wdegree-plot 5000 0 5000 "/tmp/degree-rp-fine.dat" pval pcnt)

(define (pcnt WRD) (freq-obj 'right-wild-logli WRD))
(wdegree-plot 5000 0 5000 "/tmp/degree-rlogp-fine.dat" pval pcnt)

(define (pcnt WRD) (freq-obj 'right-wild-fentropy WRD))
(wdegree-plot 5000 0 5000 "/tmp/degree-rfent-fine.dat" pval pcnt)

(define (pcnt WRD) (freq-obj 'right-wild-fmi WRD))
(wdegree-plot 5000 0 5000 "/tmp/degree-rfmi-fine.dat" pval pcnt)

; ---------
; Print the words with the highest out-degrees
(define ala (make-any-link-api))
(define star-obj (add-pair-stars ala))

(define lwords (star-obj 'left-basis))
(length lwords)
(define sup-obj (add-support-api star-obj))
(define (degree WRD) (sup-obj 'right-support WRD))

(define sorted-words
	(sort lwords (lambda (A B) (> (degree A) (degree B)))))

(for-each (lambda (W)
	(format #t "~A, ~A\n" (cog-name W) (degree W)))
	(take sorted-words 30))

; Table of three columns
(define (pt W) (format #t "~A, ~D, " (cog-name W) (degree W)))
(define (pn N) (pt (list-ref sorted-words N)))
(for-each (lambda (N)
	(pn N) (pn (+ N 20)) (pn (+ N 40))
	(format #t "\n"))
	(iota 20))

; -----------------
; degree vs rank, degree vs count.

(define (count WRD) (sup-obj 'right-count WRD))

(define oport (open-file "/tmp/rank-degree.dat" "w"))
(define (prtn N)
	(define W (list-ref sorted-words N))
	(format oport "~D\t~A\t~D\t~D\n" N (cog-name W) (degree W) (count W)))
(for-each prtn (iota 20000))
(close oport)

; -----------------
