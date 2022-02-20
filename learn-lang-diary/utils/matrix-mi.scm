;
; matrix-mi.scm
;
; Scripts for graphing the distribution of matrix-based MI.
; This is NOT the MMT MI, but the simpler, asymmmetric MI.
; Used in diary part six.
;
; Loop over all pairs, and bincount:
; support (this is the degree)
;

(define all-pairs (star-obj 'get-all-elts))
(define freq-obj (add-pair-freq-api star-obj))

(define (pair-plot NBINS LO HI FILE VALFN WEIFN)

	(define bins (bin-count all-pairs NBINS VALFN WEIFN LO HI))

	(define oport (open-file FILE "w"))
	(print-bincounts-tsv bins oport)
	(close oport)
)

(define (log2 x)
	(if (< 0 x) (/ (log x) (log 2)) 0.0))

; For each left-word, the right-support is the "degree" of that node.
(define (pval WRD) (freq-obj 'pair-fmi WRD))
(define (pcnt WRD) 1)
(pair-plot 500 -10 10 "/tmp/pair-fmi.dat" pval pcnt)


; -----
