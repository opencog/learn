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
(define (pval PAIR) (freq-obj 'pair-fmi PAIR))
(define (pcnt PAIR) 1)
(pair-plot 500 -12 27 "/tmp/pair-fmi.dat" pval pcnt)

(define (common-mi PAIR)
	(+ (freq-obj 'pair-fmi PAIR) (* 0.5 (+
		(freq-obj 'left-wild-logli (star-obj 'right-element PAIR))
		(freq-obj 'right-wild-logli (star-obj 'left-element PAIR))
	))))


(define q (log2 (sup-obj 'total-support-right)))
(define (ranked-mi PAIR) (- (common-mi PAIR) q))

(define (pval PAIR) (ranked-mi PAIR))
(define (pcnt PAIR) 1)
(pair-plot 500 -30 29 "/tmp/pair-rmi.dat" pval pcnt)


; -----
