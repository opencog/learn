;
; degree.scm
;
; Scripts for graphing the degree.
; Used in diary part six
;
; Loop over left-basis, and bincount:
; support (this is the degree)
;

(define (degree-plot LLOBJ NBINS MAX-DEG)

	(define lwords (LLOBJ 'left-basis))

	(define sup-obj (add-support-api LLOBJ))

	; For each left-word, the right-support is the "degree" of that node.
	(define (pval WRD) (sup-obj 'right-support WRD))
	(define (pcnt WRD) 1)

	(define bins (bin-count lwords NBINS pval pcnt 0 MAX-DEG))

	(define oport (open-file "/tmp/foo.dat" "w"))
	(print-bincounts-tsv bins oport)
	(close oport)
)
