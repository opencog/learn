;
; density-of-states.scm
;
; Create a histogram of the density of states. This is just a binning
; of the values of -log_2 p(sigma) over the probability p(sigma) over
; some state space sigma.
;
; This was used to create the graph in "Diary Part Six" (Feb 2022).
;
; Thi can ONLY be used by hand, by cutting and pasting the interesting
; bits from this file, into a guile prompt. This is effectively a kind-of
; work-log of what was needed to generate those pictures. Of course, this
; can be recycled for other datasets, too.
;
; Copyright (c) 2022 Linas Vepstas
;
; ----------------------------------------------------------------------
;
; Get all pairs up front, avoid the CPU overhead.
(define all-pairs (star-obj 'get-all-elts))
(define freq-obj (add-pair-freq-api star-obj))

(define (weighted-density NBINS LO HI WEIFN FILENAM)
"
  density LLOBJ - create histogram of density of states.

  Example:
  (weighted-density 200 7 30 (lambda (PAIR) 1.0) \"/tmp/density.dat\")
"
	; 'pair-logli PAIR   -- return -log_2 P(x,y)
	(define (pval ITEM) (freq-obj 'pair-logli ITEM))

	(define bins (bin-count all-pairs NBINS pval WEIFN LO HI))

	; The actual bin counts are the second elt.
	; The first elt is bin centers.
	(define counts (array->list (second bins)))

	(define bin-total
		(fold 
			(lambda (bin tot) (+ tot bin))
			0
			counts))

	; Bin-width dh = (b-a)/nbins
	(define bwid (/ (- HI LO) NBINS))

	(format #t "Total count = ~A bin-width = ~A\n"
		bin-total bwid)

	; Dump to bogus file.
	(define oport (open-file FILENAM "w"))
	(format oport "#\n# Total count = ~A bin-width = ~A\n"
		bin-total bwid)
	(print-bincounts-tsv bins oport)
	(close oport)
)

; Print density of states, uniform weighting.
(define (pcnt ITEM) 1)
(weighted-density 200 7 30 pcnt "/tmp/density.dat")
;
; ----------------------------------------------------------------------
