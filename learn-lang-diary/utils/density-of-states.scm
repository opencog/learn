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
(define (density LLOBJ NBINS LO HI)
"
  density LLOBJ - bin count

  Example:
  (density star-obj 200 0 30)
"
	; If frequencies are available, then uncomment below
	;	(define fra (add-pair-freq-api star-obj))
	; 'pair-logli PAIR   -- return -log_2 P(x,y)
	; (define pval (lambda ITEM) (fra 'pair-logli ITEM))

	; Marginals are not avaialble.  Work with the raw count.
	; Ugh.
	(define all-pairs (LLOBJ 'get-all-elts))
	(define tot-cnt
		(fold (lambda (ITEM CNT) (+ CNT (LLOBJ 'get-count ITEM)))
			0 all-pairs))
	(format #t "Total pair count = ~A\n" tot-cnt)
	(define ol2 (/ 1 (log 2)))
	(define olt (* ol2 (log tot-cnt))) 

	(define (pval ITEM) (- olt (* ol2 (log (LLOBJ 'get-count ITEM)))))
	(define (pcnt ITEM) 1)

	(define bins (bin-count all-pairs NBINS pval pcnt LO HI))

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

	(define oport (open-file "/tmp/foo.dat" "w"))
	(format oport "#\n# Total count = ~A bin-width = ~A\n"
		bin-total bwid)
	(print-bincounts-tsv bins oport)
	(close oport)
)
