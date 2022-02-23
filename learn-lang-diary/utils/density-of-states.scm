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
  density NBINS LO HI WEIFN FILENAM - create histogram of density of states.

  Example: Density of states just counts 1.0 for each pair.
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

; ----
; Left and right marginal probabilites
(define (pcnt PAIR)
	(define LWRD (star-obj 'left-element PAIR))
	(freq-obj 'right-wild-freq LWRD))
(weighted-density 200 7 30 pcnt "/tmp/density-leftp.dat")

(define (pcnt PAIR)
	(define LWRD (star-obj 'right-element PAIR))
	(freq-obj 'left-wild-freq LWRD))
(weighted-density 200 7 30 pcnt "/tmp/density-rightp.dat")

; ----
; Weights that are other marginals
(define (pcnt PAIR)
	(define LWRD (star-obj 'left-element PAIR))
	(freq-obj 'right-wild-logli LWRD))
(weighted-density 200 7 30 pcnt "/tmp/density-lmarg-logli.dat")

(define (pcnt PAIR)
	(define LWRD (star-obj 'left-element PAIR))
	(freq-obj 'right-wild-fentropy LWRD))
(weighted-density 200 7 30 pcnt "/tmp/density-lmarg-fent.dat")

(define (pcnt PAIR)
	(define LWRD (star-obj 'left-element PAIR))
	(freq-obj 'right-wild-fmi LWRD))
(weighted-density 200 7 30 pcnt "/tmp/density-lmarg-fmi.dat")

; ----
; Weights that are pair MI's
; Well, don't bother with this first one, it's just
; the fmi times the energy.
(define (pcnt PAIR) (freq-obj 'pair-mi PAIR))
(weighted-density 200 7 30 pcnt "/tmp/density-mi.dat")

(define (pcnt PAIR) (freq-obj 'pair-fmi PAIR))
(weighted-density 200 7 30 pcnt "/tmp/density-fmi.dat")

;
; ----------------------------------------------------------------------
