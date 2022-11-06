;
; laplacian.scm
;
; Examine the peculiar difference equation from end of chapter six.
; This is the "Hamming Laplacian"; it is the discrete Laplacian
; (the graph Laplacian) applied to sigma algebras...
;
; For word-pairs.
;
; Get generic word-pairs

(define ala (make-any-link-api))
(define als (add-pair-stars ala))

; First, lets orient ourselves.  We want to get word-pair frequency
(define alf (add-pair-freq-api als))

(define wl (Word "she"))
(define wr (Word "said"))
(define pr (ala 'get-pair wl wr))

; Return -log_2 p(wl, wr)
(alf 'pair-logli pr)

; ------------------------------
; Bingo. That's it.
; Now we need the left and right marginals for this beast.
; There will be oceans of these, so need to cache them.
; This will eat a lot of wall-clock time :-(

(define nwords 391548)

; Perform sums -sum_wl log_2 (wl, wr)
(define (sum-log-left WR)
	(/ (fold (lambda (ITEM ACC) (+ ACC (alf 'pair-logli ITEM)))
			0 (als 'left-stars WR))
		(* 2 nwords)))

(define lap (Predicate "*-Laplacian Marginal-*"))

; Cache the left-sums for one word.
(define (cache-sum-log-left WR)
	(define lwild (ala 'left-wildcard WR))
	(cog-set-value! lwild lap (FloatValue (sum-log-left WR)))
	(store-atom lwild))

; Cache them all.
(define rwords (als 'right-basis))
(length rwords) ; 306920

(define elapsed-secs (make-elapsed-secs))
(elapsed-secs)
(for-each cache-sum-log-left rwords)
(format #t "Right took ~A secs\n" (elapsed-secs))
; Right took 1746 secs

; --------------
; Do it again, same as above, but the right wildcards.
; Perform sums -sum_wr log_2 (wl, wr)
(define (sum-log-right WL)
	(/ (fold (lambda (ITEM ACC) (+ ACC (alf 'pair-logli ITEM)))
			0 (als 'right-stars WL))
		(* 2 nwords)))

(define (cache-sum-log-right WL)
	(define rwild (ala 'right-wildcard WL))
	(cog-set-value! rwild lap (FloatValue (sum-log-right WL)))
	(store-atom rwild))

(define lwords (als 'left-basis))
(length lwords) ; 304085

(define lelapse (make-elapsed-secs))
(lelapse)
(for-each cache-sum-log-right lwords)
(format #t "Left took ~A secs\n" (lelapse))
; Left took 1554 secs
; So 200/second Not bad.

; ----------------------------------------------------------------------
; Next/finally, bin-count
; Again, this is time-consuming.
;
; Get all pairs up front, avoid the CPU overhead.
(define all-pairs (als 'get-all-elts))
(length all-pairs)  ; 28184319

; Scope it out.
(define pr (car all-pairs))
(ala 'left-element pr)
(ala 'right-element pr)

(define (get-sum-log-right WL)
	(define rwild (ala 'right-wildcard WL))
	(cog-value-ref (cog-value rwild lap) 0))

(define (get-sum-log-left WR)
	(define lwild (ala 'left-wildcard WR))
	(cog-value-ref (cog-value lwild lap) 0))

(define (get-lap PR)
	(- (alf 'pair-logli PR)
		(+ (get-sum-log-left (ala 'right-element PR))
			(get-sum-log-right (ala 'left-element PR)))))

; Cache this. For scatter-lots, later on...
(define (cache-lap PR)
	(define lapy (get-lap PR))
	(store-atom (cog-set-value! PR lap (FloatValue lapy)))
	lapy)

(for-each cache-lap all-pairs)

(define (fet-lap PR) (cog-value-ref (cog-value PR lap) 0))

(define sorted-pairs
   (sort all-pairs (lambda (A B) (> (cache-lap A) (cache-lap B)))))


(define (lap-hist NBINS LO HI FUN FILENAM PAIR-LIST)
"
  lap-hist NBINS LO HI FILENAM - create histogram of laplacian thing.

  (lap-hist 200 7 30 get-lap \"/tmp/laplace-dist.dat\")
"
	(define WEIFN (lambda (PAIR) 1.0))
	(define bins (bin-count PAIR-LIST NBINS FUN WEIFN LO HI))

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
(lap-hist 200 2 32 get-lap "/tmp/laplace-dist.dat" (take all-pairs 40000))

(lap-hist 400 2 32 get-lap "/tmp/laplace-dist.dat" all-pairs)

(elapsed-secs)
(lap-hist 400 -2 30 get-lap "/tmp/laplace-dist.dat" all-pairs)
(format #t "Bin count took ~A secs\n" (elapsed-secs))

; ------------
; Doubled...

(define (get-dbl PR)
	(- (alf 'pair-logli PR)
		(* 2 (+ (get-sum-log-left (ala 'right-element PR))
			(get-sum-log-right (ala 'left-element PR))))))

(lap-hist 200 0 32 get-dbl "/tmp/dbl-dist.dat" (take all-pairs 140000))

(define (sumo PR)
	(+ (get-sum-log-left (ala 'right-element PR))
		(get-sum-log-right (ala 'left-element PR))))

(lap-hist 200 0 12 sumo "/tmp/sum-dist.dat" (take all-pairs 140000))

; ----------------------------------------------------------------------

(define sup-obj (add-support-api star-obj))

; ------------
