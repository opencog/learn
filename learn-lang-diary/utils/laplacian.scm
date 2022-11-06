;
; laplacian.scm
;
; Examine the peculiar difference equation from end of chapter six,
; start of chapter nine. For word-pairs.
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
(format #t "It tool ~A secs\n" (elapsed-secs))

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
(format #t "It tool ~A secs\n" (lelapse))

; ------------
; Next/finally, bin-count
; Again, this is time-consuming.

(define sup-obj (add-support-api star-obj))

; ------------
