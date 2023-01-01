;
; batch-pair.scm
;
; Batch-compute the mutual information of pairs of items, such as
; natural-language words.
;
; Copyright (c) 2013, 2014, 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------

;; Call the function only once, ever.
;; The database loads are slow, so don't repeat them, if they are
;; not needed.
(define call-only-once
	(let ((called '()))
		(lambda (func)
			(if (not (member func called))
				(begin (func)
					(set! called (cons func called))))))
)

; ---------------------------------------------------------------------
; Handy-dandy main entry points.

(define-public (batch-pairs LLOBJ)

	; Make sure all item-pairs are in the atomspace.
	(call-only-once (lambda() (LLOBJ 'fetch-pairs)))
	(display "Finished loading sparse matrix pairs\n")

	(cog-report-counts)
	(batch-all-pair-mi LLOBJ)
	(print-matrix-summary-report LLOBJ)
)

; ---------------------------------------------------------------------
