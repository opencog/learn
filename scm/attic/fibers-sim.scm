;
; fibers-sim.scm
;
; Experimental code to parallelize similarity computations using fibers.
; It's kind-of-ish works, but not reliably. Has wild swings in performance
; and also crashes with bug https://github.com/wingo/fibers/issues/52
;
; Running one fiber gives:
; done in 28 secs
; done in 29 secs
; done in 89 secs
; done in 55 secs
; done in 164 secs
; done in 51 secs
; done in 51 secs
; compared to baseline (no fibers) of 29 seconds.
; Also, above requires 3x more grand total CPU.
;
; Six fibers runs consistently (more or less) at 37 seconds, so
; no actual perallelism improvements.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------


(use-modules (fibers) (fibers channels))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; Where the simiarity scores will be stored
(define SIM-ID "shape-mi")

; ---------------------------------------------------------------

(define (make-fsimmer LLOBJ)
"
  make-fsimmer LLOBJ -- return function that computes and stores MI's.

  This computes and stores both the MI and the Ranked-MI scores.
"
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define smi (add-symmetric-mi-compute LLOBJ))

	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (if (< 0 x) (* (log x) ol2) -inf.0))

	(define mmt-q (smi 'mmt-q))

	; Compute and save both the fmi and the ranked-MI.
	; The marginal is sum_d P(w,d)P(*,d) / sum_d P(*,d)P(*,d)
	; The mmt-q is sum_d P(*,d)P(*,d) =
	;              sum_d N(*,d)N(*,d) / [ sum_d N(*,d) ]^2
	(define (do-compute-sim WA WB)
		(define fmi (smi 'mmt-fmi WA WB))
		(define mwa (smi 'mmt-marginal WA))
		(define mwb (smi 'mmt-marginal WB))
		(define rmi (+ fmi (* 0.5 (log2 (* mwa mwb))) mmt-q))

		; Print something, so user has something to look at.
		(if (< -6 fmi)
			(format #t "\tMI(`~A`, `~A`) = ~6F  rank-MI = ~6F\n"
				(cog-name WA) (cog-name WB) fmi rmi))
		(store-atom
			(sap 'set-pair-similarity
				(sap 'make-pair WA WB)
				(FloatValue fmi rmi))))

	(define request-chan #f)
	(define reply-chan #f)

	(define (mi-request-handler)
		(define (wait-for-chan)
			(when (not reply-chan) (yield) (sleep 0.1) (wait-for-chan)))
		(define pr (get-message request-chan))
		(define sim (do-compute-sim (car pr) (cdr pr)))
		(put-message reply-chan sim)
		(mi-request-handler))

	(call-with-new-thread (lambda ()
		(run-fibers (lambda ()
			(set! request-chan (make-channel))
			(set! reply-chan (make-channel))
		;	(spawn-fiber (mi-request-handler))
		;	(spawn-fiber (mi-request-handler))
		;	(spawn-fiber (mi-request-handler))
		;	(spawn-fiber (mi-request-handler))
		;	(spawn-fiber (mi-request-handler))
			(mi-request-handler)))))

	; Try to force the above to complete, before returning to caller.
	; Yield is not enough to do it; the sleep from module fibers is.
	(yield)
	(sleep 0.1)

	(define (compute-sim WA WB)
		(put-message request-chan (cons WA WB))
		(get-message reply-chan)
	)


	; Return the function that computes the MI for pairs.
	compute-sim
)

; ---------------------------------------------------------------

(define-public (fcompute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH)
"
  compute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH - compute MI.

  This will compute the MI similarity of words lying around a diagonal.
  The width of the diagonal is DEPTH. The diagonal is defined by the
  the ranked words. Computations start at START-RANK and proceed to
  DEPTH.  If the Similarity has already been recorded, it will not
  be recomputed.

  Think of a tri-diagonal matrix, but instead of three, its N-diagonal
  with N given by DEPTH.

  WORDLI is a list of words, presumed sorted by rank.

  Examples: If START-RANK is 0 and DEPTH is 200, then the 200x200
  block matrix of similarities will be computed. Since similarities
  are symmetric, this is a symmetric matrix, and so 200 x 201 / 2
  grand total similarities are computed.

  If START-RANK is 300 and DEPTH is 200, then computations start at
  the 300'th ranked word. This results in a total of 200x200
  similarities, as 200 rows are computed, out to 200 places away from
  the diagonal.

"
	; Create a new simmer each time, so we get the updated
	; mmt-q value for this session.
	(define do-compute-sim (make-fsimmer LLOBJ))

	; Don't recompute similarity, if we've already got it.
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (compute-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if (not miv) (do-compute-sim WA WB)))

	; Perform similarity computations for one row.
	(define (batch-simlist ITEM ITEM-LIST)
		(for-each
			(lambda (item) (compute-sim ITEM item))
			ITEM-LIST))

	; Take the word list and trim it down.
	(define nwords (length WORDLI))
	(define start (min START-RANK nwords))   ; avoid overflow
	(define depth (min DEPTH (- nwords start)))  ; avoid overflow
	(define row-range (take (drop WORDLI start) depth)) ; list of words to do
	(define (col-start off) (max 0 (- (+ start off) depth))) ;  column start
	(define (col-end off) (min (+ start off) depth)) ;  column end
	(define (col-range off)   ; reverse, so we go from diagonal outwards
		(reverse (take (drop WORDLI (col-start off)) (col-end off))))

	(define (do-one-row off)
		(define pone (+ 1 off))
		(batch-simlist (list-ref row-range off) (col-range pone)))

	(define rpt-one-row
		(make-progress-rpt do-one-row 10 #f
			"Diag: Finished ~D rows in ~D secs (~D/sec)\n"
			60))

	; Perform the similarity calculations, looping over the fat diagonal.
(define e (make-elapsed-secs))
	(for-each (lambda (n) (rpt-one-row n)) (iota depth))
(format #t "done in ~A secs\n" (e))

)

; ---------------------------------------------------------------
#! ========
;
; Example usage

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

; Do above with `guile -l cogserver-gram.scm`

(define wli (list (Word "the") (Word "a") (Word "this")))
(for-each cog-delete! (cog-get-atoms 'Similarity))
(fcompute-diag-mi-sims star-obj wli 0 5)

==== !#
