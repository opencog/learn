;
; similarity-jacc.scm
; September 2021 version
;
; Bulk computation of MI and Jaccard similarity. (See also the earlier
; `similarity.scm`, which did cosines).
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))


; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sim (add-similarity-compute pcs))
(define sap (add-similarity-api pcs))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)
(sap 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

(for-each store-atom (cog-get-atoms 'Similarity))

(call-with-new-thread
	(lambda ()
		(define (stosle)
			(for-each store-atom (cog-get-atoms 'Similarity))
			(sleep 600)
			(stosle))
		(stosle)))


; ---------------------------------------
; General exploration. Skip this section.

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))

	(define wrds (LLOBJ 'left-basis))
	(define ranked-words
		(sort wrds
			(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

	(define short-list (take ranked-words NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

(define wli (top-ranked pcs 100))

; ---------------------------------------
; Compute overlaps.
; Want ability to restart.
(define overlap-done? (make-once-predicate))

; The 'right-overlap returns similarity: 1.0 means perfectly similar.
(define (overlap WA WB)
	(if (overlap-done? (Unordered WA WB))
		-100000
		(log2 (sim 'right-overlap WA WB))))

; Print only -5 and better. Less than that looks like junk.
; Actually -4 is an even better limit, but it's nice to keep
; an eye on things.
(define (prt-overlap WA WB)
	(define rv (overlap WA WB))
	(if (< -5 rv)
		(format #t ">>~A<< -- >>~A<< log2 overlap = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

; We're going to take -8.0 as the cutoff, because this was seen
; in run-3 experiments (documented in diary part-two) That is,
; bad intercluster values were around -8 so that seems like a
; reasonable place to halt comparison, for now.
(define bover (batch-similarity pcs #f "overlap" -8.0 prt-overlap))
(bover 'batch-compute 500)

; ---------------------------------------

(define condjacc-done? (make-once-predicate))

; The 'right-cond-jacc returns distance: 0.0 means very close.
; So we subtract from 1.0 to get similarity.
(define (condjacc WA WB)
	(if (condjacc-done? (Unordered WA WB))
		-100000
		(log2 (- 1.0 (sim 'right-cond-jacc WA WB)))))

; Print only -5 and better. Less than that looks like junk.
; Actually, -4 is a better limit, but its nice to keep an
; eye on things.
(define (prt-condjacc WA WB)
	(define rv (condjacc WA WB))
	(if (< -5 rv)
		(format #t ">>~A<< -- >>~A<< log2 condjacc = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

(define bcond (batch-similarity pcs #f "condjacc" -8.0 prt-condjacc))
(bcond 'batch-compute 500)

; ---------------------------------------

(define mi-done? (make-once-predicate))

(define cmi (add-symmetric-mi-compute pcs))

(define (mi WA WB)
	(if (mi-done? (Unordered WA WB))
		-100000
		(cmi 'mmt-fmi WA WB)))

; MI=3 and better looks pretty healthy.
(define (prt-mi WA WB)
	(define rv (mi WA WB))
	(if (< 3 rv)
		(format #t ">>~A<< -- >>~A<< mi = ~6F\n"
			(cog-name WA) (cog-name WB) rv))
	rv)

; Want an MI of greater than zero.
(define bami (batch-similarity pcs #f "mi" 0.0 prt-mi))
(bami 'batch-compute 800)

; ---------------------------------------
; Compute MI on the diagonals.

(define (ranked LLOBJ)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))

	(define wrds (LLOBJ 'left-basis))
	(define ranked-words
		(sort wrds
			(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

	(format #t "Sorted ~A words\n" (LLOBJ 'left-basis-size))
	ranked-words
)

(define diag-cnt 0
(for-each
	(lambda (WRD)
		(set! diag-cnt (+ diag-cnt 1))
		(format #t "~A " diag-cnt)
		(bami 'compute-similarity WRD WRD))
	(ranked pcs))

; ---------------------------------------------------------------

(define (sim-k n)
	(fold (lambda (x cnt)
		(if (equal? n (length (cog-keys x))) (+ cnt 1) cnt))
		0 (cog-get-atoms 'Similarity)))

(define (sim-rpt)
	(format #t "Done 1: ~A   2: ~A  3: ~A  tot: ~A\n"
		(sim-k 1) (sim-k 2) (sim-k 3)
		(length (cog-get-atoms 'Similarity)))
	*unspecified*)
