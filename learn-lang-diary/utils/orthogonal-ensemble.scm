;
; orthogonal-ensemble.scm
;
; Experiments with gaussian orthogonal ensembles.
; Part of experiments/run-15, described in diary part eight.
;
; Sept 2022
; -------------------------------------
; Graphs, verify it still looks gaussian.
; See similarity-graphs.scm -- this is a cut-n-paste from there.

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

; See line 196ff of similarity-graphs.scm
(define all-sims ((add-pair-stars smi) 'get-all-elts))

(define wmi (/ 2.0 (length all-sims)))
(define mi-dist
   (bin-count all-sims 100
      (lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
      (lambda (SIM) wmi)
      -25 25))

(define (prt-mi-dist)
	(define csv (open "/tmp/sim-mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv mi-dist csv)
	(close csv))

; -------------------------------------
(define (filter the top list...
(add-similarity-api (
