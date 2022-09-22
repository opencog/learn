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

(define pca (make-pseudo-cset-api)) ; shapes not needed to fetch sims.
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

; See line 196ff of similarity-graphs.scm
(define all-sims ((add-pair-stars smi) 'get-all-elts))

(define wmi (/ 2.0 (length all-sims)))

; Plain MI distrsibution
(define mi-dist
   (bin-count all-sims 100
      (lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
      (lambda (SIM) wmi)
      -25 25))

; Ranked MI distribution
(define rmi-dist
   (bin-count all-sims 100
      (lambda (SIM) (cog-value-ref (smi 'get-count SIM) 1))
      (lambda (SIM) wmi)
      -25 25))

(define (prt-mi-dist)
	(define csv (open "/tmp/sim-mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv mi-dist csv)
	(close csv))

(define (prt-rmi-dist)
	(define csv (open "/tmp/sim-rmi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv rmi-dist csv)
	(close csv))

; -------------------------------------
; TODO filter the top lists
; (define (filter the top list...

; Wrap similarity, to create a new base object.
(define sob (add-pair-stars smi))

; Counts on smi are FloatValues of two floats.
; First float is mi-sim
; Second float is ranked-mi-sim
(define (get-mi SIM) (cog-value-ref (smi 'get-count SIM) 0))
(define (get-rmi SIM) (cog-value-ref (smi 'get-count SIM) 1))

; Compute vector norms
(define ssc (add-support-compute sob))
(ssc 'all-left-marginals)
