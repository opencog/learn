;
; orthogonal-ensemble.scm
;
; Experiments with gaussian orthogonal ensembles.
; Part of experiments/run-15, described in diary part eight.
;
; Sept 2022
; -------------------------------------
; Ingest data
(define pca (make-pseudo-cset-api)) ; shapes not needed to fetch sims.
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

; -------------------------------------
; Graphs, verify it still looks gaussian.
; See similarity-graphs.scm -- this is a cut-n-paste from there.
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
; That is, (smi 'get-count PR) returns a FloatValue.
; So, unwrap it.
(define (add-mi-sim LLOBJ)
	(lambda (message . args)
		(case message
			((get-mi)  (cog-value-ref (LLOBJ 'get-count (car args)) 0))
			((get-rmi) (cog-value-ref (LLOBJ 'get-count (car args)) 1))
			(else      (apply LLOBJ (cons message args))))
	))

(define ami (add-mi-sim sob))

; -------------------------------------
; Compute vector norms. Use plain MI, for now.
; Its fast, (5 seconds) so do both left and right, to avoid confusion.
; Except we don't actually need this for anything ...
(define ssc (add-support-compute ami 'get-mi))
; (ssc 'all-left-marginals)
(ssc 'cache-all)

; Verify that values are not insane.
(define w (car (ssc 'left-basis)))
(ssc 'left-support w)
(ssc 'left-count w)

; The support API will provide access to the vector lengths.
(define gmi (add-support-api sob))
(gmi 'left-support w)
(gmi 'left-count w)

; The summary report is convoluted and ugly. Oh well.
; ((make-central-compute sob) 'cache-all)
; (print-matrix-summary-report sob)

; -------------------------------------
; Look at dot products
(define goe (add-gaussian-ortho-api ami 'get-mi))

; Make sure things work as expected.
(define gos (add-support-compute goe))
(gos 'all-left-marginals)

(define w (first (gos 'left-basis)))
(define u (second (gos 'left-basis)))

(gos 'left-support w)
(gos 'left-count w)
(gos 'left-length w)

(define god (add-similarity-compute gos))
(god 'left-cosine w u)

(god 'left-cosine (Word "the") (Word "a"))

; -------------------------------------
; Compute a bunch of them.
(define allw (rank-words pcs))
(smi 'fetch-pairs)
(loop-upper-diagonal LLOBJ FUN ITEMLI START-RANK DEPTH)

; -------------------------------------
