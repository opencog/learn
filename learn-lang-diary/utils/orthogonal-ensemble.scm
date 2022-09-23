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
	(define (get-ref PR IDX)
		; Expect either a FloatValue or #f if absent.
		(define flov (LLOBJ 'get-count PR))
		(if flov (cog-value-ref flov IDX) -inf.0))

	(lambda (message . args)
		(case message
			((get-mi)  (get-ref (car args) 0))
			((get-rmi) (get-ref (car args) 1))
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
(goe 'mean-rms)

; Make sure things work as expected.
(define gsu (add-support-compute goe))
(gsu 'all-left-marginals)

(define w (first (goe 'left-basis)))
(define u (second (goe 'left-basis)))

(gsu 'left-support w)
(gsu 'left-count w)
(gsu 'left-length w)

(define god (add-similarity-compute gsu))
(god 'left-cosine w u)

(god 'left-cosine (Word "the") (Word "a"))

(god 'left-product (Word "the") (Word "the"))

(gsu 'left-length (Word "the"))

; WTF why is it wrong?
      (define (valid? VAL) (and (not (eqv? 0 VAL)) (< -inf.0 VAL)))

         (prod-obj  (add-support-compute
            (add-fast-math star-obj * GET-CNT)))

(goe 'mean-rms)
;  (-1.4053400751699667 2.898486631855367)
(define self (Similarity (Word "the") (Word "the")))

(cog-value self (PredicateNode "*-SimKey shape-mi"))
; (FloatValue 4.892396662694156 10.02792661666408)
; first should be just (ami 'get-mi) ... and it is. Good.
(ami 'get-mi self)

; Verify this too -- looks OK
(goe 'get-count self)
; 2.1727672188133718

; wtf is this??
(cog-value self (PredicateNode "*-SimKey goe"))

(define bas (take allwo 2500))

; WTF why is it wrong?
(fold
	(lambda (wrd sum)
		(define sl (Similarity wrd (Word "the")))
(format #t "wrd=~A<<\n" (cog-name wrd))
(format #t "sim=~A\n" sl)
(format #t "mi=~A\n" (ami 'get-mi sl))
		(define cmp (goe 'get-count sl))
(format #t "cmp=~A\n" cmp)
		(+ sum (* cmp cmp))
	)
	0 bas)

(define wtf (Similarity (Word "the") (Word "-")))
(define wtf (Similarity (Word "-") (Word "the")))
(cog-keys wtf)
(ami 'get-mi wtf)

; -------------------------------------
; Compute a bunch of them.
(define allwo (rank-words pcs))
(smi 'fetch-pairs)

; goe provides the 'get-count method that returns a renormalized
; version of whatever 'get-mi returns.
(define goe (add-gaussian-ortho-api ami 'get-mi))
(goe 'mean-rms)
(define gos (add-similarity-api ami #f "goe"))
(define god (add-similarity-compute goe))

(define (do-compute A B)
	(define sim (god 'left-cosine A B))
	(format #t "cos=~7F for (\"~A\", \"~A\")\n" sim (cog-name A) (cog-name B))
	; (store-atom ...)
	(gos 'set-pair-similarity
		(gos 'make-pair A B)
		(FloatValue sim)))

(define (dot-prod A B)
	(define have-it (gos 'pair-count A B))
	(if (not have-it) (do-compute A B)))

(loop-upper-diagonal dot-prod allwo 0 50)

; add-similarity-api ??
; -------------------------------------
