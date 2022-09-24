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
; SKIP THIS. Its not needed.
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
(define gsu (add-support-compute goe #f "goe"))
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

; =================================================
; Below is a LONG debugging session. Ignore it.
; WTF why is it wrong?
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

; So this is OK...
(define allwo (rank-words pcs))
(define nwrd 0)
(define nzwords '())
(fold
	(lambda (wrd sum)
; (format #t "wrd=~A<<\n" (cog-name wrd))
		(define sl (Similarity wrd (Word "the")))
; (format #t "sim=~A\n" sl)
; (format #t "mi=~A sum=~A\n" (ami 'get-mi sl) sum)
		(define cmp (goe 'get-count sl))
		; (define cmp (ami 'get-mi sl))
; (format #t "cmp=~A\n" cmp)
		(if (and (not (eqv? cmp 0)) (< -inf.0 cmp))
			(begin
;(format #t "cmp=~A old sum=~A wrd=~A<<\n" cmp sum (cog-name wrd))
;(format #t "wtf new sum=~A\n" (+ sum (* cmp cmp)))
				(set! nwrd (+ 1 nwrd))
				(set! nzwords (cons wrd nzwords))
				(+ sum (* cmp cmp)))
			sum)
	)
	0 allwo)
; 4360.614619250912
(sqrt 4360.614619250912)
; 66.03494998295155
2460 words participated.

(define gsu (add-support-compute goe))
(gsu 'left-length (Word "the"))
; 66.03494998295147

(define god (add-similarity-compute gsu))
(god 'left-product (Word "the") (Word "the"))
; 4365.335536638053

(define prod-obj (add-support-compute
    (add-fast-math goe * 'get-count)))
(define prod-obj (add-support-compute (add-fast-math goe *)))
(prod-obj 'left-count (list (Word "the") (Word "the")))
; 4365.335536638053
(prod-obj 'left-sum (list (Word "the") (Word "the")))
; 4365.335536638053

(define tup (add-support-compute (add-tuple-math goe *)))
(tup 'left-sum (list (Word "the") (Word "the")))
; 4360.614619250912

(define fma (add-fast-math goe * 'get-count))

(goe 'get-count (Similarity (Word "the") (Word "o'clock")))
-1.9411586964140686

(define sl (Similarity (Word "the") (Word "o'clock")))
(fma 'get-count (list sl sl))
; 3.768097084663966  OK.

(define df 0)
(define sm 0)
(for-each
	(lambda (wrd)
		(define sl (Similarity (Word "the") wrd))
		(define fpr (fma 'get-count (list sl sl)))
		(define cor (goe 'get-count sl))
		(define cors (* cor cor))
		(set! df (+ df (- cors fpr)))
		(set! sm (+ sm cors))
		(format #t "~6f ~6f dif=~6f sum=~6f ~A\n" fpr cors df sm (cog-name wrd))
	)
	nzwords)
; 4360.614619250919

	allwo)
; 4360.614619250912

(define fas (add-support-compute fma))
(fas 'left-sum (list (Word "the") (Word "the")))
4365.3355366380665

add-support-compute 'left-sum
(length (fma 'left-stars (list (Word "the") (Word "the"))))
; 9496
; 2501 ... wtf ..

(length allwo)
; 9495

(define fwo
	(append
	(map (lambda (prs) (gar (car prs)))
		(fma 'left-stars (list (Word "the") (Word "the"))))
	(map (lambda (prs) (gdr (car prs)))
		(fma 'left-stars (list (Word "the") (Word "the"))))))

(define faswo (delete-dup-atoms fwo))
(length faswo)
2500

(atoms-subtract faswo allwo)

(define wl '())
(for-each
	(lambda (prs)
		(define sl (car prs))
		(define fw (gar sl))
		(define sw (gdr sl))
		(define ow (if (equal? (Word "the") fw) sw fw))
		(when (equal? (Word "the") ow)
			(format #t "yo its ~A" sl))
		(set! wl (cons ow wl)))
	(fma 'left-stars (list (Word "the") (Word "the"))))
(length wl)
; 2501
(length (delete-dup-atoms wl))
; 2500
(keep-duplicate-atoms wl)
; (WordNode "the")

(goe 'get-count (Similarity (Word "the") (Word "the")))
; 2.1727672188133718

(* 2.1727672188133718 2.1727672188133718)
; 4.720917387149995
(- 4365.335536638053 4360.614619250912)
; 4.7209173871406165

Holy cow.
So (fma 'left-stars (list (Word "the") (Word "the"))))
has a duplicate entry! Sheesh, that took a long time.

; Confirm.
(keep-duplicate-atoms
	(map car (fma 'left-stars (list (Word "the") (Word "the")))))

(define row-var (uniquely-named-variable))
(define LLOBJ goe)
(define (thunk-type TY) (if (symbol? TY) (TypeNode TY) TY))
(define row-type (thunk-type (LLOBJ 'left-type)))
; (TypeNode "WordNode")
(define COL-TUPLE (list (Word "the") (Word "the")))
(define term-list
    (map (lambda (COL) (LLOBJ 'make-pair row-var COL)) COL-TUPLE))

(define qry
   (Meet
     (TypedVariable row-var row-type)
     (Present term-list)))
(define rowset (cog-value->list (cog-execute! qry)))
(length rowset)
; 2501

Got it. Must deduplicate.
Fixed in 4d4c7fe854208798e36c76fb8d740d89b54aa949
; =================================================

; wtf is this??
(cog-value self (PredicateNode "*-SimKey goe"))


; This is not the minus sign, its some utf8 dash
(define wtf (Similarity (Word "the") (Word "‑")))
(cog-keys wtf)
(ami 'get-mi wtf)

; =================================================

(god 'left-cosine (Word "the") (Word "a"))

(god 'left-product (Word "the") (Word "the"))
; 4360.614619250908
(god 'left-cosine (Word "the") (Word "the"))
; 0.999999999999999

Yayyy!

; -------------------------------------
; Compute a bunch of them.
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

(define allwo (rank-words pcs))
(loop-upper-diagonal dot-prod allwo 0 50)

cos=0.33705 for ("by", ".")
(define sl (Similarity (Word "by") (Word ".")))
(cog-keys sl)
(cog-value sl (PredicateNode "*-SimKey goe"))
; Yayyy!

; -------------------------------------
