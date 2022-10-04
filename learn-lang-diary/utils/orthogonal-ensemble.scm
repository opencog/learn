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

; Plain MI distribution
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

(gsu 'left-length (Word "the"))

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
; Graphs of cosine distance distributions.

(gos 'pair-count (Word "house") (Word "the"))
(gos 'get-count (Similarity (Word "house") (Word "the")))

(define all-sims ((add-pair-stars smi) 'get-all-elts))
(define all-sims (cog-get-atoms 'Similarity))
(length all-sims) ; 3126250
(define all-cosi (filter (lambda (sl) (gos 'get-count sl)) all-sims))
(length all-cosi) ; 31375 = (251 * 250) / 2

; 100 because 100 bins, and 2.0 because of width
(define wmi (/ 100 (* 2.0 (length all-cosi))))

; Plain cosine-MI distribution
(define cos-mi-dist
   (bin-count all-cosi 100
      (lambda (SIM) (cog-value-ref (gos 'get-count SIM) 0))
      (lambda (SIM) wmi)
      -1 1))

(define cos-rmi-dist
   (bin-count all-cosi 100
      (lambda (SIM) (cog-value-ref (gos 'get-count SIM) 1))
      (lambda (SIM) wmi)
      -1 1))

(define (prt-cos-mi-dist)
	(define csv (open "/tmp/cos-mi-dist-250.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv cos-mi-dist csv)
	(close csv))

(define (prt-cos-rmi-dist)
	(define csv (open "/tmp/cos-rmi-dist-250.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv cos-rmi-dist csv)
	(close csv))

; ----------
; Again same as above out to M=500 insead of 250

(define cosi-500 (filter (lambda (sl) (gos 'get-count sl)) all-sims))
(length cosi-500) ; 125250 = 500*501 / 2

(define cosi-1k (filter (lambda (sl) (gos 'get-count sl))
	(cog-get-atoms 'SimilarityLink)))
(length cosi-1k) ; 500500 = 1000 * 1001 / 2


(define (cos-dist LST)
	(define wnc (/ 100 (* 2.0 (length LST))))
   (bin-count LST 100
      (lambda (SIM) (cog-value-ref (gos 'get-count SIM) 0))
      (lambda (SIM) wnc)
      -1 1))

(define cos-mi-dist-500 (cos-dist cosi-500))
(define cos-mi-dist-1k (cos-dist cosi-1k))

(define (prt-cos-mi-500-dist)
	(define csv (open "/tmp/cos-mi-dist-500.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv cos-mi-dist-500 csv)
	(close csv))

(define (prt-cos-mi-1k-dist)
	(define csv (open "/tmp/cos-mi-dist-1k.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv cos-mi-dist-1k csv)
	(close csv))

; -------
; Remove diagonal entries; they have cosine of exactly 1.0
; also explicitly bin-count theta.

(define off-diags
	(filter (lambda (SL) (not (equal? (gar SL) (gdr SL)))) cosi-1k))

(define (theta-dist LST)
	(define wnc (/ 200 (* 3.14159 (length LST))))
   (bin-count LST 200
      (lambda (SIM) (acos (cog-value-ref (gos 'get-count SIM) 0)))
      (lambda (SIM) wnc)
      0 3.14159))

(define theta-bin (theta-dist off-diags))

(define (prt-theta-1k-dist)
	(define csv (open "/tmp/theta-dist-1k.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv theta-bin csv)
	(close csv))


; ---------------------------------------
; Dump datafile -- goe cos-MI vs MI scatterplot
; Graphed with p8-goe/scatter-goe-mi-rmi.gplot and related.

(chdir "/home/ubuntu/experiments/run-15/data")

(define (scatter-goe DOT-LIST FILENAME)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# MI and Cosines\n#\n")
	(format csv "#\n# mi\trmi\tcos-mi\tcos-rmi\n")
	(for-each
		(lambda (SL)
			(format csv "~8F\t~8F\t~8F\t~8F\n"
				(cog-value-ref (smi 'get-count SL) 0)
				(cog-value-ref (smi 'get-count SL) 1)
				(cog-value-ref (gos 'get-count SL) 0)
				(cog-value-ref (gos 'get-count SL) 1)))
		DOT-LIST)
	(close csv)
)

(scatter-goe all-cosi "scatter-goe.dat")

; ---------------------------------------
; Top most similar words (according to goe)

(define (lessi A B)
	(> (cog-value-ref (gos 'get-count A) 0)
		(cog-value-ref (gos 'get-count B) 0)))

(define all-cosi-ord (sort all-cosi lessi))

(define distinct-cosi-ord
	(filter (lambda (SL) (not (equal? (gar SL) (gdr SL)))) all-cosi-ord))

(define (top-pairs LST N)
	(for-each (lambda (SL)
		(format #t "~6F, ~A, ~A\n" (cog-value-ref (gos 'get-count SL) 0)
			(cog-name (gar SL)) (cog-name (gdr SL))))
		(take LST N)))

(top-pairs distinct-cosi-ord 20)

(top-pairs (drop distinct-cosi-ord 200) 20)

; --------------
; Do it again, but for old RMI -- the top-20 RMI-associated words.
(define (lessr A B)
	(> (cog-value-ref (smi 'get-count A) 1)
		(cog-value-ref (smi 'get-count B) 1)))

(define all-rmi-ord (sort all-cosi lessr))

(define distinct-rmi-ord
	(filter (lambda (SL) (not (equal? (gar SL) (gdr SL)))) all-rmi-ord))

(top-pairs distinct-rmi-ord 20)

(define (top-pairs LST N)
	(for-each (lambda (SL)
		(format #t "~6F, ~6F, ~A, ~A\n"
			(cog-value-ref (gos 'get-count SL) 0)
			(cog-value-ref (smi 'get-count SL) 1)
			(cog-name (gar SL)) (cog-name (gdr SL))))
		(take LST N)))

; ---------------------------------------
; WTF. Seems the top-20 results are strongly rank-dependent.
; Here's how to explore that.

(define all-sims (cog-get-atoms 'Similarity)) 
(define all-cosi (filter (lambda (sl) (gos 'get-count sl)) all-sims))
(define all-cosi-ord (sort all-cosi lessi))
(define distinct-cosi-ord               
   (filter (lambda (SL) (not (equal? (gar SL) (gdr SL)))) all-cosi-ord))

(define in-set? (make-aset-predicate (take allwo 250)))

(define in-wordlist
	(filter (lambda (SL) (and (in-set? (gar SL)) (in-set? (gdr SL))))
		distinct-cosi-ord))

(top-pairs distinct-cosi-ord 20)
(top-pairs in-wordlist 20)

; =================================================================
; Vector addititivity

(define gos (add-similarity-api ami #f "goe"))

(define (most-sim A B C WLIST)
	(define wa (cog-node 'WordNode A))
	(define wb (cog-node 'WordNode B))
	(define wc (cog-node 'WordNode C))
	(if (not (and wa wb wc))
		(throw 'bad-word 'most-sim "a word doesnt exist"))

	(define (get-sim wp wq)
		; (define OFF 1)
		(define OFF 1)
		(define fa (gos 'pair-count wp wq))
		(if fa (cog-value-ref fa OFF) 0.0))

	(define sims (map
		(lambda (W)
			(define sa (get-sim W wa))
			(define sb (get-sim W wb))
			(define sc (get-sim W wc))
			(define vs (+ (- sa sb) sc))
			(cons W vs))
		WLIST))

	(define sosi
		(sort sims (lambda (L R) (> (cdr L) (cdr R)))))

	(for-each (lambda (ITM)
		(format #t "~A, ~6F\n" (cog-name (car ITM)) (cdr ITM)))
		(take sosi 10))
)

(most-sim "husband" "man" "woman" (take allwo 1000))
(most-sim "brother" "man" "woman" (take allwo 1000))
(most-sim "boy" "man" "woman" (take allwo 1000))

(most-sim "Paris" "France" "Spain" (take allwo 1000))
(most-sim "Paris" "France" "Germany" (take allwo 1000))
(most-sim "London" "England" "Germany" (take allwo 1000))

(most-sim "tree" "leaf" "flower" (take allwo 1000))
(most-sim "dog" "puppy" "cat" (take allwo 1000))
(most-sim "kitten" "cat" "puppy" (take allwo 1000))

(most-sim "hammer" "nail" "comb" (take allwo 1000))

(most-sim "black" "white" "up" (take allwo 1000))
(most-sim "black" "white" "good" (take allwo 1000))
(most-sim "black" "white" "smile" (take allwo 1000))
(most-sim "black" "white" "love" (take allwo 1000))

(most-sim "short" "light" "long" (take allwo 1000))
(most-sim "speak" "sing" "walk" (take allwo 1000))
(most-sim "like" "love" "dislike" (take allwo 1000))
(most-sim "left" "right" "north" (take allwo 1000))

(most-sim "flood" "rain" "drought" (take allwo 1000))
(most-sim "giggle" "laugh" "sniffle" (take allwo 1000))

(most-sim "blue" "sky" "green" (take allwo 1000))

; ---------------------------------------
; Recursion and hypervectors

; gos will fish out the pre-computed goe-mi and goe-rmi similarities
(define gos (add-similarity-api ami #f "goe"))

; At this time, there are only 1K precomputed similarities available.
(define kay-oh (take allwo 1000))
(define gob (add-keep-filter gos kay-oh kay-oh #t))

; Verify that we're getting counts as expected.
(gob 'get-count (car (gob 'get-all-elts)))

(define (add-goe-sim LLOBJ)
	(define (get-ref PR IDX)
		; Expect FloatValue always IDX=0 is the MI sims, and 1 is the RMI
; (if (not (LLOBJ 'get-count PR)) (format #t "duuude fail for ~A\n" PR))
		(cog-value-ref (LLOBJ 'get-count PR) IDX))

	(lambda (message . args)
		(case message
			((get-count)  (get-ref (car args) 0))
			(else      (apply LLOBJ (cons message args))))
	))

(define goc (add-goe-sim gob))
(goc 'get-count (car (goc 'get-all-elts)))

(define eft (add-gaussian-ortho-api goc))
(eft 'mean-rms)
; (0.09074988924648515 0.33889054461938395)

(define efc (add-similarity-compute eft))

(define efs (add-similarity-api gob #f "goe f-2"))

(define (f2-compute A B)
   (define f2 (efc 'left-cosine A B))
   (format #t "F2=~7F for (\"~A\", \"~A\")\n"
      f2 (cog-name A) (cog-name B))
   (store-atom
      (efs 'set-pair-similarity
         (efs 'make-pair A B)
         (FloatValue f2))))

(define (f2-dot-prod A B)
   (define have-it (efs 'pair-count A B))
   (if (not have-it) (f2-compute A B)))

; (define allwo (rank-words pcs))
(loop-upper-diagonal f2-dot-prod allwo 0 50)
(loop-upper-diagonal f2-dot-prod allwo 0 250)

; ========================================
; Above worked great. Now some graphs.

; We've computed only top 250 so far, so ...
(define topwo (take allwo 250))
(define tof (add-keep-filter efs topwo topwo #t))

(define all-f2 (tof 'get-all-elts))
(define wmi (/ (* 0.5 100) (length all-f2)))

; F2 distribution
(define f2-dist
   (bin-count all-f2 100
      (lambda (SIM) (cog-value-ref (efs 'get-count SIM) 0))
      (lambda (SIM) wmi)
      -1 1))

(define (prt-f2-dist)
	(define csv (open "/tmp/f2-dist-250.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv f2-dist csv)
	(close csv))

; ---------------------------------------
; Top most similar words (according to f2)

(define (fessi A B)
	(> (cog-value-ref (efs 'get-count A) 0)
		(cog-value-ref (efs 'get-count B) 0)))

(define all-f2-ord (sort all-f2 fessi))

(define distinct-f2-ord
	(filter (lambda (SL) (not (equal? (gar SL) (gdr SL)))) all-f2-ord))

(define (top-pairs LST N)
	(for-each (lambda (SL)
		(format #t "~6F, ~A, ~A\n" (cog-value-ref (efs 'get-count SL) 0)
			(cog-name (gar SL)) (cog-name (gdr SL))))
		(take LST N)))

(top-pairs distinct-f2-ord 20)

(top-pairs (drop distinct-f2-ord 100) 20)

; ---------------------------------------
; Dump datafile -- F2 vs f1 (aka goe cos-MI) scatterplot
; Graphed with p8-goe/scatter-f2.gplot

(define (scatter-f2 DOT-LIST FILENAME)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# f1 and f2\n#\n")
	(format csv "#\n# f1\tf2\n")
	(for-each
		(lambda (SL)
			(format csv "~8F\t~8F\n"
				(cog-value-ref (gos 'get-count SL) 0)
				(cog-value-ref (efs 'get-count SL) 0)))
		DOT-LIST)
	(close csv)
)

(scatter-f2 all-f2 "/tmp/scatter-f2.dat")

; ---------------------------------------
