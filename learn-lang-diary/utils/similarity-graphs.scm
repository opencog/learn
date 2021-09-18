;
; similarity-graphs.scm
;
; Ad hoc scripts for greating assorted graphs and datasets comparing
; different similarity measures. This is for diary Part Three, Sept
; 2021.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))


; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sup (add-support-api pcs))
(define sap (add-similarity-api pcs))
(define smi (add-similarity-api pcs #f "mi"))
(define sov (add-similarity-api pcs #f "overlap"))
(define scj (add-similarity-api pcs #f "condjacc"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)
(sap 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

; ---------------------------------------

; Return a list of all words, ranked by count.
; If counts are equal, then rank by support.
(define (rank-words LLOBJ)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))

	(define wrds (LLOBJ 'left-basis))
	(sort wrds
		(lambda (ATOM-A ATOM-B)
			(define na (nobs ATOM-A))
			(define nb (nobs ATOM-B))
			(if (equal? na nb)
				(> (nsup ATOM-A) (nsup ATOM-B))
				(> na nb))))
)

; Same as above, but ranked by support first, and if equal,
; only then obs.
(define (sup-rank-words LLOBJ)
	(define sup (add-support-api LLOBJ))
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))
	(define wrds (LLOBJ 'left-basis))
	(sort wrds
		(lambda (ATOM-A ATOM-B)
			(define na (nsup ATOM-A))
			(define nb (nsup ATOM-B))
			(if (equal? na nb)
				(> (nobs ATOM-A) (nobs ATOM-B))
				(> na nb))))
)

(define ranked-words (rank-words pcs))
(define sup-ranked-words (sup-rank-words pcs))

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)

	(define short-list (take ranked-words NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

(define wli (top-ranked pcs 1200))

(list-ref wli 101)

; ---------------------------------------
; Dump datafile -- Self-similary rank vs MI scatterplot

(chdir "/home/ubuntu/experiments/run-6/data")

(define (rank-mi-scatter WORD-LIST TITLE FILENAME)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# ~A\n#\n" TITLE)
	(format csv "#\n# rank\tword\tcount\tself-mi\tsupport\n")
	(for-each
		(lambda (WRD)
			(define fv-mi (smi 'pair-count WRD WRD))
			(set! cnt (+ 1 cnt))
			(format csv
				"~A\t~A\t~D\t~6F\t~D\n" cnt (cog-name WRD)
				(inexact->exact (sup 'right-count WRD))
				(cog-value-ref fv-mi 0)
				(inexact->exact (sup 'right-support WRD))
			)
		)
		WORD-LIST
	)
	(close csv)
)

(rank-mi-scatter ranked-words
	"Observation-Count Rank words vs. self-MI"
	"rank-mi-scatter.dat")

; Same as above, but ranked by support.
(rank-mi-scatter sup-ranked-words
	"Support Rank words vs. self-MI"
	"sup-rank-mi-scatter.dat")

; ---------------------------------------
; How many words share a common connector sequence?

(define fi (sup 'right-duals (Word "fishermen")))
(map (lambda (CONSEQ) (length (sup 'left-stars CONSEQ))) fi)

; ---------------------------------------
; Take a look at connector sequences on specific words...
;

; Compute average connectorseq length for a word.
; Unweighted -- every conseq counted once.
(define (avg-conlen WORD)
	(define conseqs (sup 'right-duals WORD))
	(exact->inexact (/
		(fold + 0 (map (lambda (CONSEQ) (cog-arity CONSEQ)) conseqs))
		(length conseqs))))

; Compute weighted average connectorseq length for a word.
; Average is weighted by number of words that conseq is in.
(define (avg-weighted-conlen WORD)
	(define conseqs (sup 'right-duals WORD))
	(exact->inexact (/
		(fold + 0 
			(map 
				(lambda (CONSEQ) (* 
					(cog-arity CONSEQ)
					(length (sup 'left-stars CONSEQ))))
				conseqs))
		(fold + 0 
			(map 
				(lambda (CONSEQ) (length (sup 'left-stars CONSEQ)))
				conseqs)))))

; Scatterplot of MI vs average connector lengths.
(define (conlen-mi-scatter WORD-LIST)
	(define csv (open "conlen-mi-scater.dat" (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# ConnectorSeq Averge Lenghts\n#\n")
	(format csv "#\n# rank\tword\tself-mi\tAvg\tWeighted\n")
	(for-each
		(lambda (WRD)
			(define fv-mi (smi 'pair-count WRD WRD))
			(set! cnt (+ 1 cnt))
			(format csv
				"~A\t~A\t~6F\t~6F\t~6F\n" cnt (cog-name WRD)
				(cog-value-ref fv-mi 0)
				(avg-conlen WRD)
				(avg-weighted-conlen WRD)
			)
			(force-output csv)
		)
		WORD-LIST
	)
	(close csv)
)

(conlen-mi-scatter ranked-words)

; ---------------------------------------
; Bin-count of self-MI

(define self-mi-hist
	(bin-count ranked-words 34
		(lambda (WRD) (cog-value-ref (smi 'pair-count WRD WRD) 0))
		(lambda (WRD) 1)
		0 34))

(define (prt-self-mi-hist)
	(define csv (open "self-mi-hist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv self-mi-hist csv)
	(close csv))

; ---------------------------------------
; Bin-count of word similarities.

(define wli (top-ranked pcs 1200))

; Construct long list of simlinks.
(define (get-simlinks WLI)
	(filter-map
		(lambda (WPR)
			(define sim (cog-link 'Similarity (car WPR) (cdr WPR)))
			(if (nil? sim) #f sim))
		(concatenate!   ; a list of all word-pairs
			(map
				(lambda (N)
					(define tli (drop WLI N))
					(define head (car tli))
					(map (lambda (WRD) (cons head WRD)) tli))
				(iota (length WLI))))))

(define all-sims (get-simlinks wli))
(length all-sims) ; 386380

(define nbins 100)
(define width 50)
(define pind (/ 2.0 (length all-sims)))
(define mi-dist
	(bin-count all-sims 100
		(lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
		(lambda (SIM) pind)
		-25 25))

(define (prt-mi-dist)
	(define csv (open "mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv mi-dist csv)
	(close csv))

; ---------------------------------------
; List of lists, used for inverted-MI exploration.

; Create a sorted list by sim(WRD, word in WLIST)
; LLOBJ should be smi sov or scj
(define (sorted-by-sim LLOBJ WRD WLIST)
	(define valid-wlist
		(filter (lambda (ATOM) (LLOBJ 'pair-count ATOM WRD)) WLIST))
	(stable-sort valid-wlist
		(lambda (ATOM-A ATOM-B)
			(define sa (LLOBJ 'pair-count ATOM-A WRD))
			(define sb (LLOBJ 'pair-count ATOM-B WRD))
			(> (cog-value-ref sa 0) (cog-value-ref sb 0)))))

; (define x (sorted-by-sim smi (WordNode "of") wli))
; (any (lambda (wrd) (equal? wrd (WordNode "of"))) x)
; (map (lambda (wrd) (smi 'pair-count wrd (WordNode "of"))) x)

% list of ranked lists, sorted by MI.
% Acutally, cons pairs: (word . sorted-list)
(define rli
	(map (lambda (WRD) (cons WRD (list (sorted-by-sim smi WRD wli)))) wli))

; ---------------------------------------
; Sccripts for MI inversion

(define upr
	(filter
		(lambda (SLI) (not (equal? (car SLI) (caadr SLI))))
		rli))

(length upr)  ; 135

(WordNode "in") ; le
(smi 'pair-count (WordNode "in") (WordNode "in")) ; 4.6861
(smi 'pair-count (WordNode "in") (WordNode "le")) ; 5.0586
(smi 'pair-count (WordNode "in") (WordNode "le")) ; 16.088

(define (take-upr SLI)
	(define head-word (car SLI))
	(take-while
		(lambda (WRD) (not (equal? WRD head-word)))
		(cadr SLI)))

(for-each
	(lambda (SLI)
		(format #t "~A has ~A\n" (car SLI) (take-upr SLI)))
	upr)

(for-each
	(lambda (SLI)
		(define head-word (car SLI))
		(define bigs (take-upr SLI))
		(format #t "~A has ~A\n" head-word bigs)
		(for-each (lambda (SWD)
			(format #t "its: ~A -- ~A : ~6F vs ~6F\n"
				(cog-name head-word) (cog-name SWD)
				(cog-value-ref (smi 'pair-count SWD head-word) 0)
				(cog-value-ref (smi 'pair-count SWD SWD) 0)))
			bigs)
		(format #t "its: ~A -- ~A : ~6F\n"
			(cog-name head-word) (cog-name head-word)
			(cog-value-ref (smi 'pair-count head-word head-word) 0))
	)
	upr)


; ---------------------------------------
