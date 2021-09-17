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

(define wli (top-ranked pcs 700))

(list-ref wli 101)

; ---------------------------------------
; Self-similary rank vs MI scatterplot

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

(define (wtf WORD-LIST)
	(for-each
		(lambda (WRD)
			(define ct (sup 'right-count WRD))
			(define su (sup 'right-support WRD))
			(if (< 1 (/ su ct))
				(format #t "wtf ~A ~A ~A\n" WRD su ct)))
		WORD-LIST))

(define fi (sup 'right-duals (Word "fishermen")))
(map (lambda (CONSEQ) (length (sup 'left-stars CONSEQ))) fi)

; ---------------------------------------

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
