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

(define wli (top-ranked pcs 700))

(list-ref wli 101)

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
