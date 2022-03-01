;
; similarity-p5.scm
;
; Ad hoc scripts for greating assorted graphs and datasets comparing
; similarity between clusters, and between words, and crossed between
; the two.  This is for diary Part Five, February 2022.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; Ad hoc globals.
(define cset-obj (make-pseudo-cset-api))
(define covr-obj (add-covering-sections cset-obj))
(define star-obj covr-obj)

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(covr-obj 'fetch-pairs)
(covr-obj 'explode-sections)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

(define SIM-ID "shape-mi")
(define smi (add-similarity-api star-obj #f SIM-ID))
(define sup (add-support-api star-obj))

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

(define ranked-words (rank-words star-obj))

(define ranked-classes (filter
	(lambda (ELT) (equal? (cog-type ELT) 'WordClassNode))
	ranked-words))

(define ranked-wonly (filter
	(lambda (ELT) (equal? (cog-type ELT) 'WordNode))
	ranked-words))

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)

	(define short-list (take ranked-wonly NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

; ---------------------------------------
; Dump datafile --

(chdir "/home/ubuntu/experiments/run-12/data")

(define (prt-hist HIST FNAME)
	(define csv (open FNAME (logior O_WRONLY O_CREAT)))
	(define binsum (fold + 0 (array->list (second HIST))))
	(define centers (array->list (first HIST)))
	(define nbins (length centers))
	(define width (*
		(/ nbins (- nbins 1))
		(- (list-ref centers (- nbins 1)) (first centers))))
	(format csv "#\n# ~A\n#\n" FNAME)
	(format csv "# count= ~A bins= ~A width= ~A norm= ~9g\n#\n"
		binsum nbins width (/ (+ nbins 1e-30) (* width binsum)))
	(print-bincounts-tsv HIST csv)
	(close csv))

; ---------------------------------------
; Bin-count of self-MI

(define (self-mi-hist LST)
	(bin-count LST 40
		(lambda (WRD) (cog-value-ref (smi 'pair-count WRD WRD) 0))
		(lambda (WRD) 1)
		0 40))

(prt-hist (self-mi-hist ranked-classes) "self-mi-n3-classes.dat")
(prt-hist (self-mi-hist ranked-wonly) "self-mi-n3-words.dat")

(prt-hist (self-mi-hist ranked-classes) "self-mi-n4-classes.dat")
(prt-hist (self-mi-hist ranked-wonly) "self-mi-n4-words.dat")

; ---------------------------------------
; Tools for bin-counting of class and word similarities.

; Construct long list of similarity pairs, excluding
; the self-pairs.
(define (get-simlinks WLI)
	(filter-map
		(lambda (WPR)
			(define sim (cog-link 'Similarity (car WPR) (cdr WPR)))
			(if (or (nil? sim) (> -200 (cog-value-ref (smi 'get-count sim) 0)))
				 #f sim))
		; A list of all pairs, excluding the self-pairs
		(concatenate!
			(map
				(lambda (N)
					(define tli (drop WLI N))
					(define head (car tli))
					(define rest (cdr tli))
					(map (lambda (WRD) (cons head WRD)) rest))
				(iota (- (length WLI) 1))))))

; ---------------------------------------
; Look at the similarities between just the word-classes, only.

(define class-sims (get-simlinks ranked-classes))

(define mi-dist
	(bin-count class-sims 100
		(lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
		(lambda (SIM) 1)
		-20 20))

(prt-hist mi-dist "class-mi-n3.dat")
(prt-hist mi-dist "class-mi-n4.dat")

; ---------------------------------------
; Look at the similarities between just the words (no word-classes), only.

(define wli (top-ranked star-obj 530))
(define wli (top-ranked star-obj 536))

(define word-sims (get-simlinks wli))

(define mi-dist
	(bin-count word-sims 100
		(lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
		(lambda (SIM) 1)
		-18 12))

(prt-hist mi-dist "word-mi-n3.dat")
(prt-hist mi-dist "word-mi-n4.dat")

; ---------------------------------------
; Look at the similarities between words and classes

(define (get-p-simlinks WLA WLB)
	(filter-map
		(lambda (WPR)
			(define sim (cog-link 'Similarity (car WPR) (cdr WPR)))
			(if (or (nil? sim) (> -200 (cog-value-ref (smi 'get-count sim) 0)))
				 #f sim))
		; A list of all pairs, excluding the self-pairs
		(concatenate!
			(map
				(lambda (WA) (map (lambda (WB) (cons WA WB)) WLB))
				WLA))))

(define clawrd-sims (get-p-simlinks wli ranked-classes))

(define mi-dist
	(bin-count clawrd-sims 100
		(lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
		(lambda (SIM) 1)
		-18 12))

(prt-hist mi-dist "clawrd-mi-n3.dat")
(prt-hist mi-dist "clawrd-mi-n4.dat")

; ---------------------------------------
; ---------------------------------------
