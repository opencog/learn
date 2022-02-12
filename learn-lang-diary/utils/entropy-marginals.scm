;
; Entropy-marginals.scm
;
; Assorted ad-hoc scripts for exploring marginal entropy distributions
; for word-disjunct pairs.  These are used to create the graphs and
; analysis in "Diary Part Five" (Jan 2022).
;
; These can ONLY be used by hand, by cutting and pasting the interesting
; bits from this file, into a guile prompt. This is effectively a kind-of
; work-log of what was needed to generate those pictures. Of course, this
; can be recycled for other datasets, too.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; Load the dataset that is analyzed throughout.
;
; Starting point: `r9-sim.rdb` which is loaded using
; `guile -l cogserver-gram.scm` followed by `(batch-all-pair-mi star-obj)`
; The former does this:
;
; (define cset-obj (make-pseudo-cset-api))
(define covr-obj (add-covering-sections cset-obj))
(covr-obj 'fetch-pairs)
(covr-obj 'explode-sections)
(define star-obj covr-obj)

; From this, the `r9-sim+mi.rdb` was genrated by
(batch-all-pair-mi star-obj)

; ---------------------------------------------------------------------
; Ranking and printing utilities
;
; Assign each word a score, using SCORE-FN
(define (score SCORE-FN WORD-LIST)
	(map (lambda (wrd) (cons (SCORE-FN wrd) wrd)) WORD-LIST))

; Assign each word a score, using SCORE-FN, and then rank them by
; score: i.e. sort them, with highest score first.
(define (score-and-rank SCORE-FN WORD-LIST)
	(sort
		(map (lambda (wrd) (cons (SCORE-FN wrd) wrd)) WORD-LIST)
		(lambda (a b) (> (car a) (car b)))))

; Print to port a tab-separated table of rankings
(define (print-ts-rank-fn scrs port fn-str)
	(define cnt 0)
	(for-each
		(lambda (pr)
			(set! cnt (+ cnt 1))
			(format port "~A  ~A \"~A\"\n" cnt (car pr) (fn-str (cdr pr))))
		scrs))

(define (print-ts-rank scrs port)
	(print-ts-rank-fn scrs port cog-name))

; Print histogram
(define (print-histo FILENAME HISTO)
	(define outport (open-file FILENAME "w"))
	(print-bincounts-tsv HISTO outport)
	(close outport))

; ---------------------------------------------------------------------

(define all-words (star-obj 'left-basis))
(length all-words)  ; 15083

(define frq-obj (add-pair-freq-api star-obj))
(define (right-freq WRD) (frq-obj 'right-wild-freq WRD))
(define (right-ent WRD) (frq-obj 'right-wild-entropy WRD))
(define (right-fent WRD) (frq-obj 'right-wild-fentropy WRD))
(define (right-fmi WRD) (frq-obj 'right-wild-fmi WRD))

; ---------------------------------------------------------------------
; Zipf ranking

(define word-freq
	(score-and-rank right-freq all-words))

(let ((outport (open-file "/tmp/rank-wfreq.dat" "w")))
	(print-ts-rank word-freq outport)
	(close outport))

; Print to port a tab-separated table of rankings
(define (print-ts-rank-scat scrs port)
	(define cnt 0)
	(for-each
		(lambda (pr)
			(define wrd (cdr pr))
			(set! cnt (+ cnt 1))
			(format port "~A\t~10G\t~7F\t~7F\t\"~A\"\n" cnt (car pr)
				(right-fent wrd)
				(right-fmi wrd)
				(cog-name wrd)))
		scrs))

(let ((outport (open-file "/tmp/rank-wfreq.dat" "w")))
	(print-ts-rank-scat word-freq outport)
	(close outport))

(define word-entropy
	(score-and-rank right-ent all-words))

(let ((outport (open-file "/tmp/rank-went.dat" "w")))
	(print-ts-rank word-entropy outport)
	(close outport))

; ---------------------------------------------------------------------
; Unweighted entropy

(define word-fentropy
	(score-and-rank right-fent all-words))

(define binned-went (bin-count-simple word-fentropy 200 17.0 24.0))

(print-histo "/tmp/bin-went.dat" binned-went)

; ---------------------------------------------------------------------
; Weighted entropy

(define bin-wei-went
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fentropy WRD))
		(lambda (WRD) (frq-obj 'right-wild-freq WRD))
		16 24))

(print-histo "/tmp/bin-wei-went.dat" bin-wei-went)

; ---------------------------------------------------------------------
; Unweighted MI

(define bin-wmi
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fmi WRD))
		(lambda (WRD) 1.0)
		2 22))

(print-histo "/tmp/bin-wmi.dat" bin-wmi)

; ---------------------------------------------------------------------
; Weighted MI

(define bin-wei-wmi
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fmi WRD))
		(lambda (WRD) (frq-obj 'right-wild-freq WRD))
		2 22))

(print-histo "/tmp/bin-wei-wmi.dat" bin-wei-wmi)

; ---------------------------------------------------------------------
; Similarity distributions

(define SIM-ID "shape-mi")
(define sap (add-similarity-api LLOBJ #f SIM-ID))
(define smi (add-symmetric-mi-compute LLOBJ))
(define mmt-q (smi 'mmt-q))

(load-atoms-of-type 'Similarity)
(define all-sims (cog-get-atoms 'Similarity))

; There are 20100 similarity pairs. Of these, 175 have no MI
; at all on them.

(define (get-mi PR) (cog-value-ref (sap 'get-count PR) 0))
(define (get-rmi PR) (cog-value-ref (sap 'get-count PR) 1))
(define (get-hmi PR)
	(define WA (gar PR))
	(define WB (gdr PR))
	(+ (get-mi PR) (* 0.5 (+ 
		(frq-obj 'right-wild-fentropy WA)
		(frq-obj 'right-wild-fentropy WB)))))

(define (get-mmi PR)
	(define WA (gar PR))
	(define WB (gdr PR))
	(+ (get-mi PR) (* 0.5 (+ 
		(frq-obj 'right-wild-fmi WA)
		(frq-obj 'right-wild-fmi WB)))))

(define scored-mi (score-and-rank get-mi all-sims))
(define scored-rmi (score-and-rank get-rmi all-sims))
(define scored-hmi (score-and-rank get-hmi all-sims))
(define scored-mmi (score-and-rank get-mmi all-sims))

(define sim-mi (bin-count-simple scored-mi 200 -15.0 15.0))
(define sim-rmi (bin-count-simple scored-rmi 200 -15.0 15.0))
(define sim-hmi (bin-count-simple scored-hmi 200 0 30.0))
(define sim-mmi (bin-count-simple scored-mmi 200 -15.0 15.0))

(print-histo "/tmp/sim-mi.dat" sim-mi)
(print-histo "/tmp/sim-rmi.dat" sim-rmi)
(print-histo "/tmp/sim-hmi.dat" sim-hmi)
(print-histo "/tmp/sim-mmi.dat" sim-mmi)

; ---------------------------------------------------------------------
; Top-twenty list, ignore self-MI.

(define (prt-top LST)
	(define top #f)
	(for-each (lambda (SCPR)
		(if top (format #t "~5F\t" (- (car SCPR) top))
			(begin (set! top (car SCPR)) (format #t "~5F\t" top)))
		(format #t "~A ~A\n"
			(cog-name (gar (cdr SCPR)))
			(cog-name (gdr (cdr SCPR)))))
		LST))

(define (self? SCPR) (not (equal? (gar (cdr SCPR)) (gdr (cdr SCPR)))))

(prt-top (filter self? (take scored-mi 90)))

(define (prt-r LST)
	(define top #f)
	(for-each (lambda (SCPR)
		(if top (format #t "~5F\n" (- (car SCPR) top))
			(begin (set! top (car SCPR)) (format #t "~5F\n" top))))
		LST))

(define (prt-w LST)
	(define top #f)
	(for-each (lambda (SCPR)
		(format #t "~A ~A\n"
			(cog-name (gar (cdr SCPR)))
			(cog-name (gdr (cdr SCPR)))))
		LST))

; ---------------------------------------------------------------------
