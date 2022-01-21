;
; Entropy-merginals.scm
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

; ---------------------------------------------------------------------

(define all-words (star-obj 'left-basis))
(length all-words)  ; 15083

(define frq-obj (add-pair-freq-api star-obj))
(define (right-freq WRD) (frq-obj 'right-wild-freq WRD))
(define (right-ent WRD) (frq-obj 'right-wild-entropy WRD))
(define (right-fent WRD) (frq-obj 'right-wild-fentropy WRD))

; ---------------------------------------------------------------------
; Zipf ranking

(define word-freq
	(score-and-rank right-freq all-words))

(let ((outport (open-file "/tmp/rank-wfreq.dat" "w")))
	(print-ts-rank word-freq outport)
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

(let ((outport (open-file "/tmp/bin-went.dat" "w")))
	(print-bincounts-tsv binned-went outport)
	(close outport))


; ---------------------------------------------------------------------
; Weighted entropy

(define bin-wei-went
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fentropy WRD))
		(lambda (WRD) (frq-obj 'right-wild-freq WRD))
		16 24))

(let ((outport (open-file "/tmp/bin-wei-went.dat" "w")))
	(print-bincounts-tsv bin-wei-went outport)
	(close outport))

; ---------------------------------------------------------------------
; Unweighted MI

(define bin-wmi
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fmi WRD))
		(lambda (WRD) 1.0)
		2 22))

(let ((outport (open-file "/tmp/bin-wmi.dat" "w")))
	(print-bincounts-tsv bin-wmi outport)
	(close outport))

; ---------------------------------------------------------------------
; Weighted MI

(define bin-wei-wmi
	(bin-count all-words 200
		(lambda (WRD) (frq-obj 'right-wild-fmi WRD))
		(lambda (WRD) (frq-obj 'right-wild-freq WRD))
		2 22))

(let ((outport (open-file "/tmp/bin-wei-wmi.dat" "w")))
	(print-bincounts-tsv bin-wei-wmi outport)
	(close outport))

; ---------------------------------------------------------------------
