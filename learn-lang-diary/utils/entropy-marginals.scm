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

; ---------------------------------------------------------------------

(define all-words (star-obj 'left-basis))
(length all-words)  ; 15083

(define frq-obj (add-pair-freq-api star-obj))
(define (right-ent WRD) (frq-obj 'right-wild-fentropy WRD))

; ---------------------------------------------------------------------
; Unweighted entropy

(define word-entropy
	(score-and-rank right-ent all-words))

(define binned-went (bin-count-simple word-entropy 200 17.0 24.0))

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

