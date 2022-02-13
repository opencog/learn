;
; zipf-plot.scm
;
; Assorted ad-hoc scripts for generating ranked lists of atoms. Such
; lists, when graphed, typically have a Zipfian distribution. These kind
; of scripts have been used for many years to generate lots of graphs
; appearing in the diary. Yet, oddly, there hasn't been a centralized
; location for them. So here they are. Its quite simple.
;
; These can ONLY be used by hand, by cutting and pasting the interesting
; bits from this file, into a guile prompt. This is effectively a kind-of
; work-log of what was needed to generate those pictures. Of course, this
; can be recycled for other datasets, too.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; Ranking and printing utilities
;
; Assign each item a score, using SCORE-FN
(define (score SCORE-FN ITEM-LIST)
	(map (lambda (itm) (cons (SCORE-FN itm) itm)) ITEM-LIST))

; Assign each item a score, using SCORE-FN, and then rank them by
; score: i.e. sort them, with highest score first.
(define (score-and-rank SCORE-FN ITEM-LIST)
	(sort
		(map (lambda (wrd) (cons (SCORE-FN wrd) wrd)) ITEM-LIST)
		(lambda (a b) (> (car a) (car b)))))

; Print to port a tab-separated table of rankings
(define (print-ts-rank-fn scrs port fn-str)
	(define cnt 0)
	(for-each
		(lambda (pr)
			(set! cnt (+ cnt 1))
			(format port "~A  ~A \"~A\"\n" cnt (car pr) (fn-str (cdr pr))))
		scrs))

; Same as above, but only print when the score has changed.
(define (print-sparse-rank-fn scrs port fn-str)
	(define cnt 0)
	(define score 1e30)
	(for-each
		(lambda (pr)
			(define iscor (inexact->exact (car pr)))
			(set! cnt (+ cnt 1))
			(when (< iscor score)
				(set! score iscor)
				(format port "~A  ~A \"~A\"\n" cnt iscor (fn-str (cdr pr)))
				(force-output port)))
		scrs)

	; Print the very last one too.
	(define final (car (take-right scrs 1)))
	(format port "~A  ~A \"~A\"\n" (length scrs)
		(inexact->exact (car final)) (fn-str (cdr final)))
	(force-output port)
)

; ---------------------------------------------------------------------
; Zipf graph from Diary Part Six

; This list has 28 million entries in it.
(define all-pairs (star-obj 'get-all-elts))

(define (pobs ITEM) (star-obj 'get-count ITEM))

(define pair-list (score-and-rank pobs all-pairs))

(define (prt-pair ITEM)
	(string-append (cog-name (star-obj 'left-element ITEM))
		" <<>> " (cog-name (star-obj 'right-element ITEM))))

(let ((outport (open-file "/tmp/pair-rank.dat" "w")))
	; (print-ts-rank-fn pair-list outport prt-pair)
	(print-sparse-rank-fn pair-list outport prt-pair)
	(close outport))

; ---------------------------------------------------------------------
