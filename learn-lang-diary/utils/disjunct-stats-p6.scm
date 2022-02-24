;
; disjunct-stats-p6.scm
;
; Assorted ad-hoc collection of tools for understanding the
; word-disjunct MI distribution.  This is an extract of the
; earlier, 2017-era disjunct-stats.scm file, redone for Part Six
; of the diary, i.e. with newer datasets.
;
; Copyright (c) 2022 Linas Vepstas


(use-modules (srfi srfi-1))

(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
(define psf (add-pair-freq-api psa))
(define psu (add-support-api psa))

(psa 'fetch-pairs)

; ---------------------------------------------------------------------
; Ranking and printing utilities
;
; Assign each item a score, using SCORE-FN
(define (score SCORE-FN ITEM-LIST)
   (map (lambda (wrd) (cons (SCORE-FN wrd) wrd)) ITEM-LIST))

; ---------------------------------------------------------------------
; Bin-count word-disjunct pairs according to thier fractional MI.

(define all-sects (psa 'get-all-elts))

(define (sect-mi SECT) (psf 'pair-fmi SECT))

(define scored-sect-mi (score sect-mi all-sects))

(define binned-sect-mi (bin-count-simple scored-sect-mi 200))

(let ((outport (open-file "/tmp/r4-sect-mi-2-2-2.dat" "w")))
   (print-bincounts-tsv binned-sect-mi outport)
   (close outport))

(define (sect-freq SECT) (psf 'pair-freq SECT))

(define weighted-sect-mi
   (bin-count-weighted scored-sect-mi 200
      (lambda (scored-item) (sect-freq (cdr scored-item)))))

(let ((outport (open-file "/tmp/r4-wei-mi-2-2-2.dat" "w")))
   (print-bincounts-tsv weighted-sect-mi outport)
   (close outport))

; -------------
