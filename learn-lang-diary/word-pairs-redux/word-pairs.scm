;
; word-pairs.scm
;
; Assorted ad-hoc collection of tools for understanding word-pairs.
;
; Used to create graphs for diary entry in 2019.
;
; Copyright (c) 2019 Linas Vepstas
;

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------
; Load the dataset that is analyzed throughout.
(use-modules (opencog) (opencog persist) (opencog persist-sql))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog matrix))
(use-modules (opencog cogserver))
(start-cogserver)
(sql-open "postgres:///en_pairs_mi_1.8tmp")

(define wpa (make-any-link-api))
(define wps (add-pair-stars wpa))

(define wpf (add-pair-freq-api wps))
(define wpu (add-support-api wps))

(wps 'fetch-pairs)
(print-matrix-summary-report wps)

; ----------
(define wall (wps 'get-all-elts))
(length wall)  ; << 15795739

; Just the bin counts on the cut dataset
(define cut-bins (bin-count wall 400
	(lambda (item) (wpf 'pair-fmi item))
	(lambda (item) 1)
	-15 30))

(let ((outport (open-file "/tmp/cut-fmi.dat" "w")))
   (print-bincounts-tsv cut-bins outport)
   (close outport))

; ----------
(define fpm (add-fmi-filter wps 1.8 #t))  ;; <<< #t so filter-keys!
; (batch-all-pair-mi fpm) ; << how it got created. 
(define fpf (add-pair-freq-api fpm))
(print-matrix-summary-report fpf)

; Recomputed FMI on the very same pairs...
(define recomp-bins (bin-count wall 400
	(lambda (item) (fpf 'pair-fmi item))
	(lambda (item) 1)
	-15 30))

(let ((outport (open-file "/tmp/cut-recomp-fmi.dat" "w")))
   (print-bincounts-tsv recomp-bins outport)
   (close outport))

; ----------
; As above, but all pairs, unfiltered.
(sql-open "postgres:///en_pairs_mi")
(define all-bins (bin-count wall 400
	(lambda (item) (wpf 'pair-fmi item))
	(lambda (item) 1)
	-15 30))

(let ((outport (open-file "/tmp/full-fmi.dat" "w")))
   (print-bincounts-tsv all-bins outport)
   (close outport))

(define call-bins (bin-count wall 400
	(lambda (item) (wpf 'pair-fmi item))
	(lambda (item) (wpf 'get-count item))
	-15 30))

(let ((outport (open-file "/tmp/full-weighted-fmi.dat" "w")))
   (print-bincounts-tsv call-bins outport)
   (close outport))



; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
