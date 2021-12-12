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
(use-modules (opencog nlp) (opencog learn))
(use-modules (opencog matrix))
(use-modules (opencog cogserver))
(start-cogserver)

(define wpa (make-any-link-api))
(define wps (add-pair-stars wpa))

(define wpf (add-pair-freq-api wps))
(define wpu (add-support-api wps))

(wps 'fetch-pairs)
(print-matrix-summary-report wps)

; ----------
; Start with the cut dataset, where all pairs with MI<1.8 were
; thrown away...
(sql-open "postgres:///en_pairs_mi_1.8cut")
(define wall (wps 'get-all-elts))
(length wall)  ; << 15795739

; Just the bin counts on the cut dataset
; This should be idntical to the bin-counts on the full dataset,
; but with a sharp cliff at MI=1.8
(define cut-bins (bin-count wall 400
	(lambda (item) (wpf 'pair-fmi item))
	(lambda (item) 1)
	-15 30))

(let ((outport (open-file "/tmp/cut-fmi.dat" "w")))
   (print-bincounts-tsv cut-bins outport)
   (close outport))

; ----------
; Like above, but now we access the recomputed MI on the same
; pairs. This smooths away the cliff.
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
; Scatterplot of both mi's. Huge dataset.

(let ((outport (open-file "/tmp/scatter-fmi.dat" "w")))
	(format outport "#\n# Cut-FMI	Recomputed-FMI\n#\n")
	(for-each (lambda (pr)
		(format outport "~6F	~6F\n" (wpf 'pair-fmi pr) (fpf 'pair-fmi pr)))
		wall)
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
; ----------
; Like above, but for the recomputed MI on the deeper cut.

; Recomputed FMI on the 5.3<MI dataset.
(sql-open "postgres:///en_pairs_5.3_remi")
(define wall (wps 'get-all-elts))
(length wall)  ; << 8953449

(define recomp-bins (bin-count wall 400
	(lambda (item) (wpf 'pair-fmi item))
	(lambda (item) 1)
	-15 30))

(let ((outport (open-file "/tmp/deep-recomp-fmi.dat" "w")))
   (print-bincounts-tsv recomp-bins outport)
   (close outport))

; ---------------------------------------------------------------------
; ----------
Scatterplot of frequency vs. mi. Huge dataset.

(define wall (wps 'get-all-elts))
(define wpf (add-pair-freq-api wps))
(let ((outport (open-file "/tmp/scatter-freq-fmi.dat" "w")))
	(format outport "#\n# Frequency FMI\n#\n")
	(for-each (lambda (pr)
		(format outport "~11G	~8F\n" (wpf 'pair-freq pr) (wpf 'pair-fmi pr)))
		wall)
   (close outport))

; ---------------------------------------------------------------------
; Distribution of Banach norms 

(define wpu (add-support-api wps))
(let ((outport (open-file "/tmp/banach-l0-row.dat" "w")))
	(format outport "#\n# Banach l_0 row\n#\n")
	(for-each
		(lambda (num) (format outport "~8F\n" num))
		(sort (map (lambda (row) (wpu 'right-support row))
				(wps 'left-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l0-col.dat" "w")))
	(format outport "#\n# Banach l_0 col\n#\n")
	(for-each
		(lambda (num) (format outport "~8F\n" num))
		(sort (map (lambda (row) (wpu 'left-support row))
				(wps 'right-basis))   >))
   (close outport))


(let ((outport (open-file "/tmp/banach-l1-row.dat" "w")))
	(format outport "#\n# Banach l_1 row\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (wpu 'right-count row))
				(wps 'left-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l1-col.dat" "w")))
	(format outport "#\n# Banach l_1 col\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (wpu 'left-count row))
				(wps 'right-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l2-row.dat" "w")))
	(format outport "#\n# Banach l_2 row\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (wpu 'right-length row))
				(wps 'left-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l2-col.dat" "w")))
	(format outport "#\n# Banach l_2 col\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (wpu 'left-length row))
				(wps 'right-basis))   >))
   (close outport))


; ---------------------
Like above, but ratios

(let ((outport (open-file "/tmp/banach-l1-l0-row.dat" "w")))
	(format outport "#\n# Banach l_1/l_0 row\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (/ (wpu 'right-count row)
					(wpu 'right-support row)))
				(wps 'left-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l1-l0-col.dat" "w")))
	(format outport "#\n# Banach l_1/l_0 col\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (/ (wpu 'left-count row)
					(wpu 'left-support row)))
				(wps 'right-basis))   >))
   (close outport))


(let ((outport (open-file "/tmp/banach-l2-l0-row.dat" "w")))
	(format outport "#\n# Banach l_2/l_0 row\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (/ (wpu 'right-length row)
					(wpu 'right-support row)))
				(wps 'left-basis))   >))
   (close outport))

(let ((outport (open-file "/tmp/banach-l2-l0-col.dat" "w")))
	(format outport "#\n# Banach l_2/l_0 col\n#\n")
	(for-each
		(lambda (num) (format outport "~9F\n" num))
		(sort (map (lambda (row) (/ (wpu 'left-length row)
					(wpu 'left-support row)))
				(wps 'right-basis))   >))
   (close outport))

; ---------------------
; Correlation of support, count, length

(let ((outport (open-file "/tmp/banach-l0-l1-l2-row.dat" "w")))
	(format outport "#\n# l_0	l1	l2 row\n#\n")
	(for-each
		(lambda (row) (format outport "~9F	~9F	~9F\n" 
			(wpu 'right-support row)
			(wpu 'right-count row)
			(wpu 'right-length row)))

		(sort (wps 'left-basis)
			(lambda (ra rb)
				(> (wpu 'right-support ra) (wpu 'right-support rb)))))
   (close outport))

(let ((outport (open-file "/tmp/banach-l0-l1-l2-col.dat" "w")))
	(format outport "#\n# l_0	l1	l2 row\n#\n")
	(for-each
		(lambda (row) (format outport "~9F	~9F	~9F\n" 
			(wpu 'left-support row)
			(wpu 'left-count row)
			(wpu 'left-length row)))

		(sort (wps 'right-basis)
			(lambda (ra rb)
				(> (wpu 'left-support ra) (wpu 'left-support rb)))))
   (close outport))

; ---------------------------------------------------------------------
; Bin-counted stuff.
;
(define bincs (bin-count (wps 'right-basis) 200
	; 1.442695 is 1/log 2 so that we plot log2.
	(lambda (item)
		(* 1.442695 (log (wpu 'left-support item))))
	(lambda (item) 1)
	0 20))

(let ((outport (open-file "/tmp/hist-support.dat" "w")))
   (print-bincounts-tsv bincs outport)
   (close outport))

; ---------
(define bincs (bin-count (wps 'right-basis) 200
	; 1.442695 is 1/log 2 so that we plot log2.
	(lambda (item)
		(* 1.442695 (log (wpu 'left-count item))))
	(lambda (item) 1)
	0 30))

(let ((outport (open-file "/tmp/hist-count.dat" "w")))
   (print-bincounts-tsv bincs outport)
   (close outport))

(define bincs (bin-count (wps 'right-basis) 200
	; 1.442695 is 1/log 2 so that we plot log2.
	(lambda (item)
		(* 1.442695 (log (wpu 'left-length item))))
	(lambda (item) 1)
	0 25))

(let ((outport (open-file "/tmp/hist-length.dat" "w")))
   (print-bincounts-tsv bincs outport)
   (close outport))

; ---------------------------------------------------------------------
;
(define all-words (cog-get-atoms 'WordNode #f))
