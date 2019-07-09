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

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
