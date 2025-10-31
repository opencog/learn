#! /usr/bin/env -S guile -l ./marginals-pair.scm --
!#
;
; marginals-pair.scm
;
; After word-pair counting has been completed, this script is run to
; compute marginal probabilities and word-pair mutual information.
;
; ---------------------------------------------------------------------
(load "cogserver.scm")

; Init the statistics objects.
(define ala (make-any-link-api))
(define aca (add-count-api ala))
(define asa (add-pair-stars aca))

; Load the data.
(display "Start loading sparse matrix pairs\n")
(asa 'fetch-pairs)
(display "Finished loading sparse matrix pairs\n")
(cog-report-counts)

; Compute all pair MI's
(batch-all-pair-mi asa)
(print-matrix-summary-report asa)

(barrier storage-node)
; (cog-close storage-node)

; ---------------------------------------------------------------------
