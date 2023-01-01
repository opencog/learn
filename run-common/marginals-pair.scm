#! /usr/bin/env -S guile
!#
;
; marginals-pair.scm
;
; After word-pair counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before the next step.
;
; A reasonable alternative to running this is running `trim-pairs.scm`
; which will trim down the dataset, and then compute the marginals for
; the smaller dataset. Except that maybe trimming is a bad idea.
; Although the jury is still out on that. Trimming is important, but
; subtle.
;
; ---------------------------------------------------------------------
(load "cogserver.scm")

; Init the statistics objects.
(define ala (make-any-link-api))
(define aca (add-count-api ala))
(define asa (add-pair-stars aca))

; Load the data.
(asa 'fetch-pairs)))
(display "Finished loading sparse matrix pairs\n")
(cog-report-counts)

; Compute all pair MI's
(batch-all-pair-mi asa)
(print-matrix-summary-report asa)

(barrier storage-node)
; (cog-close storage-node)

; ---------------------------------------------------------------------
