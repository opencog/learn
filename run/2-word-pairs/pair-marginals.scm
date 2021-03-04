#! /usr/bin/env -S guile
!#
;
; pair-marginals.scm
;
; After word-pair counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before the next step.
;
(load "../common/cogserver-rocks.scm")

; Init the statistics objects.
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))

; Compute the statistics
(batch-pairs asa)

; (print-matrix-summary-report asa)

; (cog-rocks-close)
