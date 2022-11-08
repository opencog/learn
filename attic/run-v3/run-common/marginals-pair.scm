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
; the smaller dataset.
;
(load "cogserver.scm")

; Init the statistics objects.
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))

; Compute the statistics
(batch-pairs asa)

(barrier storage-node)
; (cog-close storage-node)
