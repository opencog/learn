#! /usr/bin/env -S guile
!#
;
; marginals-pair.scm
;
; After word-pair counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before the next step.
;
(load "cogserver.scm")

; Load up the words
(display "Fetch all words from database. This may take several minutes.\n")
(fetch-all-words)

; Init the statistics objects.
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all word-pairs. This may take well over half-an-hour!\n")
(asa 'fetch-pairs)

; Compute the statistics
(batch-pairs asa)

; (print-matrix-summary-report asa)

(barrier storage-node)
; (cog-close storage-node)
