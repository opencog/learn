#! /usr/bin/env -S guile
!#
;
; marginals-mst.scm
;
; After disjunct counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before disjunct clustering can be
; done.
;
(load "cogserver.scm")

; Init the statistics objects.
(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))
(define btr (batch-transpose psa))

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all disjuncts. This may take well over half-an-hour!\n")
(psa 'fetch-pairs)

; Compute the lp-norms
; Not needed, btr will do this for us...
; ((add-support-compute psa) 'cache-all)

; Cache the total matrix dimensions
; ((make-central-compute psa) 'cache-total)

; Compute the matrix-transpose marginals
(btr 'mmt-marginals)

(print-matrix-summary-report psa)

(barrier storage-node)
; (cog-close storage-node)
