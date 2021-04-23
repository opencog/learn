#! /usr/bin/env -S guile
!#
;
; marginals-mst-shape.scm
;
; After disjunct counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before disjunct clustering can be
; done.
;
; This is a variant of the `marginals-mst.scm` script that uses the
; "more accurate" (but larger and slower) shape vectors.
;
(load "cogserver.scm")

; Init the statistics objects.
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all sections. This may take a long while!\n")
(csc 'fetch-pairs)
(display "Create cross-sections. This may take a long while!\n")
(csc 'explode-sections)

; Compute the matrix-transpose marginals. As a side-effect, this will
; also compute support marginals and the central sums.
(define btr (batch-transpose csc))
(btr 'mmt-marginals)

(print-matrix-summary-report csc)

(barrier storage-node)
; (cog-close storage-node)
