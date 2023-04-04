#! /usr/bin/env -S guile -l ./marginals-mst-shape.scm --
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
(define csa (add-covering-sections pca))
(define csc (add-count-api csa))

; Load up the disjuncts -- this can take over half an hour!
(display "Fetch all sections. This may take a long while!\n")
(csc 'fetch-pairs)
(display "Create cross-sections. This may take a long while!\n")
(csc 'explode-sections)

; Compute the matrix-transpose marginals. As a side-effect, this will
; also compute support marginals and the central sums.
(define btr (batch-transpose csc))
(btr 'clobber)
(btr 'mmt-marginals)

(print-matrix-summary-report csc)

; XXX The current merge code tracks stats that requires the MI
; for word-disjunct pairs to be computed. These stats are not
; required, but are interesting to observe. To get these, the
; MI needs to be computed, as below:
; (batch-all-pair-mi csc)

(barrier storage-node)
; (cog-close storage-node)
