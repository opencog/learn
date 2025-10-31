#! /usr/bin/env -S guile
!#
;
; trim-pair.scm
;
; After word-pair counting has been completed, this script is run to
; to remove word-pairs with low observation counts. This will also
; compute the marginals, so can be run instead of `marginals-pair.scm`
;
(load "cogserver.scm")

; Init the statistics objects.
(define ala (make-any-link-api))
(define aca (add-count-api ala))
(define asa (add-pair-stars aca))
(define trm (add-trimmer asa))

; Compute the statistics; these are needed for trimming.
(batch-pairs asa)
(barrier storage-node)

; Trim the dataset.
; XXX This includes some ad-hoc constants. See the Sept 2021 diary entry
; for why these are chosen, instead of something else. Basically,
; trimming this much raises the average MI of the dataset to its
; highest value. It also cuts the number of word pairs about half.
(trm 'subtotal-trim asa 10 10 4)

; Re-init the stats.
(set! ala (make-any-link-api))
(set! aca (add-count-api ala))
(set! asa (add-pair-stars aca))
(batch-all-pair-mi asa)
(barrier storage-node)

; (cog-close storage-node)
