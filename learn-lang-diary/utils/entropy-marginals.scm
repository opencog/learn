;
; Ad hoc scripts for exploring marginal entropy distributions for
; word-disjunct pairs.  These are used in Diary part Five.
;
; Starting point: `r9-sim.rdb` which is loaded using
; `guile -l cogserver-gram.scm` followed by `(batch-all-pair-mi star-obj)`
; The former does this:
;
; (define cset-obj (make-pseudo-cset-api))
(define covr-obj (add-covering-sections cset-obj))
(covr-obj 'fetch-pairs)
(covr-obj 'explode-sections)
(define star-obj covr-obj)

; From this, the `r9-sim+mi.rdb` was genrated by
(batch-all-pair-mi star-obj)


