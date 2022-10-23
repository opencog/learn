;
; cogserver-lg.scm
;
; Run everything needed to get ready to start the link-grammar parsing
; pipeline. Unlike the earlier gram-class startup scripts, this does
; NOT create cross-sections, as these are not neede for LG parsing.
;
; Starts the CogServer, opens the database, loads the disjuncts in the
; database (which can take minutes, up to an hour ... if it's taking
; more, your datasets are too big.).
;
(include "cogserver.scm")

; Load up the disjuncts -- this can take over half an hour!
(display "Fetch all disjuncts. This may take 5-10 minutes.\n")
(display "If this is taking longer, your datasets might be too big!\n")
(display "In this case, review the README about trimming.\n")

; The object which will be providing disjunct-counts for us.
(define cset-obj (make-pseudo-cset-api))
(cset-obj 'fetch-pairs)
(define star-obj cset-obj)

; Get the logger data, too
(fetch-atom (AnchorNode "data logger"))

(print-matrix-summary-report star-obj)

; (cog-close storage-node)
