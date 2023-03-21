#! /usr/bin/env -S guile -l ./cogserver-gram.scm --
!#
;
; cogserver-gram.scm
;
; Run everything needed to get ready to start the language-learning
; grammatical class clustering pipeline. Starts the CogServer, opens
; the database, loads the disjuncts in the database (which can take
; minutes, up to an hour ... if it's taking more, your datasets are
; too big.).
;
(include "cogserver.scm")

; Load up the disjuncts -- this can take over half an hour!
(display "Fetch all disjuncts. This may take 5-10 minutes.\n")
(display "If this is taking longer, your datasets might be too big!\n")
(display "In this case, review the README about trimming.\n")

; The object which will be providing disjunct-counts for us.
(define cset-obj (make-pseudo-cset-api))
(define covr-obj (add-covering-sections cset-obj))
(covr-obj 'fetch-pairs)
(covr-obj 'explode-sections)
(define star-obj covr-obj)

; Get the logger data, too
(fetch-atom (AnchorNode "data logger"))

; Check to see if the marginals have been computed.
; Common error is to forget to do them manually.
; So we check, and compute if necessary.
(catch #t
	(lambda () ((add-report-api star-obj) 'num-pairs))
	(lambda (key . args)
		; User needs to run `compute-mst-marginals.sh`
		(format #t "Disjunct marginals missing; go back and compute them!\n")
		#f))

(print-matrix-summary-report star-obj)

; (cog-close storage-node)
