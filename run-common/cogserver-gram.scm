;
; cogserver-gram.scm
;
; Run everything needed to get ready to start the language-learning
; grammatical class clustering pipeline. Starts the CogServer, opens
; the database, loads the disjuncts in the database (which can take
; an hour or more!).
;
(load "cogserver.scm")

; Load up the disjuncts -- this can take over half an hour!
; XXX Do we need to actually do this? I think not!?
(display "Fetch all disjuncts. This may take well over half-an-hour!\n")

; The object which will be providing disjunct-counts for us.
(define cset-obj (make-pseudo-cset-api))

#! -------------
; This is the non-cross-section variant. It's commented out.
; This is a temporary historical reference for what to do.
(when (not do-cross-sections)
	(define gram-obj (add-cluster-gram cset-obj))
	(define star-obj (add-pair-stars gram-obj))
	(cset-obj 'fetch-pairs)
)
-------- !#

; Add cross-sections to all sections.  This improves the accuracy
; of the clustering, although it does run 3x or 4x slower, and uses
; 3x or 5x more RAM.  This should be configurable for easy
; experimentation.
(define covr-obj (add-covering-sections cset-obj))
(define gram-obj (add-cluster-gram covr-obj))
(covr-obj 'fetch-pairs)
(covr-obj 'explode-sections)
(define star-obj gram-obj)

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
