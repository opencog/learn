;
; cogserver-mst.scm
;
; Run everything needed to get ready to start the language-learning
; disjunct-counting pipeline. Starts the CogServer, opens the database,
; loads the word-pairs in the database (which can take an hour or more!).
;
; Before disjunct counting can be started, the pair-marginals must be
; computed, using `marginals-pair.scm`. Use the shell script
; `3-mst-parsing/compute-mst-marginals.sh` to do this.
;
; After disjunct counting has finished, but before grammatical
; clustering is started, the marginals must be computed, using either
; `marginals-mst.scm` or `marginals-mst-shape.scm`.
;
(load "cogserver.scm")

; Load up the words
(display "Fetch all words from database. This may take several minutes.\n")
(load-atoms-of-type 'WordNode)

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all word-pairs. This may take well over half-an-hour!\n")

; The object which will be providing pair-counts for us.
; We can also do MST parsing with other kinds of pair-count objects,
; for example, the clique-pairs, or the distance-pairs.
(define pair-obj (make-any-link-api))
(define star-obj (add-pair-stars pair-obj))
(pair-obj 'fetch-pairs)

; Check to see if the marginals have been computed.
; Common error is to forget to do them manually.
; So we check, and compute if necessary.
(catch #t
	(lambda () ((add-report-api star-obj) 'num-pairs))
	(lambda (key . args)
		(format #t "Word pair marginals missing; computing them now.\n")
		(batch-pairs star-obj)
		#f))

; Print the sql stats
; (sql-stats)
; (monitor-storage storage-node)

; Clear the sql cache and the stats counters
; (sql-clear-cache)
; (sql-clear-stats)

(print-matrix-summary-report star-obj)

; (cog-close storage-node)
