;
; cogserver-mst.scm
;
; Run everything needed for the language-learning disjunct-counting
; pipeline. Starts the CogServer, opens the database, loads the
; word-pairs in the database (which can take an hour or more!).
;
(load "cogserver.scm")

; Load up the words
(display "Fetch all words from database. This may take several minutes.\n")
(fetch-all-words)

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
; (cog-rocks-stats)

; Clear the sql cache and the stats counters
; (sql-clear-cache)
; (sql-clear-stats)

(print-matrix-summary-report star-obj)

; (cog-close storage-node)
