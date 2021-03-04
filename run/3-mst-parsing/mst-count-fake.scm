;
; mst-count-fake.scm
;
; Run everyting needed for the language-learning disjunct-counting
; pipeline. Starts the CogServer, opens the database, loads the
; database (which can take an hour or more!).
;
(use-modules (system repl common))
(use-modules (system repl server))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist) (opencog persist-rocks))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog matrix))
(use-modules (opencog cogserver))

(start-cogserver "config/opencog-mst-fake.conf")

; Open the database.
; Edit the below, setting the database name
(cog-rocks-open "rocks:///home/ubuntu/data/expt-3/fake_disjuncts.rdb")

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

; Print the sql stats
; (sql-stats)
; (cog-rocks-stats)

; Clear the sql cache and the stats counters
; (sql-clear-cache)
; (sql-clear-stats)
(print-matrix-summary-report star-obj)
