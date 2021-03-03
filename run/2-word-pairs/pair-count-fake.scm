;
; pair-count-fake.scm
;
; Set up everything needed for the language-learning word-pair
; counting pipeline. Starts the CogServer, opens the database.
;
; This has assorted hard-coded file-paths in it. Adjust these to taste.
;
(use-modules (system repl common))
(use-modules (system repl server))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist) (opencog persist-rocks))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog cogserver))

(repl-default-option-set! 'prompt "scheme@(fake-pairs)> ")

; Start the cogserver on port 17008
(start-cogserver "config/opencog-pairs-fake.conf")

; Open the database.
; Edit the below, setting the database name as desired
(cog-rocks-open "rocks:///home/ubuntu/data/expt-42/fake_pairs.rdb")
