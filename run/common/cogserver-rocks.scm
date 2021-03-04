#! /usr/bin/env -S guile
!#
;
; cogserver-rocks.scm
;
; Set up everything needed for the language-learning pipeline
; Starts the CogServer, opens the database.
;
; Configurable paramters are pulled from the shell environment.
;
(use-modules (system repl common))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist) (opencog persist-rocks))
(use-modules (opencog matrix))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog cogserver))

(repl-default-option-set! 'prompt (getenv "PROMPT"))

; Start the cogserver using the indicated config file.
(start-cogserver (getenv "COGSERVER_CONF"))

; Open the database.
(cog-rocks-open (getenv "ROCKS_DB_URL"))
