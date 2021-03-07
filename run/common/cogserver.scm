#! /usr/bin/env -S guile
!#
;
; cogserver.scm
;
; Set up everything needed for the language-learning pipeline
; Starts the CogServer, opens the database.
;
; Configurable paramters are pulled from the shell environment.
;
(use-modules (system repl common))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog cogserver))

(repl-default-option-set! 'prompt (getenv "PROMPT"))

; Start the cogserver using the indicated config file.
(start-cogserver (getenv "COGSERVER_CONF"))

; Open the database.
(define sns (getenv "STORAGE_NODE"))
(cond
	((string-prefix? "(Rocks" sns) (use-modules (opencog persist-rocks)))
	((string-prefix? "(Postgres" sns) (use-modules (opencog persist-sql)))
	((string-prefix? "(Cog" sns) (use-modules (opencog persist-cog)))
	(else (throw 'bad-storage-node 'unknown
		(format #f "Unknown storage node type ~A\n" sns))))

(define storage-node (eval-string sns))
(cog-open storage-node)
