#! /usr/bin/env -S guile
!#
;
; cogserver.scm
;
; Perform language-learning pipeline module loading and CogServer setup.
; Loads all needed modules, starts the CogServer, opens the database.
;
; Configurable paramters are pulled from the shell environment.
;
(use-modules (system repl common))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog cogserver))

(define env-prompt (getenv "PROMPT"))

; Prompt magic, copied from `module/system/repl/common.scm`
(define (cog-prompt)
	(let ((level (length (cond
				((fluid-ref *repl-stack*) => cdr)
				(else '())))))
		(if (zero? level)
			(string-append env-prompt "> ")
			(format #f "~A [~A]> " env-prompt level))))

(repl-default-prompt-set! cog-prompt)

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

; -----------------------------------------------------------
; Enable automated server shutdown. This waits until the server
; is idle (taken to be a sign that there's no more text to be
; processed) and then exits guile.
(define (exit-server)
	(block-until-idle 0.01)
	(cog-close storage-node)
	(block-until-idle 0.01)
	(exit 0))
