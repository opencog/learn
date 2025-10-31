#! /usr/bin/env -S guile
!#
;
; cogserver.scm
;
; Perform language-learning pipeline module loading and CogServer setup.
; Loads all needed modules, starts the CogServer, opens the database.
;
; Configurable parameters are pulled from the shell environment.
;
(use-modules (system repl common))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(use-modules (opencog nlp) (opencog learn))
(use-modules (opencog cogserver))
(use-modules (srfi srfi-1))

(define env-prompt (getenv "PROMPT"))

; Avoid mystery crash on cold startup.
(when (not env-prompt)
	(format #t "Error: Learning pipeline not configured!\n")
	(format #t "Did you forget to source a config file?\n")
	(format #t "Config files are in the `run-config` directory.\n")
	(exit -1))

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
(start-cogserver (getenv "COGSERVER_CONF") #:web 0)

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

; If there are multiple frames, then fetch all of them.
; Set the cogserver atomspace to the top frame.
(define frame-tops (load-frames))
(if (< 1 (length frame-tops))
	(throw 'bad-frameset 'too-many-tops
		(format #f "Found more than one frame top: ~A\n" frame-tops)))
(when (< 0 (length frame-tops))
	(cog-set-atomspace! (car frame-tops))
	(set-cogserver-atomspace! (cog-atomspace)))

; -----------------------------------------------------------
; Enable automated server shutdown. This waits until the server
; is idle (taken to be a sign that there's no more text to be
; processed) and then exits guile.
(define (exit-server)
	(block-until-idle 0.01)
	(cog-close storage-node)
	(block-until-idle 0.01)
	(exit 0))

; -----------------------------------------------------------
