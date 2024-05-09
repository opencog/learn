#! /usr/bin/env -S guile -l ./cogserver.scm --
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

(define startup-gate (make-gate))

(define env-prompt (getenv "PROMPT"))

; Avoid mystery crash on cold startup.
(when (not env-prompt)
	(format #t "Error: Learning pipeline not configured!\n")
	(format #t "Did you forget to source a config file?\n")
	(format #t "Config files are in the `run-config` directory.\n")
	(exit -1))

; Prompt magic, copied from `module/system/repl/common.scm`
; This makes the scheme shell prompt work correctly, even when
; exceptions are caught.
(define (cog-prompt)
	(let ((level (length (cond
				((fluid-ref *repl-stack*) => cdr)
				(else '())))))
		(if (zero? level)
			(string-append env-prompt "> ")
			(format #f "~A [~A]> " env-prompt level))))

(repl-default-prompt-set! cog-prompt)

; Open the database.
(define sns (getenv "STORAGE_NODE"))
(cond
	((string-prefix? "(Rocks" sns) (use-modules (opencog persist-rocks)))
	((string-prefix? "(Mono" sns) (use-modules (opencog persist-mono)))
	((string-prefix? "(Postgres" sns) (use-modules (opencog persist-sql)))
	((string-prefix? "(Cog" sns) (use-modules (opencog persist-cog)))
	(else (throw 'bad-storage-node 'unknown
		(format #f "Unknown storage node type ~A\n" sns))))

; Do write buffering by default. We could also make this configurable
; from the run config scripts. But we don't. So there.
; Use a ten-minute (600-second) time decay. This allows the pool
; to get large, more than half of total edges being counted. That
; should ensure a very high deduplication rate (buffer hit rate).
; Steady-state seems to run at inflow that is 14x of outflow.
(define base-storage-node (eval-string sns))

(define write-buff (WriteBufferProxy "write buffer"))
(ProxyParameters write-buff base-storage-node (Number 600))

(define storage-node (ReadWriteProxy "buffered writer"))
(ProxyParameters storage-node (List base-storage-node write-buff))

(cog-open storage-node)

; If there are multiple frames, then fetch all of them.
; Set the cogserver atomspace to the top frame.
; Monospace does not have frames
(define frame-tops '())
(when (not (string-prefix? "(Mono" sns))
	(set! frame-tops (load-frames))
	(if (< 1 (length frame-tops))
		(throw 'bad-frameset 'too-many-tops
			(format #f "Found more than one frame top: ~A\n" frame-tops)))
	(when (< 0 (length frame-tops))
		(cog-set-atomspace! (car frame-tops))))

; Start the cogserver using the configured parameters.
; Start the cogserver *after* opening the DB and setting frames.
; That way, any remote procs waiting on the socket don't start
; sending data until *after* the DB is opened.
(start-cogserver
	#:port (string->number (getenv "PORT"))
	#:scmprompt (getenv "PROMPT")
	#:prompt (getenv "OCPROMPT")
	#:logfile (getenv "LOGFILE")
	#:web 0)

; XXX Is this needed? Didn't cogserver already get the top?
(when (< 0 (length frame-tops))
	(set-cogserver-atomspace! (cog-atomspace)))

; Release anyone who is waiting on us.
(open-gate startup-gate)

; -----------------------------------------------------------
; Enable automated server shutdown. This waits until the server
; is idle (taken to be a sign that there's no more text to be
; processed) and then exits guile.
(define (exit-server)
	(block-until-idle)
	(cog-close storage-node)
	(block-until-idle)
	(exit 0))

; -----------------------------------------------------------
