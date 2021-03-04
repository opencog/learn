#! /usr/bin/env -S guile
!#
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

(if (not (equal? 4 (length (program-arguments))))
	(begin
		(format #t "Usage: ~A <prompt> <cog-conf> <rocks-url>\n"
			(car (program-arguments)))
		(exit -1)))

(repl-default-option-set! 'prompt (cadr (program-arguments)))

; Start the cogserver using the indicated config file.
(start-cogserver (caddr (program-arguments)))

; Open the database.
(cog-rocks-open (cadddr (program-arguments)))
