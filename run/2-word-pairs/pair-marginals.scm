#! /usr/bin/env -S guile
!#
;
; pair-marginals.scm
;
; After word-pair counting has been completed, this script is run to
; compute marginal statistics. This computes assorted totals and
; conditional probabilities needed before the next step.
;
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist) (opencog persist-rocks))
(use-modules (opencog matrix))
(use-modules (opencog nlp) (opencog nlp learn))

(if (not (equal? 2 (length (program-arguments))))
	(begin
		(format #t "Usage: ~A <rocks-url>\n"
			(car (program-arguments)))
		(exit -1)))

; Open the database.
(cog-rocks-open (cadddr (program-arguments)))

; Init the statistics objects.
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))

; Compute the statistics
(batch-pairs asa)

; (print-matrix-summary-report asa)

(cog-rocks-close)
