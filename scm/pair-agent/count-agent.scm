
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))

(define text-blob "this is just a bunch of words you know")
(define NUML (Number 4))
(define DICT (LgDict "any"))

; Same as always

(define (obs-txt PLAIN-TEXT)
	(define parses (cog-execute! (PureExecLink
		(LgParseBonds (Phrase PLAIN-TEXT) DICT NUML))))

	parses
)

(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp) (opencog nlp lg-parse))


; Parses arrive as link values. We want to apply a function to each.
; How?
; Need:
; 1) execute in subspace DONE PureExecLink
; 2) decimate w/ accept (for unbounded number of edges) NO
; 2a) select
; 3) select on Index IndexLink SelectLink
; 4) IncrementLinks link cog-inc-value!


(define (dobs-txt PLAIN-TEXT)
	(define parses (cog-execute!
		(Decimate
			(Number 1)
			(Decimate
				(Number 0 1)
				(LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))))
	parses
)

(define (obs-txt PLAIN-TEXT)
	(define parses (cog-execute!
		(Decimate
			(Number 0 1 1 1 1)
			; (PureExec
				(LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))))
	(define temp-as (cog-set-atomspace! base-as))




; (obs-txt text-blob)
