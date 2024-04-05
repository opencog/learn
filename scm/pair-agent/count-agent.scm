
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))

(define text-blob "this is just a bunch of words you know")
(define NUML (Number 4))
(define DICT (LgDict "any"))


; From inside to out:
; LGParseBonds tokenizes a sentence, and then parses it.
; The PureExecLink makes sure that the parsing is done in a sub-AtomSpace
; so that the main AtomSpace is not garbaged up.
;
; The result of parsing is a list of pairs. First item in a pair is
; the list of words in the sentence; the second is a list of the edges.
; Thus, each pair has the form
;     (LinkValue
;         (LinkValue (Word "this") (Word "is") (Word "a") (Word "test"))
;         (LinkValue (Edge ...) (Edge ...) ...))
;
; The Filter matches this, so that (Variable "$x") is equated with the
; list of Edges.
;
(define (obs-txt PLAIN-TEXT)

	(define parser
		(PureExecLink (LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))
	(define filt
		(Filter
			(Lambda
				(Variable "$x")
				(LinkSignature
					(Type 'LinkValue)
					(Type 'LinkValue)
					(Variable "$x")))
			parser))

	(define parses (cog-execute! filt))
	parses
)

; Parses arrive as LinkValues. We want to apply a function to each.
; How? Need:
; 3) ConcatenateLink like AccumulateLink, but flattens
;    Is this needed?
; 4) IncrementLink just like cog-inc-value!
; 5) Support RuleLink so that sttream forking can be done with
;    multiple targets in the rule.

; Works but non-atomic.
; Also requires RuleLink to work
(define ed (Edge (Bond "ANY") (List (Word "words") (Word "know"))))
(define (doinc)
	(cog-execute!
		(SetValue ed (Predicate "count")
			(Plus (Number 0 0 1)
				(Cond (FloatValueOf ed (Predicate "count"))
					(FloatValueOf ed (Predicate "count"))
					(FloatValueOf (Number 0 0 0)))))))


; (load "count-agent.scm")
; (obs-txt text-blob)
