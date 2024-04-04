
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
	(define parses (cog-execute!
		(Filter
			(Lambda
				(Variable "$x")
				(LinkSignature
					(Type 'LinkValue)
					(Type 'LinkValue)
					(Variable "$x")))
			(PureExecLink (LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))))
	parses
)

; Parses arrive as LinkValues. We want to apply a function to each.
; How? Need:
; 3) ConcatenateLink like AccumulateLink, but flattens
;    Is this needed?
; 4) IncrementLinks link cog-inc-value!
; 5) ForkStream to create two streams (so that each can get a
;    distinct filter.)
; 5a) Alternately, support RuleLink so that multiple targets are allowed.
;     Then, ForkStream is a special case of Rule.


; (load "count-agent.scm")
; (obs-txt text-blob)
