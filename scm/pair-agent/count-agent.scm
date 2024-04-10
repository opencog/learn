
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))

; Here's the design we want:
; 1) Some source of text strings; actually a source of PhraseNode's
;    This source blocks if there's nothing to be read; else it
;    returns the next PhraseNode. The SRFI's call this a generator.
; 2) A PromiseLink that wraps (PureExecLink (LgParseBonds ...))
;    so that, when its executed, it calls the source and parses.
;    Or perhaps just a (Filter (Rule ...)) combo, instead of a
;    promise? The promise can be made later.
;    https://wiki.opencog.org/w/PromiseLink#Multiplex_Example
; 3) A Filter that takes above and increments pair counts.
; 4) Execution control. There are two choices:
;    Pull: infinite loop polls on promise. Source blocks if
;          no input.
;    Push: Nothing happens until source triggers; then a cascade
;          of effects downstream.
;    Right now, the general infrastructure supports Pull naturally.
;    There aren't any push demos.
;    Attention controls is easier with Pull.


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
; XXX Major design flaw, still needs work: the PhraseLink is placed in
; the main AtomSpace. Thus, it accumulates there, plus also all of the
; other Links that wrap it, including LgParseBinds, Rule, Filter etc.
; Nuking the PhraseLink also nukes those, so lots of churn. Alternative
; is to pass PhraseLink as a value, but then have threading issues...
; ugh. let me think a moment.
;
(define (obs-txt PLAIN-TEXT)

	(define (incr-cnt edge)
		(SetValue edge (Predicate "count")
			(Plus (Number 0 0 1)
				(FloatValueOf edge (Predicate "count") (FloatValueOf (Number 0 0 0))))))

	(define (extract stuff)
		(Filter
			(Rule
				(Variable "$edge")
				; (TypedVariable (Variable "$edge") (Type 'Edge))
				(Variable "$edge")
				(incr-cnt (Variable "$edge")))
			stuff))

	(define (parseli phrali)
		(PureExecLink (LgParseBonds phrali DICT NUML)))

	(define phrali (Phrase PLAIN-TEXT))

	(define filty
		(Filter
			(Rule
				; Type decl
				(Glob "$x")
				; Match clause - one per parse.
				(LinkSignature
					(Type 'LinkValue) ; the wrapper for the pair
					(Type 'LinkValue) ; the word-list
					(LinkSignature    ; the edge-list
						(Type 'LinkValue)  ; edge-list wrapper
						(Glob "$x")))      ; all of the edges
				; Rewrite
				(extract (Glob "$x"))
			)
			(parseli (phrali))))

	(define parses (cog-execute! filty))

	; Remove the phrase-link, too.
	(cog-extract-recursive! phrali)
	parses
)

; Parses arrive as LinkValues. We want to apply a function to each.
; How? Need:
; 4) IncrementLink just like cog-inc-value!
;    or is it enough to do atomic lock? Make SetValue exec under lock.
;
; cog-update-value calls
; asp->increment_count(h, key, fvp->value()));

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
