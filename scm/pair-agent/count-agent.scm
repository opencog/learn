
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))

(define text-blob "this is just a bunch of words you know")
(define NUML (Number 24))
(define DICT (LgDict "any"))

; Same as always

(define (obs-txt PLAIN-TEXT)
	(define base-as (cog-push-atomspace))
	(define parses (cog-execute!
		(LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))
	(define temp-as (cog-set-atomspace! base-as))

	(cog-set-atomspace! temp-as)
	(cog-pop-atomspace)

	parses
)

; (obs-txt text-blob)
