
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))

(define text-blob "this is just a bunch of words you know")
(define NUML (Number 4))
(define DICT (LgDict "any"))


(define (obs-txt PLAIN-TEXT)
	(define parses (cog-execute!
		(Filter
			(Signature
				(Type 'LinkValue))
			(PureExecLink (LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))))
	parses
)

;returns NUML pairs in list equal to SizeOf but SizeOf might be less
;first in pair is wordlist, second is edgelist.


; Parses arrive as link values. We want to apply a function to each.
; How?
; Need:
; 1) execute in subspace DONE PureExecLink
; 2) FilterLink with LinkValue signature
; 3) IncrementLinks link cog-inc-value!


; (obs-txt text-blob)
