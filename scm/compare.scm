;
; compare.scm -- Compare parses from two different LG dictionaries.
;
; -----------------------------------------------------------------
; Overview:
; ---------
; Goal is to obtain accuracy, recall vs. LG.
;
(use-modules (opencog) (opencog exec) (opencog nlp))
(use-modules (opencog nlp lg-dict) (opencog nlp lg-parse))


(LgDictNode "en")



(define (lg-compare en-dict other-dict SENT)
"
  lg-compare
  SENT should be a string sentence

"
	; Get a parse, one for each dictionary.
	(define en-sent (cog-execute!
		(LgParseLink (PhraseNode SENT) en-dict (NumberNode 1))))
	(define other-sent (cog-execute!
		(LgParseLink (PhraseNode SENT) other-dict (NumberNode 1))))

	; Since only one parse, we expect only one...
	(define en-parse (gar (car
		(cog-incoming-by-type en-sent 'ParseLink))))
	(define other-parse (gar (car
		(cog-incoming-by-type other-sent 'ParseLink))))

	; Get a list of the words in each parse.
	(define en-word-inst-list
		(cog-incoming-by-type en-parse 'WordInstanceLink))

	(define other-word-inst-list
		(cog-incoming-by-type other-parse 'WordInstanceLink))

	(define ewlilen (length en-word-inst-list))
	(define owlilen (length other-word-inst-list))

	; Both should have the same length, assuming the clever
	; English tokenizer hasn't tripped.
	(if (not (eq? ewlilen owlilen))
		(format #t "Length miscompare: ~A vs ~A" ewlilen owlilen))

	; The sequence of words should compare.
)
