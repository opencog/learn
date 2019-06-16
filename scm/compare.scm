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
  EN-DICT and OTHER-DICT should be LgDictNodes.

"
	; -------------------
	; Misc utilities

	; Get the word of the word instance, i.e. the WordNode.
	(define (get-word-of-winst WRD)
		(gdr (car (cog-incoming-by-type WRD 'ReferenceLink))))

	; Get the index of the word instance, i.e. the NumberNode.
	(define (get-index-of-winst WRD)
		(gdr (car (cog-incoming-by-type WRD 'WordSequenceLink))))

	; -------------------
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
	(define other-word-inst-list
		(map gar (cog-incoming-by-type other-parse 'WordInstanceLink)))

	(define left-wall (WordNode "###LEFT-WALL###"))
	(define right-wall (WordNode "###RIGHT-WALL###"))
	; Get the list of words in the standard dict.
	; XXX Temp hack. Currently, the test dicts are missing LEFT-WALL
	; and RIGHT-WALL and so we filter these out manually. This
	; should be made more elegant.
	(define en-word-inst-list
		(filter
			(lambda (winst)
				(define wrd (get-word-of-winst winst))
				(and
					(not (equal? wrd left-wall))
					(not (equal? wrd right-wall))))
			(map gar (cog-incoming-by-type en-parse 'WordInstanceLink))))

	(define ewlilen (length en-word-inst-list))
	(define owlilen (length other-word-inst-list))

	; Both should have the same length, assuming the clever
	; English tokenizer hasn't tripped.
	(if (not (eq? ewlilen owlilen))
		(format #t "Length miscompare: ~A vs ~A\n" ewlilen owlilen))

	; The sequence of words should compare.
	(for-each
		(lambda (ewrd owrd)
			(if (not (equal? (get-word-of-winst ewrd) (get-word-of-winst owrd)))
				(format #t "Word miscompare at ~A: ~A vs ~A\n"
					(get-index-of-winst ewrd)
					(get-word-of-winst ewrd) (get-word-of-winst owrd))))
		en-word-inst-list other-word-inst-list)
)
