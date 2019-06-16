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

	; Place the word-instance list into sequential order.
	(define (sort-word-inst-list LST)
		(sort LST
			(lambda (wa wb)
				(define (get-num wi)
					(string->number (cog-name (get-index-of-winst wi))))
				(< (get-num wa) (get-num wb)))))

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

	; Sort into sequential order. Pain-in-the-neck. Hardly worth it.
	(define en-sorted (sort-word-inst-list en-word-inst-list))
	(define other-sorted (sort-word-inst-list other-word-inst-list))

	(define ewlilen (length en-sorted))
	(define owlilen (length other-sorted))

	; Both should have the same length, assuming the clever
	; English tokenizer hasn't tripped.
	(if (not (eq? ewlilen owlilen))
		(format #t "Length miscompare: ~A vs ~A\n" ewlilen owlilen))

	; The words should compare. We are not currently comparing the
	; sequence; doing so would be complicated, and I don't see how
	; the sequences could ever mis-compare...
	(for-each
		(lambda (ewrd owrd)
			(if (not (equal? (get-word-of-winst ewrd) (get-word-of-winst owrd)))
				(format #t "Word miscompare at ~A: ~A vs ~A\n"
					(get-index-of-winst ewrd)
					(get-word-of-winst ewrd) (get-word-of-winst owrd))))
		en-sorted other-sorted)
)
