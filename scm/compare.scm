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



(define (make-lg-comparator en-dict other-dict)
"
  lg-compare EN-DICT OTHER-DICT - Return a sentence comparison function.

  EN-DICT and OTHER-DICT should be the two LgDictNodes to compare.
  The code makes weak assumptinos that EN-DICT is the reference or
  \"golden\" lexis to compare to, so that any differences found in
  OTHER-DICT are blamed on OTHER-DICT.

  This returns a comparison function.  To use it, pass one or more
  sentence strings to it; it will compare the resulting parses.
  When finished, pass it `#f`, and it will print a summary report.

  Example usage:
     (define compare (make-lg-comparator
        (LgDictNode \"en\") (LgDictNode \"micro-fuzz\")))
     (compare \"I saw her face\")
     (compare \"I swooned to the floor\")
     (compare #f)
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
	; i.e. left-to-right order, as the word appear in a sentence.
	(define (sort-word-inst-list LST)
		(sort LST
			(lambda (wa wb)
				(define (get-num wi)
					(string->number (cog-name (get-index-of-winst wi))))
				(< (get-num wa) (get-num wb)))))

	; Return a sorted list of the other word-instances that this
	; word links to. To avoid double-counting, this returns only the
	; links connecting to the right.
	(define (get-linked-winst WRD)
		(sort-word-inst-list
			; Accept only those ListLinks that are bonfide word-links
			(filter
				(lambda (lili)
					(define evli (cog-incoming-by-type lili 'EvaluationLink))
					(and (equal? (gar lili) WRD)
						(not (eq? '() evli))
						(equal? 'LgLinkInstanceNode (cog-type (gar (car evli))))))
				(cog-incoming-by-type WRD 'ListLink))))

	; -------------------
	; Comparison functions

	; The words should compare. We are not currently comparing the
	; sequence; doing so would be complicated, and I don't see how
	; the sequences could ever mis-compare...
	(define (compare-words ewrd owrd)
		(if (not (equal? (get-word-of-winst ewrd) (get-word-of-winst owrd)))
			(format #t "Word miscompare at ~A: ~A vs ~A\n"
				(get-index-of-winst ewrd)
				(get-word-of-winst ewrd) (get-word-of-winst owrd))))

	; Compare links. For the given words, find the words that link to
	; the right. Verify that there are the same number of them, and
	; that they have the same targets.
	(define (compare-links ewin owin)
		(define ewrd (get-word-of-winst  ewin))
		(define elinks (get-linked-winst ewin))
		(define olinks (get-linked-winst owin))
		(define elinks-len (length elinks))
		(define olinks-len (length olinks))
		(if (not (equal? elinks-len olinks-len))
			(format #t "Miscompare number of right-links: ~A vs ~A for ~A\n"
				elinks-len olinks-len ewin))
		(for-each
			(lambda (erwi orwi)
				(define erwrd (get-word-of-winst erwi))
				(define orwrd (get-word-of-winst orwi))
				(if (not (equal? erwrd orwrd))
					(format #t "Miscompare of link targets: ~A vs ~A for ~A\n"
						erwrd orwrd ewrd)))
			elinks olinks)
	)

	; -------------------
	; The main comparison function
	(define (do-compare SENT)
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
		(if (not (equal? ewlilen owlilen))
			(format #t "Length miscompare: ~A vs ~A\n" ewlilen owlilen))

		(for-each
			(lambda (ewrd owrd)
				(compare-words ewrd owrd)
				(compare-links ewrd owrd)
			)
			en-sorted other-sorted)

		(format #t "Finish compare of sentence\n")
	)

	; -------------------
	; The main comparison function
	(lambda (SENT)
		(if (not SENT)
			(format #t "Finished comparing!\n")
			(do-compare SENT)))
)
