;
; lg-compare.scm -- Compare parses from two different LG dictionaries.
;
; -----------------------------------------------------------------
; Overview:
; ---------
; The general goal of the code here is to obtain the accuracy, recall
; and other statistics of a test-dictionary, compared to a gold-standard
; dictionary. This is done by creating a comparison function that can
; parse sentences with both dictionaries, and then verifies the presence
; or absence of links between words.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec) (opencog nlp))
(use-modules (opencog nlp lg-dict) (opencog nlp lg-parse))



(define-public (make-lg-comparator en-dict other-dict
;;;;       #:key (INCLUDE-MISSING #f)
)
"
  lg-compare GOLD-DICT OTHER-DICT - Return a sentence comparison function.

  GOLD-DICT and OTHER-DICT should be the two LgDictNodes to compare.
  The code makes weak assumptions that GOLD-DICT is the reference or
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

  By default, this does not test sentences that have words that are
  not in the test dictionary.  Over-ride this by supplying the optional
  argument INCLUDE-MISSING.
"
	(define INCLUDE-MISSING #f)
	(define verbose #f)

	; -------------------
	; Stats we are keeping
	(define total-sentences 0)
	(define total-compares 0)
	(define incomplete-dict 0)
	(define temp-cnt 0)
	(define bad-sentences 0)
	(define total-words 0)
	(define total-links 0)
	(define length-miscompares 0)
	(define word-miscompares 0)
	(define link-count-miscompares 0)
	(define missing-links 0)
	(define extra-links 0)
	(define missing-link-types '())
	(define missing-words (make-atom-set))
	(define vocab-words (make-atom-set))

	; ----------------------------------------
	; Misc utilities

	; Get the word of the word instance, i.e. the WordNode.
	(define (get-word-of-winst WRD)
		(gdr (car (cog-incoming-by-type WRD 'ReferenceLink))))

	; Get the index of the word instance, i.e. the NumberNode.
	(define (get-index-of-winst WRD)
		(gdr (car (cog-incoming-by-type WRD 'WordSequenceLink))))

	; ------
	; Place the word-instance list into sequential order.
	; i.e. left-to-right order, as the word appear in a sentence.
	(define (sort-word-inst-list LST)
		(sort LST
			(lambda (wa wb)
				(define (get-num wi)
					(string->number (cog-name (get-index-of-winst wi))))
				(< (get-num wa) (get-num wb)))))

	; ------
	; Return a sorted list of the other word-instances that this
	; word links to. To avoid double-counting, this returns only the
	; links connecting to the right.
	(define (get-linked-winst WIN)
		(sort-word-inst-list
			(map gdr
				; Accept only those ListLinks that are bonfide word-links
				(filter
					(lambda (lili)
						(and
							; Is the first word (left word) ours?
							(equal? (gar lili) WIN)
							; Are any of the possible EvaluationLink's
							; an LG relation? If so, we are happy.
							(any
								(lambda (evlnk)
									(equal? 'LinkGrammarRelationshipNode
										(cog-type (gar evlnk))))
								(cog-incoming-by-type lili 'EvaluationLink))
						))
					(cog-incoming-by-type WIN 'ListLink)))))

	; ------
	; Return the name of a link connecting lwin (left word
	; instance) and rwin (right word instance). There must actually
	; be a link connecting them, else bad things happen.
	(define (get-link-name lwin rwin)
		(gar (car (filter
			(lambda (evl)
				(equal? (cog-type (gar evl)) 'LinkGrammarRelationshipNode))
			(cog-incoming-by-type (ListLink lwin rwin)
				'EvaluationLink)))))

	; Return the string-name of a link. Truncate link subtypes.
	(define (get-link-str-name lwin rwin)
		(string-trim-right
			(cog-name (get-link-name lwin rwin))
			(char-set-adjoin char-set:lower-case #\*)))

	; ------
	; Increment count for a missing link type
	(define (incr-link-str-count link-name)
		(define cnt (assoc-ref missing-link-types link-name))
		(if (not cnt) (set! cnt 0))
		(set! missing-link-types
			(assoc-set! missing-link-types link-name (+ 1 cnt))))

	(define (incr-link-count lwin rwin)
		(incr-link-str-count (get-link-str-name lwin rwin)))

	; ----------------------------------------
	; Comparison functions

	; Return the number of words in the sentence that are not in
	; the test dictionary. This also adds them to the mising-words
	; set.
	(define (num-missing-words winli dict)
		(fold
			(lambda (win cnt)
				(define wrd (get-word-of-winst win))
				(if (< 0.5 (cog-tv-mean
						(cog-evaluate! (LgHaveDictEntry wrd dict))))
					cnt
					(begin
						(missing-words wrd)
						(+ cnt 1))))
			0 winli))

	; Return #t if the sentnce contains missing words.
	(define (has-missing-words winli)
		(if (< 0 (num-missing-words winli other-dict))
			(begin
				(set! incomplete-dict (+ 1 incomplete-dict))
				#t)
			#f))

	; ---
	; The length of the sentences should match.
	(define (compare-lengths en-sorted other-sorted)
		(define ewlilen (length en-sorted))
		(define owlilen (length other-sorted))

		(set! total-words (+ total-words ewlilen))

		; Both should have the same length, assuming the clever
		; English tokenizer hasn't tripped.
		(if (not (equal? ewlilen owlilen))
			(begin
				(format #t "Length miscompare: ~A vs ~A\n" ewlilen owlilen)
				(set! length-miscompares (+ 1 length-miscompares)))))

	; ---
	; The words should compare. We are not currently comparing the
	; sequence; doing so would be complicated, and I don't see how
	; the sequences could ever mis-compare...
	(define (compare-words ewinst owinst)
		(define ewrd (get-word-of-winst ewinst))
		(define owrd (get-word-of-winst owinst))
		(vocab-words ewrd)
		(if (not (equal? ewrd owrd))
			(begin
				(if verbose
					(format #t "Word miscompare at ~A: ~A vs ~A\n"
						(get-index-of-winst ewinst) ewrd owrd))
				(set! word-miscompares (+ 1 word-miscompares)))))

	; ---
	; Compare links. For the given words, find the words that link to
	; the right. Verify that there are the same number of them, and
	; that they have the same targets.
	; ewin should be a word-instance from the EN side
	; owin should be the OTHER word instance.
	(define (compare-links ewin owin)
		(define ewrd (get-word-of-winst ewin))
		(define elinked (get-linked-winst ewin))
		(define olinked (get-linked-winst owin))
		(define elinked-len (length elinked))
		(define olinked-len (length olinked))

		; Obtain sets of the links words (not the word-instances)
		(define ewords (map get-word-of-winst elinked))
		(define owords (map get-word-of-winst olinked))

		; A set of words in ewords that are not in owords
		(define miss-w (lset-difference equal? ewords owords))

		; Keep only word-instances that are in the word-set.
		(define (trim-wili wili wrd-set)
			(filter
				(lambda (wi)
					(any
						(lambda (wrd) (equal? (get-word-of-winst wi) wrd))
						wrd-set))
				wili))

		; Missing linked word-instances...
		(define missing-wi (trim-wili elinked miss-w))

		; Keep statistics
		(set! total-links (+ total-links elinked-len))
		(if (< elinked-len olinked-len)
			(set! extra-links (+ extra-links (- olinked-len elinked-len)))
			(set! missing-links (+ missing-links (- elinked-len olinked-len))))

		; Compare number of links
		(if (not (equal? elinked-len olinked-len))
			(begin
				(if verbose
					(format #t "Miscompare number of right-links: ~A vs ~A for ~A"
						elinked-len olinked-len ewrd))
				(set! link-count-miscompares (+ 1 link-count-miscompares))))

		; Make a note of missing link types.
		(for-each
			(lambda (misw) (incr-link-count ewin misw))
			missing-wi)
	)

	; -------------------
	; The main comparison function
	(define (do-compare SENT)
		; Get a parse, one for each dictionary.
		(define en-sent (cog-execute!
			(LgParseMinimal (PhraseNode SENT) en-dict (NumberNode 1))))
		(define other-sent (cog-execute!
			(LgParseMinimal (PhraseNode SENT) other-dict (NumberNode 1))))

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

		(define dict-has-missing-words (has-missing-words other-sorted))

		(set! total-sentences (+ total-sentences 1))

		(if dict-has-missing-words
			(format #t "Dictionary is missing words in: \"~A\"\n" SENT))

		; Don't do anything more, if the dict is missing words in the
		; sentence.
		(if (or (not dict-has-missing-words) INCLUDE-MISSING)
			(begin
				(set! total-compares (+ total-compares 1))
				(set! temp-cnt link-count-miscompares)

				; Compare sentence lengths
				(compare-lengths en-sorted other-sorted)

				; Compare words and links
				(for-each
					(lambda (ewrd owrd)
						(compare-words ewrd owrd)
						(compare-links ewrd owrd)
					)
					en-sorted other-sorted)

				(if (not (equal? temp-cnt link-count-miscompares))
					(set! bad-sentences (+ 1 bad-sentences)))
				(format #t "Finish compare of sentence ~A/~A: \"~A\"\n"
					total-compares total-sentences SENT)
			))
	)

	; -------------------
	; The main comparison function
	(define (do-compare-gc SENT)
		(define (kill typ)
			(for-each cog-extract-recursive (cog-get-atoms typ)))
		(do-compare SENT)

		; Cleanup most stuff, but not WordNodes, because
		; they have to be saved in the "missing words" list.
		; Sadly, cannot use push-pop atomspace as a result.
		(kill 'NumberNode)
		(kill 'WordInstanceNode)
		(kill 'SentenceNode)
		(kill 'PhraseNode)
		(kill 'ParseNode)
		(kill 'LinkGrammarRelationshipNode)
		(kill 'LgHaveDictEntry)
	)

	; -------------------
	(define (report-stats)
		; Compute link precision and recall.
		(define link-expected-positives (exact->inexact total-links))
		(define link-true-positives (- link-expected-positives missing-links))
		(define link-false-positives extra-links)
		(define link-recall (/ link-true-positives link-expected-positives))
		(define link-precision (/ link-true-positives
			(+ link-true-positives link-false-positives)))
		(define link-f1 (/ (* 2.0 link-recall link-precision)
			(+ link-recall link-precision)))

		; Put missing link counts into sorted order.
		(define sorted-missing-links
			(sort missing-link-types
				(lambda (ia ib) (> (cdr ia) (cdr ib)))))

		(format #t
			"Examined ~A sentences; ~A had words not in dictionary (~6F %).\n"
			total-sentences incomplete-dict
			(/ (* 100.0 incomplete-dict) total-sentences))
		(format #t
			"Finished comparing ~A parses; ~A parsed differently (~6F %).\n"
			total-compares bad-sentences
			(/ (* 100.0 bad-sentences) total-compares))
		(format #t
			"Found ~A word instances, vocab= ~A words; expect to find ~A links\n"
			total-words (length (vocab-words #f)) total-links)
		(format #t "Dictionary was missing ~A words\n"
			(length (missing-words #f)))
		(format #t "Found ~A length-miscompares\n" length-miscompares)
		(format #t "Found ~A word-miscompares\n" word-miscompares)
		(format #t
			"Found ~A words w/linkage diffs; ~A missing and ~A extra links\n"
			link-count-miscompares
			missing-links extra-links)

		(format #t "Link precision=~6F recall=~6F F1=~6F\n"
			link-precision link-recall link-f1)
		(newline)
		(format #t "Missing link-type counts: ~A\n\n" sorted-missing-links)
		(format #t "Missing words: ~A\n\n"
			(map cog-name (missing-words #f)))
	)

	; -------------------
	; The main comparison function
	(lambda (SENT)
		(if (not SENT)
			(report-stats)
			(do-compare-gc SENT)))
)
