;
; word-pair-count.scm
;
; Word-pair counting via random planar trees. This takes a uniformly
; distributed random sampling ouut of all possible planar parse trees.
; The uniform sampling of parse trees produces mildly different
; word-pair distributions as compared to sliding-window techniques.
; (The old sliding-window code has been retired. It is called "clique
; counting" and can be found in the `attic` directory.)
;
; This uses the Link Grammar (LG) parser "any" language to generate
; random trees. The "any" parser splits off basic punctuation, and is
; thus suitable for English and many IndoEuropean languages; possibly
; also some Asian languages. The primary downside here is that the
; "any" parser peforms only basic tokenization; There's no "first-
; principles" ML/AI tokenizer available yet.
;
; If you are not working with natural-language sentences, don't use
; this code! This makes various assumptions that are appropriate only
; for natural language.
;
; Copyright (c) 2013, 2017 Linas Vepstas <linasvepstas@gmail.com>
;
; Main entry point: `(observe-text plain-text)`
;
; Call this entry point with exactly one sentence as a plain text
; string. It will be parsed, and the resulting Link Grammar link usage
; counts will be updated in the atomspace. The counts are flushed to
; the database so that they're not forgotten.
;
; Several different kinds of counts are maintained, depending on the
; mode. Usually, not all of these are maintained at the same time, as
; this will result in excessively large atomspaces. Some of the counts
; that can be maintained are:
; *) how many sentences have been observed.
; *) how many parses were observed (when using parse-driven counting).
; *) how many words have been observed (counting once-per-word-per-parse)
; *) how many word-order pairs have been observed.
;
; Sentences are counted by updating the count on `(SentenceNode "ANY")`.
; Parses are counted by updating the count on `(ParseNode "ANY")`.
; Words are counted by updating the count on the `WordNode` for that
; word. It is counted with multiplicity: once for each time it occurs
; in a parse.  That is, if a word appears twice in a parse, it is counted
; twice.
;
; Word-pairs are obtained from Link Grammar parses of a sentence.
; A Link Grammar parse creates a list of typed links between
; pairs of words in the sentence. Each such link is counted once, for
; each time that it occurs.  These counts are maintained in the CountTV
; on the EvaluationLink for the LgLinkNode for that word-pair.
;
; Not implemented: a count is maintained of the length of that link.
;
(use-modules (opencog) (opencog nlp) (opencog persist))
(use-modules (opencog exec) (opencog nlp lg-parse))
(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------
; make-word-sequence -- extract the sequence of words in a parse.
;
; The parser proves a numbered sequence of word-instances, for example:
;
;    (WordSequenceLink
;         (WordInstanceNode "foo@9023e177")
;         (NumberNode "4567"))
;
; This returns the corresponding links, replacing the WordInstances
; with WordNodes. The NumberNode is replaced by a new NumberNode,
; starting at zero with the LEFT-WALL.  ; For example, this would
; return
;
;    (WordSequenceLink
;         (WordNode "foo")
;         (NumberNode "4"))
;
; when the sentence was "this is some foo".
;
(define (make-word-sequence PARSE)

	; Get the scheme-number of the word-sequence number
	(define (get-number word-inst)
		(cog-number (word-inst-get-number word-inst)))

	; A comparison function, for use as kons in fold
	(define (least word-inst lim)
		(define no (get-number word-inst))
		(if (< no lim) no lim))

	; Get the number of the first word in the sentence (the left-wall)
	(define wall-no (fold least 9e99 (parse-get-words PARSE)))

	; Convert a word-instance sequence number into a word sequence
	; number, starting with LEFT-WALL at zero.
	(define (make-ordered-word word-inst)
		(WordSequenceLink
			(word-inst-get-word word-inst)
			(NumberNode (- (get-number word-inst) wall-no))))

	; Ahhh .. later code will be easier, if we return the list in
	; sequential order. So, define a compare function and sort it.
	(define (get-no seq-lnk)
		(cog-number (gdr seq-lnk)))

	(sort (map make-ordered-word (parse-get-words PARSE))
		(lambda (wa wb)
			(< (get-no wa) (get-no wb))))
)

; ---------------------------------------------------------------------
; update-word-counts -- update counts for sentences, parses and words,
; for the given list of sentences.
;
; As explained above, the counts on `(SentenceNode "ANY")` and
; `(ParseNode "ANY")` and on `(WordNode "foobar")` are updated.
;
(define (update-word-counts single-sent)
	(define any-sent (SentenceNode "ANY"))
	(define any-parse (ParseNode "ANY"))

	(define (count-one-word word-inst)
		(count-one-atom (word-inst-get-word word-inst)))

	(count-one-atom any-sent)
	(for-each
		(lambda (parse)
			(count-one-atom any-parse)
			(for-each count-one-word (parse-get-words parse)))
		(sentence-get-parses single-sent))
)

; ---------------------------------------------------------------------
; for-each-lg-link -- loop over all link-grammar links in a sentence.
;
; Each link-grammar link is of the general form:
;
;   EvaluationLink
;      LgLinkNode "FOO"
;      ListLink
;         WordInstanceNode "word@uuid"
;         WordInstanceNode "bird@uuid"
;
; The PROC is a function to be invoked on each of these.
;
(define (for-each-lg-link PROC SENT)
	(for-each
		(lambda (parse)
			(for-each PROC (parse-get-links parse)))
		(sentence-get-parses SENT))
)

; ---------------------------------------------------------------------
; make-word-link -- create a word-link from a word-instance link
;
; Get the LG word-link relation corresponding to a word-instance LG link
; relation. An LG link is simply a single link-grammar link between two
; words (or two word-instances, when working with a single sentence).
;
; This function simply strips off the unique word-ids from each word.
; For example, given this as input:
;
;   EvaluationLink
;      LgLinkNode "FOO"
;      ListLink
;         WordInstanceNode "word@uuid"
;         WordInstanceNode "bird@uuid"
;
; this creates and returns this:
;
;   EvaluationLink
;      LgLinkNode "FOO" -- gar
;      ListLink                          -- gdr
;         WordNode "word"                -- gadr
;         WordNode "bird"                -- gddr
;
(define (make-word-link lg-rel-inst)
	(let (
			(rel-node (gar lg-rel-inst))
			(w-left  (word-inst-get-word (gadr lg-rel-inst)))
			(w-right (word-inst-get-word (gddr lg-rel-inst)))
		)
		(EvaluationLink rel-node (ListLink w-left w-right))
	)
)

; ---------------------------------------------------------------------
; make-word-cset -- create a word-cset from a word-instance cset
;
; A cset is a link-grammar connector set. This takes, as input
; a cset that is attached to a word instance, and creates the
; corresponding cset attached to a word. Basically, it just strips
; off the UUID from the word-instance.
;
; For example, given this input:
;
;   LgWordCset
;      WordInstanceNode "foobar@1233456"
;      LgAnd ...
;
; this creates and returns this:
;
;   LgWordCset
;      WordNode "foobar"  -- gar
;      LgAnd ...          -- gdr
;
(define (make-word-cset CSET-INST)
	(LgWordCset
		(word-inst-get-word (gar CSET-INST))
		(gdr CSET-INST))
)

; ---------------------------------------------------------------------
; update-lg-link-counts -- Increment link counts
;
; This routine updates LG link counts in the database. The algo is trite:
; fetch the LG link from storage, increment the attached CountTruthValue,
; and save back to storage.

(define (update-lg-link-counts single-sent)

	; Due to a RelEx bug, `make-word-link` can throw an exception.  See
	; the documentation for `word-inst-get-word` for details. Look for
	; this exception, and avoid it, if possible.
	(define (try-count-one-link link)
		(catch 'wrong-type-arg
			(lambda () (count-one-atom (make-word-link link)))
			(lambda (key . args) #f)))

	(for-each-lg-link try-count-one-link (list single-sent))
)

; ---------------------------------------------------------------------
; update-disjunct-counts -- Increment disjunct counts
;
; Just like the above, but for the disjuncts.

(define (update-disjunct-counts SENT)

	(define (try-count-one-cset CSET)
		(catch 'wrong-type-arg
			(lambda () (count-one-atom (make-word-cset CSET)))
			(lambda (key . args) #f)))

	(for-each
		(lambda (parse)
			(for-each (lambda (wi) (try-count-one-cset (word-inst-get-cset wi)))
				(parse-get-words parse)))
		(sentence-get-parses SENT))
)

; --------------------------------------------------------------------

(define-public monitor-parse-rate (make-rate-monitor))
(set-procedure-property! monitor-parse-rate 'documentation
"
   monitor-parse-rate MSG - monitor the parse rate.

   Call this function with a string MSG to print out the current
   parse rate; that is, how quickly `observe-text-mode` is progressing.
")

(define-public (observe-text plain-text)
	NUM-LINKAGES 24
"
   observe-text PLAIN-TEXT --
      update word and word-pair counts by observing raw text.

 Uses the LG parser to create 24 different random planar tree parses
 per sentence. Why 24? No particular reason; it provides a reasonable
 sample of all possible planar parses. The number of word-pairs
 sampled will be at least N pairs per parse, where N is the length
 of the sentence.

   PLAIN-TEXT is a utf8 string of text.

   Creates random planar parse trees, using the LG parser
   'any' language.  All word-pairs linked in a linkage will be
   counted. The NUM-LINKAGES specifies the number of linkages
   (the number of random trees) to create and count. Thus, any
   given word will participate in at least NUM-LINKAGES word-pairs,
   and often in at least twice as many. The 'any' language
   tokenizes the sentence string according to white space, and
   also separates out punctuation.

   The parse rate can be monitored by calling, by hand, the guile function
    `(monitor-parse-rate MSG)` for some string MSG.
"
	; Count the atoms in the sentence, according to the counting method
	; passed as argument, then delete the sentence.

	; Note: update-disjunct-counts commented out. It generates some
	; data, but none of it will be interesting to most people.
	(define (process-sent SENT cnt-mode win-size)
		(update-word-counts SENT)
		(update-lg-link-counts SENT)
		; If you uncomment this, be sure to also uncomment
		; LgParseLink below, because LgParseMinimal is not enough.
		; (update-disjunct-counts sent)
		(delete-sentence SENT)
		(monitor-parse-rate #f))

	; -------------------------------------------------------
	; Process the text locally (in RAM), with the LG API link or clique-count.

	; try-catch wrapper for duplicated text. Here's the problem:
	; If this routine is called in rapid succession with the same
	; block of text, then only one PhraseNode and LgParseLink will
	; be created for both calls.  The extract at the end will remove
	; this, even while these atoms are being accessed by the second
	; call.  Thus, `lgn` might throw because `phr` doesn't exist, or
	; `cog-execute!` might throw because lgn doesn't exist. Either of
	; the cog-extracts might also throw. Hide this messiness.
	(catch #t
		(lambda ()
			(let* ((phr (Phrase TXT))
					; needs at least one linkage for tokenization
					(lgn (LgParseMinimal phr
						(LgDict "any") (Number NUM-LINKAGES)))
					(sent (cog-execute! lgn))
				)
				(process-sent sent obs-mode cnt-reach)
				; Remove crud so it doesn't build up.
				(cog-extract! lgn)
				(cog-extract! phr)
			))
		(lambda (key . args) #f))
	)
)

; ---------------------------------------------------------------------
