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
; Copyright (c) 2013, 2017, 2022 Linas Vepstas <linasvepstas@gmail.com>
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
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

; Assume the curr atomspace is some temp space.
; Navigate to the botton, increment the count,
; and return to where we were.
(define (base-count ATOM)
	(define curspace (cog-atomspace))
	(cog-set-atomspace! (car (cog-atomspace-env)))
	(count-one-atom ATOM)
	(cog-set-atomspace! curspace)
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
		(base-count (word-inst-get-word word-inst)))

	(base-count any-sent)
	(for-each
		(lambda (parse)
			(base-count any-parse)
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
; incr-pair-w-marginals -- update counts on word-pairs, and their
; marginals. Argument is expected to be this:
;
;   EvaluationLink
;      LgLinkNode "FOO"
;      ListLink
;         WordInstanceNode "word@uuid123"
;         WordInstanceNode "bird@uuid456"
;
; Using the LLOBJ, the counts will be updated on the word-pair
; (WordNode "word" WordNode "bird") as well on on the marginals
; (AnyNode "left" WordNode "bird") and (WordNode "word" AnyNode "right")
; as well as the wild-card (AnyNode "left" AnyNode "right") The
; actual pairs to update are obtained from LLOBJ.
;
; Currently LLOBJ is hard-coded to `any-link-api`.
;
; Hmmm.
(define any-link-api (make-any-link-api))
(define wild-wild (any-link-api 'wild-wild))

(define (incr-pair-w-marginals lg-rel-inst)
	(define curspace (cog-atomspace))

	; Extract the left and right words.
	(define w-left  (word-inst-get-word (gadr lg-rel-inst)))
	(define w-right (word-inst-get-word (gddr lg-rel-inst)))

	; Pop down to the base atomspace.
	(cog-set-atomspace! (car (cog-atomspace-env)))
	(count-one-atom (any-link-api 'make-pair w-left w-right))
	(count-one-atom (any-link-api 'left-wildcard w-right))
	(count-one-atom (any-link-api 'right-wildcard w-left))
	(count-one-atom wild-wild)
	(cog-set-atomspace! curspace)
)

; ---------------------------------------------------------------------

(define-public monitor-parse-rate (make-rate-monitor))
(set-procedure-property! monitor-parse-rate 'documentation
"
   monitor-parse-rate MSG - monitor the parse rate.

   Call this function with a string MSG to print out the current
   parse rate; that is, how quickly `observe-text-mode` is progressing.
")

; --------------------------------------------------------------------

(define*-public (observe-text PLAIN-TEXT
	#:key (NUM-LINKAGES 24))
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
	(define DANY (LgDict "any"))
	(define NUML (Number NUM-LINKAGES))

	; Do all work in a temp atomspace.  The idea here is that this will
	; make counting thread-safe, as each sentence get's processed in
	; it's own unique per-thread atomspace.
	;
	; Because I'm paranoid, the `base-count` function will transition
	; to the base atomspace, before incrementing the count. I suspect
	; that this probably not needed, that it's enough that
	; `count-one-atom` is incrementing and storing.  But I'm slightly
	; paranoid, here. The performance hit is presumably mnimal.
	(cog-push-atomspace)
	(let ((SENT (cog-execute!
				(LgParseMinimal (Phrase PLAIN-TEXT) DANY NUML))))
		(update-word-counts SENT)

		; Update the pair counts.
		(for-each-lg-link incr-pair-w-marginals (list SENT))
	)
	(cog-pop-atomspace)
	(monitor-parse-rate #f)
)

; ---------------------------------------------------------------------
