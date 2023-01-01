;
; word-pair-count.scm
;
; Word-pair counting via random planar trees. This takes a uniformly
; distributed random sampling out of all possible planar parse trees.
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
(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec) (opencog persist))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define-public monitor-parse-rate (make-rate-monitor))
(set-procedure-property! monitor-parse-rate 'documentation
"
   monitor-parse-rate MSG - monitor the parse rate.

   Call this function with a string MSG to print out the current
   parse rate; that is, how quickly `observe-text-mode` is progressing.
")

; --------------------------------------------------------------------

; This is similar to `make-disjunct-counter`, and perhaps could be
; unified with that?
(define*-public (make-pair-counter LLOBJ
	#:key
		(NUM-LINKAGES 24)
		(DICT (LgDict "any"))
	)
"
  make-pair-counter LLOBJ --
     Return a function that will update word-pair counts on LLOBJ.

  The LLOBJ should be a matrix object that can hold a pair of words
  on the left and right. The `any-link-api` object will do.

  This returns a function that takes a single argument, a plain-text
  UTF-8 string holding a single sentence, and sends it to the
  Link Grammar parser for parsing. The individual links in the
  resulting parses are sent to the LLOBJ for pair-counting.

  This takes two optional parameters:

  #:NUM-LINKAGES -- the number of linkages that the LG parser should
  generate. Recall that each linkage is a different parse of the
  sentence; these are returned in cost-sorted order. Default is 24.

  #:DICT -- the `LgDictNode` to use. This is the dictionary to use for
  parsing. By default, this is the `any` dictionary, which creates
  uniformly-distributed random parse trees.

  The parse rate can be monitored by calling, by hand, the guile function
   `(monitor-parse-rate MSG)` for some string MSG.
"
	(define base-space (cog-atomspace))
	(define NUML (Number NUM-LINKAGES))
	(define wild-wild (LLOBJ 'wild-wild))

	; XXX should we do this here, or at a higher layer?
	(define count-obj (add-count-api LLOBJ))
	(define store-obj (add-storage-count count-obj))
	(define marg-obj (add-marginal-count store-obj))

	(define any-sent (SentenceNode "ANY"))
	(define any-parse (ParseNode "ANY"))

	; update-word-counts -- update the count of the individual words
	; in a parse.
	; XXX TODO: this should probably be converted to an 1xN matrix
	; and handled with a matrix API.
	(define (update-word-counts WRD-LIST)
		(for-each count-one-atom (cog-value->list WRD-LIST)))

	; Increment the count on a word-pair. Also increment the marginal
	; counts. The `EVLINK` argument is assumed to be of the form
	;
	;   (Evaluation (BondNode "FOO")
	;       (ListLink (Word "surfin") (Word "bird")))
	;
	; The corresponding WordNodes are located and passed to LLOBJ
	; for pair-handling.
	(define (incr-pair EVLINK)
		; Extract the left and right words.
		(define w-left  (gadr EVLINK))
		(define w-right (gddr EVLINK))
		(marg-obj 'pair-inc w-left w-right 1.0))

	; Loop over the list of word-pairs.
	(define (update-pair-counts PAIR-LIST)
		(for-each incr-pair (cog-value->list PAIR-LIST)))

	(define (obs-txt PLAIN-TEXT)
		(define temp-as #f)
		(define parses #f)

		; Create the parse in a scratch AtomSpace.
		(define base-as (cog-push-atomspace))
		(set! parses (cog-execute!
			(LgParseBonds (Phrase PLAIN-TEXT) DICT NUML)))
		(set! temp-as (cog-set-atomspace! base-as))

		(count-one-atom any-sent)

		; `parses` is a LinkValue of parses.
		; Each parse is two LinkValues: a list of words,
		; and a list of links.
		(for-each
			(lambda (PARSE)
				(count-one-atom any-parse)
				(update-word-counts (cog-value-ref PARSE 0))
				(update-pair-counts (cog-value-ref PARSE 1)))
			(cog-value->list parses))
		(monitor-parse-rate #f)
		(cog-set-atomspace! temp-as)
		(cog-pop-atomspace)
	)

	; Return the function defined above.
	obs-txt
)

; Backwards compat
(define-public observe-text (make-pair-counter (make-any-link-api)))

; ---------------------------------------------------------------------
