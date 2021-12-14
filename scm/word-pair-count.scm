;
; word-pair-count.scm
;
; Word-pair counting via random planar trees. Planar trees provide subtly
; different word-pair statistics from windowed counting techniques. This
; uses the Link Grammar (LG) parser "any" language to generate random trees.
; Also handy, as the "any" parser does split off basic punctuation, and is
; thus suitable for almost all IndoEuropean languages, and possibly some
; Asian languages.
;
; If you are not working with natural-language sentences, don't use this code!
; This was designed to counts words, several kinds of word-pairs (links,
; distances), as well (random) disjuncts, parses and sentences.
;
; Copyright (c) 2013, 2017 Linas Vepstas <linasvepstas@gmail.com>
;
; This code is part of the language-learning effort.  The project
; requires that a lot of text be observed, with the goal of deducing
; a grammar from it, using entropy and other basic probability methods.
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
; *) the distance between words in the above pairs.
; *) how many link-relationship triples have been observed.
; *) how many disjuncts have been observed.
;
; Sentences are counted by updating the count on `(SentenceNode "ANY")`.
; Parses are counted by updating the count on `(ParseNode "ANY")`.
; Words are counted by updating the count on the `WordNode` for that
; word. It is counted with multiplicity: once for each time it occurs
; in a parse.  That is, if a word appears twice in a parse, it is counted
; twice.
;
; Word-pairs show up, and can be counted in four different ways. One
; method  is a windowed clique-counter. If two words appear within a
; fixed distance from each other (the window size), the corresponding
; word-pair count is incremented. This is a clique-count, because every
; possible pairing is considered. This count is stored in the CountTV
; for the EvaluationLink on (PredicateNode "*-Sentence Word Pair-*").
; A second count is maintained for this same pair, but including the
; distance between the two words. This is kept on a link identified by
; (SchemaNode "*-Pair Distance-*"). Please note that the pair-distance
; counter can lead to very large atomspaces, because for window widths
; of N, a given word-pair might be observed with every possible
; distance between them, i.e. up to N times.
;
; XXX FIXME we should probably not store this way. We should probably
; have just one word-pair, and hold the counts in different values,
; instead. This needs a code redesign. XXX
;
; Word-pairs are also designated by means of Link Grammar parses of a
; sentence. A Link Grammar parse creates a list of typed links between
; pairs of words in the sentence. Each such link is counted once, for
; each time that it occurs.  These counts are maintained in the CountTV
; on the EvaluationLink for the LgLinkNode for that ; word-pair.
; In addition, a count is maintained of the length of that link.
; XXX where??? This is not implemented ??? XXX
;
; For the initial stages of the language-learning project, the parses
; are produced by the "any" language parser, which produces random planar
; trees.  This creates a sampling of word-pairs that is different than
; merely having them show up in the same sentence.  That is, a covering
; of a sentence by random trees does not produce the same edge statistics
; as a clique of edges drawn between all words. This is explored further
; in the diary, in a section devoted to this topic.
;
; These different counting modes can be turned on and off with the
; "mode" flag.
;
; The Link Grammar parse also produces and reports the disjuncts that were
; used for each word. These are useful in and of themselves; they indicate
; the hubbiness (link-multiplicity) of each word. The disjunct counts are
; maintained on the LgWordCset for a given word. XXX This is currently
; disabled in the code, and are not handled by the mode flag. This needs
; to be (optionally) turned back on. XXX
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
; This returns the corresponding structures, for words, starting with
; the left-wall at number zero.  Thus, this would return
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

	(count-one-atom (word-inst-get-word word-inst))
	(count-one-atom any-sent)
	(for-each
		(lambda (parse)
			(count-one-atom any-parse)
			(for-each try-count-one-word (parse-get-words parse)))
		(sentence-get-parses single-sent))
)

; ---------------------------------------------------------------------
; update-clique-pair-counts -- count occurrences of random word-pairs.
;
; This generates what are termed "clique pairs" throughout: these are
; all possible word-pair combinations, given a sequence of words.
; No parsing is involved; this code simply generates one word-pair
; for each and every edge in the clique of the sequence of the words.
;
; This code is problematic for multiple reasons:
; 1) The kinds of pairs it generates occur with different frequencies
;    than they would in a random planar tree parse.  In particular,
;    it generates more pairs between distant words than the planar tree
;    would. This could be ameliorated by simply not generating pairs
;    for words that are more than 6 lengths apart. Or, alternately,
;    only the statistics for closer pairs closer together than 6 could
;    be used.  Anyway, this is probably not a big deal, by itself.
;
; 2) This generates pairs tagged with the distance between the pairs.
;    (See below for the format).  This is might be interesting to
;    look at for academic reasons, but it currently puts a huge
;    impact on the size of the atomspace, and the size of the
;    database, impacting performance in a sharply negative way.
;    That's because, for every possible word-pair, chances are that
;    it will appear, sooner or later, with with every possible distance
;    from 1 to about 30. Each distance requires it's own atom to keep
;    count: thus requiring maybe 30x more atoms for word-pairs!  Ouch!
;    This is huge!
;
;    Limiting pair-counts to distances of 6 or less still blows up
;    the database size by 6x... which is still a lot.
;
;    We might be able to cut down on this by using different values
;    (on the same pair-atom) to count the different lengths, but the
;    hit is still huge.
;
; 3) On a per-sentence basis, when clique-counting is turned on, the
;    number of database updates increases by 3x-4x atom value updates.
;    If your database is on spinning disks, not SSD, this means that
;    database updates will be limited by the disk I/O subsystem, and
;    this additional traffic can slow down statistics gathering by...
;    3x or 4x.
;
; Thus, clique-counting is currently disabled. You can turn it on
; by uncommenting this routine in the main loop, below.
;
; Note that this might throw an exception...
;
; The structures that get created and incremented are of the form
;
;     EvaluationLink
;         PredicateNode "*-Sentence Word Pair-*"
;         ListLink
;             WordNode "lefty"  -- or whatever words these are.
;             WordNode "righty"
;
;     ExecutionLink
;         SchemaNode "*-Pair Distance-*"
;         ListLink
;             WordNode "lefty"
;             WordNode "righty"
;         NumberNode 3
;
; Here, the NumberNode encodes the distance between the words. It is always
; at least one -- i.e. it is the difference between their ordinals.
;
; Parameters:
; MAX-LEN -- integer: don't count a pair, if the words are farther apart
;            than this.
; RECORD-LEN -- boolean #t of #f: enable or disable recording of lengths.
;            If enabled, see warning about the quantity of data, above.
;
(define (update-pair-counts-once PARSE MAX-LEN RECORD-LEN)

	(define pair-pred (PredicateNode "*-Sentence Word Pair-*"))
	(define pair-dist (SchemaNode "*-Pair Distance-*"))

	; Get the scheme-number of the word-sequence number
	(define (get-no seq-lnk)
		(cog-number (gdr seq-lnk)))

	; Create and count a word-pair, and the distance.
	(define (count-one-pair left-seq right-seq)
		(define dist (- (get-no right-seq) (get-no left-seq)))

		; Only count if the distance is less than the cap.
		(if (<= dist MAX-LEN)
			(let ((pare (ListLink (gar left-seq) (gar right-seq))))
				(count-one-atom (EvaluationLink pair-pred pare))
				(if RECORD-LEN
					(count-one-atom
						(ExecutionLink pair-dist pare (NumberNode dist)))))))

	; Create pairs from `first`, and each word in the list in `rest`,
	; and increment counts on these pairs.
	(define (count-pairs first rest)
		(if (not (null? rest))
			(begin
				(count-one-pair first (car rest))
				(count-pairs first (cdr rest)))))

	; Iterate over all of the words in the word-list, making pairs.
	(define (make-pairs word-list)
		(if (not (null? word-list))
			(begin
				(count-pairs (car word-list) (cdr word-list))
				(make-pairs (cdr word-list)))))

	; If this function throws, then it will be here, so all counting
	; will be skipped, if any one word fails.
	(define word-seq (make-word-sequence PARSE))

	; What the heck. Go ahead and count these, too.
	(for-each count-one-atom word-seq)

	; Count the pairs, too.
	(make-pairs word-seq)
)

; See above for explanation.
(define (update-clique-pair-counts SENT MAX-LEN RECORD-LEN)
	; In most cases, all parses return the same words in the same order.
	; Thus, counting only requires us to look at only one parse.
	(update-pair-counts-once
		(car (sentence-get-parses SENT))
		MAX-LEN RECORD-LEN)
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

(define-public (observe-text-mode observe-mode count-reach plain-text)
"
   observe-text-mode OBSERVE-MODE COUNT-REACH PLAIN-TEXT --
      update word and word-pair counts by observing raw text.

   OBSERVE-MODE is either the string 'any' or the string 'clique'.
   COUNT-REACH is parameter whose interpretation depends on OBSERVE-MODE.
   PLAIN-TEXT is a utf8 string of text.

   There are currently two observing modes:
   - 'any': creates random planar parse trees, using the LG parser
         'any' language.  All word-pairs linked in a linkage will be
         counted. The COUNT-REACH specifies the number of linkages
         (the number of random trees) to create and count. Thus, any
         given word will participate in at least COUNT-REACH word-pairs,
         of usually in at least twice as many. The 'any' language
         tokenizes the sentence string according to white space, and
         also separates out punctuation.
   - 'clique': Tokenizes the sentence string into words, according to
         white-space, separating punctuation as well. Uses the LG parser
         to perform this splitting. It then forms all word-pairs within
         a sliding window of width COUNT-REACH, and updates counts on
         those pairs. Thus, each word will participate in exactly
         COUNT-REACH-1 word pairs.

   Distance is defined as the difference between word positions in the
   sentence, so neighboring words have distance of 1.

   The parse rate can be monitored by calling, by hand, the guile function
    `(monitor-parse-rate MSG)` for some string MSG.
"
	; Count the atoms in the sentence, according to the counting method
	; passed as argument, then delete the sentence.

	; Note: update-disjunct-counts commented out. It generates some
	; data, but none of it will be interesting to most people.
	(define (process-sent SENT cnt-mode win-size)
		(update-word-counts SENT)
		(if (equal? cnt-mode "any")
			(update-lg-link-counts SENT)
			(update-clique-pair-counts SENT win-size #f))
		; If you uncomment this, be sure to also uncomment
		; LgParseLink below, because LgParseMinimal is not enough.
		; (update-disjunct-counts sent)
		(delete-sentence SENT)
		(monitor-parse-rate #f))

	; -------------------------------------------------------
#! ====
As of guile-3.0, the RAM usage issues seem to have gone away,
and so manual garbage collection is not needed any more.
(I don't know if this is due to changes in guile, or due to
how we run the pipeline, in general.)

	; Manually run the garbage collector, every now and then.
	; This helps keep RAM usage down, which is handy on small-RAM
	; machines. However, it does cost CPU time, in exchange.
	; Adjust `how-often` up or down to suit your whims.
	(define sometimes-gc
		(let ((cnt 0)
				(how-often 10)) ; every 10 times.
			(lambda ()
				(set! cnt (+ cnt 1))
				(if (eqv? 0 (modulo cnt how-often)) (gc)))))

	; Perform GC whenever it gets larger than a fixed limit.
	; Less than one GB should be enough, but the huge strings
	; from relex seem to cause bad memory fragmentation.
	(define maybe-gc
		(let ((cnt 0)
				(max-size (* 2750 1000 1000)))  ; 2750 MB
			(lambda ()
				(if (< max-size (- (assoc-ref (gc-stats) 'heap-size)
							(assoc-ref (gc-stats) 'heap-free-size)))
					(begin
						(gc)
						(set! cnt (+ cnt 1))
						;(report-avg-gc-cpu-time)
					)))))
==== !#
	; -------------------------------------------------------
	; Process the text locally (in RAM), with the LG API link or clique-count.
	(define (local-process TXT obs-mode cnt-reach)
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
						(num-parses (if (equal? obs-mode "any") cnt-reach 1))
						(lgn (LgParseMinimal phr (LgDict "any") (Number num-parses)))
						(sent (cog-execute! lgn))
					)
					(process-sent sent obs-mode cnt-reach)
					; Remove crud so it doesn't build up.
					(cog-extract! lgn)
					(cog-extract! phr)
				))
			(lambda (key . args) #f))
	)

	; Handle the plain-text locally
	(local-process plain-text observe-mode count-reach)
)

(define-public (observe-text plain-text)
"
 Wrapper to allow shell scripts to have a simple form.
 Passes default parameters to observe-text-mode.

 Uses the LG parser to create 24 different random planar tree parses
 per sentence. Why 24? No particular reason; it provides a reasonable
 sample of all possible planar parses. The number of word-pairs
 sampled will be at least N pairs per parse, where N is the length
 of the sentence.
"
	(observe-text-mode "any" 24 plain-text)
)

(define-public (observe-clique winsz plain-text)
"
 Wrapper to allow shell scripts to have a simple form.
 Passes default parameters to observe-text-mode.

 Uses the window counting technique, to examine all possible pairs.
"
	(observe-text-mode "clique" winsz plain-text)
)

; ---------------------------------------------------------------------
