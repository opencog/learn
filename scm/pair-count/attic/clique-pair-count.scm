;
; clique-pair-count.scm
;
; Word-pair counting by counting all possible pairings (the "clique")
; inside a window. The window slides along (sliding window).
;
; Copyright (c) 2013, 2017 Linas Vepstas <linasvepstas@gmail.com>
;
; Word-pairs show up, and can be counted in several different ways. One
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
(use-modules (opencog) (opencog nlp) (opencog persist))
(use-modules (opencog exec))
(use-modules (srfi srfi-1))

(define *-word-pair-dist-* (SchemaNode "*-Pair Distance-*"))
(define *-word-pair-tag-* (PredicateNode "*-Sentence Word Pair-*"))

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

	; Get the scheme-number of the word-sequence number
	(define (get-no seq-lnk)
		(cog-number (gdr seq-lnk)))

	; Create and count a word-pair, and the distance.
	(define (count-one-pair left-seq right-seq)
		(define dist (- (get-no right-seq) (get-no left-seq)))

		; Only count if the distance is less than the cap.
		(if (<= dist MAX-LEN)
			(let ((pare (ListLink (gar left-seq) (gar right-seq))))
				(count-one-atom (EvaluationLink *-word-pair-tag-* pare))
				(if RECORD-LEN
					(count-one-atom
						(ExecutionLink *-word-pair-dist-* pare (NumberNode dist)))))))

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

; --------------------------------------------------------------------

(define-public (observe-clique count-reach plain-text)
"
   observe-clique COUNT-REACH PLAIN-TEXT --
      update word and word-pair counts by observing raw text.
      Uses the window counting technique, to examine all possible pairs.

   COUNT-REACH is the window size.
   PLAIN-TEXT is a utf8 string of text.

   Tokenizes the sentence string into words, according to white-space.
   It then forms all word-pairs within a sliding window of width
   COUNT-REACH, and updates counts on those pairs. Thus, each word will
   participate in exactly COUNT-REACH-1 word pairs.

   Distance is defined as the difference between word positions in the
   sentence, so neighboring words have distance of 1.

   The parse rate can be monitored by calling, by hand, the guile function
    `(monitor-parse-rate MSG)` for some string MSG.
"
	; Count the atoms in the sentence.
	(define (process-sent SENT win-size)
		(update-word-counts SENT)
		(update-clique-pair-counts SENT win-size #f)
		(delete-sentence SENT)
		(monitor-parse-rate #f))

	; -------------------------------------------------------

	; Handle the plain-text locally
	(local-process plain-text count-reach)
)

; ---------------------------------------------------------------------
