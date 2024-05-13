;
; word-pair-pipe.scm -- Hack: random word-pair counting via Atomese pipe.
;
; Hacked backwards-compat replacement for `word-pair-count.scm` for
; counting of random word-pairs.
;
; This is a "hack" because it provides a backwards-compat shim between
; the new way of doing things (with pipes) and the old way (with the
; matrix API). It's not fully API-compatible, but is good enough to work
; great with the existing batch processing code.
;
; The only exported routine is `make-block-pipe-observer`, which is
; functionally compatible with `make-block-pair-observer` (defined in
; `word-pair-count.scm`)
;
; It works, and it runs 3x faster than the old code. This is now used
; as the default for pair-counting.
;
; The default is set in:
;    run-config/2-pair-conf.sh:export OBSERVE="observe-block-pairs"
; and
;    run-common/cogserver-pair.scm:(define (observe-block-pairs TXT)
;
; TODO:
; * Need ((add-count-api LLOBJ) 'count-key) to replace hard-coded count
;   But this is not urgent, because the count-api itself is hard coded.

(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog matrix))
(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------
; If the current cog-storage-node never changes, then the parser only
; needs to be created only once. In the long term, there is a reasonable
; expectation that maybe this should work with multiple different storage
; nodes, maybe even a different one in each thread? This futre remains
; uncertain, so for now, assume only one global. FIXME someday, if
; needed.
(define pair-pipe-parser #f)
(define (get-pair-pipe-parser)
	(if (not pair-pipe-parser)
		(begin
			(set! pair-pipe-parser
				(make-random-pair-parser
					(ValueOf (Anchor "pair pipe") (Predicate "text src"))
					(cog-storage-node)))))
	pair-pipe-parser
)

; Parse one line of text. The text string is assumed to be a scheme
; string.
(define (pair-obs-text TXT-STRING)

	(cog-set-value! (Anchor "pair pipe") (Predicate "text src")
		(StringValue TXT-STRING))

	; Run parser once.
	(cog-execute! (get-pair-pipe-parser))

	; Increment sentence count. Not handled in pipeline above.
	(define any-sent (SentenceNode "ANY"))
	(count-one-atom any-sent)
)

; Example usage:
#|
(use-modules (opencog learn))
(use-modules (opencog persist))
(use-modules (opencog persist-rocks))
(load "../common.scm")
(load "pipe-count.scm")
(define rsn (RocksStorageNode "rocks:///tmp/foo"))
(cog-open rsn)
(pair-obs-text "this is a test")
(cog-report-counts)
(cog-get-atoms 'AnyNode)
(cog-get-atoms 'WordNode)
(define CNT (PredicateNode "*-TruthValueKey-*"))
(cog-execute! (ValueOf (SentenceNode "ANY") CNT))
(cog-execute! (ValueOf (ParseNode "ANY") CNT))
(cog-execute! (ValueOf (WordNode "is") CNT))
(cog-execute! (ValueOf (Edge (Bond "ANY") (List (Word "is") (Word "a"))) CNT))
(cog-close rsn)
; ...
(load-atomspace)
|#

; ---------------------------------------------------------------------

; Backwards-compatible counting API, used for counting pairs.
(define-public (make-block-random-pair-observer)
"
   make-block-random-pair-observer -- Make an observer for counting
   random pairs in text blocks. Returns a function of the following
   form:

   func TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for word-pair counting.

   TEXT-BLOCK is a utf8 string of text. A sliding window, of the default
   width of 9 words, is created on that block. Everything within the
   window is sent to the LG 'any' random-planar-tree parser. The word
   pairs in the random tree are then counted. Counts are stored.
"
	(make-observe-block pair-obs-text #:WIN-SIZE 9)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
