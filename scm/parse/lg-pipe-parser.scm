;
; lg-pipe-parser.scm -- Atomese LG-based parsing framework.
;
; See ../pipe-parse/pipe-count.scm for info about Atomese pipe
; counting. This is a new way of counting that avoids the matrix
; API. It is backwards-compat, more-or-less, but the design is
; is still a bit fluid. It's not yet fully generalized.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog matrix))

; XXX FIXME. The next 30 lines of code are a cut-n-paste of the
; pair parsing pipeline code. It is needed because the batch processor
; force-feeds use text, instead of allowing us to read on our own.
; It just wires up a pipeline to feed text.

; If the current cog-storage-node never changes, then the parser only
; needs to be created only once. In the long term, there is a reasonable
; expectation that maybe this should work with multiple different storage
; nodes, maybe even a different one in each thread? This futre remains
; uncertain, so for now, assume only one global. FIXME someday, if
; needed.
(define disjunct-pipe-parser #f)
(define (get-disjunct-pipe-parser)
	(if (not disjunct-pipe-parser)
		(begin
			(set! disjunct-pipe-parser
				(make-disjunct-parser
					(ValueOf (Anchor "disjunct pipe") (Predicate "text src"))
					(cog-storage-node)))))
	disjunct-pipe-parser
)

; Parse one line of text. The text string is assumed to be a scheme
; string.
(define (disjunct-obs-text TXT-STRING)

	(cog-set-value! (Anchor "disjunct pipe") (Predicate "text src")
		(StringValue TXT-STRING))

	; Run parser once.
	(cog-execute! (get-disjunct-pipe-parser))

	; Increment sentence count. Not handled in pipeline above.
	(define mst-sent (SentenceNode "MST"))
	(count-one-atom mst-sent)
)

; ---------------------------------------------------------------------
; Backwards-compatible counting API, used for counting disjuncts.
(define-public (make-block-mpg-pipe-observer)
"
   make-block-mpg-pipe-observer -- Make an observer for counting
      MST/MPG disjuncts in text blocks. Uses the Atomese pipeline.

   The returned function has the form:

   func TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for MPG/MST parsing.

   TEXT-BLOCK is a utf8 string of text. A sliding window is created
   on that text block, of default width 12. The words within the
   window are then sent to the LG parser, using the 'dict-pair'
   dictionary.  This dictionary is presumed to hold word-pairs
   with valid word-MI on them, accessible via the `BondNode ANY`
   EvaluationLinks.

   The LG parser creates MST/MPG parses using that dictionary.
   Then the count on each disjunct in the parse is incremented.

   This is a block observer, because, at this point, we do not yet know
   where the sentence boundaries might be, and so a sliding window is
   used to examine everything in the general vicinity.
"
	; Larger window sizes no longer hurt performance.
	(make-observe-block disjunct-obs-text #:WIN-SIZE 12)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
