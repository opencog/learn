;
; lg-pipe-parser.scm -- Atomese LG-based parsing framework.
;
; See ../pair-count/pipe-count.scm for info about Atomese pipe
; counting. This is a new way of counting that avoids the matrix
; API. It is backwards-compat, more-or-less, but the design is
; is still a bit fluid. It's not yet fully generalized.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog matrix))

(define (make-disjunt-parser txt-stream STORAGE)

	(define NUML (Number 3))
	(define DICT (LgDict "dict-pair"))

	; XXX FIXME Both of these are global and stateful in LG and
	; should be a property of the dict. They must not change,
	; once set.
	(define atml
		; Where is the dictionary? If #t, use the local AS.
		(if ATOMSPACE
			(if (cog-atom? ATOMSPACE) ATOMSPACE (cog-atomspace))
		'()))

	(define args (list DICT NUML atml))

	(define mst-sent (SentenceNode "MST"))
	(define mst-parse (ParseNode "MST"))

	(define parser (LgParsexxxx Bonds txt-stream args))

	; Return the assembled counting pipeline.
	; All that the user needs to do is to call `cog-execute!` on it,
	; until end of file is reached.
	(make-parse-pipe parser STORAGE mst-parse)
)

; --------------------------------------------------------------------
; --------------------------------------------------------------------
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
	(define any-sent (SentenceNode "ANY"))
	(count-one-atom any-sent)
)

; Example usage:
#|
(use-modules (opencog learn))
(use-modules (opencog persist))
(use-modules (opencog persist-rocks))
(load "../common.scm")
(load "lg-pipe-parser.scm")
(define rsn (RocksStorageNode "rocks:///tmp/foo"))
(cog-open rsn)
(disjunct-obs-text "this is a test")
(cog-report-counts)
(cog-get-atoms 'WordNode)
(cog-get-atoms 'EdgeLink)
(cog-get-atoms 'Section)
(define CNT (PredicateNode "*-TruthValueKey-*"))
(cog-execute! (ValueOf (SentenceNode "MST") CNT))
(cog-execute! (ValueOf (ParseNode "MST") CNT))
(cog-execute! (ValueOf (WordNode "is") CNT))
(cog-execute! (ValueOf (Edge (Bond "xxxwhatever") (List (Word "is") (Word "a"))) CNT))
(cog-close rsn)
; ...
(load-atomspace)
|#


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
	; `pca` is the basic disjunct API.
	; `pcc` adds a default counting API.
	; `pcs` adds an API that stores the updated counts to storage.
	; `pcm` adds an API that maintains marginal counts dynamically.
	(define pca (make-pseudo-cset-api))
	(define pcc (add-count-api pca))
	(define pcs (add-storage-count pcc))

	; Larger window sizes no longer hurt performance.
	(make-observe-block pcs obs-mpg #:WIN-SIZE 12)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
xxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxx

; --------------------------------------------------------------------

; Backwards-compatible counting API, used for counting pairs.
(define-public (make-block-pair-pipe-observer)
"
   make-block-pair-pipe-observer -- Make an observer for counting pairs in
   text blocks. Returns a function of the following form:

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
