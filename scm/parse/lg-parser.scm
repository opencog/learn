;
; lg-parser.scm -- Unified LG-based parsing framework.
;
; ---------------------------------------------------------------------

(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (srfi srfi-1))

(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec))

(define*-public (make-disjunct-counter LLOBJ DICT
	#:key
		(NUM-LINKAGES 3)
		(ATOMSPACE #t)
		(STORAGE #f)
	)
"
  make-disjunct-counter LLOBJ DICT -- Parse text using DICT.

  Return a function that will parse text strings, and update Section
  counts (using LLOBJ) for the parses. The LgDictNode DICT will be
  used to access the dictionary.

  The LLOBJ should be a matrix object that holds Sections. Typically,
  the `make-pseudo-cset-api` object, or something similar, should be
  used.

  The DICT should be an `LgDictNode` specifying the dictionary.
  Recommend the one in `run-common/dict-combined`.

  This returns a function that takes a single argument, a plain-text
  UTF-8 string holding a single sentence, and sends it to the
  Link Grammar parser for parsing. The resulting parses are converted
  into Sections and given to the LLOBJ for counting.

  This takes three optional parameters:

  #:NUM-LINKAGES -- The number of linkages that the LG parser should
  generate. Recall that each linkage is a different parse of the
  sentence; these are returned in cost-sorted order. Default is 3.

  #:ATOMSPACE -- Use the provided AtomSpace for dictionary contents.
  If not specified, use the current AtomSpace in the current thread.
  Set this to #f to use a local, private AtomSpace. In this case, the
  DICT must specify a StorageNode which can provide the dictionary
  contents.

  #:STORAGE -- If the current AtomSpace is being used, or a custom
  AtomSpace has been specified, then this parameter can be any
  StorageNode (including ProxyNodes) that will be used to access
  dictionary data. This can be used to specify a ProxyNode that
  computes MI or other statistics on-the-fly, for the accessed
  dictionary definitions.

  In addition to counting disjuncts, this will keep a running total
  of the numer of times this was called (aka 'the number of sentences')
  and the number of parses.

  Sentences are counted by updating the count on `(SentenceNode \"MST\")`.
  Parses are counted by updating the count on `(ParseNode \"MST\")`.
  XXX TODO Make above configurable.
"
	(define stol (if STORAGE (list STORAGE) '()))
	(define atml
		; Where is the dictionary? If #t, use the local AS.
		(if ATOMSPACE
			(cons
				(if (cog-atom? ATOMSPACE) ATOMSPACE (cog-atomspace))
				stol)
		'()))

	(define args (list DICT (Number NUM-LINKAGES) atml))

	(define mst-sent (SentenceNode "MST"))
	(define mst-parse (ParseNode "MST"))

	; Each section arrives already in the correct format
	; Thus, counting is trivial.
	(define (update-section-counts SECTL)
		(count-one-atom mst-parse)
		(for-each
			(lambda (SECT) (LLOBJ 'inc-count SECT 1.0))
			(cog-value->list SECTL)))

	(define (obs-txt PLAIN-TEXT)
		; Do the parsing in a temp atomspace, and the counting in
		; the base space. The temp space must remain until we are
		; done counting, else the atoms will disappear.
		;
		; The counting is done in a thunk, as the parser can throw
		; a C++ exception if the parser times out. We avoid count
		; increments if the exception is thrown.
		(define base-as (cog-push-atomspace))
		(define (pthunk)
			(define parses	(cog-value->list
				(cog-execute! (LgParseSections (Phrase PLAIN-TEXT) args))))
			(cog-set-atomspace! base-as)
			(count-one-atom mst-sent)
			(for-each update-section-counts parses))
		(with-throw-handler #t pthunk (lambda (key . args) #f))
		(cog-pop-atomspace)

		(monitor-parse-rate #f)
	)

	; Return the function defined above.
	obs-txt
)

; Backwards compat API for single-sentence MPG parsing. This accepts
; single sentences, and updates the resulting disjunct counts.
(define-public observe-mpg
	(make-disjunct-counter
		(add-storage-count (add-count-api (make-pseudo-cset-api)))
		(LgDictNode "dict-pair")))

; --------------------------------------------------------------------

(define (make-block-mpg-observer)
"
	Make an observer for counting MST/MPG disjuncts in text blocks.
   See above and below.
"
	; `pca` is the basic disjunct API.
	; `pcc` adds a default counting API.
	; `pcs` adds an API that stores the updated counts to storage.
	; `pcm` adds an API that maintains marginal counts dynamically.
	(define pca (make-pseudo-cset-api))
	(define pcc (add-count-api pca))
	(define pcs (add-storage-count pcc))

	; Skip performing the marginal counts for just right now, until
	; the rest of the dynamic-MI infrastructure is in place. Dynamic
	; marginal counts just add overhead to the counting process, if
	; we are not actually using the results.
	; (define pcm (add-marginal-count pcs))

	; The dict to use
	(define dict (LgDictNode "dict-pair"))

	; The counter for the window itself.
	(define obs-mpg (make-disjunct-counter pcs dict #:NUM-LINKAGES 3))

	; Large #:WIN-SIZE results in terrible LG parse-times (parse time
	; overflows) because there are just too many word-pairs, and they
	; suck up power-pruning. In particular, 12 is too large. So we try 9.
	(make-observe-block pcs obs-mpg #:WIN-SIZE 9)
)

(define-public observe-block-mpg (make-block-mpg-observer))

(set-procedure-property! observe-block-mpg 'documentation
"
   observe-block-mpg TEXT-BLOCK
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
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Example:
; (define dict (LgDictNode "dict-pair"))
; (define psa (make-pseudo-cset-api))
; (define psc (add-count-api psa))
; (define pst (add-storage-count psc))
; (define psm (add-marginal-count pst))
;
; (define parser (make-disjunct-counter psm dict))
; (parser "this is a test")
; ---------------------------------------------------------------------
