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
		(NUM-LINKAGES 6)
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
  sentence; these are returned in cost-sorted order. Default is 6.

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
"
	(define stol (if STORAGE (list STORAGE) '()))
	(define atml
		; Where is the dictionary? If #t, use the local AS.
		(if ATOMSPACE
			(cons
				(if (cog-atom? ATOMSPACE) ATOMSPACE (cog-atomspace))
				stol))
		'())

	(define args (list DICT (Number NUM-LINKAGES) atl))

	; Each section arrives already in the correct format
	; Thus, counting is trivial.
	(define (update-section-counts SECTL)
		(for-each
			(lambda (SECT) (LLOBJ 'inc-count SECT 1.0))
			(cog-value->list SECTL)))

	(define (obs-txt PLAIN-TEXT)
		; Do the parsing in a temp atomspace, and the counting in
		; the base space. The temp space must remain until we are
		; done counting, else the atoms will disappear.
		(define base-as (cog-push-atomspace))
		(define parses (cog-execute!
			(LgParseSections (Phrase PLAIN-TEXT) args)))
		(define temp-as (cog-set-atomspace! base-as))

		(for-each update-section-counts (cog-value->list parses))
		(cog-set-atomspace! temp-as)
		(cog-pop-atomspace)

		(monitor-parse-rate #f)
	)

	; Return the function defined above.
	obs-txt
)

; ---------------------------------------------------------------------
; Example:
; (define dict (LgDictNode "run-config/dict-combined"))
; (define psa (make-pseudo-cset-api))
; (define psc (add-count-api psa))
; (define pst (add-storage-count psc))
; (define psm (add-marginal-count pst))
;
; (define parser (make-disjunct-counter psm dict))
; (parser "this is a test")
; ---------------------------------------------------------------------
