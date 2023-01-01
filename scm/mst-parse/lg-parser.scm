;
; lg-parser.scm -- Unified LG-based parsing framework.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec))

(define*-public (make-disjunct-counter LLOBJ
	#:key
		(NUM-LINKAGES 6)
		(DICT (LgDict "any"))
	)
"
  make-disjunct-counter LLOBJ --
     Return a function that will update Section counts using LLOBJ.

  The LLOBJ should be a matrix object that holds Sections. Typically,
  the `make-pseudo-cset-api` object, or something similar, should be
  used.

  This returns a function that takes a single argument, a plain-text
  UTF-8 string holding a single sentence, and sends it to the
  Link Grammar parser for parsing. The resulting parses are converted
  into Sections and given to the LLOBJ for counting.

  This takes two optional parameters:

  #:NUM-LINKAGES -- the number of linkages that the LG parser should
  generate. Recall that each linkage is a different parse of the
  sentence; these are returned in cost-sorted order. Default is 6.

  #:DICT -- the `LgDictNode` to use. This is the dictionary to use for
  parsing. 
"
	(define NUML (Number NUM-LINKAGES))

	; TODO: These should be moved elsewhere...
	(define count-obj (add-count-api LLOBJ))
	(define store-obj (add-storage-count count-obj))
	(define marg-obj (add-marginal-count store-obj))

	; Each section arrives already in the correct format
	; Thus, counting is trivial.
	(define (update-section-counts SECTL)
		(for-each
			(lambda (SECT) (marg-obj 'inc-count SECT 1.0))
			(cog-value->list SECTL)))

	(define (obs-txt PLAIN-TEXT)
		; Do the parsing in a temp atomspace, and the counting in
		; the base space. The temp space must remain until we are
		; done counting, else the atoms will disappear.
		(define base-as (cog-push-atomspace))
		(define parses (cog-execute!
			(LgParseSections (Phrase PLAIN-TEXT) DICT NUML)))
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
; (define pca (make-pseudo-cset-api))
;
; (define parser (make-disjunct-counter pca #:DICT dict))
; (parser "this is a test")
; ---------------------------------------------------------------------
