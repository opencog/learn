;
; lg-parser.scm -- Unified LG-based parsing framework.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(use-modules (opencog) (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog exec))


; FYI -- This is very similar to `make-pair-counter`, with small
; changes, designed to count Sections instead of word-pairs. It
; should probably be unified with that block of code!?
;
(define*-public (make-disjunct-counter LLOBJ
	#:key
		(NUM-LINKAGES 6)
		(DICT (LgDict "any"))
	)
"
  make-disjunct-counter LLOBJ --
     Return a function that will update Section counts on LLOBJ.

  The LLOBJ should be a matrix object that can hold Sections.
  Suggest the `xxx-what-api` object be used.

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

	(define (update-section-counts SENT)
		(format #t "duuude yo ~A\n" SENT)
	)

	(define (obs-txt PLAIN-TEXT)
		(define sect-list (cog-execute!
			(LgParseSections (Phrase PLAIN-TEXT) DICT NUML))))
		(update-section-counts sect-list)
		(monitor-parse-rate #f)
	))

	; Return the function defined above.
	obs-txt
)

; ---------------------------------------------------------------------
; Example:
; (define dict (LgDictNode "run-config/dict-combined"))
;
; (define parser (make-disjunct-counter foo #:DICT dict))
; (parser "this is a test")
; ---------------------------------------------------------------------
