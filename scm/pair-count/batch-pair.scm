;
; batch-pair.scm
;
; Define item-pair and word-pair access API objects.
; Batch-compute the mutual information of pairs of items, such as
; natural-language words.
;
; Copyright (c) 2013, 2014, 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The objects below define API's to access pairs of items, such as
; natural language word-pairs, stored in the AtomSpace, as a rank-2
; matrix, i.e. as a matrix of (left, right) pairs.  This provides
; exactly the API needed for use with the `(use-modules (opencog matrix))`
; statistical analysis subsystem.
;
; Given a generic API, the `(opencog matrix)` can do things such as
; computing the Yuret-style lexical attraction between pairs of items.
; (See `compute-mi.scm` for more detail about what is computed, and how.)
;
; Given the generic API, there is a handful of small scripts, at the
; bottom of this file, that will perform the MI calculations as a batch
; job.  As a batch job, and may take hours to complete. The results are
; stored in the currently-open database, for future reference.
;
; An example of this is the structure used to store word-pair counts.
; This is used in `word-pair-count.scm` to accumulate counts:
;
;     EvaluationLink
;         LgLinkNode "ANY"
;         ListLink
;             WordNode "left-word"
;             WordNode "right-word"
;
; An example of generic item pairs is used by `pair-count-window.scm`
; and has the structure
;
;     EvaluationLink
;         PredicateNode "*-Item pairs-*"
;         ListLink
;             ItemNode "left item"
;             ItemNode "right item"
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------

(define-public (make-any-link-api)
"
  make-any-link-api -- Word-pair access methods from random planar parse.

  This implements a word-pair object, where the two words are connected
  with an LG link-type of \"ANY\", in an EvaluationLink.

  That is, a word pair is represented as:

    EvaluationLink
       LgLinkNode \"ANY\"
       ListLink
          WordNode \"word\"
          WordNode \"bird\"

  After various counts, frequencies, entropies, etc pertaining to
  this particular pair are computed, they will be hung, as values,
  on the above EvaluationLink.

  The 'get-pair method returns the above EvaluationLink, if it exists.
  The 'make-pair method will create it, if it does not exist.

  Left-side counts, frequencies, etc. such as N(*,y) P(*,y) or
  log_2 P(*,y) will be placed on the following, which is returned
  by the 'left-wildcard method:

    EvaluationLink
       LgLinkNode \"ANY\"
       ListLink
          AnyNode \"left-word\"
          WordNode \"bird\"

  The corresponding N(x,*) P(x,*) etc are hung on the atom returned
  by the 'right-wildcard method:

    EvaluationLink
       LgLinkNode \"ANY\"
       ListLink
          WordNode \"word\"
          AnyNode \"right-word\"

  Finally, the 'left-type and 'right-type methods return the type
  of the the two sides of the pair.
"
	; Just use the generic code to implement the above.
	(make-evaluation-pair-api
		(LgLinkNode "ANY")
		'WordNode
		'WordNode
		(AnyNode "left-word")
		(AnyNode "right-word")
		"ANY"
		"Link Grammar ANY link Word Pairs")
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

;; Call the function only once, ever.
;; The database loads are slow, so don't repeat them, if they are
;; not needed.
(define call-only-once
	(let ((called '()))
		(lambda (func)
			(if (not (member func called))
				(begin (func)
					(set! called (cons func called))))))
)

; ---------------------------------------------------------------------
; Handy-dandy main entry points.

(define-public (batch-pairs LLOBJ)

	; Make sure all item-pairs are in the atomspace.
	(call-only-once (lambda() (LLOBJ 'fetch-pairs)))
	(display "Finished loading sparse matrix pairs\n")

	(cog-report-counts)
	(batch-all-pair-mi LLOBJ)
	(print-matrix-summary-report LLOBJ)
)

; ---------------------------------------------------------------------
; misc unit-test-by-hand stuff
;
; (use-modules (opencog) (opencog persist) (opencog persist-sql))
; (use-modules (opencog nlp) (opencog learn))
; (sql-open "postgres:///en_pairs_tone_mst?user=linas")
; (use-modules (opencog cogserver))
; (start-cogserver "opencog2.conf")
; (load-atoms-of-type 'WordNode)
;
; (define wc (cog-count-atoms 'WordNode))
; (length (cog-get-atoms 'WordNode))
; (define wc (get-total-atom-count (cog-get-atoms 'WordNode)))
; Should match 
; SELECT sum(valuations.floatvalue[3]) FROM valuations, atoms, typecodes
; WHERE valuations.atom=atoms.uuid AND atoms.type=typecodes.type
; AND typecodes.typename='WordNode';
;
; If it all looks good, then:
; (batch-pairs (add-count-api (make-any-link-api)))
;
; (define wtfl  (EvaluationLink  (LgLinkNode "ANY")
;   (ListLink (AnyNode "left-word") (WordNode "famille"))))
;
; (define wtfr  (EvaluationLink  (LgLinkNode "ANY")
;     (ListLink (WordNode "famille") (AnyNode "right-word"))))
;
; anynode is type 105
;  select * from atoms where type=105;
; uuid is 43464152
;         43464155
;
; select count(*) from atoms where outgoing @> ARRAY[cast(43464152 as bigint)];
; returns the number of word-pairs which we've wild-carded.
; select * from atoms where outgoing = ARRAY[cast(43464152 as bigint), cast(43464155 as bigint)];
