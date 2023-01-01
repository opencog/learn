;
; batch-clique-pair.scm
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
(define-public (make-item-pair-api)
"
  make-item-pair-api -- Item-pair access methods for generic item pairs.

   The counts are obtained from EvaluationLinks of the form
      (EvaluationLink
          (PredicateNode \"*-Item Pair-*\")
          (List left-atom right-atom))
"
	; Just use the generic code to implement the above.
	(make-evaluation-pair-api
		*-item-pair-tag-* ; defined as (PredicateNode "*-Item Pair-*")
		'ItemNode
		'ItemNode
		(AnyNode "left-item")
		(AnyNode "right-item")
		"item-pairs"
		"Generic ItemNode Pairs")
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Clique-based-counting word-pair access methods.
; ---------------------------------------------------------------------

;
(define-public (make-clique-pair-api)
"
  make-clique-pair-api -- Word-pair access methods from clique-counting.

  Object for getting word-pair counts, obtained from clique counting.
  The counts are stored on EvaluationLinks with the predicate
  (PredicateNode \"*-Sentence Word Pair-*\").
"
	; Just use the generic code to implement the above.
	(make-evaluation-pair-api
		*-word-pair-tag-* ;; defined as (Predicate "*-Sentence Word Pair-*")
		'WordNode
		'WordNode
		(AnyNode "left-word")
		(AnyNode "right-word")
		"cliq"
		"Sentence Clique Word Pairs")
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Clique-based-counting length-limited word-pair access methods.
; ---------------------------------------------------------------------

;
(define-public (make-distance-pair-api MAX-DIST)
"
  make-distance-pair-api -- Word-pair access methods from
  clique-counting, but limited by edge-length.

  Object for getting word-pair counts, obtained from clique counting.
  The counts are considered only if the distance between the words
  was observed to be MAX-DIST or less.

  The counts are stored in the form of ExecutionLinks:

    ExecutionLink
       SchemaNode  *-Pair Distance-*
       ListLink
          WordNode lefty
          WordNode righty
       NumberNode 3

"
	(let* ((max-dist MAX-DIST)
			(dist-name (format #f "*-Pair Max Dist ~A-*" max-dist))
			(pair-max (PredicateNode dist-name)))

		; Get the observational count on ATOM.
		(define (get-count ATOM) (cog-count ATOM))
		(define (set-count ATOM CNT)
			(cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

		; Get the numeric distance from the ExecutionLink
		(define (get-dist ATOM)
			(cog-number (cog-outgoing-atom ATOM 2)))

		(define any-left (AnyNode "left-word"))
		(define any-right (AnyNode "right-word"))

		(define (get-left-type) 'WordNode)
		(define (get-right-type) 'WordNode)
		(define (get-pair-type) 'EvaluationLink)

		; Return the atom holding the count, if it exists, else
		; return nil.
		(define (get-pair L-ATOM R-ATOM)
			(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
			(if (null? maybe-list) '()
				(cog-link 'EvaluationLink pair-max maybe-list)))

		; Create an atom to hold the count (if it doesn't exist already).
		(define (make-pair L-ATOM R-ATOM)
			(EvaluationLink pair-max (ListLink L-ATOM R-ATOM)))

		; Return the left member of the pair. Given the pair-atom,
		; locate the left-side atom.
		(define (get-left-element PAIR)
			(gadr PAIR))
		(define (get-right-element PAIR)
			(gddr PAIR))

		; Return the raw observational count on PAIR.
		; If the PAIR does not exist (was not observed) return 0.
		; Return a list of atoms that hold the count.
		(define (get-pair-count L-ATOM R-ATOM)
			(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
			(if (null? maybe-list) 0
				(fold
					(lambda (pr sum) (+ sum (get-count pr)))
					0
					(filter!
						(lambda (lnk) (<= (get-dist lnk) max-dist))
						(cog-incoming-by-type maybe-list 'ExecutionLink)))))

		(define (get-left-wildcard WORD)
			(make-pair any-left WORD))

		(define (get-right-wildcard WORD)
			(make-pair WORD any-right))

		(define (get-wild-wild)
			(make-pair any-left any-right))

		; fetch-distance-pairs -- fetch all counts for distance-pairs
		; from the database.
		(define (fetch-distance-pairs)
			(define start-time (current-time))
			(fetch-incoming-set *-word-pair-dist-*)
			(format #t "Elapsed time to load distance pairs: ~A secs\n"
				(- (current-time) start-time)))

		; Tell the stars object what we provide.
		(define (provides meth)
			(case meth
				((pair-count)     get-pair-count)
				((get-pair)       get-pair)
				((get-count)      get-count)
				((set-count)      set-count)
				((make-pair)      make-pair)
				((left-element)   get-left-element)
				((right-element)  get-right-element)
				(else             #f)))

		; Methods on the object
		(lambda (message . args)
			(apply (case message
					((name) (lambda () "Sentence Clique Distance-Limited Word Pairs"))
					((id)   (lambda () "cldist"))
					((left-type) get-left-type)
					((right-type) get-right-type)
					((pair-type) get-pair-type)
					((pair-count) get-pair-count)
					((get-pair) get-pair)
					((get-count) get-count)
					((set-count) set-count)
					((make-pair) make-pair)
					((left-element) get-left-element)
					((right-element) get-right-element)
					((left-wildcard) get-left-wildcard)
					((right-wildcard) get-right-wildcard)
					((wild-wild) get-wild-wild)
					((fetch-pairs) fetch-distance-pairs)
					((provides) provides)
					((filters?) (lambda () #f))
					(else (error "Bad method call on clique-pair:" message)))
				args))))


; ---------------------------------------------------------------------

(define-public (verify-clique-pair-sums)
"
  This checks consistency of the the clique-pair total count, with
  the subcounts of each pair, according to the distance between
  the words. The sum of the subtotals should equal the total.
  It should not throw.

  Example usage: (verify-clique-pair-sums)
"
	(define cliq (make-clique-pair-api))
	(define dist (make-distance-pair-api 10000000))
	(define all-pairs (cliq 'get-all-elts))

	(define cnt 0)
	(for-each
		(lambda (PAIR)
			(set! cnt (+ cnt 1))
			(if (not (eqv? (cliq 'get-count PAIR) (dist 'get-count PAIR)))
				(throw 'bad-count 'foobar PAIR)
				(format #t "Its OK ~A\n" cnt)
			))
		all-pairs)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

; ---------------------------------------------------------------------
; Handy-dandy main entry points.

(define-public (batch-any-pairs)
	(batch-pairs (make-any-link-api))
)

(define-public (batch-clique-pairs)
	(batch-pairs (make-clique-pair-api))
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
; (batch-pairs (make-any-link-api))
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
