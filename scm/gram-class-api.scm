;
; gram-class-api.scm
;
; Representing word-classes as vectors of (pseudo-)connector-sets.
;
; Copyright (c) 2017, 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file provide the "matrix-object" API that allows grammatical
; classes of words to be treated as vectors of connector-sets (vectors
; of disjuncts; vectors of Sections).
;
; This is effectively the same thing as `make-pseudo-cset-api`, except
; that the rows of the matrix correspond to `WordClassNodes` instead
; of `WordNodes`.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

(define-public (make-gram-class-api)
"
  make-gram-class-api -- Create a wordclass-disjunct matrix.

  The matrix consists of (word-class, disjunct) pairs (a 'disjunct' and a
  'cset' or 'connector set' are all different names for the same thing).
  The word-classes (grammatical classes) appear as rows of the matrix;
  the disjuncts as columns.

  Recall that word classes are marked with a 'WordClassNode', and that
  the membership of a word to a WordClass is denoted as

      (MemberLink (WordNode \"foo\") (WordClassNode \"bar\"))

  Keep in mind that a word might belong to more than one WordClass.

  This class performs no self-consistency checking; it simply provides
  access to all elements of the form

     (Section (WordClassNode ...) (ConnectorSeq ...))

  that can be found in the atomspace. In particular, the ConnectorSeq
  may reference words that are not in some WordClass.  Thus, you may
  want to layer some filtering on top of this, to get a self-consistent
  network.

  For a detailed description, see the `pseudo-csets.scm` file.
"

	; Get the observational count on ATOM
	(define (get-count ATOM) (cog-count ATOM))

	(define any-left (AnyNode "gram-class-word"))
	(define any-right (AnyNode "gram-class-disjunct"))

	(define (get-left-type) 'WordClassNode)
	(define (get-right-type) 'ConnectorSeq)
	(define (get-pair-type) 'Section)

	; Get the pair, if it exists.
	(define (get-pair L-ATOM R-ATOM)
		(cog-link 'Section L-ATOM R-ATOM))

	; Get the count, if the pair exists.
	(define (get-pair-count L-ATOM R-ATOM)
		(define stats-atom (get-pair L-ATOM R-ATOM))
		(if (null? stats-atom) 0 (get-count stats-atom)))

	(define (make-pair L-ATOM R-ATOM)
		(Section L-ATOM R-ATOM))

	(define (get-left-element PAIR) (gar PAIR))
	(define (get-right-element PAIR) (gdr PAIR))

	(define (get-left-wildcard DJ)
		(ListLink any-left DJ))

	(define (get-right-wildcard WRD-CLS)
		(ListLink WRD-CLS any-right))

	(define (get-wild-wild)
		(ListLink any-left any-right))

	; Fetch (from the database) all disjuncts
	(define (fetch-disjuncts)
		(define start-time (current-time))
		; marginals are located on any-left, any-right
		(fetch-incoming-set any-left)
		(fetch-incoming-set any-right)
		; Fetch only the Sections that have a WordClass in them,
		; and not the others.
		(load-atoms-of-type 'WordClassNode)
		(for-each fetch-incoming-set (cog-get-atoms 'WordClassNode))
		(format #t "Elapsed time to load grammatical classes: ~A secs\n"
			(- (current-time) start-time)))

	; Methods on the object
	(lambda (message . args)
		(apply (case message
			((name) (lambda () "WordClass-Disjunct Pairs"))
			((id)   (lambda () "gram-class"))
			((left-type) get-left-type)
			((right-type) get-right-type)
			((pair-type) get-pair-type)
			((pair-count) get-pair-count)
			((get-pair) get-pair)
			((get-count) get-count)
			((make-pair) make-pair)
			((left-element) get-left-element)
			((right-element) get-right-element)
			((left-wildcard) get-left-wildcard)
			((right-wildcard) get-right-wildcard)
			((wild-wild) get-wild-wild)
			((fetch-pairs) fetch-disjuncts)
			((provides) (lambda (symb) #f))
			((filters?) (lambda () #f))
			(else (error "Bad method call on gram-class-api:" message)))
		args))
)

; ---------------------------------------------------------------------

(define-public (add-singletons LLOBJ)
"
"
)

; ---------------------------------------------------------------------

(define-public (add-wordclass-filter LLOBJ)
"
  add-wordclass-filter LLOBJ - Modify the wordclass-disjunct LLOBJ so
  that the only connector sequences appearing on the right consist
  entirely of connectors that have words in word-classes appearing on
  the left. The resulting wordclass-disjunct is then self-consistent,
  and does not contain any connectors unable to form a connection to
  some word-class.
"
	(define stars-obj (add-pair-stars LLOBJ))

	; Always keep any WordClassNode we are presented with.
	(define (left-basis-pred WRDCLS) #t)

	; ---------------
	; Cached set of valid connector-seqs, for the right-basis-pred.
	; We need to know if every connector in a connector sequence is
	; a member of some word-class. Verifying this directly is very
	; inefficient. It is much faster to precompute the set of known-
	; good connector sequences, and refer to that.

	; Return a list of words in word-classes
	(define (get-wordclass-words)
		(define word-set (make-atom-set))
		(for-each
			(lambda (wcls)
				(for-each word-set
					(map gar (cog-incoming-by-type wcls 'MemberLink))))
			(cog-get-atoms 'WordClassNode))
		(word-set #f)
	)

	; Return a list of connectors containing a word in the list
	(define (get-connectors WRD-LST)
		(define con-set (make-atom-set))
		(for-each
			(lambda (word)
				(for-each con-set
					(cog-incoming-by-type word 'Connector)))
			WRD-LST)
		(con-set #f)
	)

	; Return a list of ConnectorSeq containing one or more
	; connectors from the CON-LST
	(define (get-con-seqs CON-LST)
		(define seq-set (make-atom-set))
		(for-each
			(lambda (ctr)
				(for-each seq-set
					(cog-incoming-by-type ctr 'ConnectorSeq)))
			CON-LST)
		(seq-set #f)
	)

	; Create a predicate that returns true if a given ConnectorSeq
	; consists of WordNodes eintirely in some WordClass.  That is,
	; this returns that predicate. This may take a few minutes to
	; run, if there are millions of ConnectorSeq's.
	(define (make-conseq-predicate)
		; Unwanted words are not part of the matrix.
		(define unwanted-words
			(atoms-subtract
				(cog-get-atoms 'WordNode) (get-wordclass-words)))

		(define unwanted-cnctrs
			(get-connectors unwanted-words))

		(define unwanted-conseqs
			(get-con-seqs unwanted-cnctrs))

		(define good-conseqs
			(atoms-subtract
				; Take all connector-sequences, and subtract the bad ones.
				; (cog-get-atoms 'ConnectorSeq)
				(LLOBJ 'right-basis)
				unwanted-conseqs))

		; Return the predicate that returns #t only for good ones.
		(make-aset-predicate good-conseqs)
	)

	(define ok-conseq? #f)

	; Only accept a ConnectorSeq if every word in every connector
	; is in some word-class.
	(define (right-basis-pred CONSEQ)
		(if (not ok-conseq?) (set! ok-conseq? (make-conseq-predicate)))
		(ok-conseq? CONSEQ)
	)

	; Always keep any Section that passed the duals test.
	(define (pair-pred WRDCLS) #t)

	(define id-str "wordclass-filter")

	; ---------------
	(add-generic-filter LLOBJ
		left-basis-pred right-basis-pred pair-pred id-str #f)
)

; ---------------------------------------------------------------------
