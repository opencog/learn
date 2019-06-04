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

  The matrix consists of (word-class,disjunct) pairs (a 'disjunct' and a
  'cset' or 'connector set' are all different names for the same thing).
  The word-classes (grammatical classes) appear as rows of the matrix;
  the disjuncts as columns.

  Recall that word classes are marked with a 'WordClassNode', and that
  the membership of a word to a WordClass is denoted as

      (MemberLink (WordNode \"foo\") (WordClassNode \"bar\"))

  Keep in mind that a word might belong to more than one WordClass.

  For a detailed description, see the `pseudo-csets.scm` file.
"
	(let ((all-csets '()))

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
			args)))
)

; ---------------------------------------------------------------------
