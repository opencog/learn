;
; pseudo-csets.scm
;
; Representing words as vectors of (pseudo-)connector-sets.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file provide the "matrix-object" API that allows words to be
; treated as vectors of connector-sets (vectors of disjuncts; vectors
; of Sections).  The vector representation is important, because vectors
; are the core input to a number of important machine-learning
; algorithms.  Most immediately and directly, one can take the cosine
; product between vectors (see `word-cosine.scm` for examples).
;
; This just provides the vector API, and nothing more. Other objects,
; as a part of the `(opencog matrix)` suite, do various things with the
; vector (such as compute cosines, perform other similarity measures,
; filter out unwanted/bad components, etc.)
;
; Here and below, "connector set" and "connector sequence" mean the same
; thing. The word "disjunct" alternately refers to either a connector
; sequence, or a "Section", the difference being that a Section
; indicates what word the connector sequence is attached to.
;
; A vector can be decomposed into a sum over "basis elements", as
; follows:
;
;     v = a_1 e_1 + a_2 e_2 + ... + a_n e_n
;
; where each a_k is a (real) number, and the e_k are the "basis
; elements".  Here, each distinct e_k corresponds to each distinct
; ConnectorSeq atom.  Each number a_k corresponds to the observational
; frequency, stored as a Value on the Section atom.  There is one
; such vector for each word, and thus, the set of all words can be
; treated as a matrix: each column of the matrix is a word-vector.
; Alternately, one can think of this as a collection of pairs, of
; the form (word, cset), and a count/frequency for each pair. The
; left side of the pair are WordNodes; the right side are ConnectorSeq's
; and the pair itself is a Section (which is why the counts are stored
; on the Section).  That is, each pair is of the form:
;
;    (Section
;       (WordNode "foobar")
;       (ConnectorSeq ... )
;
; An example connector-set, for the word "playing", illustrating
; that it can connect to the word "level" on the left, and "field"
; on the right, is shown below. (Please note that this is for
; illustrating the atomese structure, and not for illustrating actual
; English grammar; its a bad grammatical example, although the MST
; parser does find things like this.)
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (WordNode "level")
;             (ConnectorDir "-"))
;          (Connector
;             (WordNode "field")
;             (ConnectorDir "+"))))
;
; In dictionary short-hand, this would be written as
;
;     playing:  level- & field+;
;
; The `ConnectorSeq` part of this structure is referred to as the
; pseudo-disjunct, in that it resembles a normal link-grammar
; disjunct, but has words appearing where connectors should be.
;
; Any given word may have dozens or hundreds or thousands of these
; sections. The totality of these sections, for a given, fixed word
; form a vector.  The connector seq (the disjunct) is a basis element,
; and the raw observational count on the `Section` is the magnitude
; of the vector in that basis direction.
;
; Note that these vectors are sparse: if a particular disjunct is
; missing, then the associated count is zero.  Note that the dimension
; of the vector-space is extremely high, possibly in the
; tens-of-millions, while a "typical" English word might have a few
; thousand non-zero sections.
;
; Note: At this time, there can only ever be one such object in the
; entire AtomSpace. There is nothing in here to distinguish one
; collection of sections from another. I suppose that this should be
; fixed, but so far, there has not been any need for this.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

(define-public (make-pseudo-cset-api)
"
  make-pseudo-cset-api -- Create a word-disjunct matrix.

  The matrix consists of (word,disjunct) pairs (a 'disjunct' and a
  'cset' or 'connector set' are all different names for the same thing).
  The words appear as rows of the matrix; the disjuncts as columns.

  For a detailed description, see the `pseudo-csets.scm` file.
"
	(define any-left (AnyNode "cset-word"))
	(define any-right (AnyNode "cset-disjunct"))

	(define (get-left-type) (Type 'WordNode))
	(define (get-right-type) (Type 'ConnectorSeq))
	(define (get-pair-type) (Type 'Section))

	; Get the pair, if it exists.
	(define (get-pair L-ATOM R-ATOM)
		(cog-link 'Section L-ATOM R-ATOM))

	(define (make-pair L-ATOM R-ATOM)
		(Section L-ATOM R-ATOM))

	(define (get-left-element PAIR) (gar PAIR))
	(define (get-right-element PAIR) (gdr PAIR))

	(define (get-left-wildcard DJ)
		(ListLink any-left DJ))

	(define (get-right-wildcard WORD)
		(ListLink WORD any-right))

	(define (get-wild-wild)
		(ListLink any-left any-right))

	; Fetch (from the database) all pseudo-csets
	(define (fetch-pseudo-csets)
		(define start-time (current-time))

		; Marginals are located on a ListLink on any-left, any-right
		(fetch-incoming-by-type any-left 'ListLink)
		(fetch-incoming-by-type any-right 'ListLink)

		; Loading Sections is a bit too much, as that will also
		; pick up WordClassNodes. But I guess that is OK for now.
		(load-atoms-of-type 'Section)
		(format #t "Elapsed time to load csets: ~A secs\n"
			(- (current-time) start-time)))

	(define (describe)
		(display (procedure-property make-pseudo-cset-api 'documentation)))

	; Methods on the object
	(lambda (message . args)
		(apply (case message
			((name) (lambda () "Word-Disjunct Pairs (Connector Sets)"))
			((id)             (lambda () "cset"))
			((left-type)      get-left-type)
			((right-type)     get-right-type)
			((pair-type)      get-pair-type)
			((get-pair)       get-pair)
			((make-pair)      make-pair)
			((left-element)   get-left-element)
			((right-element)  get-right-element)
			((left-wildcard)  get-left-wildcard)
			((right-wildcard) get-right-wildcard)
			((wild-wild)      get-wild-wild)
			((fetch-pairs)    fetch-pseudo-csets)
			((provides)       (lambda (symb) #f))
			((filters?)       (lambda () #f))
			((help)           describe)
			((describe)       describe)
			((obj)            (lambda () "make-pseudo-cset-api"))
			((base)           (lambda () #f))
			(else (error "Bad method call on pseudo-cset:" message)))
		args))
)

; ---------------------------------------------------------------------
