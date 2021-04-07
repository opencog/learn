;
; shape-vec.scm
;
; Representing connector-words as vectors over shapes (word-shape pairs)
;
; Copyright (c) 2018, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file provides a "matrix-object" API that allows words to be
; treated as vectors, with the word taken to live inside of a connector,
; and the vector basis ranging over all section-shapes (that contain
; that word in a connector).
;
; A key idea in grammatical classification is that words can be treated
; as vectors, and thus, vector-style algorithms can be applied to them.
; There are many kinds of vectors that are possible:
; -- N-grams, where a word is associated with a vector of counts of
;    the neighboring N words.
; -- skip-grams, as above, but some words are skipped.
; Neither of the above are implemented anywhere in this directory, but
; they convey the idea of a word-vector.
;
; Another vector is a word-disjunct vector, where the word is associated
; with a vector of counts of how often a disjunct is associated with it.
; This is implemented in `pseudo-csets.scm`.  Its a good place to start.
; The word-disjunct vectors behave a whole lot like skip-grams, for many
; practical purposes.
;
; The vector *IN THIS FILE* is between a word living in a connector,
; and all sections that contain that connector. This allows one to look
; at how contexts cross over to other linked words.  It is looking at
; disjuncts from the connector point of view.
;
; Consider, for example, the word "level". It can appear in a connector
; both as
;
;    (Connector (WordNode "level") (ConnectorDir "-"))
;
; and as
;
;    (Connector (WordNode "level") (ConnectorDir "+"))
;
; One of these connectors appears in the section
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
; and therefore, this section is paired with (WordNode "level")
;
; Left wild-cards
; ---------------
; In the above example, the corresponding left-wildcard for "level"
; would be (conceptually) the shape:
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (Variable "$wildcard")
;             (ConnectorDir "-"))
;          (Connector
;             (WordNode "field")
;             (ConnectorDir "+"))))
;
; I.e. with (Variable "$wildcard") replacing (WordNode "level")
; These wildcards are needed to store the left-marginals.  In practice,
; we don't want to pollute the namespace with ConnectorSeq's and
; Sections that have variables in them, so the actual representation is
; flattened. See below for its actual form.
;
; Its convenient to give these the name of "shape".
;
; Pairs
; -----
; In order to track statistics, including the entropies and the mutual
; information, pairs consisting of a word, and the left wild-card
; ("shape") must be created. The section will not do for this purpose,
; because the section is ambiguous as to the pairing: multiple different
; word-shape pairs correspond to a single section.  Basically, if a
; connector sequence has N connectors in it, there are N shapes, and
; N word-shape pairs, but only one associated section.
;
; Using the above example: the shape will be
;
;    (Shape
;       (WordNode "playing")
;       (Connector
;          (Variable "$wildcard")
;          (ConnectorDir "-"))
;       (Connector
;          (WordNode "field")
;          (ConnectorDir "+"))))
;
; and the word-shape pair will be
;
;    (CrossSection
;       (WordNode "level")
;       (Shape ... the above shape))
;
; TODO: with appropriate cleanup, this probably should be moved
; to a generic "section" or "sheaf" module.  That is because it
; generically explodes a section into all of it's constituent
; connector-shape pairs, which is presumably something everyone
; will want to do. There's nothing special about WordNodes, here.
;
; The redesign requires passing in the correct object holding the
; sections that should be shaped.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
(define-public (add-shape-vec-api LLOBJ)
"
  add-shape-vec-api -- Provide API for CrossSections obtained from
  Sections. Assumes that LLOBJ provides an API that gives access to
  Sections.

  A more detailed description is at the top of this file.
"
	(let ((l-basis '())
			(r-basis '())
			(l-size 0)
			(r-size 0)

			; Temporary atomspace
			(mtx (make-mutex))
			(aspace (cog-new-atomspace (cog-atomspace)))
		)

		(define star-wild (Variable "$connector-word"))

		(define any-left (AnyNode "shape word"))
		(define any-right (AnyNode "shape section"))

		; Well, left-type can also be a WordClassNode, but we lie
		; about that, here.
		(define (get-left-type) 'WordNode)
		(define (get-right-type) 'ShapeLink)
		(define (get-pair-type) 'CrossSection)

		; Get the observational count on the word-shape pair
		(define (get-count SHAPE-PR) (cog-count SHAPE-PR))

		; L-ATOM is a WordNode or WordClassNode. R-ATOM is a shape.
		(define (get-pair L-ATOM R-ATOM)
			(cog-link 'CrossSection L-ATOM R-ATOM))

		; As above, but force the creation of the pair.
		(define (make-pair L-ATOM R-ATOM)
			(CrossSection L-ATOM R-ATOM))

		; Get the left and right parts of the pair.
		; The zeroth atom is the predicate.
		(define (get-pair-left SHAPE-PR)
			(cog-outgoing-atom SHAPE-PR 0))

		(define (get-pair-right SHAPE-PR)
			(cog-outgoing-atom SHAPE-PR 1))

		; Create the section corresponding to the word-shape pair.
		; That is, unexplode (implode?) the word-shape pair back
		; into a section, again. This is a projection from the
		; entire space of exploded word-shape pairs to the
		; base-space of sections.
		;
		; Disassemble the SHAPE, insert WORD into the variable
		; location, and return the Section. Note that a Section
		; always exists, because it was impossible to make a shape,
		; without having had the underlying section that it reduces to.
		; See (explode-sections) below for documentation
		; about the structure of the shape.
		(define (get-section SHAPE-PR)
			(define WORD (second SHAPE-PR))
			(define SHAPE (third SHAPE-PR))
			(define tmpl (cdr (cog-outgoing-set SHAPE)))
			(define point (car tmpl))
			(define conseq (cdr tmpl))
			(define (not-var? ITEM) (not (equal? (gar ITEM) star-wild)))
			(define begn (take-while not-var? conseq))
			(define rest (drop-while not-var? conseq))
			(define dir (gdr (car rest)))
			(define end (cdr rest))
			(define ctcr (Connector WORD dir))
			(define cseq (ConnectorSeq begn ctcr end))
			(LLOBJ 'make-pair point cseq))

		; Get the count, if the pair exists.
		(define (get-pair-count L-ATOM R-ATOM)
			(define sect (get-pair L-ATOM R-ATOM))
			(if (null? sect) 0 (get-count sect)))

		; Use ListLinks for the wild-cards.
		(define (get-right-wildcard WORD)
			(ListLink WORD any-right))

		; The left-wildcard really should be
		; (ListLink any-left R-ATOM) but we've already
		; blown too much storage creating atoms, so keep
		; it simple, here.
		(define (get-left-wildcard R-ATOM) R-ATOM)

		(define (get-wild-wild)
			(ListLink any-left any-right))

		; -------------------------------------------------------
		; Stars API.
		; Get both the Words and the WordClasses; put WordClasses first.
		(define (get-left-basis)
			(if (null? l-basis) (set! l-basis
				(append! (cog-get-atoms 'WordClassNode) (cog-get-atoms 'WordNode))))
			l-basis)

		(define (get-right-basis)
			(if (null? r-basis) (set! r-basis (cog-get-atoms 'ShapeLink)))
			r-basis)

		(define (get-left-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		; Invalidate the caches
		(define (clobber)
			(set! l-basis '())
			(set! r-basis '())
			(set! l-size 0)
			(set! r-size 0)
			(if (LLOBJ 'provides 'clobber) (LLOBJ 'clobber))
		)

		; -------------------------------------------------------
		; Create all of the word-shape pairs that correspond to a
		; section. This explodes a section into all of the word-shape
		; pairs that cover it (in the sense of a "covering space").
		; Basically, given a Section, it walks over the ConnectorSeq
		; inside of it, replaces each word with a variable (to define
		; the shape) and then creates a pair consisting of that word,
		; and that shape.
		;
		; We copy the observation count from the observation count on
		; the section. This is the "right thing to do", because every
		; observation of a section is also an observation of every shape
		; in that section, and so we can weight all of these equally.
		; A case can be made for an alternative: the obsservation of
		; connectors. In this case, the count on the shapes (and the
		; count on the sections!) should be devided by the arity of the
		; disjunct. But that would not alter the counts here.
		;
		; Note that the shapes will hold marginal counts.
		;
		; This does not need to be done, if restoring from the database;
		; viz if the pairs were previously stored, and now have been
		; fetched with 'fetch-pairs above.
		;
		; Conceptually, the shapes (left-stars) are of the form:
		; (Section (Word "foo") (ConnectorSeq
		;     (Connector (Word "bar") (ConnectorDir "-"))
		;     (Connector (Variable $X) (ConnectorDir "-))))
		; where (Variable $X) is the wildcard.  However, we want to
		; avoid using both ConnectorSeq and Section directly, because
		; these pollute the space of data. So, the above gets encoded
		; as
		; (Shape (Word "foo")
		;     (Connector (Word "bar") (ConnectorDir "-"))
		;     (Connector (Variable $X) (ConnectorDir "-)))
		; with the left-word "foo" heading up the list.
		; This can be easily dis-assembled to run actual queries against
		; the atomspace.
		(define (explode-sections)

			; Walk over a section, and insert a wild-card.
			(define (explode-section SEC)
				; The root-point of the seed
				(define point (gar SEC))
				; The list of connectors
				(define cncts (cog-outgoing-set (gdr SEC)))
				(define num-cncts (length cncts))

				; Copy the count. All shapes have the same count as
				; the section itself. XXX should use getter on section.
				; (define weight (CountTruthValue 1 0 (cog-count SEC)))
				(define weight (cog-tv SEC))

				; Place the wild-card into the N'th location of the section.
				; Of course, this creates the section, if it does not yet
				; exist. Well, we don't want to create actual sections; that
				; would screw up other code that expects sections to not
				; have wildcards in them. So we are creating ShapeLinks
				; instead.
				(define (insert-wild N)
					(define front (take cncts N))
					(define back (drop cncts N))
					(define ctr (car back)) ; the connector being exploded
					(define wrd (gar ctr))  ; the word being exploded
					(define dir (gdr ctr))  ; the direction being exploded
					(define wild (Connector star-wild dir))
					(CrossSection wrd
						(Shape point front wild (cdr back))
						weight))

				; Create all the wild-cards for this section.
				; (map insert-wild (iota num-cncts))
				(for-each insert-wild (iota num-cncts))
			)

			; Ask the LLOBJ for all Sections.
			(define start-time (current-time))
			(for-each explode-section (LLOBJ 'get-all-elts))
			(format #t "Elapsed time to create shapes: ~A secs\n"
				(- (current-time) start-time))
		)

#! ========================  XXX THE CODE BELOW IS DEAD CODE
Hang on ... how dead is this, really? I think we need to provide
the ability to get left-duals and stars that have WordClassNodes
in them, and not just WordNodes... so this code needs to be fixed
up, right?  Or do we just get lucky, and everything works right?
I'm confused ...

I'm going to keep this code here for a while, because it does some
interesting pattern matching to mine out section wild-cards that
correspond to words, and to shapes. However, this is no longer
needed, in the current implementation. It might be needed in the
future, and it was tricky to write and debug, so I am keeping it
around for a while.
		; -------------------------------------------------------
		; The right-stars of a WordNode are all of the Sections in
		; which that Word appears in some Connector. A BindLink is
		; used to get those sections. This is done in a private,
		; temporary atomspace, to avoid polluting the main atomspace
		; with junk. Its also done RAII style, so that ctrl-C does
		; not leave things in a fumbled state.
		;
		; Run a query, and return all Sections that contain WORD
		; in a Connector, some Connector, any Connector.
		;
		; Use Put-Get instead of BindLink, so that mutiple distinct
		; Get's that might generate identical Put's are generated
		; correctly (i.e. with multiplicity)
		(define (right-stars-query WORD)
			; The WORD must occur somewhere, anywhere in a conector.
			(define body (Section
				(Variable "$point")
				(ConnectorSeq
					(Glob "$begin")
					(Connector WORD (Variable "$dir"))
					(Glob "$end"))))

			(define vardecl (VariableList
				(TypedVariable (Variable "$point")
					(TypeChoice (Type 'WordClassNode) (Type 'WordNode)))
				(TypedVariable (Glob "$begin") (Interval (Number 0) (Number -1)))
				(TypedVariable (Variable "$dir") (Type "ConnectorDir"))
				(TypedVariable (Glob "$end") (Interval (Number 0) (Number -1)))))

			; The types that are matched must be just-so.
			(Put body (Get vardecl body)))

		; Just like above, but return the shape, not the section.
		(define (right-duals-query WORD)
			; The WORD must occur somewhere, anywhere in a conector.
			(define body (Section
				(Variable "$point")
				(ConnectorSeq
					(Glob "$begin")
					(Connector WORD (Variable "$dir"))
					(Glob "$end"))))

			; We use the DontExec hack, as otherwise the execution of
			; this attempts to evaluate the resulting ShapeLink.
			; (XXX really? I don't think so...)
			(define shape (DontExec (Shape
				(Variable "$point")
				(Glob "$begin")
				(Connector star-wild (Variable "$dir"))
				(Glob "$end"))))

			(define vardecl (VariableList
				(TypedVariable (Variable "$point")
					(TypeChoice (Type 'WordClassNode) (Type 'WordNode)))
				(TypedVariable (Glob "$begin") (Interval (Number 0) (Number -1)))
				(TypedVariable (Variable "$dir") (Type "ConnectorDir"))
				(TypedVariable (Glob "$end") (Interval (Number 0) (Number -1)))))

			; The types that are matched must be just-so.
			(Put (VariableList
				(Variable "$point")
				(Glob "$begin")
				(Variable "$dir")
				(Glob "$end")) shape (Get vardecl body)))

		;-------------------------------------------
		; The left-stars consist of all Sections of a fixed shape,
		; that shape given by R-ATOM, but with any word occuring
		; in the connector-location in that shape.
		(define (left-stars-query R-ATOM)
			(define body (make-pair star-wild R-ATOM))

			; The types that are matched must be just-so.
			(Put body (Get (TypedVariable star-wild
					(TypeChoice (Type 'WordClassNode) (Type 'WordNode)))
				body)))

		; Just like above, but return only the words, not the Sections.
		(define (left-duals-query R-ATOM)
			(Get (TypedVariable star-wild
					(TypeChoice (Type 'WordClassNode) (Type 'WordNode)))
				(make-pair star-wild R-ATOM)))

		;-------------------------------------------
		; Explain the non-default provided methods.
		(define (provides meth)
			(case meth
				((left-basis)         get-left-basis)
				((right-basis)        get-right-basis)
				((left-basis-size)    get-left-size)
				((right-basis-size)   get-right-size)
				((left-star-pattern)  left-stars-query)
				((right-star-pattern) right-stars-query)
				((left-dual-pattern)  left-duals-query)
				((right-dual-pattern) right-duals-query)

				((make-left-stars)    explode-sections)
				(else #f)
		))
========================  XXX THE CODE ABOVE IS DEAD CODE
!#
		;-------------------------------------------
		; Fetch (from the database) all sections,
		; as well as all the marginals.
		(define (fetch-sections)
			(define start-time (current-time))
			; marginals are located on any-left, any-right
			(fetch-incoming-set any-left)
			(fetch-incoming-set any-right)
			(LLOBJ 'fetch-pairs)
			(format #t "Elapsed time to load word sections: ~A seconds\n"
				(- (current-time) start-time))
			(set! start-time (current-time))
			(load-atoms-of-type 'CrossSection)
			(format #t "Elapsed time to load word-shape pairs: ~A seconds\n"
				(- (current-time) start-time))

			; We need to also fetch the shapes. There are two things
			; we could do here: fetch them from RAM, or just re-create
			; them. For now, just re-creating them seems to be suficient.
			; This does lose any kinds of distinct data that may have been
			; stored on the sections. It also clobbers the counts, and
			; fails to restore frequencies... is this OK? I'm confused.
			(explode-sections)
		)

		;-------------------------------------------
		(define (describe)
			(display (procedure-property add-shape-vec-api 'documentation)))

		;-------------------------------------------
		; Explain the non-default provided methods.
		(define (provides meth)
			(case meth
				((left-basis)         get-left-basis)
				((right-basis)        get-right-basis)
				((left-basis-size)    get-left-size)
				((right-basis-size)   get-right-size)
				((clobber)            clobber)
				(else #f)
		))

		; Methods on the object
		(lambda (message . args)
			(apply (case message
				((name)       (lambda () "Cross-section Words"))
				((id)         (lambda () "cross-section"))
				((left-type)        get-left-type)
				((right-type)       get-right-type)
				((pair-type)        get-pair-type)
				((pair-count)       get-pair-count)
				((get-pair)         get-pair)
				((get-count)        get-count)
				((make-pair)        make-pair)
				((left-element)     get-pair-left)
				((right-element)    get-pair-right)
				((left-wildcard)    get-left-wildcard)
				((right-wildcard)   get-right-wildcard)
				((wild-wild)        get-wild-wild)
				((fetch-pairs)      fetch-sections)

				; Custom call. These need to be explicitly made.
				((explode-sections) explode-sections)
				((get-section)      get-section)

				((provides)         provides)
				((clobber)          clobber)
				((filters?)         (lambda () #f))

				((describe)         (describe))
				(else (error "Bad method call on cross-section:" message)))
			args))
))

; ---------------------------------------------------------------------
; Example usage:
;
; (define cva (add-shape-vec-api (make-pseudo-cset-api)e))
; (cva 'fetch-pairs)
; (define cvs (add-pair-stars cva))
; (cvs 'left-basis-size)
;
; (cvs 'right-stars (Word "wiped"))
; (cvs 'right-stars (Word "ride"))
