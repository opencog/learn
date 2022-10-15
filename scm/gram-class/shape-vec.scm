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

(define (not-is-member? WRD CLS)
	(nil? (cog-link 'MemberLink WRD CLS))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
(define-public (add-shape-vec-api LLOBJ)
"
  add-shape-vec-api -- Provide API for CrossSections (word-shape pairs
  that correspond to Sections).  Assumes that LLOBJ provides an API
  that gives access to Sections.

  A CrossSection has the following form:
      (CrossSection
          germ  <-- this is a WordNode or a WordClassNode
          (Shape
             point  <-- this is a WordNode or a WordClassNode
             (ConnectorLink ...)
             (ConnectorLink ...)
             ...))

  A more detailed description is given in the `shape-vec.scm` file.

  In addition to the usual methods, this class also provides a
  collection of methods that are used to merge Sections and
  CrossSections by replacing specific Words by WordClasses both
  in the germ, and in the Connectors.  These are described below.

  'make-section CROSS  -- Create and return the section that corresponds
       to the CrossSection CROSS.

  'get-section CROSS  -- Return the section that corresponds to the
       CrossSection CROSS, if it exists.

  'get-cross-sections SECT -- Return all of the CrossSections that
       cover the Section SECT. This returns only those cross-sections
       that are already in the AtomSpace; it does not create them.

  'explode-sections -- create all possible CrossSections that correspond
       to existing Sections (on LLOBJ).  The count on each cross-section
       will be set to the count on the section. (This is the correct
       way to handle counts, if one wants clustering to commute with
       the creation of sections.)

  'implode-sections -- remove CrossSections so that they no longer appear
       in the basis.

  'make-flat CLS PNT -- Rewrite PNT, replacing occurances of any atoms
       belonging to CLS by CLS.

  'flatten CLS PNT -- Rewrite PNT, replacing occurances of any atoms
       belonging to CLS by CLS.  If PNT is already flat, (i.e. if there
       is nothing to be rewritten), then return #f. This just calls
       one of the two methods below, basecd on the type of PNT.

  'flatten-section CLS SECT -- Rewrite SECT, replacing the germ by CLS,
       and also any connectors that belong to CLS by the corresponding
       connector for CLS. If no connectors belong to CLS, then return #f.

  'flatten-cross CLS CROSS -- Rewrite CROSS, replacing the germ by CLS.
       If the 'point' belong to CLS, it is replaced by CLS. If any
       connectors belong to CLS, they are replaced by the corresponding
       connector for CLS. If neither the point, nor the connectors belong
       to CLS, then return #f.
"
	(let ((l-basis #f)
			(r-basis #f)
			(l-size 0)
			(r-size 0)
		)

		(define star-wild (Variable "$connector-word"))

		(define any-left (AnyNode "cross word"))
		(define any-right (AnyNode "cross shape"))

		; The types are all non-trivial.
		; We expect (LLOBJ 'right-type) to be (Type 'ConnectorSeq)
		; We expect (LLOBJ 'pair-type) to be (Type 'Section)
		(define (get-right-type)
			(TypeChoice (LLOBJ 'right-type) (Type 'ShapeLink)))
		(define (get-pair-type)
			(TypeChoice (LLOBJ 'pair-type) (Type 'CrossSection)))

		; Get the observational count on the word-shape pair
		(define (get-count SHAPE-PR) (cog-count SHAPE-PR))
		(define (set-count SHAPE-PR CNT)
			(cog-set-tv! SHAPE-PR (CountTruthValue 1 0 CNT)))

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

		; ------------------------------------------------
		; Analyze the CrossSection (the word-shape pair.)  Disasemble
		; it into it's key parts, with intent that these parts can be
		; assembled into the originating Section.
		;
		; See (explode-sections) below for documentation
		; about the structure of the shape.
		(define (analyze-xsection XSECT)
			(define SHAPE-PR (cog-outgoing-set XSECT))
			(define GERM (first SHAPE-PR))
			(define SHAPE (second SHAPE-PR))
			(define tmpl (cog-outgoing-set SHAPE))
			(define point (car tmpl))
			(define conseq (cdr tmpl))
			(define (not-var? ITEM) (not (cog-equal? (gar ITEM) star-wild)))
			(define begn (take-while not-var? conseq))
			(define rest (drop-while not-var? conseq))
			(define dir (gdr (car rest)))
			(define end (cdr rest))
			(list GERM dir begn end point))

		; Create the Section corresponding to the CrossSection
		; (the word-shape pair.)  That is, unexplode (implode?)
		; the CrossSection back into a Section, again. This can
		; be thought of as a projection from the entire space of
		; exploded word-shape pairs to the base-space of Sections.
		; (A projecting from the covering space to the base space).
		;
		; Disassemble the SHAPE, insert GERM into the variable
		; location, and return the Section.
		;
		; This code is typically used after flattening a CrossSection,
		; in order to make the Section that corresponds to the flattend
		; cross.
		(define (make-section XSECT)
			(define parts (analyze-xsection XSECT))
			(define GERM  (list-ref parts 0))
			(define dir   (list-ref parts 1))
			(define begn  (list-ref parts 2))
			(define end   (list-ref parts 3))
			(define point (list-ref parts 4))
			(define ctcr (Connector GERM dir))
			(define cseq (ConnectorSeq begn ctcr end))
			(LLOBJ 'make-pair point cseq))

		(define (get-section XSECT)
			(define parts (analyze-xsection XSECT))
			(define GERM  (list-ref parts 0))
			(define dir   (list-ref parts 1))
			(define begn  (list-ref parts 2))
			(define end   (list-ref parts 3))
			(define point (list-ref parts 4))
			(define ctcr (cog-link 'Connector GERM dir))
			(define cseq (if (nil? ctcr) '()
				(cog-link 'ConnectorSeq begn ctcr end)))
			(if (nil? cseq) '()
				(LLOBJ 'get-pair point cseq)))

		; Build a new CrossSection, by replacing the point of
		; the given XSECT by GLS. See above for the definition
		; of a "point".
		(define (re-cross GLS XSECT)
			(define SHAPE-PR (cog-outgoing-set XSECT))
			(define GERM (first SHAPE-PR))
			(define SHAPE (second SHAPE-PR))
			(define tmpl (cog-outgoing-set SHAPE))
			(define conseq (cdr tmpl))
			(CrossSection GERM (Shape GLS conseq)))

		; --------------------------------------------------

		; Rewrite SECT by replacing Connectors in the ConnectorSeq
		; that belong to CLS by CLS itself.
		;
		; Caution: after using this method, it is advisable that
		; the matching CrossSections should be created, with the
		; `make-cross-sections` method.
		(define (flatten-section CLS SECT)
			; conseq is the connector sequence
			(define conseq (cog-outgoing-set (get-pair-right SECT)))
			(define non-flat #f)

			; Walk through the connector sequence. If any of them
			; appear in the cluster, create a new connector sequence
			; with the cluster replacing that particular connector.
			(define newseq
				(map (lambda (con)
					(define clist (cog-outgoing-set con))
					(if (not-is-member? (car clist) CLS)
						con
						(begin (set! non-flat #t)
							(Connector CLS (cdr clist)))))
					conseq))

			(define germ (get-pair-left SECT))
			(define newgerm
				(if (not-is-member? germ CLS) germ
					(begin (set! non-flat #t) CLS)))

			; Are any of the connectors in the cluster? If so, then
			; return the rewritten section; else return false.
			(if non-flat (LLOBJ 'make-pair newgerm (ConnectorSeq newseq)) #f))

		; --------------------------------------------------

		; Rewrite XSECT by replacing Connectors in the Shape
		; that belong to CLS by CLS itself.
		;
		; Caution: after using this method, it is advisable that the
		; matching Section should be created, with the `make-section`
		; method. And then, potentially one more step: to create the
		; other, matching cross-sections with `make-cross-sections`.
		(define (flatten-cross CLS XSECT)
			(define SHAPE-PR (cog-outgoing-set XSECT))
			(define germ (first SHAPE-PR))
			(define SHAPE (second SHAPE-PR))
			(define tmpl (cog-outgoing-set SHAPE))
			(define point (car tmpl))
			(define conseq (cdr tmpl))

			(define non-flat #f)
			(define newpoint
				(if (not-is-member? point CLS) point
					(begin (set! non-flat #t) CLS)))

			; Walk through the connector sequence. If any of them
			; appear in the cluster, create a new connector sequence
			; with the cluster replacing that particular connector.
			(define newseq
				(map (lambda (con)
					(define clist (cog-outgoing-set con))
					(if (not-is-member? (car clist) CLS)
						con
						(begin (set! non-flat #t)
							(Connector CLS (cdr clist)))))
					conseq))

			(define newgerm
				(if (not-is-member? germ CLS) germ
					(begin (set! non-flat #t) CLS)))

			; Was the point, or any of the connectors rewritten? If so, then
			; return the rewritten cross section; else return false.
			(if non-flat (CrossSection newgerm
				(Shape newpoint newseq)) #f))

		; --------------------------------------------------

		; Dispatch to one of the two cases above.
		(define (flatten CLS PNT)
			(if (equal? 'Section (cog-type PNT))
				(flatten-section CLS PNT)
				(flatten-cross CLS PNT)))

		; --------------------------------------------------

		; Like `flatten` but always returns something.
		(define (make-flat CLS PNT)
			(define flat (flatten CLS PNT))
			(if flat flat
				(let* ((germ (get-pair-left PNT))
						(newgerm
							(if (not-is-member? germ CLS) germ CLS))
						(DJ (get-pair-right PNT)))
					(if (equal? (cog-type PNT) 'CrossSection)
						(CrossSection newgerm DJ)
						(LLOBJ 'make-pair newgerm DJ)))))

		; --------------------------------------------------

		; Do any of the Connectors in SECT have CLS in them?
		; Return #t if at least one connector has CLS in it.
		(define (is-nonflat-section? CLS SECT)

			; Walk through the connector sequence. If any of them
			; appear in the cluster, return true.
			(any
				(lambda (con) (cog-equal? (gar con) CLS))
				(cog-outgoing-set (get-pair-right SECT))))

		; -----------------------------------------------
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

		(define (get-right-basis)
			(if (not r-basis) (set! r-basis (cog-get-atoms 'ShapeLink)))
			r-basis)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		; Invalidate the caches
		(define (clobber)
			(set! r-basis #f)
			(set! r-size 0)
			(if (LLOBJ 'provides 'clobber) (LLOBJ 'clobber))
		)

		; -------------------------------------------------------
		; Return all of the cross-sections (word-shape pairs) that
		; correspond to a section. This explodes a section into all
		; of the word-shape pairs that cover it (in the sense of a
		; "covering space"). Basically, given a Section, it walks
		; over the ConnectorSeq inside of it, replaces each word
		; with a variable (to define the shape) and then gets a pair
		; consisting of that word, and that shape.
		;
		; This only returns those cross-sections that are already
		; in the AtomSpace; it does NOT create them!
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
		;
		(define (get-cross-sections SEC)
			; The root-point of the seed
			(define point (gar SEC))
			; The list of connectors
			(define cncts (cog-outgoing-set (gdr SEC)))
			(define num-cncts (length cncts))

			; Place the wild-card into the N'th location of the section.
			(define (insert-wild N)
				(define front (take cncts N))
				(define back (drop cncts N))
				(define ctr (car back)) ; the connector being exploded
				(define wrd (gar ctr))  ; the word being exploded
				(define dir (gdr ctr))  ; the direction being exploded
				(define wild (Connector star-wild dir))
				(define shape
					(cog-link 'Shape point front wild (cdr back)))
				(if (nil? shape) #f
					(let ((cross (cog-link 'CrossSection wrd shape)))
						(if (nil? cross) #f cross))))

			; Return all the cross-sections for this section.
			(filter-map insert-wild (iota num-cncts))
		)

		; Same as above, but the cross-sections are created.
		(define (make-cross-sections SEC)
			; The root-point of the seed
			(define point (gar SEC))
			; The list of connectors
			(define cncts (cog-outgoing-set (gdr SEC)))
			(define num-cncts (length cncts))

			; Place the wild-card into the N'th location of the section.
			(define (insert-wild N)
				(define front (take cncts N))
				(define back (drop cncts N))
				(define ctr (car back)) ; the connector being exploded
				(define wrd (gar ctr))  ; the word being exploded
				(define dir (gdr ctr))  ; the direction being exploded
				(define wild (Connector star-wild dir))
				(define shape (Shape point front wild (cdr back)))
				(CrossSection wrd shape))

			; Return all the cross-sections for this section.
			(map insert-wild (iota num-cncts))
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
		; count on the sections!) should be divided by the arity of the
		; disjunct. But that would not alter the counts here.
		;
		; Note that the shapes will hold marginal counts.
		;
		; This does not need to be done, if restoring from the database;
		; viz if the pairs were previously stored, and now have been
		; fetched with 'fetch-sections below.
		;
		(define (explode-sections)

			; Walk over a section, and create the matching cross-sections.
			; Copy the count from the section to each of the cross-secions.
			(define (explode-section SEC)

				; Copy the count. All shapes must have the same
				; count as the section itself.
				(define weight (get-count SEC))
				(define (copy-weight XES) (set-count XES weight))
				(for-each copy-weight (make-cross-sections SEC))
			)

			; Ask the LLOBJ for all Sections.
			(define start-time (current-time))
			(for-each explode-section (LLOBJ 'get-all-elts))

			; Invalidate any caches that might be holding things.
			(clobber)
			(format #t "Elapsed time to create shapes: ~A secs\n"
				(- (current-time) start-time))
		)

		;-------------------------------------------
		; Undo the work of the above.
		(define (implode-sections)
			(define (extract-cross PNT)
				(when (equal? 'CrossSection (cog-type PNT))
					; Get rid of the ShapeLink
					(cog-extract (gdr PNT))
					(cog-extract! PNT)))

			; The extract-recursive clobbers everything.
			; (for-each extract-cross (LLOBJ 'get-all-elts))

			; Extracting the star-wild will clobber all CrossSections.
			; XXX FIXME: we should give the star-wild a unique name,
			; so we don't accidentally clobber CrossSections in other
			; objects.
			(for-each cog-extract-recursive! (cog-incoming-set star-wild))

			; Invalidate any caches that might be holding things.
			(clobber)
		)

		;-------------------------------------------
		; Fetch (from the database) the cross-sections (only),
		; as well as all the marginals for the cross-sections.
		(define (fetch-sections)
			(define start-time (current-time))

			; marginals are located on any-left, any-right
			(fetch-incoming-set any-left)
			(fetch-incoming-set any-right)

			; OK, so (fetch-incoming-set any-left) actually fails, because
			; get-left-wildcard up above is not actually using it. The
			; actual margins are stored on Shape links, and so we need to
			; explicitly fetch those to get the marginals attahed to them.
			(load-atoms-of-type 'Shape)

			; Get the actual pairs (if any)
			(load-atoms-of-type 'CrossSection)

			(format #t "Elapsed time to load cross-sections: ~A seconds\n"
				(- (current-time) start-time))
		)

		;-------------------------------------------
		(define (describe)
			(display (procedure-property add-shape-vec-api 'documentation)))

		;-------------------------------------------
		; Explain the non-default provided methods.
		(define (provides meth)
			(case meth
				((right-basis)      get-right-basis)
				((right-basis-size) get-right-size)
				((provides)         provides)
				((clobber)          clobber)
				((flatten)          flatten)

				((pair-count)       get-pair-count)
				((get-pair)         get-pair)
				((get-count)        get-count)
				((make-pair)        make-pair)
				((left-element)     get-pair-left)
				((right-element)    get-pair-right)
				(else #f)
		))

		; Methods on the object
		(lambda (message . args)
			(apply (case message
				((name)       (lambda () "Cross-section Words"))
				((id)         (lambda () "cross-section"))
				; ((left-type)        get-left-type)  ; provided by LLOBJ
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

				; Overloaded stars methods
				((right-basis)      get-right-basis)
				((right-basis-size) get-right-size)
				((clobber)          clobber)

				; Custom calls.
				((explode-sections) explode-sections)
				((implode-sections) implode-sections)
				((make-section)     make-section)
				((get-section)      get-section)
				((make-cross-sections) make-cross-sections)
				((get-cross-sections)  get-cross-sections)
				((re-cross)         re-cross)

				((make-flat)        make-flat)
				((flatten)          flatten)
				((flatten-section)  flatten-section)
				((flatten-cross)    flatten-cross)
				((is-nonflat?)      is-nonflat-section?)

				((provides)         provides)
				((filters?)         (lambda () #f))

				((describe)         describe)
				((help)             describe)
				((obj)              (lambda () "add-shape-vec-api"))
				((base)             (lambda () LLOBJ))
				(else               (lambda ( . rest )
				                        (apply LLOBJ (cons message args)))))
			args))
))

; ---------------------------------------------------------------------
;
(define-public (add-covering-sections LLOBJ)
"
  add-covering-sections LLOBJ -- Direct sum of Sections and CrossSections

  This object accepts an LLOBJ that exposes vectors of Sections, and
  provides an API for vectors of Sections-oplus-CrossSections, i.e. for
  the direct sum of these two.  Thus, any given vector will have basis
  elements taken from both. The CrossSections are provided by the
  `add-shape-vec-api` object.

  This is done in order to get a self-consistent view into the
  (word,disjunct) pair matrix, when words are being clustered into
  clusters. The issue is that words appear not only on the left, but
  also within Connectors in the disjunct. During clustering, the
  Connectors need to be merged, and the disjuncts updated. The
  CrossSections provide an \"almost-linear\" API that helps perform
  this task. That is, merging is inherently non-linear, and, in some
  cases, non-cummutative. The CrossSections and Shapes help keep things
  almost linear as far into the process as possible.

  Since the whole point of this object is to support merging, it will
  wrap the LLOBJ with the `add-gram-class-api` object.

  See docs for `add-shape-vec-api` for more info about CrossSections
  and Shapes. See `direct-sum` for more info about the direct sum.
  See `add-gram-class-api` for more about grammatical classes.
"
	(define gram-obj (add-gram-class-api LLOBJ))
	(define stars-obj (add-pair-stars gram-obj))
	(define shape-obj (add-shape-vec-api stars-obj))
	(define shape-stars (add-pair-stars shape-obj))

	; The direct sum creates a flattened vector that contains both
	; Sections and Cross-sections.
	(define cover-obj (direct-sum stars-obj shape-stars))
	(define cover-stars (add-pair-stars cover-obj))

	; Pass explode to the shape object, and then clobber all caches.
	(define (explode-sections)
		(shape-obj 'explode-sections)
		(cover-stars 'clobber))

	(define (implode-sections)
		(shape-obj 'implode-sections)
		(cover-stars 'clobber))

	;-------------------------------------------
	(define (describe)
		(display (procedure-property add-gram-class-api 'documentation)))

	(define (flatten CLS PNT)
		(shape-obj 'flatten CLS PNT))

	;-------------------------------------------
	; Explain the non-default provided methods.
	(define (provides meth)
		(case meth
			((provides)         provides)
			((flatten)          flatten)
			((cover-base)       (lambda () LLOBJ))

			; cover-stars is the direct product, and it handles the rest.
			(else               (cover-stars 'provides meth))
			; (else               #f)
		))

	;-------------------------------------------
	; Methods on the object
	(lambda (message . args)
		(case message
			((name)                "Covering Sections for Words")
			((id)                  "cover-section")

			; pass-through
			((fetch-pairs)         (cover-obj 'fetch-pairs))
			((explode-sections)    (explode-sections))
			((implode-sections)    (implode-sections))

			((make-section)        (apply shape-obj (cons message args)))
			((get-section)         (apply shape-obj (cons message args)))
			((make-cross-sections) (apply shape-obj (cons message args)))
			((get-cross-sections)  (apply shape-obj (cons message args)))
			((re-cross)            (apply shape-obj (cons message args)))
			((make-flat)           (apply shape-obj (cons message args)))
			((flatten)             (apply shape-obj (cons message args)))
			((is-nonflat?)         (apply shape-obj (cons message args)))

			((cluster-type)        (apply gram-obj (cons message args)))
			((make-cluster)        (apply gram-obj (cons message args)))
			((get-clusters)        (apply gram-obj (cons message args)))
			((store-aux)           (apply gram-obj (cons message args)))

			((provides)            (apply provides args))
			((describe)            describe)
			((help)                describe)
			((obj)                 "add-covering-sections")
			((base)                LLOBJ)
			((cover-base)          LLOBJ)

			; cover-stars is the direct product, and it handles the rest.
			(else             (apply cover-stars (cons message args)))))
)

; ---------------------------------------------------------------------
; Example usage:
;
; (define cva (add-shape-vec-api (make-pseudo-cset-api)))
; (cva 'fetch-pairs)
; (define cvs (add-pair-stars cva))
; (cvs 'left-basis-size)
;
; (cvs 'right-stars (Word "wiped"))
; (cvs 'right-stars (Word "ride"))
