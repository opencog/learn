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
; This file provides the `matrix-object` API that allows grammatical
; classes of words to be treated as vectors of connector-sets (vectors
; of disjuncts; vectors of Sections).
;
; This is meant to be used as a wrapper around `make-pseudo-cset-api`,
; extending it so that both `WordNode`s and `WordClassNode`s are handled.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------

; This class kind-of resembles a direct sum (as coded in the
; `direct-sum` object) but its ad hoc, hard-coded, not generic.
(define-public (add-gram-class-api LLOBJ)
"
  add-gram-class-api LLOBJ -- Enable (WordClass, disjunct) pairs.

  This will take LLOBJ and extend it's native `left-type with the
  `WordClass` type, so that the left type can be either.  It also
  provides methods for managing the `MemberLink`s that indicate
  membership of the 'left-type in the `WordClass`.

  The membership of a word to a WordClass is denoted as

      (MemberLink (WordNode \"foo\") (WordClassNode \"bar\"))

  Keep in mind that a word might belong to more than one WordClass.
  Contributions to the class are stored as counts on the MemberLink.

  See the `pseudo-csets.scm` file for a general overview.

  Provided methods:
    'left-type -- returns (TypeChoice (LLOBJ 'left-type) (Type 'WordClassNode))
    'store-aux -- Store the MemberLinks above.
    'fetch-pairs -- Fetch both WordClassNodes and MemberLinks.

    'cluster-type -- returns (Type 'WordClassNode)
    'get-clusters -- return all left-basis elements that are of
                     cluster type.

    'make-cluster WA WB -- Creates a WordClassNode
"
	(define (get-left-type)
		(TypeChoice (LLOBJ 'left-type) (Type 'WordClassNode)))

	(define any-left (AnyNode "gram-class"))
	(define (get-left-wildcard DJ) (ListLink any-left DJ))

	; Recycle the right wildcard from the parent class.
	; XXX FIXME: this won't work for some classes, which store
	; marginals in a different format than pairs. That is, the
	; 'right-element method will work correctly on pairs only,
	; not on marginals. For example, direct-sum is like that.
	; Perhaps we should blame the classes for mis-handling marginals?
	(define any-right (LLOBJ 'right-element (LLOBJ 'wild-wild)))
	(define (get-wild-wild) (ListLink any-left any-right))

	; Fetch (from the database) all Sections that have a WordClass
	; on the left-hand-side. Fetch the marginals, too.
	(define (fetch-disjuncts)

		; Let the base object do the heavy lifting.
		(LLOBJ 'fetch-pairs)

		(define start-time (current-time))

		; Fetch all MemberLinks, as these indicate which Words
		; belong to which WordClasses. Sections have already been
		; fetched by the LLOBJ, so we won't do anything more, here.
		(load-atoms-of-type 'WordClassNode)
		(for-each
			(lambda (wcl)
				(fetch-incoming-by-type wcl 'MemberLink))
			(cog-get-atoms 'WordClassNode))

		; Load marginals, too. These are specific to this class.
		(fetch-incoming-set any-left)

		(format #t "Elapsed time to load grammatical classes: ~A secs\n"
			(- (current-time) start-time)))

	; Store into the database the "auxiliary" MemberLinks between
	; WordClassNodes and WordNodes. Without this, the dataset is
	; incomplete.
	(define (store-aux)
		(for-each
			; lambda over lists of MemberLink's
			(lambda (memb-list)
				(for-each
					; lambda over MemberLinks
					(lambda (memb)
						; If the right kind of MemberLink
						(if (eq? 'WordNode (cog-type (gar memb)))
							(store-atom memb)))
					memb-list))
			; Get all MemberLinks that this WordClass belongs to.
			(map (lambda (wrdcls) (cog-incoming-by-type wrdcls 'MemberLink))
				(cog-get-atoms 'WordClassNode))))

	;-------------------------------------------
	; Custom methods specific to this object.
	(define (get-cluster-type) (Type 'WordClassNode))

	; Get the clusters appearing in the left-basis.
	(define (get-clusters)
		; In current usage, LLOBJ doesn't have stars on it.
		; At any rate, we want a fresh search for the basis
		; each time we are called, as the basis may have changed.
		(define stars (add-pair-stars LLOBJ))
		(filter (lambda (W) (equal? 'WordClassNode (cog-type W)))
			(stars 'left-basis)))

	; Create a word-class out of two words, or just extend an
	; existing word class. Here, "extend" means "do nothing",
	; return the existing class. If this is called a second time
	; with the same arguments, then a new, unique name is generated!
	; Therefore, this should never be called than once!
	; XXX FIXME the semantics of this thing is ugly, and should be
	; moved to the caller. We shouldn't have to second-guess the
	; callers dsired behavior!
	(define (make-cluster A-ATOM B-ATOM)
		(define is-a-class (eq? 'WordClassNode (cog-type A-ATOM)))
		(define is-b-class (eq? 'WordClassNode (cog-type B-ATOM)))
		(cond
			(is-a-class A-ATOM)
			(is-b-class B-ATOM)
			(else (let
					((cluname (string-join
						(list (cog-name A-ATOM) (cog-name B-ATOM)))))

				; If `cluname` already exists, then append "(dup)" to the
				; end of it, and try again. Keep repeating. In the real
				; world, this should never happen more than once, maybe
				; twice, unimaginable three times. So that iota is safe.
				(every
					(lambda (N)
						(if (nil? (cog-node 'WordClassNode cluname)) #f
							(begin
								(set! cluname (string-append cluname " (dup)"))
								#t)))
					(iota 10000))
				(WordClassNode cluname)))))

	;-------------------------------------------
	(define (describe)
		(display (procedure-property add-gram-class-api 'documentation)))

	;-------------------------------------------
	; Explain the non-default provided methods.
	(define (provides meth)
		(case meth
			((left-type)        get-left-type)
			((store-aux)        store-aux)
			(else #f)
	))

	; Methods on the object
	(lambda (message . args)
		(apply (case message
			((name)           (lambda () "WordClass-Disjunct Pairs"))
			((id)             (lambda () "gram-class"))
			((left-type)      get-left-type)
			((left-wildcard)  get-left-wildcard)
			((wild-wild)      get-wild-wild)

			((fetch-pairs)    fetch-disjuncts)
			((store-aux)      store-aux)

			((cluster-type)   get-cluster-type)
			((get-clusters)   get-clusters)
			((make-cluster)   make-cluster)

			((provides)       provides)
			((filters?)       (lambda () #f))
			((describe)       describe)
			((help)           describe)
			((obj)            (lambda () "add-gram-class-api"))
			((base)           (lambda () LLOBJ))
			(else             (lambda ( . rest ) (apply LLOBJ (cons message args))))
		) args))
)

; ---------------------------------------------------------------------
