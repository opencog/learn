;
; bond-links.scm
;
; Associate LG link names with word-pairs.
;
; Copyright (c) 2023 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file provide the "matrix-object" API that associates word-pairs
; with LG link names. The association takes the form
;
;   (Edge (Bond "XYZ") (List (Word ...) (Word ...)))
;
; This association is needed for two reasons:
; 1) When LG is asked to perform parses using Atomese data, it needs to
;    be provided with LG-style connector names. But that is not the
;    native format used during counting in the AtomSpace. Thus, there
;    there needs to be a way of translating word-pairs (or word-class
;    pairs) into LG connector names.
; 2) We want to keep track of how often each LG link was used in some
;    parse. For that, we store counts on the above structure.
;
; There are several issues that arise here.
; A) The LG `dict-atomese` C++ code already does this association, in
;    C++, and there's no need for this object. Yet, to use this object,
;    it needs to be kept in sync with the C++ code there. Caveat emptor.
; B) The above structure is not quite a classic "matrix" structure, and
;    so doesn't quite fit into the conventional matrix API. The
;    structure is actually a tensor.  We do a force-fit, here, and make
;    it work. You've been warned.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

(define-public (make-bond-link-api)
"
  make-bond-link-api -- Associate word-pairs and LG connector names.

  The matrix consists of (word,word) pairs and the corresponding LG
  connector name. Matrix elements have the form

    (Edge (Bond \"XYZ\") (List (Word ...) (Word ...)))

  where XYZ is the Link Grammar connector (link) name.

  For a detailed description, see the `bond-links.scm` file.
"
	(define any-left (AnyNode "bond-left"))
	(define any-right (AnyNode "bond-right"))

	(define (get-left-type) (Type 'WordNode))
	(define (get-right-type) (Type 'WordNode))
	(define (get-pair-type) (Type 'EdgeLink))

	; We do NOT want ANY bonds!
	(define bany (BondNode "ANY"))

	; Get the pair, if it exists. Reject pairs with (BondNode "ANY")
	; edges between them. They are not one of us.
	(define (get-pair L-ATOM R-ATOM)
		(define pr #f)
		(define wp (cog-link 'List L-ATOM R-ATOM))
		(if (nil? wp) wp
			(for-each
				(lambda (EDGE)
					(if (not (equal? bany (gar EDGE)))
						(set! pr EDGE)))
				(cog-incoming-by-type wp 'EdgeLink)))
		pr)

	; Given integer, return link string. Mapping is
	; 0 -> A
	; 1 -> B
	; 25 -> Z
	; 26 -> AA
	; 27 -> AB
	; This mapping is *identical* to that used in the link-grammar
	; C++ code base, `link-names.cc` file, and needs to be kept in
	; sync.  Failure to do so will result in hard-to-debug errors.
	(define (make-id-str nid lst)
		; 65 is ASCII capital A
		(if (> 0 nid)
			(list->string lst)
			(make-id-str (- (floor (/ nid 26)) 1)
				(cons (integer->char (+ 65 (modulo nid 26))) lst))))

	; Anchor where next ID is stored. This location must be kept in
	; sync with the link-grammar C++ code base, `link-names.cc` file.
	(define idanch (Anchor "*-LG-issued-link-id-*"))
	(define (issue-link)
		(define nid (cog-count idanch))
		(define linkstr (make-id-str nid '()))
		(set! nid (+ 1 nid))
		; (make-id-str 1064) is "ANY" and we must avoid that.
		(if (equal? nid 1064) (set! nid (+ 1 nid)))

		; Save and store. May need it again, some other day.
		(cog-set-tv! idanch (CountTruthValue 1 0 nid))
		(store-atom idanch)

		; Return the link string
		linkstr)

	; Create the pair, if it does not yet exist.
	(define (make-pair L-ATOM R-ATOM)
		(define pr (get-pair L-ATOM R-ATOM))
		(if (not (nil? pr)) pr
			(Edge (Bond (issue-link)) (ListLink L-ATOM R-ATOM))))

	(define (get-left-element EDGE) (gadr EDGE))
	(define (get-right-element EDGE) (gddr EDGE))

	(define (get-left-wildcard WORD)
		(Edge idanch (ListLink any-left WORD)))

	(define (get-right-wildcard WORD)
		(Edge idanch (ListLink WORD any-right)))

	(define (get-wild-wild)
		(Edge idanch (ListLink any-left any-right)))

	; Fetch (from the database) all labelled edges.
	(define (load-all-bonds)
		(define mon (make-rate-monitor))
		(fetch-atom idanch)
		(load-atoms-of-type 'BondNode)
		(for-each
			(lambda (BOND)
				(mon #f)
				(fetch-incoming-by-type BOND 'EdgeLink))
			(cog-get-atoms 'BondNode))

		; Marginals are located on the anchor
		(fetch-incoming-by-type idanch 'EdgeLink)

		(mon "Loaded ~D edges in ~D secs; rate=~5F\n"))

	(define (describe)
		(display (procedure-property make-bond-link-api 'documentation)))

	(define (store-aux) (store-atom idanch))

	(define (provides meth)
		(case meth
			((store-aux)        store-aux)
			(else #f)
	))

	; Methods on the object
	(lambda (message . args)
		(apply (case message
			((name) (lambda () "LG-Link - word-pair Association"))
			((id)             (lambda () "bond-link"))
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
			((fetch-pairs)    load-all-bonds)
			((provides)       provides)
			((store-aux)      store-aux)
			((filters?)       (lambda () #f))
			((help)           describe)
			((describe)       describe)
			((obj)            (lambda () "make-bond-link-api"))
			((base)           (lambda () #f))
			(else (error "Bad method call on bond-link:" message)))
		args))
)

; ---------------------------------------------------------------------
