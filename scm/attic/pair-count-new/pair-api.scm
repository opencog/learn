;
; pair-api.scm
;
; Define a matrix-API object for accessing word pairs.
;
; Copyright (c) 2013, 2014, 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The object below defines an API to access pairs of natural language
; word-pairs, stored in the AtomSpace, as a rank-2 matrix, i.e. as a
; matrix of (left, right) word pairs.  This provides exactly the API
; needed for use with the `(use-modules (opencog matrix))` statistical
; analysis subsystem.
;
; The specific Atomese uses this structure:
;
;     EdgeLink
;         BondNode "ANY"
;         ListLink
;             WordNode "left-word"
;             WordNode "right-word"
;
; The above is meant to be a special case of a general, generic form
; of item pairs; for example:
;
;     EvaluationLink
;         PredicateNode "*-Item pairs-*"
;         ListLink
;             ItemNode "left item"
;             ItemNode "right item"
;
; The generic form can be created with the `make-evaluation-pair-api`
; object. The special-case form is used because EdgeLinks require less
; RAM than EvaluationLinks.
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------

(define-public (make-any-link-api)
"
  make-any-link-api -- Word-pair access methods.

  This implements a word-pair object, where the two words are connected
  with a BondNode \"ANY\", in an EdgeLink.

  That is, a word pair is represented as:

    EdgeLink
       BondNode \"ANY\"
       ListLink
          WordNode \"word\"
          WordNode \"bird\"

  The above provides a location for storing various counts, frequencies,
  entropies, etc pertaining to this particular pair.

  The 'get-pair method returns the above EdgeLink, if it exists.
  The 'make-pair method will create it, if it does not exist.

  Left-side counts, frequencies, etc. such as N(*,y) P(*,y) or
  log_2 P(*,y) will be placed on the left-marginal, which is returned
  by the 'left-wildcard method:

    EdgeLink
       BondNode \"ANY\"
       ListLink
          AnyNode \"left-word\"
          WordNode \"bird\"

  The corresponding N(x,*) P(x,*) etc are hung on the right-marginal,
  returned by the 'right-wildcard method:

    EdgeLink
       BondNode \"ANY\"
       ListLink
          WordNode \"word\"
          AnyNode \"right-word\"

  Finally, the 'left-type and 'right-type methods return the type
  of the the two sides of the pair.
"
	; Just use the generic code to implement the above.
	(make-edge-pair-api
		'EdgeLink
		(BondNode "ANY")
		'WordNode
		'WordNode
		(AnyNode "left-word")
		(AnyNode "right-word")
		"ANY"
		"Link Grammar ANY link Word Pairs")
)

; ---------------------------------------------------------------------
