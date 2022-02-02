;
; gram-filters.scm
;
; Filters altering grammatical-class matrix objects.
;
; Copyright (c) 2017, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file provides an assortment of filters for altering the
; grammatical-class `matrix-object`.  These filters will remove
; infrequently-occurring words, or will remove all words entirely
; (leaving only word-classes). They also perform consistency checks,
; to veryify that all disjuncts are composed of connectors that are
; able to connect to something.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))

; ---------------------------------------------------------------------

(define-public (add-singleton-classes LLOBJ)
"
  add-singleton-classes LLOBJ -- manage singleton WordClassNodes

  After clustering, there will still be many WordNodes that have not
  been assigned to clusters. This may be the case for three reasons:
   * The Wordnode really is in a class of it's own.
   * The clustering algo has not gotten around to it yet.
   * It is rare, infrequently-observed junk.
  This object provides management callbacks to place some words into
  singleton WordClassNodes, based on their count or relative rank.

  Note that using this object will cause the MI values between
  word-classes and disjuncts to become invalid; if these are needed,
  then they will need to be recomputed.

  Provided methods:
     'delete-singles -- Remove all WordClassNodes that have only a
           single member.

     'create-singles LIST -- Create a WordClassNode for each WordNode
           in the LIST. The complete set of Sections will be copied
           from the WordNode to the WordClassNode; the values on the
           Section will be copied as well, so that the Sections have
           correct counts on them.

     'create-hi-count-singles MIN-CUTOFF -- Create a WordClassNode for
           each WordNode whose observation count is greater than
           MIN-CUTOFF. As above, Sections and values are copied.

     'create-top-rank-singles NUM -- Create a WordClassNode for the
           top-ranked NUM WordNode's having the highest observation
           counts.  As above, Sections and values are copied.

  Known bugs:
  * delete-singles is broken
"
	(if (not (LLOBJ 'provides 'flatten))
		(throw 'missing-method 'add-singleton-classes
			"The 'flatten method is needed to create singletons"))

	; XXX this is broken
	(define (delete-singles)
		; delete each word-class node..
		(throw 'not-implemented 'add-singleton-classes
			"This method is borken and don't work right!")
		(for-each cog-delete-recursive!
			; make a list of word-classes containing only one word...
			(filter
				(lambda (WRDCLS)
					(eq? 1 (cog-incoming-size-by-type WRDCLS 'MemberLink)))
				(LLOBJ 'left-basis))))

	; Need to fetch the count from the margin.
	(define pss (add-support-api LLOBJ))

	; Create singletons
	(define (create-singles WORD-LIST)
		; Copy the count-value, and anything else.
		(define (copy-values NEW OLD)
			(for-each
				(lambda (KEY)
					(cog-set-value! NEW KEY (cog-value OLD KEY)))
				(cog-keys OLD)))

		(define start-time (current-time))

		(for-each
			(lambda (WRD)
				(define wcl (WordClass (string-append (cog-name WRD) "#uni")))
				; Add the word to the new word-class (obviously)
				(define memb (MemberLink WRD wcl))
				(cog-inc-count! memb (pss 'right-count WRD))
				(store-atom memb)

				; Copy the sections
				(for-each
					(lambda (PNT)
						(copy-values (LLOBJ 'flatten wcl PNT) PNT)

						; Delete the original section, as otherwise they
						; will disrupt the marginals.
						(cog-delete! PNT)
					)
					(LLOBJ 'right-stars WRD)))
			WORD-LIST)

		(format #t "Created ~A singleton word classes in ~A secs\n"
			(length WORD-LIST) (- (current-time) start-time))

		(LLOBJ 'clobber)
	)

	; Create singletons for those words with more than MIN-CNT
	; observations.
	(define (trim-low-counts MIN-CNT)

		(define trimmed-words
			(remove (lambda (WRD)
				(or
					(equal? 'WordClassNode (cog-type WRD))
					(< (pss 'right-count WRD) MIN-CNT)))
				(LLOBJ 'left-basis)))

		(format #t "After trimming, ~A words left, out of ~A\n"
			(length trimmed-words) (LLOBJ 'left-basis-size))

		(create-singles trimmed-words)
	)

	; Create singletons for top-ranked words.
	(define (top-ranked NUM-TOP)

		; nobs == number of observations
		(define (nobs WRD) (pss 'right-count WRD))

		(define words-only
			(remove
				(lambda (WRD) (equal? 'WordClassNode (cog-type WRD)))
				(LLOBJ 'left-basis)))

		(define ranked-words
			(sort! words-only
				(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B)))))

		(define short-list (take ranked-words NUM-TOP))

		(format #t "After sorting, kept ~A words out of ~A\n"
			(length short-list) (LLOBJ 'left-basis-size))

		(create-singles short-list)
	)

	; Methods on the object
	(lambda (message . args)
		(case message
			((delete-singles) (delete-singles))
			((create-singles) (apply create-singles args))
			((create-hi-count-singles) (apply trim-low-counts args))
			((create-top-rank-singles) (apply top-ranked args))
			(else             (apply LLOBJ (cons message args)))
		))
)

; ---------------------------------------------------------------------
; The next three routines are used to build a cached set of valid
; connector-seqs, for the right-basis-pred.
; We need to know if every connector in a connector sequence is
; a member of some word in WORD-LIST. Verifying this directly is
; very inefficient. It is much faster to precompute the set of
; known-good connector sequences, and refer to that.

(define (get-connectors WRD-LST)
"
  Return a list of connectors containing a word in the list
"
	(define con-set (make-atom-set))
	(for-each
		(lambda (word)
			(for-each con-set
				(cog-incoming-by-type word 'Connector)))
		WRD-LST)
	(con-set #f)
)

;-----------------------------

(define (get-con-seqs CON-LST)
"
  Return a list of ConnectorSeq containing one or more
  connectors from the CON-LST
"
	(define seq-set (make-atom-set))
	(for-each
		(lambda (ctr)
			(for-each seq-set
				(cog-incoming-by-type ctr 'ConnectorSeq)))
		CON-LST)
	(seq-set #f)
)

;-----------------------------

(define (make-conseq-predicate STAR-OBJ ACCEPT-ITEM?)
"
  make-conseq-predicate STAR-OBJ ACCEPT-ITEM? - connector predicate

  Create a predicate that returns true when a given ConnectorSeq
  consists of items (WordNodes or WordClassNodes) accepted by the
  predicate ACCEPT-ITEM?.

  That is, ACCEPT-ITEM? should return #t if the word in a connector is
  acceptable for connecting; else return #f.  This function will then
  return a predicate for identifying connector sequences consisting
  entirely of acceptable connectors.

  This may take a few minutes to get set up, if there are millions of
  ConnectorSeq's.
"
	; Unwanted words are those to be removed.
	(define unwanted-items
		(remove ACCEPT-ITEM? (STAR-OBJ 'left-basis)))

	(define unwanted-cnctrs
		(get-connectors unwanted-items))

	(define unwanted-conseqs
		(get-con-seqs unwanted-cnctrs))

	(define good-conseqs
		(atoms-subtract
			; Take all connector-sequences, and subtract the bad ones.
			(STAR-OBJ 'right-basis)
			unwanted-conseqs))

	; Return the predicate that returns #t only for good ones.
	(make-aset-predicate good-conseqs)
)

;-----------------------------

(define-public (add-linking-filter LLOBJ ACCEPT-ITEM? ID-STR)
"
  add-linking-filter LLOBJ ACCEPT-ITEM? ID-STR - Filter the
  word-disjunct LLOBJ so that the left-basis consistes entirely of
  items acceptable to ACCEPT-ITEM?  and the right-basis consists only
  of connector sequences having connectors linking to items that are
  acceptable to ACCEPT-ITEM?

  If ID-STR is a string (and not `#f`), then the marginals for this
  filtered matrix will be stored at keyed to the string ID-STR. If
  ID-STR is #f, then marginals will appear at default locations (which
  will clobber existing values there, and is probably not what you want!)
"
	(define star-obj (add-pair-stars LLOBJ))

	; Accept a ConnectorSeq only if every word in every connector
	; is acceptable.
	(define ok-conseq? (make-conseq-predicate star-obj ACCEPT-ITEM?))

	; Input arg is a Section. Keep the section if the two predicates
	; accept both sides.
	(define (pair-pred SECT)
		(and (ACCEPT-ITEM? (LLOBJ 'left-element SECT))
			(ok-conseq? (LLOBJ 'right-element SECT))))

	; ---------------
	(add-generic-filter LLOBJ
		ACCEPT-ITEM? ok-conseq? pair-pred ID-STR ID-STR)
)

;-----------------------------

(define-public (linking-trim LLOBJ ACCEPT-ITEM?)
"
  linking-trim LLOBJ ACCEPT-ITEM? - Trim the word-disjunct LLOBJ by
  deleting (with `cog-delete!`) items and connector sequences and
  sections which contain items (words) other than those accepted by
  the ACCEPT-ITEM? predicate.

  This is like `add-linking-filter` above, except that it doesn't
  filter, it just deletes.
"
	(define star-obj (add-pair-stars LLOBJ))

	; Accept a ConnectorSeq only if every word in every connector
	; is acceptable.
	(define ok-conseq? (make-conseq-predicate star-obj ACCEPT-ITEM?))

	; Input arg is a Section. Keep the section if the two predicates
	; accept both sides.
	(define (good-elt? SECT)
		(and (ACCEPT-ITEM? (LLOBJ 'left-element SECT))
			(ok-conseq? (LLOBJ 'right-element SECT))))

	; Always keep.
	(define (keep ITM) #t)

	; ---------------
	(define trim-mtrx (add-trimmer LLOBJ))
	(trim-mtrx 'generic-trim keep keep good-elt?)
)

; ---------------------------------------------------------------------

(define-public (add-word-remover LLOBJ RENAME)
"
  add-word-remover LLOBJ - Modify the grammatical-class LLOBJ so that
  there are no WordNodes in either the left basis, nor in any Connectors
  in the ConnectorSeqs in the right basis.

  Not only are the WordNodes removed, but the remaining left and right
  basis are scrubbed, so that there are no connectors unable to form a
  connection.

  Set RENAME to #t if marginals should be stored under a filter-specific
  name. Otherwise, set to #f to use the default marginal locations.
"
	; Accept WordClasses only.
	(define (is-word-class? ITEM) (eq? 'WordClassNode (cog-type ITEM)))

	(define id-str (if RENAME "word-remover" #f))

	(add-linking-filter LLOBJ is-word-class? id-str)
)

; ---------------------------------------------------------------------

(define-public (add-wordclass-filter LLOBJ RENAME)
"
  add-wordclass-filter LLOBJ - Modify the wordclass-disjunct LLOBJ so
  that the only connector sequences appearing on the right consist
  entirely of connectors that have words in word-classes appearing on
  the left. The resulting collection of wordclass-disjunct pairs is
  then self-consistent, and does not contain any connectors unable to
  form a connection to some word-class.

  CAUTION: THIS FILTER IS CURRENTLY USELESS (OBSOLETE)! It assumes that
  WordClasses never appear in the connectors themselves; i.e. that the
  dataset has been created without using Shapes. At this time, it is
  imossible to create such datasets any more... So anyway, this code
  will do things that aren't meaningful.

  Set RENAME to #t if marginals should be stored under a filter-specific
  name. Otherwise, set to #f to use the default marginal locations.
"

	; Word-classes only
	(define classes
		(filter (lambda (ITEM) (eq? 'WordClassNode (cog-type ITEM)))
			(LLOBJ 'left-basis)))

	; Collect up all items that belong to classes.
	(define acceptable-set (make-atom-set))
	(for-each
		(lambda (wcls)
			(acceptable-set wcls)
			(for-each acceptable-set
				(map gar (cog-incoming-by-type wcls 'MemberLink))))
		classes)

	(define linkable? (make-aset-predicate (acceptable-set #f)))

	(define id-str (if RENAME "wordclass-filter" #f))

	(add-linking-filter LLOBJ linkable? id-str)
)

; ---------------------------------------------------------------------

(define-public (add-linkage-filter LLOBJ RENAME)
"
  add-linkage-filter LLOBJ - Modify the word-disjunct LLOBJ so that
  the only connector sequences appearing on the right consist entirely
  of connectors that have words appearing on the left. The resulting
  collection of word-disjunct pairs is then mostly self-consistent,
  in that it does not contain any connectors unable to form a
  connection to some word.  However, it may still contain words on
  the left that do not appear in any connectors!

  Set RENAME to #t if marginals should be stored under a filter-specific
  name. Otherwise, set to #f to use the default marginal locations.
"
	(define star-obj (add-pair-stars LLOBJ))

	; A predicate that returns OK only for left-basis items.
	(define is-in-left? (make-aset-predicate (star-obj 'left-basis)))

	(define id-str (if RENAME "linkage-filter" #f))

	(add-linking-filter LLOBJ is-in-left? id-str)
)

; ---------------------------------------------------------------------

(define-public (trim-linkage LLOBJ)
"
  trim-linkage LLOBJ - Trim the word-disjunct LLOBJ by deleting (using
  `cog-delete!`) words and connector sequences and sections which contain
  words other than those appearing in the left-basis.  This is like
  `add-linkage-filter`, except that it doesn't filter, it just deletes.

  The resulting collection of word-disjunct pairs is then self-consistent,
  in that it does not contain any connectors unable to form a connection
  to some word.
"
	(define star-obj (add-pair-stars LLOBJ))

	; A predicate that returns OK only for left-basis items.
	(define is-in-left? (make-aset-predicate (star-obj 'left-basis)))

	(linking-trim LLOBJ is-in-left?)
)

; ---------------------------------------------------------------------
