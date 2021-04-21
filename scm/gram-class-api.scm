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
  This is similar to `make-pseudo-cset-api`; the primary diffference
  being that it offers word-class access, instead of word access.
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

	; Fetch (from the database) all Sections that have a WordClass
	; on the left-hand-side. Fetch the marginals, too.
	(define (fetch-disjuncts)
		(define start-time (current-time))
		; Marginals are located on any-left, any-right
		(fetch-incoming-set any-left)
		(fetch-incoming-set any-right)
		; Fetch only the Sections that have a WordClass in them,
		; and not any other Sections.  Fetch all MemberLinks,
		; as these indicate which Words belong to which WordClasses.
		(load-atoms-of-type 'WordClassNode)
		(for-each
			(lambda (wcl)
				(fetch-incoming-by-type wcl 'Section)
				(fetch-incoming-by-type wcl 'MemberLink))
			(cog-get-atoms 'WordClassNode))
		(format #t "Elapsed time to load grammatical classes: ~A secs\n"
			(- (current-time) start-time)))

	; Store into the database the "auxilliary" MemberLinks between
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
			((store-aux) store-aux)
			((provides) (lambda (symb) #f))
			((filters?) (lambda () #f))
			(else (error "Bad method call on gram-class-api:" message)))
		args))
)

; ---------------------------------------------------------------------

(define-public (add-cluster-gram LLOBJ)
"
  add-cluster-gram LLOBJ

  Add definitions of grammatical classes to LLOBJ, for clustering.

  During clustering, the code identifies pairs of similar words,
  and provides the mechanics for merging them together, into a cluster.
  Each such cluster is a grammatical class. However, that code does not
  know how these should be laid out in the atomspace.  This object
  defines the methods needed for actually managing and storing the
  clusters generated during clustering.

  Provided methods:
    'cluster-type

    'make-cluster
"
	(define (get-cluster-type) 'WordClassNode)

	; Create a word-class out of two words, or just extend an
	; existing word class. Here, "extend" means "do nothing",
	; return the existing class.
	(define (make-cluster A-ATOM B-ATOM)
		(if (eq? 'WordClassNode (cog-type A-ATOM)) A-ATOM
			(WordClass (string-concatenate
				(list (cog-name A-ATOM) " " (cog-name B-ATOM))))))

	; Methods on the object
	(lambda (message . args)
		(case message
			((cluster-type)   (get-cluster-type))
			((make-cluster)   (apply make-cluster args))
			(else             (apply LLOBJ (cons message args)))
		))
)

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
  singleton WordClassNodes, based on thier count or relative rank.

  Note that using this object will cause the MI values between
  word-classes and disjuncts to become invalid; thse will need to be
  recomputed.

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
  * Other code expects that all of the counts are transfered from the
    word to the word-class, when generating the word-class. This is not
    being done here; the counts are only being copied. Also, it is
    expected that the MemberLink holds the total of the counts that
    were transfered. This is also not set up. This is a bug, and should
    be fixed. (Obviously, when desolving single-member classes, the
    counts should be moved back).
"
	(define (delete-singles)
		; delete each word-class node..
		(for-each cog-delete-recursive!
			; make a list of word-classes containing only one word...
			(filter
				(lambda (WRDCLS)
					; (eq? 1 (length (cog-incoming-by-type WRDCLS 'MemberLink)))
					(eq? 1 (cog-incoming-size-by-type WRDCLS 'MemberLink)))
				(LLOBJ 'left-basis))))

	; Create singletons
	(define (create-singles WORD-LIST)
		; Copy the count-value, and anything else.
		(define (copy-values NEW OLD)
			(for-each
				(lambda (KEY)
					(cog-set-value! NEW KEY (cog-value OLD KEY)))
				(cog-keys OLD)))

		; Remove words already in word-classes. XXX This is not
		; necessarily the correct action to take, depending on the
		; type of clustering, but this whole object is a kind of
		; temporary hack till the clustering algos settle down a
		; bit more. XXX review and FIXME at some later time.
		;
		; XXX specifically, if there are words with non-trivial
		; counts still left on them, they belong in singletons.
		; The problem is that the margnals are probably corrupt.
		; so we are confused about the counts left on them...
		; (The merge routines did not adjust marginals...!?)
		;
		(define unclassed-words
			(filter (lambda (wrd)
				(not (any (lambda (memb)
					(eq? 'WordClassNode (cog-type (gdr memb))))
					(cog-incoming-by-type wrd 'MemberLink))))
				WORD-LIST))

		(for-each
			(lambda (WRD)
				(define wcl (WordClass (string-append (cog-name WRD) "#uni")))
				; Add the word to the new word-class (obviously)
				(MemberLink WRD wcl)
				; Copy the sections
				(for-each
					(lambda (SEC) (copy-values (Section wcl (gdr SEC)) SEC))
					(cog-incoming-by-type WRD 'Section)))
			unclassed-words)

		(format #t "Created ~A singleton word classes\n"
			(length unclassed-words))
	)

	; Need to fetch the count from the margin.
	(define pss
		(begin
			; We expect (LLOBJ 'left-basis) to consist of WordNodes.
			(if (not (eq? 'WordNode (LLOBJ 'left-type)))
				(throw 'bad-row-type 'create-hi-count-singles
					(format #f "Expecting WordNode, got ~A"
						(LLOBJ 'left-type))))

			(add-support-api LLOBJ)))

	; Create singletons for those words with more than MIN-CNT
	; observations.
	(define (trim-low-counts MIN-CNT)

		(define trimmed-words
			(remove (lambda (WRD) (< (pss 'right-count WRD) MIN-CNT))
				(LLOBJ 'left-basis)))

		(format #t "After trimming, ~A words left, out of ~A\n"
			(length trimmed-words) (LLOBJ 'left-basis-size))

		(create-singles trimmed-words)
	)

	; Create singletons for top-ranked words.
	(define (top-ranked NUM-TOP)

		; nobs == number of observations
		(define (nobs WRD) (pss 'right-count WRD))

		(define ranked-words
			(sort! (LLOBJ 'left-basis)
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

(define (add-linking-filter LLOBJ WORD-LIST-FUNC ID-STR)
"
  add-linking-filter LLOBJ - Modify the word-disjunct LLOBJ so that
  the only connector sequences appearing on the right consist entirely
  of connectors that have words appearing in the WORD-LIST. This is
  not a public function; it is used to build several public functions.
"
	; ---------------
	; Cached set of valid connector-seqs, for the right-basis-pred.
	; We need to know if every connector in a connector sequence is
	; a member of some word in WORD-LIST. Verifying this directly is
	; very inefficient. It is much faster to precompute the set of
	; known-good connector sequences, and refer to that.

	(define star-obj (add-pair-stars LLOBJ))

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
				(cog-get-atoms 'WordNode) (WORD-LIST-FUNC)))

		(define unwanted-cnctrs
			(get-connectors unwanted-words))

		(define unwanted-conseqs
			(get-con-seqs unwanted-cnctrs))

		(define good-conseqs
			(atoms-subtract
				; Take all connector-sequences, and subtract the bad ones.
				; (cog-get-atoms 'ConnectorSeq)
				(star-obj 'right-basis)
				unwanted-conseqs))

		; Return the predicate that returns #t only for good ones.
		(make-aset-predicate good-conseqs)
	)

	; ---------------
	; Always keep any WordNode or WordClassNode we are presented with.
	(define (left-basis-pred WRDCLS) #t)

	(define ok-conseq? #f)

	; Only accept a ConnectorSeq if every word in every connector
	; is in some word-class.
	(define (right-basis-pred CONSEQ)
		(if (not ok-conseq?) (set! ok-conseq? (make-conseq-predicate)))
		(ok-conseq? CONSEQ)
	)

	; Input arg is a Section. The gdr (right hand side of it) is a
	; conseq. Keep the section if the conseq passes.
	(define (pair-pred SECT) (right-basis-pred (gdr SECT)))

	; ---------------
	(add-generic-filter LLOBJ
		left-basis-pred right-basis-pred pair-pred ID-STR #f)
)

; ---------------------------------------------------------------------

(define-public (add-wordclass-filter LLOBJ)
"
  add-wordclass-filter LLOBJ - Modify the wordclass-disjunct LLOBJ so
  that the only connector sequences appearing on the right consist
  entirely of connectors that have words in word-classes appearing on
  the left. The resulting collection of wordclass-disjunct pairs is
  then self-consistent, and does not contain any connectors unable to
  form a connection to some word-class.
"
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

	(define id-str "wordclass-filter")

	(add-linking-filter LLOBJ get-wordclass-words id-str)
)

; ---------------------------------------------------------------------

(define-public (add-linkage-filter LLOBJ)
"
  add-linkage-filter LLOBJ - Modify the word-disjunct LLOBJ so that
  the only connector sequences appearing on the right consist entirely
  of connectors that have words appearing on the left. The resulting
  collection of word-disjunct pairs is then mostly self-consistent,
  in that it does not contain any connectors unable to form a
  connection to some word.  However, it may still contain words on
  the left that do not appear in any connectors!
"
	(define star-obj (add-pair-stars LLOBJ))

	; Return a list of words
	(define (get-words) (star-obj 'left-basis))

	(define id-str "linkage-filter")

	(add-linking-filter LLOBJ get-words id-str)
)

; ---------------------------------------------------------------------
