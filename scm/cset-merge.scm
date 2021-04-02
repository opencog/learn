;
; cset-merge.scm
;
; Merge connectors into classes of connectors -  merge connector sets.
;
; Copyright (c) 2017, 2018, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The merging of words into word-classes proceeds in two parts. The
; first part is reviewed in `gram-classification.scm` and proceeds by
; comparing words to see if they share similar sets of sections. If they
; do, then the words can be judged to be similar, and merged into a word
; class.  The second part, reviewed here, is to merge connector
; sequences, so that connectors are made from word classes, instead of
; of individual words.
;
; Before any merging, a single section on a single word has the
; general form
;
;     Section
;         WordNode "foo"
;         ConnectorSeq
;             Connector
;                WordNode "bar"
;                ConnectorDir "+"
;             Connector
;                ....
;
; The goal of connector-set merging (aka disjunct-merging) is to
; replace the WordNode's in a Connector by WordClassNode's, and
; thence merge two similar ConnectorSeq's into one.
;
; Based on the analysis in the diary (Language Learning Diary - Part
; Two, subsection "Connector merging" (circa March 2021)) it appears
; that connector merging is commutative with section merging, in that
; if it has been decided that two words should be merged, then merging
; both sections and shapes (and later using the shapes to reconstruct
; sections) gives the same result as merging the connectors, directly.
; If only a fraction is merged, then the same fraction should be applied
; to the connectors, as well.
;
; The core idea is that the decision to merge is carried out elsewhere,
; and the only task remaining is to actually carry out the merge,
; transfering counts, or a fraction of the counts, as determined by
; the decision maker. If this is done, then merge results appear to
; always be self-consistent, thanks to the above-mentioned commutativity
; property.
;
; (An earlier version of this file discussed four different merge
; strategies that were not sel-consistent. That discussion has been
; removed).
;
; Worked example
; --------------
; Two connector sequences can be merged if and only if each one contains
; one of the connectors to be merged, and if all of the other connectors
; are the same.
;
; Let `P: Q+ & R+` be short-hand notation for
;
;     Section
;         WordNode "P"
;         ConnectorSeq
;             Connector
;                WordNode "Q"
;                ConnectorDir "+"
;             Connector
;                WordNode "R"
;                ConnectorDir "+"
;
; Suppose the decision-making process determines that words `S` and `Q`
; can be merged.  Then `P: Q+ & R+` can be merged with `P: S+ & R+`
; because these contain the words in question, and because the conector
; directions agree, and all the other connectors (i.e. `R+`) are
; identical. If it has been determined that only a percentage of `S`
; is to be merged, then that percentage should be applied to the counts
; on the sections, as well.
;
; Note that there are actually two distinct merge tasks: one to be
; performed on Sections, and another on CrossSections.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog sheaf) (opencog persist))

; XXX Code below is bad.  It doesn't do what the comments above state.
; ---------------------------------------------------------------
; Return a list of all words that belong to some grammatical class.

(define (get-classified-words)
	; Trace the MemberLink
	(define (memb CLS) (map gar (cog-incoming-by-type CLS 'MemberLink)))
	; Discard everything that is not a word.
	(define (wmemb CLS)
		(filter (lambda (w) (eq? 'WordNode (cog-type w))) (memb CLS)))
	; Concatenate them all together.
	(fold (lambda (CLS lst) (append! (memb CLS) lst)) '()
		(cog-get-atoms 'WordClassNode))
)

; ---------------------------------------------------------------
;
; Given a word or word-class, return a list of all sections attached to
; that word or word-class that are potentially mergable; that is, have
; a disjunct with a connector that belongs to an existing WordClass.
; The goal is to trim the list of sections to something smaller.
;
; XXX FIXME this might be pointless and useless? Its dead code, it's
; not used anywhere ...
(define (get-all-sections-in-classes WCL)

	; Return not-#f if the connector is in any class.
	(define (connector-in-any-class? CTR)
		(define wrd-of-ctr (gar CTR)) ; Word of the connector
		(find (lambda (MEMB)
				(eq? 'WordClassNode (cog-type (gdr MEMB))))
			(cog-incoming-by-type wrd-of-ctr 'MemberLink)))

	; Return not-#f if section SEC has connectors that
	; are in some WordClass, any WordClass
	(define (classifiable-section? SEC)
		; list of connectors in the section
		(define con-seq (cog-outgoing-set (gdr SEC)))
		(find connector-in-any-class? con-seq))

	; Return list of all sections that have connectors that are
	; in some (any) WordClass.
	(filter classifiable-section? (cog-incoming-by-type WCL 'Section))
)

; ---------------------------------------------------------------
;
; Given a word or a word-class, return a list of all sections that
; have disjuncts with N connectors. Just like `get-all-sections-in-classes`
; above, but filtered to the requested size.
;
; XXX This is dead code, not used anywhere...
;
(define (get-sections-by-size WCL SIZ)

	(define sects (get-all-sections-in-classes WCL))

	; Return not-#f if section SEC has SIZ connectors
	(define (size-section? SEC)
		(eq? SIZ (length (cog-outgoing-set (gdr SEC)))))

	; Return list of all sections that are of the given size
	(filter size-section? sects)
)

; ---------------------------------------------------------------

(define-public (in-gram-class? WORD GCLS)
"
  in-gram-class? WORD GRAM-CLASS - is the WORD a member of the
  grammatical class CRAM-CLASS? Returns either #t or #f.
"
	(not (null? (cog-link 'MemberLink WORD GCLS)))
)

; ---------------------------------------------------------------
; Compare two ConnectorSeq links, to see if they are the same,
; differing in only one location.  If this is the case, return
; the index offset to the location that is different. The index
; is zero-based. If they are not matchable, return #f.
;
; XXX Currently, this is dead code, not used anywhere.

(define (connector-seq-compare SEQA SEQB)
	; Get the index of the difference. Return #f if there are two
	; or more differences. If both are equal, it returns the length.
	; Could not figure out how to implement this without using set!
	(define (get-idx)
		(define mismatch-idx #f)
		(define cnt 0)
		(pair-fold
			(lambda (subseq-a subseq-b idx)
				; (format #t "duude ~A and ~A and ifx=~A\n" subseq-a subseq-b idx)
				(if (not (equal? (car subseq-a) (car subseq-b)))
					(begin (set! mismatch-idx idx)
						(set! cnt (+ cnt 1))))
				(+ idx 1))
			0 (cog-outgoing-set SEQA) (cog-outgoing-set SEQB))

		; Only one difference allowed.
		(if (eq? 1 cnt) mismatch-idx #f))

	; Return false if they are equal, else return the index
	(if (or (eq? SEQA SEQB) (not (eq? (cog-arity SEQA) (cog-arity SEQB))))
		 #f
		 (get-idx))
)

; ---------------------------------------------------------------
; Fetch from storage (load into RAM) all words that appear as members
; of one of the provided word-classes. Return the list of words.
;
; CLS-LST should be a list of word-classes.
;
; In most cases, this is not strictly necessary, as the usual case is
; that all words and word-classes are already in RAM.
;
; XXX this is maybe-dead code, its only used by `fetch-mergable-sections`
; below, which is not used anywhere...
;
(define (fetch-class-words CLS-LST)

	; Return a list without duplicates.
	(delete-dup-atoms
		(concatenate!
			; Loop over all WordClassNodes
			(map
				; This lambda returns a list of words.
				(lambda (CLS)  ; CLS is a WordClasNode
					(fetch-incoming-by-type CLS 'MemberLink)
					; map converts list of MemberLinks into list of words.
					(map
						; MEMB is a MemberLink; the zeroth atom in
						; the MemberLink is the WordNode.
						(lambda (MEMB) (cog-outgoing-atom MEMB 0))
						(cog-incoming-by-type CLS 'MemberLink)))
				CLS-LST)))
)

; ---------------------------------------------------------------
; Fetch from storage (load into RAM) all connector sequences and
; sections that are potentially mergeable; i.e. that use words from
; one of the provided word-classes. This returns a list of all the
; sections that contain a connector using a word from one of the
; word-classes.
;
; CLS-LST should be a list of word-classes.
;
; Example usage
;
; (define cls-lst (cog-get-atoms 'WordClassNode))
; (fetch-mergable-sections cls-lst)
; (get-classified-words) ; verify that they were fetched.
;
; The above gets *everything*. A better way to get everything is to say
; ((make-gram-class-api) 'fetch-pairs)
;
; XXX this is dead code, it's not used anywhere right now...
(define (fetch-mergable-sections CLS-LST)

	; Loop over all WordClassNodes
	(delete-dup-atoms
		(map fetch-endpoint-sections
			(fetch-class-words CLS-LST)))
)

; ---------------------------------------------------------------
; Example usage
;
; (define cls-lst (cog-get-atoms 'WordClassNode))
; (fetch-mergable-sections cls-lst)
; (get-classified-words) ; verify that they were fetched.
;
; (define wrd (Word "chance"))
; (define secs (cog-incoming-by-type wrd 'Section))
; (define maybe (get-all-sections-in-classes wrd))
