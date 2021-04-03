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

; ---------------------------------------------------------------
; XXX Incomplete, in development.

(define (matching-sequences CON-A CON-B)
"
  matching-sequences CON-A CON-B -- return matching ConnectorSeqs's

  Find all ConnectorSeq that contain CON-A, and check to see if an
  equivalent ConnectorSeq exists, containing CON-B. If so, create
  a scheme pair containing both. Return a list of these matching pairs.
"
	(filter-map
		(lambda (ASEQ)
			; Is there an equivalent seq with B instead of A?
			(define bseq
				(cog-link 'ConnectorSeq
					(map
						(lambda (CON) (if (equal? CON-A CON) CON-B CON))
						(cog-outgoing-set ASEQ))))
			(if (nil? bseq) #f (cons ASEQ bseq)))

		; All 'ConnectorSeq containing A
		(cog-incoming-by-type CON-A 'ConnectorSeq)))

(define (matching-sections CON-A CON-B)
"
  matching-sections CON-A CON-B -- return matching Sections

  Find all Sections that contain CON-A, and check to see if an
  equivalent Section exists, containing CON-B. If so, create
  a scheme pair containing both. Return a list of these matching pairs.
"
	(concatenate! (map
		(lambda (PR)
			; The two matching sequences.
			(define sequ-a (car PR))
			(define sequ-b (cdr PR))

			; If a Section contains ConnectorSeq A, see if there's
			; an equivalent section containing ConnectorSeq B.
			(filter-map
				(lambda (A-SEC)
					(define b-sec (cog-link 'Section (gar A-SEC) sequ-b))
					(if (nil? b-sec) #f (cons A-SEC b-sec)))
				(cog-incoming-by-type sequ-a 'Section)))

		; A list of matching sequences that contain connectors A and B.
		(matching-sequences CON-A CON-B)))
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
; Example usage
;
; (define cls-lst (cog-get-atoms 'WordClassNode))
; (fetch-mergable-sections cls-lst)
; (get-classified-words) ; verify that they were fetched.
;
; (define wrd (Word "chance"))
; (define secs (cog-incoming-by-type wrd 'Section))
; (define maybe (get-all-sections-in-classes wrd))
