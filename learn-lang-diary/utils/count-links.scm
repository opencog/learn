;
; count-links.scm
;
; ---------------------------------------------------------------------
; Link-counting utilities.
;
; If a pseudo-connector needs to be promoted to a real connector,
; i.e. a real link with a direction on it, then how many real links
; are there in a dataset? The three functions below count them.
; Warning; the last one is very very slow.
;
;; Just count how many disjuncts connect to some word, either to
;; the left or to the right. WORD-LST should be a list of WordNodes.
;; This just gets the two connectors that WordNode might appear in,
;; and then counts the number of ConnectorSeq's the connector appears
;; in. The counting is without multiplicity (i.e. if the connector
;; appears twice in the sequence, it is counted only once.)
(define (count-quarter-links WORD-LST)
	(fold
		(lambda (CTR cnt)
			(+ cnt (cog-incoming-size-by-type CTR 'ConnectorSeq)))
		0
		; A list of all the connectors
		(append-map
			(lambda (WRD) (cog-incoming-by-type WRD 'Connector))
			WORD-LST))
)

; Count the number of half-links. A half-link joins together a
; connector to a word at the head of a section. There are always
; more of these, than quarter-links, because a given connector-seq
; may appear in several sections.
(define (count-half-links WORD-LST)

	; Return the number of Sections containing the connector CON
	(define (num-sects-w-con CON)
		(fold
			(lambda (CONSEQ CNT)
				(+ CNT (cog-incoming-size-by-type CONSEQ 'Section)))
			0
			(cog-incoming-by-type CON 'ConnectorSeq)))

	(fold
		(lambda (CON cnt)
			(+ cnt (num-sects-w-con CON)))
		0
		; A list of all the connectors
		(append-map
			(lambda (WRD) (cog-incoming-by-type WRD 'Connector))
			WORD-LST))
)

; Count the total number of word-pairs that can have links between
; them. This only counts the number of pairs, and NOT all the different
; ways they might connect.
; Caution: this is very slow for large datasets, since it's brute-force
; in how it connects the two half-links.
; Example usage: (count-links (psa 'left-basis))
(define (count-links WORD-LST)
	; Return a right-pointing connector for the word, if it exists,
	; else return #f
	(define (right-con WRD)
		(define ctr (cog-link 'Connector WRD (ConnectorDir "+")))
		(and (not (equal? ctr '())) ctr))

	; Return a left-pointing connector for the word, if it exists,
	; else return #f
	(define (left-con WRD)
		(define ctr (cog-link 'Connector WRD (ConnectorDir "-")))
		(and (not (equal? ctr '())) ctr))

	; Return a list of Sections containing the connector CON
	(define (sects-w-con CON)
		(append-map
			(lambda (CONSEQ)
				(cog-incoming-by-type CONSEQ 'Section))
		(cog-incoming-by-type CON 'ConnectorSeq)))

	; Return a list of Sections that are right-half-links,
	; containing the word LEFT-WRD in some left-pointing
	; connector in the Section.  That is, these sections can
	; only appear to the right of LEFT-WRD.
	(define (right-halves LEFT-WRD)
		(define rc (left-con LEFT-WRD))
		(if rc (sects-w-con rc) '()))

	(define (left-halves RIGHT-WRD)
		(define rc (right-con RIGHT-WRD))
		(if rc (sects-w-con rc) '()))

	; Return a list of WordNodes that are linked from the left.
	; These are words that can appear to the right of LEFT-WRD
	; because they contain left-pointing connectors to LEFT-WORD.
	; This list contains no duplicated entries.
	(define (right-words LEFT-WRD)
		(define word-set (make-atom-set))
		(for-each word-set (map gar (right-halves LEFT-WRD)))
		(word-set #f))

	; Return a list of words that can appear to the right of
	; LEFT-WRD, and are mutually linked (i.e. have half-links
	; that point to one-another.)  This list might contain
	; duplicates!
	(define (linkables LEFT-WRD)
		(filter
			(lambda (R-WRD)
				(any
					(lambda (SECT) (equal? (gar SECT) LEFT-WRD))
					(left-halves R-WRD)))
			(right-words LEFT-WRD)))

	; As above, but without duplicates
	(define (links LEFT-WRD)
		(define link-set (make-atom-set))
		(for-each link-set (linkables LEFT-WRD))
		(link-set #f))

	; Now count.
	(fold
		(lambda (WRD CNT) (+ CNT (length (links WRD))))
		0 WORD-LST)
)

; As above, but attempt a fast version, using BindLink
; to obtain the results.  ... But its slower. WTF.
;(define (fast-count-links WORD-LST)

	; Define variables only once.
	(define blvars
		(VariableList
			(TypedVariable (Variable "r-word") (Type 'WordNode))
			(TypedVariable (Glob "r-pre")
				(TypeSet (Type 'Connector)
					(Interval (Number 0) (Number -1))))
			(TypedVariable (Glob "r-post")
				(TypeSet (Type 'Connector)
					(Interval (Number 0) (Number -1))))
			(TypedVariable (Glob "l-pre")
				(TypeSet (Type 'Connector)
					(Interval (Number 0) (Number -1))))
			(TypedVariable (Glob "l-post")
				(TypeSet (Type 'Connector)
					(Interval (Number 0) (Number -1))))
		))

	; Another invariant part of the pattern
	(define rcon
		(ConnectorSeq
			(Glob "l-pre")
			(Connector (Variable "r-word") (ConnectorDir "+"))
		(Glob "l-post")))

	(define (make-blink LEFT-WRD)
		(BindLink blvars
			(And
				(Present (Section LEFT-WRD rcon))
				(Present
					(Section
						(Variable "r-word")
						(ConnectorSeq
							(Glob "r-pre")
							(Connector
								LEFT-WRD
								(ConnectorDir "-"))
							(Glob "r-post")))))
			(List
				(Variable "r-word")
				(List (Glob "l-pre") (Any "Left") (Glob "l-post"))
				(List (Glob "r-pre") (Any "Right") (Glob "r-post")))
		))
		
	(define (wrap func)
		(cog-push-atomspace)
		(let ((rc (func)))
			(cog-pop-atomspace)
			rc))

	(define (links LEFT-WRD)
		(define (func)
			(cog-outgoing-set (cog-execute! (make-blink LEFT-WRD))))
		(wrap func))

;	; Now count.
;	(fold
;		(lambda (WRD CNT) (+ CNT (length (links WRD))))
;		0 WORD-LST)
;)
