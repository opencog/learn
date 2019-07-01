;
; link-class.scm
;
; Merge links into link-classes by grammatical similarity.
; Maximum-entropy style merges.
;
; Copyright (c) 2019 Linas Vepstas
;
;; SPDX-License-Identifier: AGPL-3.0-or-later
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Before link-type merging, Sections contain only pseudo-connectors,
; viz. Connectors with WordNode's in them, referencing another Section
; to connect to. So, for example:
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
;
; The goal here is to replace the WordNodes in Connectors by
; LinkClassNodes, which explcitly link to one-another.  That is, the
; WordNodes act as pseudo-connectors: they must link to the head-word
; of a section.  By contrast, a LinkClass is a real link; it must
; link to another LinkClass in another connector (having the same
; name, but the opposite polarity.



; ---------------------------------------------------------------------
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

; Count the number of half-links.
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

; Count the total number of links, that is, the total mumber of
; symmetric links between words, where each has a half-link back
; to the other.
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
	; containing the word LEFT-WRD in some right-pointing
	; connector in the Section.  That is, these sections can
	; only appear to the right of LEFT-WRD.
	(define (right-halves LEFT-WRD)
		(define rc (right-con LEFT-WRD))
		(if rc (sects-w-con rc) '()))

	; Return a list of WordNodes that are linked from the left.
	; This list contains no duplicated entries.
	(define (right-words LEFT-WRD)
		(define word-set (make-atom-set))
		(for-each word-set (map gar (right-halves LEFT-WRD)))
		(word-set #f))

	; Return a list of sections which contain LEFT-WRD on
	; the left, and are able to form a link to some word
	; on the right. This list may contain duplicates!
	(define (linkables LEFT-WRD)
		(filter
			(lambda (SECT)
				(equal? (gar SECT) LEFT-WRD))
			(append-map sects-w-con
				(filter-map left-con (right-words LEFT-WRD)))))

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
