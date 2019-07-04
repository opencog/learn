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
; All functions expect a word list. Use (psa 'left-basis)
; i.e. (define pca (make-pseudo-cset-api))
;      (define psa (add-pair-stars pca))
; Example usage: (count-linkable-word-pairs (psa 'left-basis))
;
; ------------------------------------------------------------------
(use-modules (ice-9 threads))

; ------------------------------------------------------------------
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

; ------------------------------------------------------------------
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

; ------------------------------------------------------------------
; Count the total number of word-pairs that can have links between
; them. This only counts the number of word-pairs, and NOT all the
; different ways that sections associated with those words might
; connect.
; Caution: this is very slow for large datasets, since it's brute-force
; in how it connects the two half-links.
(define (count-linkable-word-pairs WORD-LST)
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

; ------------------------------------------------------------------
; Count links between sections.
(define (count-full-links WORD-LST)

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

	; Create a BindLink that will obtain the links between sections,
	; and report the results.
	(define (make-blink LEFT-WRD)
		(BindLink blvars
			(And
				; A Setion from the LEFT-WRD connecting to the right.
				(Present (Section LEFT-WRD rcon))
				; A Section from the discovered right-word, pointing left.
				(Present
					(Section
						(Variable "r-word")
						(ConnectorSeq
							(Glob "r-pre")
							(Connector
								LEFT-WRD
								(ConnectorDir "-"))
							(Glob "r-post")))))

			; Temp stand-in for "the real thing"
			(List
				(List (Glob "l-pre") (Any "Left") (Glob "l-post"))
				(List (Glob "r-pre") (Any "Right") (Glob "r-post")))
		))

	; Avoid atomspace polution.
	(define (wrap func)
		(cog-push-atomspace)
		(let ((rc (func)))
			(cog-pop-atomspace)
			rc))

	; Return a count of the links that were found.
	(define (link-count LEFT-WRD)
		(define (func)
			(cog-arity (cog-execute! (make-blink LEFT-WRD))))
		(wrap func))

	; Why is this so slow?
	(define wrd-no 0)
	(define (word-no) (set! wrd-no (+ wrd-no 1)) wrd-no)

	(define start-time (current-time))
	(define since-time (current-time))
	(define (elapsed-secs)
		(define now (current-time))
		(define diff (- now since-time))
		(set! since-time now)
		diff)

	(define tot-cnt 0)
	(define nwrds (length WORD-LST))

	(define (link-count-x LEFT-WRD)
		(define lc (link-count LEFT-WRD))
		(define ti (- (current-time) start-time))
		(if (= 0 ti) (set! ti 1))
		(set! tot-cnt (+ tot-cnt lc))
		(format #t "~A of ~A count=~A in ~A secs wrd ~A"
			(word-no) nwrds lc (elapsed-secs) LEFT-WRD)
		(format #t "--- Elapsed: ~D:~2,'0D:~2,'0D Tot-cnt=~A Avg=~6F secs/word Rate=~6F cnts/sec\n"
			(inexact->exact (floor (/ ti 3600.0)))
			(inexact->exact (floor (/ (remainder ti 3600) 60.)))
			(remainder ti 60)
			tot-cnt
			(/ ti wrd-no)
			(/ tot-cnt ti))
		lc)

	; Count over all words.
	; (fold (lambda (WRD CNT) (+ CNT (link-count-x WRD))) 0 WORD-LST)

	; The above is painfully slow, so do a threaded version as well.
	; This threads very nicely, because almost all CPU time is wasted
	; in the C++ pattern matcher, so guile can handle these threads
	; quite well, without getting tangled up.
	(define sum-cnt 0)
	(define mtx (make-mutex))
	(par-for-each
		(lambda (WRD)
			(define cnt (link-count-x WRD))
			(lock-mutex mtx)
			(set! sum-cnt (+ sum-cnt cnt))
			(unlock-mutex mtx))
		WORD-LST)
)

; ------------------------------------------------------------------
