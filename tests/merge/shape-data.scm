;
; shape-data.scm
; Populate the AtomSpace with assorted test data (for the shape-merge test)

(use-modules (opencog) (opencog lg))

(define tvkeystr "*-TruthValueKey-*")

(define (cog-count ATOM)
	(cog-value-ref (cog-value ATOM (Predicate tvkeystr)) 2))

; ---------------------------------------------------------------
; Define sections on two words, that should be mergeable.

(define cnt-f-aa 61)
(define cnt-f-ba 38)

(define (setup-f-sections)
	(define tvkey (PredicateNode tvkeystr))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "a") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-aa))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "a") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-ba))

	*unspecified*
)

; Expected results from the above.
(define sec-f-mm #f)

(define xes-m-f-vm #f)
(define xes-m-f-mv #f)

(define (expected-a-b-sections WC-AB)
	(set! sec-f-mm
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector WC-AB (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+")))))

	(set! xes-m-f-vm
	(CrossSection
		WC-AB
		(Shape
			(Word "f")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+")))))

	(set! xes-m-f-mv
	(CrossSection
		WC-AB
		(Shape
			(Word "f")
			(Connector WC-AB (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
