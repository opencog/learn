;
; balance-data.scm
; Populate the AtomSpace with assorted test data (for some of the 
; detailed balance tests)

(use-modules (opencog) (opencog nlp))

; ---------------------------------------------------------------
; Define sections on two words, that should be mergeable.

(define cnt-a-gh 61)
(define cnt-b-gh 38)
(define cnt-c-aaa 44)

(define (setup-a-b-sections)
	(Section (ctv 1 0 cnt-a-gh)
		(Word "a")
		(ConnectorSeq
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-b-gh)
		(Word "b")
		(ConnectorSeq
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-c-aaa)
		(Word "c")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	*unspecified*
)

; Expected results from the above.
(define sec-ab-gh #f)
(define sec-c-aaa #f)

(define xes-k-c-vaaam #f)
(define xes-a-c-kvaam #f)
(define xes-a-c-kavam #f)
(define xes-a-c-kaavm #f)
(define xes-m-c-kaaav #f)

(define (expected-a-b-sections WC-AB)
	(set! sec-ab-gh
	(Section
		WC-AB
		(ConnectorSeq
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-c-aaa
	(Section
		(Word "c")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-k-c-vaaam
	(CrossSection
		(Word "k")
		(Shape
			(Word "c")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-c-kvaam
	(CrossSection
		WC-AB
		(Shape
			(Word "c")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-c-kavam
	(CrossSection
		WC-AB
		(Shape
			(Word "c")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-c-kaavm
	(CrossSection
		WC-AB
		(Shape
			(Word "c")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-m-c-kaaav
	(CrossSection
		(Word "m")
		(Shape
			(Word "c")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
