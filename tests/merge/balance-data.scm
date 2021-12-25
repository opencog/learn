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
; Almost same as above, except that the merge word appears
; simultaneously in both seeds and connectors.

(define cnt-a-aaa 43)

(define (setup-aaa-sections)
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
	(Section (ctv 1 0 cnt-a-aaa)
		(Word "a")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	*unspecified*
)

; Expected results from the above.
; (define sec-ab-gh #f)
(define sec-a-aaa #f)

(define xes-k-a-vaaam #f)
(define xes-a-a-kvaam #f)
(define xes-a-a-kavam #f)
(define xes-a-a-kaavm #f)
(define xes-m-a-kaaav #f)

(define (expected-aaa-sections WC-AB)
	(set! sec-ab-gh
	(Section
		WC-AB
		(ConnectorSeq
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-a-aaa
	(Section
		WC-AB
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-k-a-vaaam
	(CrossSection
		(Word "k")
		(Shape
			WC-AB
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-a-kvaam
	(CrossSection
		WC-AB
		(Shape
			WC-AB
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-a-kavam
	(CrossSection
		WC-AB
		(Shape
			WC-AB
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-a-a-kaavm
	(CrossSection
		WC-AB
		(Shape
			WC-AB
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-m-a-kaaav
	(CrossSection
		(Word "m")
		(Shape
			WC-AB
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector WC-AB (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; Similar to the first case, but a second donor for the connectors.

(define cnt-c-aab 23)

(define (setup-aab-sections)
	(setup-a-b-sections)

	(Section (ctv 1 0 cnt-c-aab)
		(Word "c")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "b") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	*unspecified*
)

; Expected results from the above.
; Exactly the same as the first case.
(define (expected-aab-sections WC-AB)
	(expected-a-b-sections WC-AB)
)

; ---------------------------------------------------------------
; Similar to the first case, but a third donor for the connectors.

(define cnt-c-aba 30)

(define (setup-aba-sections)
	(setup-aab-sections)

	(Section (ctv 1 0 cnt-c-aba)
		(Word "c")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "b") (ConnectorDir "+"))
			(Connector (Word "a") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	*unspecified*
)

; Expected results from the above.
; Exactly the same as the first case.
(define (expected-aba-sections WC-AB)
	(expected-aab-sections WC-AB)
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
