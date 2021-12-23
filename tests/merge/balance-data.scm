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

#! ===================
; Expected results from the above.
(define sec-ej-abc #f)
(define sec-ej-dgh #f)
(define sec-ej-klm #f)
(define sec-e-klm #f)

(define xes-b-ej-avc #f)
(define xes-k-ej-vlm #f)
(define xes-k-e-vlm #f)

(define (expected-e-j-sections WC-EJ)
	(set! sec-ej-abc
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+")))))
	(set! sec-ej-dgh
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
	(set! sec-ej-klm
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))
	(set! sec-e-klm
	(Section
		(Word "e")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-b-ej-avc
	(CrossSection
		(Word "b")
		(Shape
			WC-EJ
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+")))))
	(set! xes-k-ej-vlm
	(CrossSection
		(Word "k")
		(Shape
			WC-EJ
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))
	(set! xes-k-e-vlm
	(CrossSection
		(Word "k")
		(Shape
			(Word "e")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))
)
============= !#

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
