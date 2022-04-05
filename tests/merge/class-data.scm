;
; class-data.scm
; Populate the AtomSpace with multiple clusters.

(use-modules (opencog) (opencog nlp))

; ---------------------------------------------------------------
; Define sections on two WordClasses, that should be mergeable.

(define cnt-ej-abc 13)
(define cnt-ej-klm 44)
(define cnt-rs-abc 25)
(define cnt-rs-dgh 61)

(define mem-e-ej 10)
(define mem-j-ej 11)
(define mem-r-rs 12)
(define mem-s-rs 13)

(define (setup-ej-sections)
	(Section (ctv 1 0 cnt-ej-abc)
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-ej-klm)
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	(Member (Word "e") (WordClass "e j") (ctv 1 0 mem-e-ej))
	(Member (Word "j") (WordClass "e j") (ctv 1 0 mem-j-ej))

	; Similar to above.
	(Section (ctv 1 0 cnt-rs-abc)
		(WordClass "r s")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-rs-dgh)
		(WordClass "r s")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	; (Section k l m) intintionally absent.

	(Member (Word "r") (WordClass "r s") (ctv 1 0 mem-r-rs))
	(Member (Word "s") (WordClass "r s") (ctv 1 0 mem-s-rs))

	*unspecified*
)

; Expected results from the above.
(define sec-ej-abc #f)
(define sec-ej-dgh #f)
(define sec-ej-klm #f)

(define xes-b-ej-avc #f)
(define xes-k-ej-vlm #f)
(define xes-d-ej-vgh #f)

(define (expected-ej-sections)
	(set! sec-ej-abc
	(Section
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+")))))
	(set! sec-ej-dgh
	(Section
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
	(set! sec-ej-klm
	(Section
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))

	(set! xes-b-ej-avc
	(CrossSection
		(Word "b")
		(Shape
			(WordClass "e j")
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+")))))
	(set! xes-k-ej-vlm
	(CrossSection
		(Word "k")
		(Shape
			(WordClass "e j")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+")))))
	(set! xes-d-ej-vgh
	(CrossSection
		(Word "d")
		(Shape
			(WordClass "e j")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Additional ej sections, having rs as connector.

(define cnt-rs-abej 16)
(define cnt-ej-klrs 21)

(define (setup-ej-extra)

	; Some sections with classes as connectors
	(Section (ctv 1 0 cnt-rs-abej)
		(WordClass "r s")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (WordClass "e j") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-ej-klrs)
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "r s") (ConnectorDir "+"))))

	*unspecified*
)

(define sec-ej-abej #f)
(define sec-ej-klej #f)

(define xes-ej-ej-abv #f)
(define xes-ej-ej-klv #f)

(define (expected-ej-extra-sections)
	(set! sec-ej-abej
	(Section
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (WordClass "e j") (ConnectorDir "+")))))
	(set! sec-ej-klej
	(Section
		(WordClass "e j")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "e j") (ConnectorDir "+")))))

	(set! xes-ej-ej-abv
	(CrossSection
		(WordClass "e j")
		(Shape
			(WordClass "e j")
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-ej-ej-klv
	(CrossSection
		(WordClass "e j")
		(Shape
			(WordClass "e j")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Additional f and g sections, having rs as connector.

(define cnt-f-klrs 20)
(define cnt-g-klrs 9)
(define cnt-g-klej 8)

(define (setup-fg-extra)

	; Some f sections having "rs" as a connector.
	(Section (ctv 1 0 cnt-f-klrs)
		(Word "f")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "r s") (ConnectorDir "+"))))

	(Section (ctv 1 0 cnt-g-klrs)
		(Word "g")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "r s") (ConnectorDir "+"))))

	(Section (ctv 1 0 cnt-g-klej)
		(Word "g")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "e j") (ConnectorDir "+"))))

	*unspecified*
)

(define sec-f-klej #f)
(define sec-g-klej #f)

(define xes-ej-f-klv #f)
(define xes-ej-g-klv #f)

(define (expected-fg-extra-sections)
	(set! sec-f-klej
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "e j") (ConnectorDir "+")))))
	(set! sec-g-klej
	(Section
		(Word "g")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (WordClass "e j") (ConnectorDir "+")))))
	(set! xes-ej-f-klv
	(CrossSection
		(WordClass "e j")
		(Shape
			(Word "f")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))
	(set! xes-ej-g-klv
	(CrossSection
		(WordClass "e j")
		(Shape
			(Word "g")
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

)

; ---------------------------------------------------------------
(opencog-test-end)
; ---------------------------------------------------------------
; ---------------------------------------------------------------
