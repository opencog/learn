;
; connector-data.scm
; Populate the AtomSpace with assorted test data.

(use-modules (opencog) (opencog nlp))

; ---------------------------------------------------------------
; Define sections on two words, that should be mergable.

(define cnt-e-abc 13)
(define cnt-e-dgh 61)
(define cnt-e-klm 44)
(define cnt-j-abc 31)
(define cnt-j-dgh 16)

(define (setup-e-j-sections)
	(Section (ctv 1 0 cnt-e-abc)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-e-dgh)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-e-klm)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	; Similar to above.
	(Section (ctv 1 0 cnt-j-abc)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-j-dgh)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	; (Section k l m) intintionally absent.

	*unspecified*
)

; ---------------------------------------------------------------
; Define sections on a third word, that can be merged into above.

(define cnt-f-abc 19)
(define cnt-f-dgh 36)
(define cnt-f-klm 34)

(define (setup-f-sections)
	(Section (ctv 1 0 cnt-f-abc)
		(Word "f")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-f-dgh)
		(Word "f")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-f-klm)
		(Word "f")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	*unspecified*
)

; ---------------------------------------------------------------
; Addtional j sections, having e as connector.

(define cnt-j-abe 21)
(define cnt-j-egh 17)

(define (setup-j-extra)

	; Some j sections having "e" as a connector.
	(Section (ctv 1 0 cnt-j-abe)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-j-egh)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))

	*unspecified*
)

; ---------------------------------------------------------------
; Addtional j sections, having multiple e connectors.

(define cnt-j-ebe 11)
(define cnt-j-eeh 19)

(define (setup-j-double-e)

	; Some j sections having "e" as a connector.
	(Section (ctv 1 0 cnt-j-ebe)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+"))))
	(Section (ctv 1 0 cnt-j-eeh)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))

	*unspecified*
)

; ---------------------------------------------------------------
