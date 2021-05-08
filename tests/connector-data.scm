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

(define (setup-basic-sections)
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
