;
; connector-data.scm
; Populate the AtomSpace with assorted test data.

(use-modules (opencog) (opencog nlp))

; ---------------------------------------------------------------
; Define sections on two words, that should be mergable.
(define (setup-basic-sections)
	(Section (ctv 1 0 13)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 61)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	(Section (ctv 1 0 44)
		(Word "e")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "+"))
			(Connector (Word "m") (ConnectorDir "+"))))

	; Similar to above.
	(Section (ctv 1 0 31)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "c") (ConnectorDir "+"))))
	(Section (ctv 1 0 16)
		(Word "j")
		(ConnectorSeq
			(Connector (Word "d") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+"))))
	; (Section k l m) intintionally absent.

	*unspecified*
)

; ---------------------------------------------------------------
