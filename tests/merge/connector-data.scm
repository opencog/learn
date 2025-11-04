;
; connector-data.scm
; Populate the AtomSpace with assorted test data.

(use-modules (opencog) (opencog lg))

(define tvkeystr "*-TruthValueKey-*")

(define (cog-count ATOM)
	(cog-value-ref (cog-value ATOM (Predicate tvkeystr)) 2))

; ---------------------------------------------------------------
; Define sections on two words, that should be mergeable.

(define cnt-e-abc 13)
(define cnt-e-dgh 61)
(define cnt-e-klm 44)
(define cnt-j-abc 31)
(define cnt-j-dgh 16)

(define (setup-e-j-sections)
	(define tvkey (PredicateNode tvkeystr))
	(cog-set-value!
		(Section
			(Word "e")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "c") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-e-abc))
	(cog-set-value!
		(Section
			(Word "e")
			(ConnectorSeq
				(Connector (Word "d") (ConnectorDir "-"))
				(Connector (Word "g") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-e-dgh))
	(cog-set-value!
		(Section
			(Word "e")
			(ConnectorSeq
				(Connector (Word "k") (ConnectorDir "-"))
				(Connector (Word "l") (ConnectorDir "+"))
				(Connector (Word "m") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-e-klm))

	; Similar to above.
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "c") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-abc))
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "d") (ConnectorDir "-"))
				(Connector (Word "g") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-dgh))
	; (Section k l m) intintionally absent.

	*unspecified*
)

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

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Define sections on a third word, that can be merged into above.

(define cnt-f-abc 19)
(define cnt-f-dgh 36)
(define cnt-f-klm 34)

(define (setup-f-sections)
	(define tvkey (PredicateNode tvkeystr))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "c") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-abc))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "d") (ConnectorDir "-"))
				(Connector (Word "g") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-dgh))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "k") (ConnectorDir "-"))
				(Connector (Word "l") (ConnectorDir "+"))
				(Connector (Word "m") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-klm))

	*unspecified*
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Additional j sections, having e as connector.

(define cnt-j-abe 21)
(define cnt-j-egh 17)

(define (setup-j-extra)
	(define tvkey (PredicateNode tvkeystr))

	; Some j sections having "e" as a connector.
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-abe))
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "g") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-egh))

	*unspecified*
)

(define sec-j-abe #f)
(define sec-j-egh #f)
(define sec-ej-abv #f)
(define sec-ej-vgh #f)

(define xes-e-j-abv #f)
(define xes-e-j-vgh #f)
(define xes-ej-ej-abv #f)
(define xes-ej-ej-vgh #f)

(define (expected-j-extra-sections WC-EJ)
	(set! sec-j-abe
	(Section
		(Word "j")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-j-egh
	(Section
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-ej-abv
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
	(set! sec-ej-vgh
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-e-j-abv
	(CrossSection
		(Word "e")
		(Shape
			(Word "j")
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-e-j-vgh
	(CrossSection
		(Word "e")
		(Shape
			(Word "j")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-ej-ej-abv
	(CrossSection
		WC-EJ
		(Shape
			WC-EJ
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-ej-ej-vgh
	(CrossSection
		WC-EJ
		(Shape
			WC-EJ
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Additional e sections, having e as connector.

(define cnt-e-abe 11)
(define cnt-e-abj 9)

(define (setup-e-extra)
	(define tvkey (PredicateNode tvkeystr))

	; Some e sections having "e" as a connector.
	(cog-set-value!
		(Section
			(Word "e")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-e-abe))

	(cog-set-value!
		(Section
			(Word "e")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "j") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-e-abj))

	*unspecified*
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Define two more sections that get split indirectly.

(define cnt-a-kle 23)
(define cnt-f-kle 17)

(define (setup-indirect-sections)
	(define tvkey (PredicateNode tvkeystr))
	(cog-set-value!
		(Section
			(Word "a")
			(ConnectorSeq
				(Connector (Word "k") (ConnectorDir "-"))
				(Connector (Word "l") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-a-kle))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "k") (ConnectorDir "-"))
				(Connector (Word "l") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-kle))
)

(define sec-a-kle #f)
(define sec-f-kle #f)
(define sec-a-klv #f)
(define sec-f-klv #f)

(define (expected-indirect-sections WC-EJ)
	(set! sec-a-kle
	(Section
		(Word "a")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-f-kle
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-a-klv
	(Section
		(Word "a")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
	(set! sec-f-klv
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "k") (ConnectorDir "-"))
			(Connector (Word "l") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Additional f sections, having e as connector.
; Just like the additional j sections above.

(define cnt-f-abe 31)
(define cnt-f-egh 25)

(define (setup-f-extra)
	(define tvkey (PredicateNode tvkeystr))

	; Some j sections having "e" as a connector.
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "a") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-abe))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "g") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-egh))

	*unspecified*
)

(define sec-f-abe #f)
(define sec-f-egh #f)
(define sec-f-abej #f)
(define sec-f-ejgh #f)

(define xes-e-f-abv #f)
(define xes-e-f-vgh #f)
(define xes-ej-f-abv #f)
(define xes-ej-f-vgh #f)

(define xes-ejf-ejf-abv #f)
(define xes-ejf-ejf-vgh #f)

(define (expected-f-extra-sections WC-EJ)
	(set! sec-f-abe
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-f-egh
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-f-abej
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
	(set! sec-f-ejgh
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-e-f-abv
	(CrossSection
		(Word "e")
		(Shape
			(Word "f")
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-e-f-vgh
	(CrossSection
		(Word "e")
		(Shape
			(Word "f")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-ej-f-abv
	(CrossSection
		WC-EJ
		(Shape
			(Word "f")
			(Connector (Word "a") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-ej-f-vgh
	(CrossSection
		WC-EJ
		(Shape
			(Word "f")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "g") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Even more j sections, having multiple e connectors.

(define cnt-j-ebe 11)
(define cnt-j-eeh 19)

(define (setup-j-double-e)
	(define tvkey (PredicateNode tvkeystr))

	; Some j sections having "e" as a connector.
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-ebe))
	(cog-set-value!
		(Section
			(Word "j")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-j-eeh))

	*unspecified*
)

(define sec-j-ebe #f)
(define sec-j-eeh #f)
(define sec-ej-vbv #f)
(define sec-ej-vvh #f)

(define xes-e-j-ebv #f)
(define xes-e-j-veh #f)
(define xes-ej-ej-ebv #f)
(define xes-ej-ej-veh #f)

(define (expected-j-double-e WC-EJ)
	(set! sec-j-ebe
	(Section
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-j-eeh
	(Section
		(Word "j")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-ej-vbv
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
	(set! sec-ej-vvh
	(Section
		WC-EJ
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-e-j-ebv
	(CrossSection
		(Word "e")
		(Shape
			(Word "j")
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-e-j-veh
	(CrossSection
		(Word "e")
		(Shape
			(Word "j")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-ej-ej-ebv
	(CrossSection
		WC-EJ
		(Shape
			WC-EJ
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-ej-ej-veh
	(CrossSection
		WC-EJ
		(Shape
			WC-EJ
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; Even more f sections, having multiple e connectors.

(define cnt-f-ebe 34)
(define cnt-f-eeh 39)

(define (setup-f-double-e)
	(define tvkey (PredicateNode tvkeystr))

	; Some f sections having "e" as a connector.
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "b") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-ebe))
	(cog-set-value!
		(Section
			(Word "f")
			(ConnectorSeq
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "e") (ConnectorDir "-"))
				(Connector (Word "h") (ConnectorDir "+"))))
		tvkey
		(FloatValue 1 0 cnt-f-eeh))

	*unspecified*
)

(define sec-f-ebe #f)
(define sec-f-eeh #f)
(define sec-f-ejbej #f)
(define sec-f-ejejh #f)

(define xes-e-f-ebv #f)
(define xes-e-f-veh #f)
(define xes-ej-f-ejbv #f)
(define xes-ej-f-vejh #f)

(define xes-ejf-ejf-ejbv #f)
(define xes-ejf-ejf-vejh #f)

(define (expected-f-double-e WC-EJ)
	(set! sec-f-ebe
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "+")))))
	(set! sec-f-eeh
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! sec-f-ejbej
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "+")))))
	(set! sec-f-ejejh
	(Section
		(Word "f")
		(ConnectorSeq
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-e-f-ebv
	(CrossSection
		(Word "e")
		(Shape
			(Word "f")
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-e-f-veh
	(CrossSection
		(Word "e")
		(Shape
			(Word "f")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector (Word "e") (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))

	(set! xes-ej-f-ejbv
	(CrossSection
		WC-EJ
		(Shape
			(Word "f")
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "b") (ConnectorDir "-"))
			(Connector (VariableNode "$connector-word") (ConnectorDir "+")))))

	(set! xes-ej-f-vejh
	(CrossSection
		WC-EJ
		(Shape
			(Word "f")
			(Connector (VariableNode "$connector-word") (ConnectorDir "-"))
			(Connector WC-EJ (ConnectorDir "-"))
			(Connector (Word "h") (ConnectorDir "+")))))
)

; ---------------------------------------------------------------
(opencog-test-end)
