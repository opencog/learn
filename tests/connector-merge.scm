;
; connector-merge.scm
;

(use-modules (opencog) (opencog nlp))
(use-modules (opencog nlp learn))

(use-modules (opencog test-runner))

(opencog-test-runner)

; ---------------------------------------------------------------
; Some very simple test vectors
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
)

; ---------------------------------------------------------------
(define t-simple "simplest merge test")
(test-begin t-simple)

(setup-basic-sections)
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(csc 'explode-sections)
(define gsc (add-cluster-gram csc))

(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))




(test-equal "foo" #t (not #f))
(test-end t-simple)

; ---------------------------------------------------------------
