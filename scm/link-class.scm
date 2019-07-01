

;; Just count how many disjuncts connect to some word.
(define (count-half-links WORD-LST)
	(fold
		(lambda (CTR cnt)
			(+ cnt (cog-incoming-size-by-type CTR 'ConnectorSeq)))
		0
		(append-map
			(lambda (WRD) (cog-incoming-by-type WRD 'Connector))
			WORD-LST)))
