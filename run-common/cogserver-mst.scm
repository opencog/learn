#! /usr/bin/env -S guile -l ./cogserver-mst.scm --
!#
;
; cogserver-mst.scm
;
; Run everything needed to get ready to start the language-learning
; disjunct-counting pipeline. Starts the CogServer, opens the database,
; loads the word-pairs in the database (which can take an hour or more!).
;
; Before disjunct counting can be started, the pair-marginals must be
; computed, using `marginals-pair.scm`. Use the shell script
; `3-mst-parsing/compute-mst-marginals.sh` to do this.
;
; After disjunct counting has finished, but before grammatical
; clustering is started, the marginals must be computed, using either
; `marginals-mst.scm` or `marginals-mst-shape.scm`.
;
(use-modules (opencog) (opencog learn))
(use-modules (opencog nlp) (opencog persist) (opencog matrix))
(define mst-gate (make-gate))

; The `make-block-mpg-pipe-observer` is an Atomese pipe observer
; that should be compatible with the older make-block-mpg-observer
; but should be much faster. Make it the default.
; (define observer (make-block-mpg-observer))
(define observer (make-block-mpg-pipe-observer))

; This will be used for counting. Note: it blocks, until the
; gate is opened. The gate is not opened, until after pairs are
; loaded.
(define (observe-block-mpg TXT)
	(wait-gate mst-gate)
	(observer TXT))

; Things to do, after all text files have been submitted.
; The `mst-submit.sh` shell script will call this.
; Currently, a no-op.  The automated counters will redefine these.
(define (start-mst-submit) #f)
(define (finish-mst-submit) #f)

(load "cogserver.scm")

; Load up the words
(display "Fetch all words from database. This may take several minutes.\n")
(load-atoms-of-type 'WordNode)

; Total counts are stored here.
(fetch-atom (SentenceNode "MST"))
(fetch-atom (ParseNode "MST"))
(for-each fetch-atom (cog-get-atoms 'Anchor))

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all word-pairs. This may take well over half-an-hour!\n")

; The object which will be providing pair-counts for us.
; One can also do MST parsing with other kinds of pair-count objects,
; for example, the clique-pairs, or the distance-pairs. But, for now,
; we're working with planar-parse ANY-link pairs.
(define pair-obj (make-any-link-api))
(define cnt-obj (add-count-api pair-obj))
(define star-obj (add-pair-stars cnt-obj))
(pair-obj 'fetch-pairs)

; Also load all link-grammar bond-links.
(display "Fetch all bonds. This may take a long time as well!\n")
((make-bond-link-api) 'fetch-pairs)

; Check to see if the marginals have been computed.
; Common error is to forget to do them manually.
; So we check, and compute if necessary.
(catch #t
	(lambda ()
		((add-report-api star-obj) 'num-pairs)
		(print-matrix-summary-report star-obj)

		; Reset the parse timer. Yes, this is a hack.
		(monitor-parse-rate #t)

		; Release anyone who is waiting on us.
		(open-gate mst-gate)
	)
	(lambda (key . args)
		(format #t "Warning! Word pair marginals missing!\n")
		(format #t "MST counting will not work without these!\n")
		(format #t "Run `(batch-pairs star-obj)` to compute them.\n")
		#f))
