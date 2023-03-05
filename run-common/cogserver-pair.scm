#! /usr/bin/env -S guile -l ./cogserver-pair.scm --
!#
;
; cogserver-pair.scm
;
; Run everything needed to get ready to start the language-learning
; word-pair counting pipeline. Starts the CogServer, opens the
; database. Uses a gate to prevent counting until the CogServer is
; ready.
;
; After pair-counting has finished, but before disjunct counting is
; started, the pair-marginals must be computed, using
;  `marginals-pair.scm`. Use the shell script
; `3-mst-parsing/compute-mst-marginals.sh` to do this.
;
(use-modules (opencog) (opencog learn))
(define pair-gate (make-gate))

(define observer (make-block-pair-observer))

; This will be used for counting. Note: it blocks, until the
; gate is opened. The gate is not opened, until the StorageNode
; has been opened (in the core `coserver.scm` file).
(define (observe-block-pairs TXT)
	(wait-gate pair-gate)
	(observer TXT))

(load "cogserver.scm")

; Load up the words. Not quite needed, but ... OK.
(display "Fetch all words from database. This may take several minutes.\n")
(load-atoms-of-type 'WordNode)

; Total counts are stored here. !? If restarting counting ....
; (fetch-atom (SentenceNode "word-pairs"))
; (fetch-atom (ParseNode "word-pairs"))
; (for-each fetch-atom (cog-get-atoms 'Anchor))

; Reset the parse timer. Yes, this is a hack.
(monitor-parse-rate #t)

; Release the thundering herd.
(open-gate pair-gate)
