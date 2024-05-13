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

; Global. Create this only once.
(define observer (make-block-random-pair-observer))

; This will be used for counting. Note: it blocks, until the
; gate is opened. The gate is not opened, until the StorageNode
; has been opened (in the core `coserver.scm` file).
(define (observe-block-pairs TXT)
	(wait-gate pair-gate)
	(observer TXT))

; Things to do, after all text files have been submitted.
; The `pair-submit.sh` shell script will call this.
; Currently, a no-op.  The automated counters will redefine these.
(define (start-pair-submit) #f)
(define (finish-pair-submit) #f)

(load "cogserver.scm")

; Load up the words. Not quite needed, but ... OK.
(display "Fetch all words from database. This may take several minutes.\n")
(load-atoms-of-type 'WordNode)
(load-atoms-of-type 'SentenceNode)
(load-atoms-of-type 'ParseNode)
;;; (load-atoms-of-type 'AnchorNode)

; Reset the parse timer. Yes, this is a hack.
(monitor-parse-rate #t)

; Release the thundering herd.
(open-gate pair-gate)
