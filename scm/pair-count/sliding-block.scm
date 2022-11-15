;
; sliding-block.scm
;
; Word-pair counting via random planar trees. This takes a large block
; of UTF-8 text, places a window onto it, and then submits the window
; contents to the `observe-text` function for planar-tree parsing.
; The window emulates a "sentence", except that actual sentence
; boundaries are unknown. (These are determined at later stages).
;
; Copyright (c) 2022 Linas Vepstas <linasvepstas@gmail.com>
;
; Main entry point: `(observe-block plain-text)`
;
(use-modules (opencog))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; --------------------------------------------------------------------

(define*-public (observe-block TEXT-BLOCK
	#:key
		(WIN-SIZE 16)
		(NUM-LINKAGES 12)
		(STEP 1)
	)
"
   observe-block TEXT-BLOCK --
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for processing. The window-size is
      determined by white-space.

   TEXT-BLOCK is a utf8 string of text.
"
	; Locations at which to split text.
	(define split-pred char-set:whitespace)

	; Return a list of indexes (numbers) indicating the offset to
	; the next `word` in STR. Each number is the length of the word.
	; whitespace (successive series tokens satisfying the whitespace
	; predicate) is skipped over.
	(define (get-deltas STR DLIST MORE)
		(define white (string-index STR split-pred))
		(define nonwhite
			(if white (string-skip STR split-pred white) #f))
		(define end (if nonwhite nonwhite (string-length STR)))
		(define next (- end 1))
		(if MORE
			(get-deltas (substring STR (+ next 1)) (cons next DLIST) nonwhite)
			(reverse! DLIST)))

	(define delta-list (get-deltas TEXT-BLOCK '() #t))

	; Sum the lengths in the list.
	(define (sumy LST)
		(fold (lambda (SUM ITM) (+ SUM ITM 1)) 0 (take LST WIN-SIZE)))

	; Create a list of windows, each window starting after one word.
	(define (make-segments DLIST SEGLIST)
		(if (<= WIN-SIZE (length DLIST))
			(make-segments (cdr DLIST) (cons (sumy DLIST) SEGLIST))
			(reverse! SEGLIST)))

	(define seg-list (make-segments delta-list '()))

	(define (make-starts DLIST SUM STARTL)
		(if (not (nil? DLIST))
			(make-starts (cdr DLIST) (+ 1 SUM (car DLIST)) (cons SUM STARTL))
			(reverse! STARTL)))

	(define start-list (make-starts delta-list 0 '()))

	; Observe text blocks.
	; Drop all but every STEP'th text block.
	; For 1 < STEP, it can happen that the last STEP-1 words
	; are never observed... I see no easy/obvious work-around
	; for this. I guess non-unit steps are a bad idea...!?
	(define cnt 0)
	(for-each (lambda (START LEN)
			(define text-seg (substring TEXT-BLOCK START (+ START LEN)))
			(when (eq? 0 (modulo cnt STEP))
				(format #t "text-block: >>~A<<\n" text-seg)
				; (observe-text text-seg #:NUM-LINKAGES NUM-LINKAGES)
			)
			(set! cnt (+ cnt 1)))
		start-list seg-list)
)

; ---------------------------------------------------------------------
