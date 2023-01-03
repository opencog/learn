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

(define*-public (make-observe-block LLOBJ
	#:key
		(WIN-SIZE 16)
		(NUM-LINKAGES 4)
		(SPLIT-PRED char-set:whitespace)
		(STEP 1)
	)
"
   make-observe-block LLOBJ #:WIN-SIZE 16 #:NUM-LINKAGES 4
      Return a function that will count word-pairs in a block of text.

   This counting is performed by defining a sliding window, of the
   given size. All text within that window will be processed; then
   the window will slide over by #:STEP 1 steps, and the processing
   will be repeated, until the end of the block is reached. The window
   size is determined with respect to whitespace (which can be set with
   #:SPLIT-PRED, defaulting to `char-set:whitespace`)

   The optional parameter #:WIN-SIZE specifies the width of the
   sliding block, in units of white-space separated words. The
   default is 16.

   The optional parameter #:NUM-LINKAGES specifies the number of
   linkages to process for each block. The default is 4.

   The optional parameter #:SPLIT-PRED specifies a predicate that
   defines the white-space along which blocks will be split. The
   default is `char-set:whitespace`.

   The optional parameter #:STEP specifies how far the window should
   slide by each iteration. Defaults to 1. Setting it to a value greater
   than 1 will cause the last few words of the block to possibly remain
   uncounted.
"
	; Return a list of indexes (numbers) indicating the offset to
	; the next `word` in STR. Each number is the length of the word.
	; whitespace (successive series tokens satisfying the whitespace
	; predicate) is skipped over.
	(define (get-deltas STR DLIST MORE)
		(define white (string-index STR SPLIT-PRED))
		(define nonwhite
			(if white (string-skip STR SPLIT-PRED white) #f))
		(define end (if nonwhite nonwhite (string-length STR)))
		(define next (- end 1))
		(if MORE
			(get-deltas (substring STR (+ next 1)) (cons next DLIST) nonwhite)
			(reverse! DLIST)))

	; Sum the lengths in the list.
	(define (sumy LST)
		(fold (lambda (SUM ITM) (+ SUM ITM 1)) 0 (take LST WIN-SIZE)))

	; Create a list of windows, each window starting after one word.
	; This works only if DLIST is longer than the window size.
	(define (make-full-segments DLIST SEGLIST)
		(if (<= WIN-SIZE (length DLIST))
			(make-full-segments (cdr DLIST) (cons (sumy DLIST) SEGLIST))
			(reverse! SEGLIST)))

	; Create a list of windows, each window starting after one word.
	; If there are fewer words then the window size, then the final
	; window will be the whole block.
	(define (make-segments DLIST SEGLIST)
		(if (<= WIN-SIZE (length DLIST))
			(make-full-segments DLIST SEGLIST)
			(list (fold (lambda (SUM ITM) (+ SUM ITM 1)) 0 DLIST))))

	; Create a list of the starting points of each segment.
	(define (make-starts DLIST SUM STARTL)
		(if (not (nil? DLIST))
			(make-starts (cdr DLIST) (+ 1 SUM (car DLIST)) (cons SUM STARTL))
			(reverse! STARTL)))

	; The counter for the window itself.
	(define observe-text (make-pair-counter LLOBJ #:NUM-LINKAGES NUM-LINKAGES))

	(define (observe-block TEXT-BLOCK)
		(define delta-list (get-deltas TEXT-BLOCK '() #t))
		(define seg-list (make-segments delta-list '()))
		(define start-list (make-starts delta-list 0 '()))

		; Observe text blocks. Loops over the list of starting points
		; created above, and the corresponding segment lengths.
		; The loop can be made to drop all but every STEP'th text block.
		; For 1 < STEP, it can happen that the last STEP-1 words
		; are never observed... I see no easy/obvious work-around
		; for this. I guess non-unit steps are a bad idea...!?
		(define cnt 0)
		(for-each (lambda (START LEN)
				(define text-seg (substring TEXT-BLOCK START (+ START LEN)))
				(when (eq? 0 (modulo cnt STEP))
					; (format #t "text-block: >>~A<<\n" text-seg)
					(observe-text text-seg)
				)
				(set! cnt (+ cnt 1)))
			start-list seg-list))

	; Return the above function
	observe-block
)

; --------------------------------------------------------------------

(define*-public (observe-block TEXT-BLOCK)
"
   observe-block TEXT-BLOCK
      Impose a sliding window on the TEXT-BLOCK, and then submit
      everything in that window for processing.

   TEXT-BLOCK is a utf8 string of text. The 'processing' consists
   of counting all pairs in the block, updating the associated
   marginal counts, and storing the counts in storage.
"
	; `ala` is the basic pair API.
	; `alc` adds a default counting API.
	; `als` adds an API that stores the updated counts to storage.
	; `alm` adds an API that maintains marginal counts dynamically.
	(define ala (make-any-link-api))
	(define alc (add-count-api ala))
	(define als (add-storage-count alc))
	; (define alm (add-marginal-count als))

	; Skip performing the marginal counts for just right now, until
	; the rest of the dynamic-MI infrastructure is in place. Dynamic
	; marginal counts just add overhead to the counting process, if
	; we are not actually using the results.

	(define observer (make-observe-block als))

	(observer TEXT-BLOCK)
)

; ---------------------------------------------------------------------
