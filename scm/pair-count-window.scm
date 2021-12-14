;
; pair-count-window.scm
;
; Implement simple "windowed" pair counting for a single data stream.
; This assumes a single data stream of items, and counts the number
; of observations of all possible pairs of items within a sliding window
; of fixed width.
;
; Copyright (C) 2021 Linas Vepstas
;

(use-modules (opencog))
(use-modules (srfi srfi-1))

(define-public (split-text plain-text)
"
   split-text PLAIN-TEXT - split a text-string into text-items.

   This splits the utf8 string PLAIN-TEXT into text-items ('words')
   when they are separated by whitespace. Returns a list of the
   resulting text-items. Does not provide any special treatment for
   punctuation.

   See also: `tokenize-text`, which splits out punctuation as well.
"

	(filter!
		(lambda (s) (not (= 0 (string-length s))))
		(string-split plain-text char-set:whitespace))
)

(define-public (observe-pairs winsz atom-list)
"
   observe-pairs WINSIZE ATOM-LIST - count atom pairs in a window.

   WINSIZE is the window width.
   ATOM-LIST is a list of Atoms.

   All possible pairs of Atoms within a sliding window of width
   WINSIZE will be considered, and the count on each pair will be
   incremented. This implies that ever Atom will participate in
   exactly WINSIZE-1 pairs.

   The counts will be held in EvaluationLinks of the form
      (EvaluationLink
          (PredicateNode \"*-Item pairs-*\")
          left-atom
          right-atom)
"
	; The tag under which all counts are to be stored.
	(define pair-tag (PredicateNode "*-Item pairs-*"))

	; Count the item pair.
	(define (count-pair litem ritem)
		(define evl (EvaluationLink pair-tag (List litem ritem)))
		(cog-inc-count! evl 1)
		(store-atom evl))

	; Loop, making pairs between the head of the list,
	; and a window after the head.
	(define (window-pairs atli)
		(define litem (car atli))
		(define rest (cdr atli))
		(define rlen (length rest))
		(define winm1 (- winsz 1))
		(define tlen (if (< rlen winm1) rlen winm1))
		(define windo (take rest tlen))
		(for-each (lambda (ritem) (count-pair litem ritem)) windo))

	; Loop over the item-list, truncating list by one
	(define (head-pairs atli)
		(define rest (cdr atli))
		(when (not (null? rest))
			(window-pairs atli)
			(head-pairs rest)))

	(head-pairs atom-list)
   *unspecified*
)

(define-public (observe-window winsz plain-text)
"
   observe-window WINSIZE PLAIN-TEXT - count text-item pairs in a window.

   WINSIZE is the window width.
   PLAIN-TEXT is a utf8 text string.

   The PLAIN-TEXT text string is split into text-items (or 'words'),
   according to white space. There is no special handling for
   punctuation within the text string; splitting is only by whitespace.
   Then, all possible item pairs within a sliding window of width
   WINSIZE will be considered, and the count on each pair will be
   incremented. This implies that ever text-item will participate in
   exactly WINSIZE-1 pairs.
"
	(define item-list (map ItemNode (split-text plain-text)))

	(observe-pairs item-list)
   *unspecified*
)

; ---------------------------------------------------------------------
