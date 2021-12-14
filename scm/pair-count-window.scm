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

   *unspecified*
)

; ---------------------------------------------------------------------
