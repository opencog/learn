;
; pair-count-window.scm
;
; Implement simple "windowed" pair counting for a single data stream.
; This assumes a single data stream of items, and counts the number
; of observations of all possible pairs of items within a sliding window
; of fixed width.
;
; Copyright (C) 2021 Linas Vepstas

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
   ; (observe-text-mode "clique" winsz plain-text)
)

; ---------------------------------------------------------------------
