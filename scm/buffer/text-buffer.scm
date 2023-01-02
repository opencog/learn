;
; text-buffer.scm
;
; Provide a simple queue for tagged text streams.
;
; The goal here is for the network to provide one or more streams of
; text to process, which accumulate in a queue, and the processing code
; to dequeue and analyze it in the way that it wants to.  This way, the
; sender does not need to make any assumptions about how the receiver
; will process the data. This includes assumptions about sentence 
; splitting, ends of paragraphs, etc.
;
; Copyright (c) 2022 Linas Vepstas
;
; --------------------------------------------------------

(use-modules (opencog))
(use-modules (srfi srfi-1))

; Anchor for all text buffers in the system
(define *-buffer-base-* (AnchorNode "*-Text Buffers-*"))

; --------------------------------------------------------
(define-public (buffer-open)
"
  buffer-open -- Create a new buffer for reading and writing.

  Returns a TagNode Atom identifying the buffer stream.

  See also:
    buffer-append -- Add text to bufffer.
"
	(define buftag (random-node 'TagNode 16 "text"))
	(TagLink buftag *-buffer-base-*)

	; Return the tag.
	buftag
)

; --------------------------------------------------------

(define-public (buffer-append TAGNAME TEXT)
"
  buffer-append TAGNAME TEXT-- append utf-8 TEXT to the TAGANAME buffer

  Returns #t if sucessful, else #f.
  Failure reasons include -- no such buffer, buffer not open.

  See also:
    buffer-open -- Create a new text buffer.
"
	(define buftag (cog-node 'TagNode TAGNAME))
	(if (or (nil? buftag)
			(nil? (cog-link 'TagLink buftag *-buffer-base-*))) #f
		xx
	)
)

; --------------------------------------------------------
