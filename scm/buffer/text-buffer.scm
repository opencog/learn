;
; text-buffer.scm
;
; Provide a simple queue for tagged text streams.
;
; The goal here is for the network to provide one or more streams of
; text to process, which accumulate in a queue, and the rpocessing code
; to dequeue and analyze it in the way that it wants to.  This way, the
; sender does not need to make any assumptions about how the receiver
; will process the data. This includes assumptions about sentence 
; splitting, ends of paragraphs, etc.
;
; Copyright (c) 2022 Linas Vepstas
;
; --------------------------------------------------------

