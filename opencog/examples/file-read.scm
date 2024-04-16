;
; file-read.scm -- file-reading demo
;
; Demo opening a file and streaming the contents into a processing
; pipeline.
;
(use-modules (opencog) (opencog exec) (opencog sensory))

; Before running this demo, copy `demo.txt` to the /tmp directory.
; This is a text file, it will be read and processed in this demo.

; --------------------------------------------------------
; Basic demo: Open a file for reading, at a fixed absolute location
; in the filesystem. Executing the FileReadNode will return a text
; stream value.
(define txt-stream
	(cog-execute! (FileReadNode "file:///tmp/demo.txt")))

; Repeated references to the stream will return single lines from
; the file.
txt-stream
txt-stream
txt-stream
txt-stream
txt-stream

; The above reads until end-of-file.

; --------------------------------------------------------
; Demo: Perform indirect streaming. The file-stream will be placed
; as a Value on some Atom, where it can be accessed and processed.
;
; Open the file, get the stream, and place it somewhere.
(cog-set-value! (Concept "foo") (Predicate "some place")
	(cog-execute! (FileReadNode "file:///tmp/demo.txt")))

; A better, all-Atomese version of the above. Note that the SetValueLink
; will execute the FileReadNode, grab whatever it gets from that exec,
; and then places it at the indicated location.
(cog-execute!
	(SetValue (Concept "foo") (Predicate "some place")
		(FileRead "file:///tmp/demo.txt")))

; Define an executable node that will feed the stream of text.
(define txt-stream-gen
	(ValueOf (Concept "foo") (Predicate "some place")))

; Access the file contents. Each time this is executed, it gets the
; next line in the file.
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)

; --------------------------------------------------------
; Demo: Perform processing on the stream. In this case, parse the
; input stream into token pairs. Use the LG "any" parser for this.

(use-modules (opencog nlp) (opencog nlp lg-parse))

; As above: rewind the stream to the begining:
(cog-execute!
	(SetValue (Concept "foo") (Predicate "some place")
		(FileRead "file:///tmp/demo.txt")))

; Parse the file contents, one line at a time. The "any" dict generates
; random word-pairs. The (Number 1) asks for only one parse per
; sentence.
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))
