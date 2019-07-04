; Maximum Spanning Tree parser.
;
; Given a raw-text sentence, it splits apart the sentence into distinct
; words, and finds an (unlabelled) dependency parse of the sentence, by
; finding a dependency tree that maximizes the mutual information.
; A list of word-pairs, together with the associated mutual information,
; is returned.
;

(define-public (mst-parse-text-file plain-textblock DIST-MOD)
"
	Procedure to MST-parse sentences coming from an instance-pair weight file.
"
	; Split input textblock
	(define split-textblock
		(string-split plain-textblock #\newline)
	)

	; Define and add LEFT-WALL to current-sentence
	(define current-sentence 
		(string-append "###LEFT-WALL### " (car split-textblock))
	)

	; Assuming input is tokenized, this procedure separates by spaces
	(define (word-strs text-line)
		(string-split text-line #\ )
	)

	; Create weights 2D-array from file info
	(define weights-array
		(let* ((array-dim (length (word-strs current-sentence)))
			(tmp-array (make-array -1e40 array-dim array-dim))
			)
			; split each line and assign weight to corresponding element
			(for-each
				(lambda (weightline)
					(define split-weightline (word-strs weightline))
					(define i1 (string->number (list-ref split-weightline 0)))
					(define i2 (string->number (list-ref split-weightline 2)))
					(define weight (string->number (list-ref split-weightline 4)))

					(array-set! tmp-array weight i1 i2)
				)
				(cdr split-textblock)
			)
			tmp-array
		)
	)

	; Create a list of atoms from the sequence of strings and their position.
	; Atoms have the structure:
	; (WordSequenceLink
	;		(WordNode "example") 
	;		(NumberNode 2)
	; )
	(define (word-list list-of-words)
		(define cnt -1)
		(map 
			(lambda (str) 
				(set! cnt (+ cnt 1))
				(WordSequenceLink
					(WordNode str)
					(NumberNode cnt)
				)
			)
			list-of-words
		)
	)	

	; Define scoring function to look for values in weights-array.
	; Scorer lambda function should refer to weights-array from its
	; current environment (array was defined above).
	; Scoring function considers weight multipliers coming from DIST-MOD
	(define scorer 
		(lambda (left-atom right-atom distance)
			(define left-index (inexact->exact (string->number (cog-name (gdr left-atom)))))
			(define right-index (inexact->exact (string->number (cog-name (gdr right-atom)))))
			; modifier values are given in DIST-MOD. If distance is longer than 
			; what has been defined in the array, use last array value:
			(define modifier (list-ref DIST-MOD (- (min distance (length DIST-MOD)) 1)))

			(* modifier (array-ref weights-array left-index right-index))
		)
	)

	(define trunc-scorer (make-trunc-scorer scorer))

	; Entry point, call parser on atomized sentence with ad-hoc scorer
	(mst-parse-atom-seq (word-list (word-strs current-sentence)) trunc-scorer)

)


(define-public (mst-parse-text-mode plain-text cnt-mode mst-dist)

	; Assuming input is tokenized, this procedure separates by spaces 
	; and adds LEFT-WALL
	(define word-strs (cons '"###LEFT-WALL###" (string-split plain-text #\ ))
	)

	; Create a sequence of atoms from the sequence of strings.
	(define word-list (map (lambda (str) (WordNode str)) word-strs))

	; Define where the costs are coming from.
	(define pair-obj
		(cond
			((or (equal? cnt-mode "clique")
				 (equal? cnt-mode "clique-dist"))
					(make-clique-pair-api))
			(else (make-any-link-api))))

	(define mi-source (add-pair-freq-api pair-obj))

	(define scorer (make-score-fn mi-source 'pair-fmi))

	; Assign a bad cost to links that are too long --
	; longer than 16. This is a sharp cutoff.
	; This causes parser to run at O(N^3) for LEN < 16 and
	; a faster rate, O(N^2.3) for 16<LEN. This should help.
	; Otherwise, assign modification to scorer depending on
	; mst-parsing mode. If mst-distance accounting is activated
	; shift all mi-values by 1/LEN, where LEN is the difference
	; in positions in a sentence between words in word-pair.
	(define (trunc-scorer LW RW LEN)
		(if (< 16 LEN)
			-2e25
			(let
				((modifier (if mst-dist (/ 1 LEN) 0)))
				(+ modifier (scorer LW RW LEN)))))

	; Process the list of words.
	(mst-parse-atom-seq word-list trunc-scorer)
)

; wrapper for backwards compatibility
(define-public (mst-parse-text plain-text)
	(mst-parse-text-mode plain-text "any" #f))

; ---------------------------------------------------------------------
(define (export-mst-parse plain-text mstparse filename)
"
  Export an MST-parse to a text file named filename,
  so that parses can be examined.
  The format is:
  [sentence]
  [word1#] [word1] [word2#] [word2]
  [word2#] [word2] [word4#] [word4]
  ...
"
	; open output file
	(define file-port (open-file filename "a"))

	; functions for getting specific parts of the link
	(define (get-mi link) (cdr link))
	(define (get-lindex link)
		(- (car (car (car link))) 1))
	(define (get-rindex link)
		(- (car (cdr (car link))) 1))
	(define (get-lword link)
		(let ((atom (cdr (car (car link)))))
			(if (cog-link? atom)
				(cog-name (gar atom))
				(cog-name atom)
			)
		)
	)
	(define (get-rword link)
		(let ((atom (cdr (cdr (car link)))))
			(if (cog-link? atom)
				(cog-name (gar atom))
				(cog-name atom)
			)
		)
	)

	; link comparator to use in sort func
	(define link-comparator
		(lambda (l1 l2)
			(< (get-lindex l1) (get-lindex l2))))

	; Print the sentence first
	(if (not (null? plain-text))
		(display
			(format #f "~a\n"
				plain-text)
			file-port))

	; Print the links if they are not bad-pair
	(for-each
		(lambda (l) ; links
			(if (> (get-mi l) -1.0e10) ; bad-MI
				(display
					(format #f "~a ~a ~a ~a ~a\n"
						(get-lindex l)
						(get-lword l)
						(get-rindex l)
						(get-rword l)
						(get-mi l))
				file-port)))
		(sort mstparse link-comparator)
	)

	; Add a new line at the end
	(display "\n" file-port)

	(close-port file-port)
)

(define-public (observe-mst-mode plain-text CNT-MODE MST-DIST EXPORT-MST)
"
  observe-mst-mode -- update pseduo-disjunct counts by observing raw text.
  
  Build mst-parses using MI calculated beforehand.
  When MST-DIST is true, word-pair MI values are adjusted for distance.
  Obtained parses are exported to file if EXPORT-MST is true.
  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up.
"
	(define file-cnt-mode (if (equal? CNT-MODE "file") #t #f))
	(define parse 
		(if file-cnt-mode
			(mst-parse-text-file plain-text MST-DIST)
			(mst-parse-text-mode plain-text CNT-MODE MST-DIST)
		)
	)
	(display parse)

	; The count-one-atom function fetches from the SQL database,
	; increments the count by one, and stores the result back
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections parse)
	)
	(if EXPORT-MST
		(if file-cnt-mode
			(export-mst-parse (car (string-split plain-text #\newline)) parse "mst-parses.ull")
			(export-mst-parse plain-text parse "mst-parses.ull")
		)
	)

	parse; return the parse, for unit-test purposes
)

; Wrapper for backwards compatibility
(define-public (observe-mst plain-text)
	(observe-mst-mode plain-text "any" #f #f)
)
