;
; fake.scm -- tools for creating fake (artificial) languages.
;
; Copyright (c) 2019 - Linas Vepstas
;
; --------------------------------------------------------
; OVERVIEW:
; Assumes a ....
;

(use-modules (srfi srfi-1))

(define (base-26 N UP)
"
  base-26 N UP - Print number N in base-26. This returns a string.
  If UP is #t, then the string is all upper-case letters; else it's
  lower-case.

  0 is a blank space, Word 1 is \"a\", and so on.
"
	; 65 is the ascii-table offset to upper-case A
	; 97 is the ascii-table offset to lower-case a
	(define off (if UP 65 97))
	(define lett (string (integer->char (+ off (remainder (- N 1) 26)))))
	(define quot (quotient (- N 1) 26))
	(cond
		((<= N 0) " ")
		((= 0 quot) lett)
		(else (string-append/shared (base-26 quot UP) lett)))
)

(define (make-word N)
"
  make-word N - Return the N'th word in the vocabulary.

  Uses 26 lower-case ascii chars only, starting at \"a\".

  Basically, this just prints the number N in base-26, as a string
  of lower-case letters. Word 0 is a blank space, Word 1 is
  lower-case a, and so on.
"
	(base-26 N #f)
)

(define (create-vocab VSIZE)
"
  create-vocab VSIZE - Create a list containing VSIZE words.
"
	; Number of letters in a word, assuming ASCII 26 letters.
	(define word-length
		(inexact->exact (ceiling (/ (log VSIZE) (log 26)))))

	(list-tabulate VSIZE (lambda (N) (make-word (+ N 1))))
)

; --------------------------------------------------------
; Draw random numbers from a (Zipfian) distribution

; --------------------------------------------------------
; create word-classes -- assign words to classes with zipf distribution.
; i.e. each word-class contains zipf words in it.

; --------------------------------------------------------
; create connectors

; Create disjuncts
; To simplify text generation, each disjunct shal have:
; * Exactly one incoming (head) connector.
; * Zero or more outgoing (tail/dependent) connectors.
; * Connectors must match, as per link-grammar usual.

; create sections

; --------------------------------------------------------
; Generate text. There are two ways to do this:
; A) Start with sections, and build a sentence
; B) Create a random planar tree, and assign sections to it.
;
; Approach A) is difficult: it basically means we have to run the
; parser, using a dictionary containing the desired sections, and
; allowing an "any word" mode during parsing. This can be done,
; because we already have scheme interfaces into LG, via the
; ParseMinimalLink. But its complex and awkard.
;
; Approach B) is easier(?): create an unlabelled tree; that's easy.
; Start adding random labels to it, veryifying that each disjunct
; is in the dictionary. This is harder, as this is a coloring problem,
; and requires backtracking if the first coloring attempt fails.
; As the final step, one randomly picks a word from the dictionary that
; appears in a section for that disjunct.
