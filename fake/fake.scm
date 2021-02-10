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

; (use-modules (srfi srfi-194))
(load "zipf.scm")

; --------------------------------------------------------
; Convert integers to base-26 strings

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

; --------------------------------------------------------
; Return the N'th word in the vocabulary.

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
	; (define word-length
	;	(inexact->exact (ceiling (/ (log VSIZE) (log 26)))))

	(list-tabulate VSIZE (lambda (N) (make-word (+ N 1))))
)

; --------------------------------------------------------
; create word-classes -- assign words to classes with zipf distribution.
; i.e.  word-class contains zipf words in it.

(define (create-classes NCLASS CSIZE)
"
  create-classes NCLASS CSIZE - create word-classes

  Create NCLASS different word-classes. Each word-class will have
  a random number of words in it, drawn from a Zipfian distribution
  of size CSIZE. That is, some word classes will have a lot of words,
  and some will have very few.

  Return an association list of word-classes and the word in them.

  Any given word belongs to only one class.  Thus, all words in a
  a class are synonyms, and all words have only one 'meaning'.

  TODO: Create a variant function that assigns words to multiple
  word classes.

  TODO: We can make the zipfian distribution more uniform or steeper.
  This eventually needs to be an adjustable parameter.
"

	; Create NCLASS word-classes
	(define word-types
		(list-tabulate NCLASS
			(lambda (N) (string-append "<" (base-26 (+ N 1) #f) ">"))))

	(define zippy (make-zipf-generator CSIZE))

	; Return a list of words.
	; Each list starts with the word after the last word of the
	; previous list.
	(define next-word 1)
	(define zipper
		(lambda ()
			(define nwords (zippy))
			(define wrds
				(list-tabulate nwords (lambda (N) (make-word (+ N next-word)))))
			(set! next-word (+ next-word nwords))
			(list wrds)))

	; Populate the word-classes.
	(map
		(lambda (CLS)
			(cons CLS (zipper)))
		word-types)
)



; --------------------------------------------------------
; create connectors

(define (make-connector N DIR)
"
  make-connector N DIR - Return the N'th connector in directin DIR.

  Uses 26 upper-case ascii chars only, starting at \"A\".

  Basically, this just prints the number N in base-26, as a string
  of upper-case letters, and appends DIR. Connector 1 is \"A\", and
  so on.
"
	(string-append (base-26 N #t) DIR)
)


; Create disjuncts
; To simplify text generation, each disjunct shall have:
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
