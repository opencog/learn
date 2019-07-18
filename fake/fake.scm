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

(define (make-word N)
"
  make-word N - Return the N'th word in the vocabulary.

  Uses 26 lower-case ascii chars only, starting at "a".
"
	; 97 is the ascii-table offset to lower-case a
	(define let (string (integer->char (+ 97 (remainder (- N 1) 26)))))
	(define quot (quotient (- N 1) 26))
	(cond
		((<= N 0) " ")
		((= 0 quot) let)
		(else (string-append/shared (make-word quot) let)))
)

(define (create-vocab SIZE)
"
  create-vocab SIZE - Create a random list containing SIZE words.
"
	; Number of letters in a word, assuming ASCII 26 letters.
	(define word-length
		(inexact->exact (ceiling (/ (log SIZE) (log 26)))))

	(list-tabulate SIZE (lambda (N) (make-word (+ N 1))))
)

; create word-classes -- assign words to classes with zipf distribution.
; i.e. each word-class contains zipf words in it.

; create connectors

; create disjuncts

; create sections

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
