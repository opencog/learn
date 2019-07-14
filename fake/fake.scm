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

; use sections to generate text. This is hard, actually.
; Two ways: (1) generate random strings, let LG discard those
; that do not parse. (2) something else... yow!
