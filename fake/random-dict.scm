;
; random-dict.scm -- create random Link-Grammar dictionaries.
;
; Copyright (c) 2019, 2021 - Linas Vepstas
;
; --------------------------------------------------------
; OVERVIEW:
; Create random Link Grammar dictionaries with specific statistical
; distributions of link-types, disjunct-lengths, vocabulary size, etc.
;
; Example Usage:
; --------------
; See documentation below for description of paramaters.
; (define dictgen (create-dict-generator 10 10 10 3 20))
; (define dict (dictgen))
;
; Print dict to stdout.
; (print-LG-flat #t dict)
;
; Print dict to file.
; (define port (open-file "/tmp/4.0.dict" "w"))
; (print-LG-flat port dict)
; (fsync port) (close port)
;
; Issues: 
; -------
; This will generate grammers parts of which are "impossible".
; That is, it will generate grammars that have disjuncts that may be
; impossible to use in a sentence, because there is no way of using
; that disjunct during sentence generation.  The sentence generator
; will need to keep track of unusable disjuncts.
;
; This file has many TODO's to expand the kinds and varieties
; of grammars that could be produced.

(use-modules (srfi srfi-1))

; (use-modules (srfi srfi-194))
; (load "zipf.scm")

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

(define (make-wordlist-generator LEN)
"
  make-wordlist-generator LEN -- return a random wordlist.

  Create a list of words of at most length LEN, having a Zipfian
  distribution.

  The current implementation will never re-use a previously-used word.
  That is, synonyms will never be created.

  TODO: Create a variant that generates synonyms.

  TODO: We can make the zipfian distribution more uniform or steeper.
  This eventually needs to be an adjustable parameter.
"
	(define zippy (make-zipf-generator LEN))

	; Return a list of words.
	; Each list starts with the word after the last word of the
	; previous list.
	(define next-word 1)
	(lambda ()
		(define nwords (zippy))
		(define wrds
			(list-tabulate nwords (lambda (N) (make-word (+ N next-word)))))
		(set! next-word (+ next-word nwords))
		wrds)
)

; --------------------------------------------------------
; create connectors

(define (make-connector N DIR)
"
  make-connector N DIR - Return the N'th connector in direction DIR.

  Uses 26 upper-case ascii chars only, starting at \"A\".

  Basically, this just prints the number N in base-26, as a string
  of upper-case letters, and appends DIR. Connector 1 is \"A\", and
  so on.
"
	(string-append (base-26 N #t) DIR)
)

(define (make-connector-generator N)
"
  make-connector-generator N - Return a generator for random connectors.
  The connector will be chosen randomly out of N of them, with Zipf
  distribution and random 50-50 direction.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define zippy (make-zipf-generator N))

	(define (dire) (if (< 0 (random 2)) "+" "-"))

	(lambda () (make-connector (zippy) (dire)))
)

; Create disjuncts
;
; TODO: Implement a mode with head and tail indicators, so that
; each disjunct has:
; * Exactly one incoming (head) connector.
; * Zero or more outgoing (tail/dependent) connectors.
; * Connectors must match, as per link-grammar usual.
;
; TODO: use bi-directional directions, for order-independent languages.

(define (make-disjunct-generator NCON DSIZE)
"
  make-disjunct-generator NCON DSIZE - Create random disjuncts.

  The length of the disjuncts will be at most DSIZE, and they will
  employ at most NCON different link types.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define congen (make-connector-generator NCON))
	(define zippy (make-zipf-generator DSIZE))

	(lambda ()
		(list-tabulate (zippy) (lambda (X) (congen)))
	)
)

; create sections
(define (make-section-generator NLKTYPES DSIZE NDISJ)
"
  make-section-generator NLKTYPES DSIZE NDISJ - Create random sections.

  The length of the disjuncts will be at most DSIZE, and they will
  employ at most NLKTYPES different link types. Each section will contain
  at most NDISJ disjuncts.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define disgen (make-disjunct-generator NLKTYPES DSIZE))
	(define zippy (make-zipf-generator NDISJ))

	(lambda ()
		(list-tabulate (zippy) (lambda (X) (disgen)))
	)
)

; --------------------------------------------------------
; Create dictionaries

(define (make-tag-generator TAG)
"
  make-pos-generator TAG -- return the next unused pos or wordclass.
"
	(define next-cls 1)

	(lambda ()
		(define cls (string-append "<" TAG (base-26 next-cls #f) ">"))
		(set! next-cls (+ 1 next-cls))
		cls)
)

(define-public (create-dict-generator NCLASS CSIZE NLKTYPES DSIZE NDISJ)
"
  create-dict-generator NCLASS CSIZE NLKTYPES DSIZE NDISJ - create dictionary

  Create NCLASS different word-classes (dictionary entries)
  Each word-class will have a random number of words in it, drawn
  from a Zipfian distribution of size CSIZE. That is, some word
  classes will have a lot of words, and some will have very few.

  Associated with each word-class will be a section (a collection
  of disjuncts). Each disjunct will use at most NLKTYPES link types.
  Each section will have at most NDISJ disjuncts in it.  Each
  disjunct will be at most DSIZE in size.

  Return an association list of word-classes and the word in them.

  TODO: Fix the 'core factorization'. Currently, a word will only
  appear in any one word-class; thus no synonyms. By contrast,
  a disjunct may or may not appear in multiple word classes, and
  there is no explicit way to control this. We want explicit control
  over this.
"
	(define pos (make-tag-generator "pos-"))
	(define wcg (make-tag-generator "cls-"))
	(define wlg (make-wordlist-generator CSIZE))
	(define msg (make-section-generator NLKTYPES DSIZE NDISJ))

	; Populate the word-classes.
	(lambda ()
		(reverse (list-tabulate NCLASS
			(lambda (N) (list (wcg) (wlg) (msg))))))
)

; --------------------------------------------------------
; Print dictionary.

(define-public (print-LG-flat DEST DICT)
"
  print-LG-flat DEST DICT - print a Link-Grammar style dictionary.

  Prints a flat-file style dictionary.

  The DEST must be the destination file-handle, or #t to print to
  stdout, or #f to print to string.

  The DICT must be a dictionary.
"

	(define (prt-wordlist WL)
		(string-join
			(map (lambda (WORD) (format #f "~A" WORD)) WL)))

	(define (prt-disjunct CL)
		(string-join
			(map (lambda (CON) (format #f "~A" CON)) CL)
			" & "))

	(define (prt-section SECT)
		(string-join
			(map (lambda (DISJ) (format #f "(~A)" (prt-disjunct DISJ))) SECT)
			" or "))

	(for-each
		(lambda (ENTRY)
			(format DEST "\n")
			; (format DEST "% ~A\n" ENTRY)
			(format DEST "~A: ~A;\n" (prt-wordlist (second ENTRY)) (first ENTRY))
			(format DEST "~A: ~A;\n" (first ENTRY) (prt-section (third ENTRY)))
		)
		DICT)
)

; --------------------------------------------------------
