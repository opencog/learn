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
; See `run-common/gen-dict.scm` for an example usage.
; See documentation below for description of parameters.
;
; Issues:
; -------
; This will generate grammars parts of which are "impossible".
; That is, it will generate grammars that have disjuncts that may be
; impossible to use in a sentence, because there is no way of using
; that disjunct during sentence generation.  The sentence generator
; will need to keep track of unusable disjuncts and report them.
;
; This file has some TODO's to expand the kinds and varieties
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

; --------------------------------------------------------
; Create connectors

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

(define (make-connector-generator N EXP)
"
  make-connector-generator N EXP - generator for random connectors.

  Return a generator that returns random connectors. They will be
  chosen randomly, out of N, following a Zipfian distribution with
  exponent EXP. For EXP=1, the classic Zipf diistribution is used,
  which results in the link-type 'A' being more common than 'B' ... etc.
  For EXP=0, the distribution becomes uniform, and all link types
  are equally likely.

  The connector direction will be randomly left-right with a 50-50
  probability.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define zippy (make-zipf-generator N EXP))

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

(define (make-disjunct-generator NLINK DSIZE LINK-EXP SIZE-EXP)
"
  make-disjunct-generator NLINK DSIZE LINK-EXP SIZE-EXP - Random disjuncts.

  Return a generator for random disjuncts. Each disjunct is a
  sequence of randomly chosen connectors. Connectors are chosen
  out of a pool of NLINK different link types. The LINK-EXP is the
  Zipf exponent for choosing link types; a value of 0 gives a uniform
  distribution, a value of 1 makes the first link types more likely.

  The length of the sequence of connectors will be chosen randomly,
  but will never exceed DSIZE. The length of the sequence is given
  by the Zipfian distribution with exponent SIZE-EXP. Setting
  SIZE-EXP=1 gives the classic Zipf distribution, and most disjuncts
  will be length 1 or 2. Setting SIZE-EXP to a negative value will
  cause most disjuncts to be of length DSIZE.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define congen (make-connector-generator NLINK LINK-EXP))
	(define zippy (make-zipf-generator DSIZE SIZE-EXP))

	(lambda ()
		(list-tabulate (zippy) (lambda (X) (congen)))
	)
)

; Create sections
(define-public (make-section-generator NLKTYPES DSIZE NDISJ LINK-EXP D-EXP S-EXP)
"
  make-section-generator NLKTYPES DSIZE NDISJ LINK-EXP D-EXP S-EXP
  Create random sections.

  A 'section' is a collection of disjuncts. The number of disjuncts
  in a section is controlled by NDISJ and S-EXP.  The number of
  disjuncts in a section will vary from 1 to NDISJ, following a Zipfian
  distribution with exponent S-EXP. For S-EXP=1, most sections will
  have only 1 disjunct in them, only a few will have 2 or more. For
  S-EXP negative, most sections will have NDISJ disjuncts in them.

  The size of a disjunct is controlled by DSIZE and D-EXP.

  The distribution of connector types in a disjunct is controlled by
  NLKTYPES and LINK-EXP. See `make-disjunct-generator` for details.

  Uses 26 upper-case ascii chars only, starting at \"A\".
"
	(define disgen (make-disjunct-generator NLKTYPES DSIZE LINK-EXP D-EXP))
	(define zippy (make-zipf-generator NDISJ S-EXP))

	(lambda ()
		(list-tabulate (zippy) (lambda (X) (disgen)))
	)
)

; --------------------------------------------------------
; Create dictionaries

(define-public (make-pos-generator NPOS SECT-GEN)
"
  make-pos-generator NPOS SECT-GEN - Make lists of disjuncts

  Create NPOS different disjunct-collections (sections).
  The function SECT-GEN is called to obtain a section.

  Return an association list of pos-tags and the disjuncts in them.
"
	(define (pos N) (string-append "<pos-" (base-26 (+ 1 N) #f) ">"))

	; Convert sections to strings
	(define (sex)
		(map
			(lambda (CONLIST)
				(string-append
					"("
					(string-join CONLIST " & ")
					")"))
			(SECT-GEN)))

	; Populate the pos-tags.
	(lambda ()
		(list-tabulate NPOS (lambda (N) (list (list (pos N)) (sex)))))
)

(define-public (make-class-generator NCLASS NPOS CSIZE EXP)
"
  make-class-generator NCLASS NPOS CSIZE EXP - collections of POS tags

  Create NCLASS different word classes.
  Each word class will have at most CSIZE different pos-tags in it.
  These will be assigned randomly following a Zipfian distribution.
  The pos-tags will be drawn randomly from a pool of size NPOS.
  The slope of the distribution is controlled by EXP, with EXP=1
  giving the classic Zipf distribution, and EXP=0 giving the uniform
  distribution. A negative EXP will cause most word-classes to have
  CSIZE members in them.

  Return an association list of class-tags and the pos-tags in them.
"
	(define (pos N) (string-append "<pos-" (base-26 (+ 1 N) #f) ">"))
	(define (fcl N) (string-append "<fcl-" (base-26 (+ 1 N) #f) ">"))

	(define zippy (make-zipf-generator CSIZE EXP))
	(define (pick-pos)
		(list-tabulate (zippy) (lambda (N) (pos (random NPOS)))))

	; Populate the word-classes.
	(lambda ()
		(list-tabulate NCLASS (lambda (N) (list (list (fcl N)) (pick-pos)))))
)

; --------------------------------------------------------
;
(define-public (make-wall-generator NCLASS NROOTS NWALLS ENDERS)
"
  make-wall-generator NCLASS NROOTS NWALLS ENDERS - create root-word classes

  Out of NCLASS different word-classes, connect NWALLS of them to
  the left-wall. Use NROOTS different kinds of wall connector types.

  It is recommended that NCLASS be equal to or larger than NWALLS * NROOTS.

  This emulates the idea of a 'root word' in a dependency grammar.
  For example, in English, if NROOTS=1, this would be the 'main verb'
  of a sentence, which is indicated with a link to LEFT-WALL. If NROOTS
  was 2, then there would be connectors to the man verb and the main
  subject (main noun).

  ENDERS is a string containing sentence-ending punctuation marks.
  For example, the string '. ? !'. If this string is empty, no
  punctuation will be generated.

  Return an association list of word-classes possibly with walls on them.
"
	(define (wall-plus N) (string-append "WALL" (base-26 (+ 1 N) #t) "+"))
	(define (wall-minus N) (string-append "WALL" (base-26 (+ 1 N) #t) "-"))

	; Define the wall connectors
	(define wall-dj
		(string-join (list-tabulate NROOTS wall-plus) " & "))

	(define have-punct
		(and (not (nil? ENDERS)) (< 0 (string-length ENDERS))))

	(define dj-punct
		(if have-punct (string-append wall-dj " & PUNCT+") wall-dj))

	; Define the wall, optionally with the ending punctuation.
	(define wall
		(list
			(if (< 0 NROOTS) (list (list "LEFT-WALL") (list dj-punct)) '())
			(if have-punct (list (list ENDERS) (list "PUNCT-")) '())))

	(define (fcl N) (string-append "<fcl-" (base-26 (+ 1 N) #f) ">"))
	(define (wcl N) (string-append "<wcl-" (base-26 (+ 1 N) #f) ">"))

	; Populate the word-classes.
	(define asocs
			(list-tabulate NCLASS
				(lambda (N) (list (list (wcl N)) (list
					(if (< N (* NWALLS NROOTS))
						(string-append "(" (fcl N) " & " (wall-minus (modulo N NROOTS)) ")")
						(fcl N)))))))

	(lambda () (append wall asocs))
)

(define-public (make-sense-generator VFRAC NCLASS NSENSES EXP)
"
  make-sense-generator VFRAC NCLASS NSENSES EXP - create word senses

  Place a fraction VFRAC of the total number of vocabulary words into
  multiple classes picked randomly from NCLASS (with a uniform
  distribution). Each word will belong to at most NSENSES different
  classes.  The number of senses will be chosen randomly, with a
  Zipfian distribution with exponent EXP.  That is, some words will
  have many senses, and some will have very few.  The EXP is the
  exponent of the Zipfian distribution; set EXP=1 for a pure Zipf,
  and set EXP=0 for a uniform distribution.

  A word with multiple word-senses is just a word that has has multiple
  distinct grammatical behaviors; that it, it belongs to multiple
  different word-classes.

  Return an association list of words and the classes they below to.
"
	(define (wcl N) (string-append "<wcl-" (base-26 (+ 1 N) #f) ">"))
	(define zippy (make-zipf-generator NSENSES EXP))
	(define (pick-cls)
		(list-tabulate (zippy) (lambda (N) (wcl (random NCLASS)))))

	; Populate the word-classes.
	(lambda ()
		(define NVOCAB (inexact->exact (floor (* VFRAC next-word))))
		(list-tabulate NVOCAB
			(lambda (N) (list (list (make-word (+ N 1))) (pick-cls)))))
)

; --------------------------------------------------------
; Create word-classes -- assign words to classes with zipf distribution.
; i.e.  word-class contains zipf words in it.

; Used for counting the number of words issued.
; The final value of this will be the vocabulary size.
; This is a global, specific to this module.
(define next-word 1)
(define (get-vocab-size) next-word)

(define (make-wordlist-generator LEN EXP)
"
  make-wordlist-generator LEN EXP -- return a random wordlist.

  Create a list of words of at most length LEN, having a Zipfian
  distribution with exponent EXP. Use EXP=1 for Zipf, and EXP=0
  for a uniform distribution.

  The current implementation will never re-use a previously-used word.
  That is, multiple meanings will never be created.
"
	(define zippy (make-zipf-generator LEN EXP))

	; Return a list of words.
	; Each list starts with the word after the last word of the
	; previous list.
	(lambda ()
		(define nwords (zippy))
		(define nstart next-word)
		(set! next-word (+ next-word nwords))
		(list-tabulate nwords (lambda (N) (make-word (+ N nstart)))))
)

(define-public (make-word-generator NCLASS CSIZE EXP)
"
  make-word-generator NCLASS CSIZE EXP - create word-classes

  Create NCLASS different word-classes, placing at most CSIZE
  words into each class.
  Each word-class will have a random number of words in it, drawn
  from a Zipfian distribution of size CSIZE. That is, some word
  classes will have a lot of words, and some will have very few.
  The EXP is the exponent of the Zipfian distribution; set EXP=1
  for a pure Zipf, and set EXP=0 for a uniform distribution.

  Return an association list of word-classes and the words in them.
"
	(define (wcl N) (string-append "<wcl-" (base-26 (+ 1 N) #f) ">"))
	(define (wclr N) (wcl (- NCLASS N 1)))
	(define wlg (make-wordlist-generator CSIZE EXP))

	; Populate the word-classes.
	(lambda ()
		(reverse
			(list-tabulate NCLASS
				(lambda (N) (list (wlg) (list (wclr N)))))))
)

; --------------------------------------------------------
