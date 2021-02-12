;
; gen-dict.scm - Generate a random artificial grammar
;
; The generated dictionary is determined by the parameters in this file.
;
(use-modules (opencog) (opencog nlp fake))

; Number of Link Grammar link types (connector types)
(define num-link-types 6)

; Maximum size of disjuncts
(define max-disjunct-size 3)

; Disjuncts per section
(define num-disjuncts 20)

; Number of pos tags
(define num-pos 10)

; Number of grammatical classes
(define num-classes 10)

; Number of pos tags per class
(define class-size 8)

; Exponent of the class-size distribution.
; Setting this to 1.0 gives the classic Zipf distribution;
; setting it to 0.0 gives the uniform distribution.
; Using Zipf means that in most cases, each word class will have only
; one or two pos-tags in it; setting it to uniform means that larger
; classes (largr complexity) will be common. Setting the exponent
; negative will make most classes to be maximal in size, i.e. to have
; to have `class-size` elements.
(define class-exp -0.1)

; Number of synonyms in a word-class
(define num-synonyms 6)

; Exponent of the synonym word-class size distribution.
; Setting this to 1.0 gives the classic Zipf distribution;
; setting it to 0.0 gives the uniform distribution.
; Using Zipf means that in most cases, there will be only one or
; two synonyms; setting it to uniform means that large synonym classes
; will be common. Setting the exponent negative will make most
; synonym clases have the max allowed, i.e. to have `num-synonyms`
; in each one.
(define synonym-exp 0.5)

; Output file
(define dict-file "/tmp/4.0.dict")

; -------------------------------------------
; Generators for each of the different parts of the grammar.

(define posgen
	(create-pos-generator
		num-pos
		num-link-types
		max-disjunct-size
		num-disjuncts))

(define classgen
	(create-class-generator
		num-classes
		num-pos
		class-size
		class-exp))

(define wordgen
	(create-word-generator
		num-classes
		num-synonyms
		synonym-exp))

(define port (open-file "/tmp/4.0.dict" "w"))

(print-LG-flat port (posgen))
(print-LG-flat port (classgen))
(print-LG-flat port (wordgen))

(close port)
