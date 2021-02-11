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

; Number of synonyms in a word-class
(define num-synonyms 6)

; Output file
(define dict-file "/tmp/4.0.dict")

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
		class-size))

(define wordgen
	(create-word-generator
		num-classes
		num-synonyms))

(define port (open-file "/tmp/4.0.dict" "w"))

(print-LG-flat dict-file (posgen))
(print-LG-flat dict-file (classgen))
(print-LG-flat dict-file (wordgen))

(close port)
