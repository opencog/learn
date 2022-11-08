#! /usr/bin/env guile
!#
;
; gen-dict.scm - Generate a random artificial grammar
;
; Usage: `./gen-dict.scm <paramaters-file.scm> <output-dir>`
;
; This expects two arguments: a file containing configuration parameters
; and the directory where the generated files should be written.
;
(use-modules (opencog) (opencog nlp fake))

; Get the program arguments
(define param-file (cadr (program-arguments)))
(define dict-dir (caddr (program-arguments)))

; Program parameters. Define these so that guile compilation
; does not spew errors. (Maybe these should be #<unspecified>?)
(define num-link-types #f)
(define link-type-exp #f)
(define max-disjunct-size #f)
(define disjunct-exp #f)
(define section-size #f)
(define section-exp #f)
(define num-pos #f)
(define num-classes #f)
(define class-size #f)
(define class-exp #f)

(define num-wall-types #f)
(define num-to-wall #f)
(define sentence-enders #f)

(define sense-frac #f)
(define sense-frac #f)
(define num-senses #f)
(define sense-exp #f)

(define num-synonyms #f)
(define synonym-exp #f)

(define x
	(begin
		(if (not (access? param-file R_OK))
			(begin
				(format #t "Error: unable to access parameters file '~A'\n" param-file)
				(exit -1)))
		(load param-file)
	))

; ----------------------------------------------------------
; Generators for each of the different parts of the grammar.

(define secgen
	(make-section-generator
		num-link-types
		max-disjunct-size
		section-size
		link-type-exp
		disjunct-exp
		section-exp))

(define posgen
	(make-pos-generator
		num-pos
		secgen))

(define classgen
	(make-class-generator
		num-classes
		num-pos
		class-size
		class-exp))

(define wallgen
	(make-wall-generator
		num-classes
		num-wall-types
		num-to-wall
		sentence-enders))

(define sensegen
	(make-sense-generator
		sense-frac
		num-classes
		num-senses
		sense-exp))

(define wordgen
	(make-word-generator
		num-classes
		num-synonyms
		synonym-exp))

; Make a copy of the link-grammar boilerplate
; This copies the boilerplate files from the source dir
; to the target dir.
(define (copy-boilerplate)

	(define x
		(if (not (getenv "COMMON_DIR"))
		(begin
			(format #t "Error: Environment variable $COMMON_DIR is not defined.\n")
			(format #t "This directory needed for its template files.\n")
			(exit -1))))

	; Location of the boilerplate files.
	(define source-dir (string-append (getenv "COMMON_DIR") "/fake-lang"))

	; Recursive copy
	(define DIR_STREAM (opendir source-dir))
	(define (copy-dir)
		(define DIRENT (readdir DIR_STREAM))
		(if (not (eof-object? DIRENT))
			(let ((lgfi (string-append source-dir "/" DIRENT))
					(tofi (string-append dict-dir "/" DIRENT)))
				(if (equal? 'regular (stat:type (stat lgfi)))
					(copy-file lgfi tofi))
				(copy-dir)
			)))

	; Does the source directory exist?
	(if (not (or (getenv "COMMON_DIR") (access? source-dir R_OK)))
		(begin
			(format #t "Error: unable to access '~A'\n" (source-dir))
			(format #t "This directory needed for its template files\n")
			(exit -1)))

	; Does the target directory exist already?
	; If so, do not over-write it.
	(if (access? dict-dir R_OK)
		(begin
			(format #t "Error: target directory exists: ~A\n" dict-dir)
			(format #t "Remove or rename this directory and try again\n")
			(exit -1)))

	(mkdir dict-dir)
	(copy-dir)

	; Copy the parameters file so that we have a log of what was done.
	(copy-file param-file (string-append dict-dir "/dict-conf.scm"))
)

; Do the actual copy, first
(define xx (copy-boilerplate))

(define dict-file (string-append dict-dir "/4.0.dict"))
(define port (open-file dict-file "w"))

(format port "%\n% Randomly generated dictionary\n%\n")
(format port "% Version: 0.1\n")
(format port "% Num link types: ~A\n" num-link-types)
(format port "% Link type exponent: ~A\n" link-type-exp)
(format port "% Disjunct size: ~A\n" max-disjunct-size)
(format port "% Disjunct exponent: ~A\n" disjunct-exp)
(format port "% Section size: ~A\n" section-size)
(format port "% Number of POS: ~A\n" num-pos)
(format port "% Number of classes:  ~A\n" num-classes)
(format port "%\n")
(format port "% Class size: ~A\n" class-size)
(format port "% Class exp: ~A\n" class-exp)
(format port "%\n")
(format port "% Wall connector types: ~A\n" num-wall-types)
(format port "% Wall connections: ~A\n" num-to-wall)
(format port "% Sentence-ending punctuation: ~A\n" sentence-enders)
(format port "%\n")
(format port "% Word-sense fraction: ~A\n" sense-frac)
(format port "% Number of word-senses: ~A\n" num-senses)
(format port "% Word-sense exponent: ~A\n" sense-exp)
(format port "%\n")
(format port "% Number of synonyms:  ~A\n" num-synonyms)
(format port "% Synonym exponent: ~A\n" synonym-exp)
(format port "%\n")

(format port "#define dictionary-version-number 5.9.0;\n")
(format port "#define dictionary-locale C;\n")
(print-LG-flat port (posgen))
(print-LG-flat port (classgen))
(print-LG-flat port (wallgen))
(print-LG-flat port (sensegen))
(print-LG-flat port (wordgen))

(format port "\n<UNKNOWN-WORD>:  XXXXXX+;\n")
(close port)

; If we got to here, then everything must have worked.
(format #t "Created dictionary at ~A\n" dict-dir)

(exit 0)
