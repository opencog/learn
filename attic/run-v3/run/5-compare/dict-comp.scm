#! /usr/bin/env -S guile
!#
;
; dict-comp.scm
; Compare parses produced by two different LG dictionaries over
; a sample corpus.
;
; Usage:
; guile -s dict-comp.scm <gold-dict> <test-dict> <sentence-file>
;
; <gold-dict> should be the reference dictionary; it must be a
;        valid Link-Grammar dictionary
; <test-dict> is the dictionary that will be compared to the reference.
; <sentence-file> should be a file containing sentences whose parses
;        will be compared.
;
; Example:
; guile -s dict-comp.scm en micro-fuzz sentences.txt
;

(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (opencog) (opencog nlp) (opencog learn))

; Check usage
(if (not (equal? 4 (length (program-arguments))))
	(begin
		(format #t
			"Usage: ~A <gold-dict> <test-dict> <sentence-file>\n"
			(first (program-arguments)))
		(exit #f)))

(define gold-dict (second (program-arguments)))
(define test-dict (third (program-arguments)))
(define sent-file (fourth (program-arguments)))

; Check file access
(if (not (equal? (stat:type (stat gold-dict)) 'directory))
	(begin
		(format #t "Cannot find reference dictionary ~A\n" gold-dict)
		(exit #f)))

(if (not (equal? (stat:type (stat test-dict)) 'directory))
	(begin
		(format #t "Cannot find test dictionary ~A\n" test-dict)
		(exit #f)))

(if (not (access? sent-file R_OK))
	(begin
		(format #t "Cannot find sentence file ~A\n" sent-file)
		(exit #f)))

; Perform comparison
(format #t "Comparing \"~A\" to \"~A\" with sentences from \"~A\"\n"
	gold-dict test-dict sent-file)


;; Set #:INCLUDE-MISSING to #f to disable the processing of sentences
;; containing words that the dictionary does not know about (i.e. to
;; disable unknown-word guessing.)
(define compare
	(make-lg-comparator (LgDictNode gold-dict) (LgDictNode test-dict) '()
		#:INCLUDE-MISSING #f))

(define (process-file PORT)
	(define line (read-line PORT))
	(if (not (eof-object? line))
		(begin
			; The # symbol is a comment-card
			(if (and
				(< 0 (string-length line))
				; % is a comment for LG, ! is a directive for LG,
				; * means "bad sentence" and # is a comment for python
				(not (equal? #\# (string-ref line 0)))
				(not (equal? #\! (string-ref line 0)))
				(not (equal? #\* (string-ref line 0)))
				(not (equal? #\% (string-ref line 0))))
				(compare line))
			(process-file PORT))
		(compare #f)))

(process-file (open sent-file O_RDONLY))

(format #t "Finiished comparing \"~A\" to \"~A\" with sentences from \"~A\"\n"
	gold-dict test-dict sent-file)
