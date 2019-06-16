;
; dict-comp.scm
; Compare LG dictionary to the English dictionary.
;
; Usage:
; guile -s dict-comp.scm <dict-name> <sentence-file-name>
;
; <dict-name> should be a valid Link-Grammar dictionary
; <sentence-file-name> should be a file containing sentences
;
; Example:
; guile -s dict-comp.scm micro-fuzz sentences.txt
;

(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (opencog) (opencog nlp) (opencog nlp learn))

(if (not (equal? 2 (length (program-arguments))))
	(begin
		(format #t
			"Usage: guile -s dict-comp.scm <dict-name> <sentence-file-name>\n")
		(exit #f)))

(define test-dict (first (program-arguments)))
(define sent-file (second (program-arguments)))

(if (not (access? test-dict R_OK))
	(begin
		(format #t "Cannot find dictionary ~A\n" test-dict)
		(exit #f)))

(if (not (access? sent-file R_OK))
	(begin
		(format #t "Cannot find sentence file ~A\n" sent-file)
		(exit #f)))

(define compare
	(make-lg-comparator (LgDictNode "en") (LgDictNode test-dict)))

(define (process-file PORT)
	(define line (read-line PORT))
	(if (not (eof-object? line))
		(begin
			(compare line)
			(process-file PORT))
		(compare #f)))

(process-file (open sent-file O_RDONLY))
