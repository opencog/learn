;
; cleanup.scm
; Ad hoc scripts to clean up messy datasets.
;
; Copyright 2022 Linas Vepstas
; ------------------------------------------------------------------
;
; Due to code bugs and operational errors, assorted datasets end up
; containing stuff that they should not. The scripts below are a curated
; set of ad hoc tools to clean up stuff that has gone wrong.

; ------------------------------------------------------------------

(define (check-conseq-marginals LLOBJ)
"
  Use cog-delete! on any ConnectorSeq's that are not in the right basis.
"
	(define djs (make-aset-predicate (LLOBJ 'right-basis)))

	(define cnt 0)
	(for-each (lambda (CSQ)
		(when (not (djs CSQ))
			(set! cnt (+ 1 cnt))
			; (format #t "Unexpected ConnectorSeq ~A\n" CSQ)
		))
		(cog-get-atoms 'ConnectorSeq))

	(if (< 0 cnt)
		(format #t "Found ~A unexpected ConnectorSeq!\n" cnt)
		(format #t "Checked ConnectorSeq, all OK.\n"))
	*unspecified*
)

(define (zap-conseq-marginals LLOBJ)
"
  Use cog-delete! on any ConnectorSeq's that are not in the right basis.

  These conventionally show up as marginals, of the form
     (ListLink (AnyNode \"cset-word\") (ConnectorSeq...))
  or
     (ListLink (AnyNode \"gram-class\") (ConnectorSeq...))
"
	(define djs (make-aset-predicate (LLOBJ 'right-basis)))

	(for-each (lambda (CSQ) (when (not (djs CSQ))
		(let ((iset (cog-incoming-set CSQ)))
			(if (eq? (length iset) (cog-incoming-size-by-type CSQ 'ListLink))
				(begin
					(for-each cog-delete! iset)
					(cog-delete! CSQ))
				(begin
					(format #t "Unexpected ConnectorSeq ~A\n" CSQ))
			))))
		(cog-get-atoms 'ConnectorSeq))
)

; -------------------------------------------------------------------

(define (check-connectors LLOBJ)
"
  Verify that Connectors are used sanely.
"
	(define cnt 0)
	(for-each (lambda (CON)
		(when (not (equal?
				(cog-incoming-size CON)
				(cog-incoming-size-by-type CON 'ConnectorSeq)))
			(set! cnt (+ 1 cnt))
			; (format #t "Unexpected Connector usage ~A\n" CON) (foobar)
		))
		(cog-get-atoms 'Connector))

	(if (< 0 cnt)
		(format #t "Found ~A unexpected Connector usages!\n" cnt)
		(format #t "Checked Connectors, all OK.\n"))
	*unspecified*
)

(define (zap-connectors LLOBJ)
"
  cog-delete orphaned Connectors
"
	(for-each (lambda (CON)
		(when (equal? 0 (cog-incoming-size CON))
			(cog-delete! CON)
		))
		(cog-get-atoms 'Connector))

	*unspecified*
)

; ------------------------------------------------------------------

(define (check-word-marginals LLOBJ)
"
  Use cog-delete! on any WordNodes's that are not in the right basis.
"
	(define words (make-aset-predicate (LLOBJ 'left-basis)))

	(define cnt 0)
	(for-each (lambda (WRD)
		(when (not (words WRD))
			(set! cnt (+ 1 cnt))
			(format #t "Unexpected Word ~A\n" WRD) (foobar)
		))
		(append (cog-get-atoms 'WordNode) (cog-get-atoms 'WordClassNode)))

	(if (< 0 cnt)
		(format #t "Found ~A unexpected WordNodes!\n" cnt)
		(format #t "Checked Words and WordClasses, all OK.\n"))
	*unspecified*
)

(define (zap-word-marginals LLOBJ)
"
  Use cog-delete! on any WordNodes that are not in the left basis.

  These conventionally show up as marginals, of the form
     (ListLink (WordNode ...) (AnyNode \"cset-disjunct\"))
  or
     (Evaluation (Predicate ...) (Word ...) (Any \"right-wild-direct-sum\"))
"
	(define words (make-aset-predicate (LLOBJ 'left-basis)))

	(for-each (lambda (WRD) (when (not (djs WRD))
			(for-each (lambda (IW)
				(cond
					((eq? (cog-type IW) 'Connector) (cog-delete IW))
					((eq? (cog-type IW) 'ListLink) (cog-delete IW))
					((eq? (cog-type IW) 'EvaluationLink) (cog-delete IW))))
				(cog-incoming-set WRD))
			(cog-delete! WRD)))
		(append (cog-get-atoms 'WordNode) (cog-get-atoms 'WordClassNode)))
)

; -------------------------------------------------------------------

(define (check-linkability LLOBJ)
"
  Check to see ot connectors on the right can connect to words on left
"
	; First, we check to see if every disjunct consists of words
	; in the left basis.
	(define is-in-left? (make-aset-predicate (LLOBJ 'left-basis)))
	(define is-in-right? (make-conseq-predicate LLOBJ is-in-left?))

	(define cnt 0)
	(for-each (lambda (CSQ)
		(when (not (is-in-right? CSQ))
			; (format #t "This is unconnectale: ~A\n" CSQ) (foobar)
			(set! cnt (+ 1 cnt))))
		(LLOBJ 'right-basis))

	(if (< 0 cnt)
		(format #t "Found ~A ConnectorSeqs that cannot connect!\n" cnt)
		(format #t "Checked ConnectorSeq for connectivity, all OK.\n"))
	*unspecified*
)

; -------------------------------------------------------------------

(define (check-gram-dataset LLOBJ)
	(check-conseq-marginals LLOBJ)
	(check-word-marginals LLOBJ)
	(check-connectors LLOBJ)
)

(define (cleanup-gram-dataset LLOBJ)
	(zap-conseq-marginals LLOBJ)
	(zap-word-marginals LLOBJ)
	(zap-connectors LLOBJ)
)
