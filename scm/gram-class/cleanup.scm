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

(define (check-gram-dataset LLOBJ)
	(check-conseq-marginals LLOBJ)
)
