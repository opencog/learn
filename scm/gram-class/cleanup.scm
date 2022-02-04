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
  Look for any ConnectorSeq's that are not in the right basis.
"
	(define djs (make-aset-predicate (LLOBJ 'right-basis)))

	(define cnt 0)
	(for-each (lambda (CSQ)
		(when (not (djs CSQ))
			(set! cnt (+ 1 cnt))
			; (format #t "Unexpected ConnectorSeq ~A\n" CSQ) (foobar)
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

  This will delete the merginals, and then the ConnectorSeq.
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
				(+
					(cog-incoming-size-by-type CON 'ConnectorSeq)
					(cog-incoming-size-by-type CON 'ShapeLink))))
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
  Look for any WordNodes's that are not in the right basis.
"
	(define words (make-aset-predicate (LLOBJ 'left-basis)))

	(define cnt 0)
	(for-each (lambda (WRD)
		(when (not (words WRD))
			(set! cnt (+ 1 cnt))
			; (format #t "Unexpected Word ~A\n" WRD) ; (foobar)
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

	(for-each (lambda (WRD) (when (not (words WRD))
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
  Check to see if connectors on the right can connect to words on left
  This checks in both directions: first, that every disjunct contains
  only words that appear in the left basis, and second, that every word
  in the left basis appears in some connector.
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
		(format #t "All Connectors can connect to some word; all OK.\n"))

	; ----------
	; Next, go in the opposite direction: make sure every word
	; appears in some connector.
	(define word-set (make-atom-set))
	(for-each (lambda (DJ)
		(when (eq? 'ConnectorSeq (cog-type DJ))
			(for-each
				(lambda (CON) (word-set (gar CON)))
				(cog-outgoing-set DJ))))
		(LLOBJ 'right-basis))

	(set! cnt 0)
	(define is-in-connector? (make-aset-predicate (word-set #f)))
	(for-each (lambda (WRD)
		(when (not (is-in-connector? WRD))
			; (format #t "This word is unconnectale: ~A" WRD) ; (foobar)
			(set! cnt (+ 1 cnt))))
		(LLOBJ 'left-basis))

	(if (< 0 cnt)
		(format #t "Found ~A Words that are not in Connectors!\n" cnt)
		(format #t "All words appear in some Connector; all OK.\n"))

	*unspecified*
)

(define (zap-unlinkables LLOBJ)
"
  Use cog-delete! to remove any words& conseq that cannot connect.
"
	(trim-linkage LLOBJ)
	(zap-conseq-marginals LLOBJ)
	(zap-word-marginals LLOBJ)
	(zap-connectors LLOBJ)
)

; -------------------------------------------------------------------

(define (check-gram-dataset LLOBJ)
"
  check everything
"
	(define stars-obj (add-pair-stars LLOBJ))

	(check-conseq-marginals stars-obj)
	(check-word-marginals stars-obj)
	(check-connectors stars-obj)
	(check-linkability stars-obj)
)

(define (cleanup-gram-dataset LLOBJ)
	(define stars-obj (add-pair-stars LLOBJ))

	(zap-conseq-marginals stars-obj)
	(zap-word-marginals stars-obj)
	(zap-connectors stars-obj)
	(zap-unlinkables stars-obj)
)
