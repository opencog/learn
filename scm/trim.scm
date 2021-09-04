;
; trim.scm
;
; Like filter.scm, but removes Atoms outright from the AtomSpace.
; The goal is to be a bit more CPU and storage efficient.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define-public (trim-matrix LLOBJ
	LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED)
"
	trim-matrix LLOBJ - remove Atoms from the AtomSpace that
	pass the predicates. They ae removed from storage too.
"
	(define early-stars (add-pair-stars LLOBJ))
	(define elapsed-secs (make-elapsed-secs))

	; Walk over the left and right basis.
	; The use of cog-delete-recursive! may knock out other
	; elements in the matrix, and so `cog-atom?` is used
	; to see if that particular entry still exists.
	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (LEFT-BASIS-PRED base))
				(cog-delete-recursive! base)))
		(early-stars 'left-basis))

	(format #t "Trimmed left basis in ~A seconds.\n" (elapsed-secs))

	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (RIGHT-BASIS-PRED base))
				(cog-delete-recursive! base)))
		(early-stars 'right-basis))

	(format #t "Trimmed right basis in ~A seconds.\n" (elapsed-secs))

	; Walk over the list of all entries and just delete them.
	(for-each
		(lambda (atom)
			(if (and (cog-atom? atom) (PAIR-PRED atom))
				(cog-delete-recursive! atom)))
		(early-stars 'get-all-elts))

	(format #t "Trimmed all pairs in ~A seconds.\n" (elapsed-secs))
)
