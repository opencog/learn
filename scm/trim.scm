;
; trim.scm
;
; Like filter.scm, but removes Atoms outright from the AtomSpace.
; The goal is to be a bit more CPU and storage efficient.
;
; XXX TODO
; -- use either cog-extract! or cog-delete!
; -- finish writing documentation
; -- move to the (opencog matrix) module
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define (make-elapsed-secs)
   (define start-time (current-time))
   (lambda ()
      (define now (current-time))
      (define diff (- now start-time))
      (set! start-time now)
      diff)
)

; ---------------------------------------------------------------------

(define-public (trim-matrix LLOBJ
	LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED)
"
  trim-matrix LLOBJ - remove Atoms from the AtomSpace that pass the
  predicates. If storage is connected, then these are removed from
  storage too.
"
	(define star-obj (add-pair-stars LLOBJ))
	(define elapsed-secs (make-elapsed-secs))

	; After removing pairs, it may now happen that there are left
	; and right basis elements that are no longer in any pairs.
	; Remove these too.
	(define (trim-type BASIS-LIST)
		(define party (star-obj 'pair-type))
		(for-each
			(lambda (base)
				(if (and (cog-atom? base)
						(equal? 0 (cog-incoming-size-by-type base party)))
					(cog-delete! base)))
			BASIS-LIST))

	; Walk over the left and right basis.
	; The use of cog-delete-recursive! may knock out other
	; elements in the matrix, and so `cog-atom?` is used
	; to see if that particular entry still exists.
	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (not (LEFT-BASIS-PRED base)))
				(cog-delete-recursive! base)))
		(star-obj 'left-basis))

	(format #t "Trimmed left basis in ~A seconds.\n" (elapsed-secs))

	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (not (RIGHT-BASIS-PRED base)))
				(cog-delete-recursive! base)))
		(star-obj 'right-basis))

	(format #t "Trimmed right basis in ~A seconds.\n" (elapsed-secs))

	; Walk over the list of all entries and just delete them.
	(for-each
		(lambda (atom)
			(if (and (cog-atom? atom) (not (PAIR-PRED atom)))
				(cog-delete-recursive! atom)))
		(star-obj 'get-all-elts))

	; After removing pairs, it may now happen that there are left
	; and right basis elements that are no longer in any pairs.
	; Remove these too.
	(trim-type (star-obj 'left-basis))
	(trim-type (star-obj 'right-basis))

	(format #t "Trimmed all pairs in ~A seconds.\n" (elapsed-secs))
)


(define-public (subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT
  Just like `add-subtotal-filter` but it trims.
"
	(define stars-obj (add-pair-stars LLOBJ))
	(define sup-obj (add-support-api stars-obj))

	; ---------------
	; Remove rows and columns that are below-count.
	;
	; Yes, we want LEFT-CUT < right-wild-count this looks weird,
	; but is correct: as LEFT-CUT gets larger, the size of the
	; left-basis shrinks.
	(define (left-basis-pred ITEM)
		(< LEFT-CUT (sup-obj 'right-count ITEM)))

	(define (right-basis-pred ITEM)
		(< RIGHT-CUT (sup-obj 'left-count ITEM)))

	(define (pair-pred PAIR)
		(< PAIR-CUT (LLOBJ 'get-count PAIR)))

	; ---------------
	(trim-matrix stars-obj
		left-basis-pred right-basis-pred pair-pred)
)
