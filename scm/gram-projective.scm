;
; gram-projective.scm
;
; Merge vectors into clusters. Basic core tools.
; Also: detailed description of projective merge.
;
; Copyright (c) 2017, 2018, 2019, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
; Debugging prints which print Section Cross-Sections in a short-hand.
; This short-hand is used extensively in the unit tests.

(define-public (prt-word W)
	(define t (cog-type W))
	(cond
		((equal? t 'WordClassNode) (format #f "{~A}" (cog-name W)))
		((equal? t 'WordNode) (cog-name W))
		((equal? t 'VariableNode) "$")
	)
)

(define (prt-conseq LST)
	(string-concatenate
		(map (lambda (CON)
			(format #f " ~A~A" (prt-word (gar CON)) (cog-name (gdr CON))))
			LST))
)

(define (prt-shape SHAPE)
	(format #f "<~A, ~A>"
		(prt-word (gar SHAPE))
		(prt-conseq (cdr (cog-outgoing-set SHAPE))))
)

(define-public (prt-dj DJ)
	(if (equal? (cog-type DJ) 'ShapeLink)
		(prt-shape DJ)
		(prt-conseq (cog-outgoing-set DJ)))
)

(define-public (prt-dj-list LST)
	(string-concatenate
		(map (lambda (ELT)
			(format #f "~A\n" (prt-dj ELT)))
			LST))
)

(define (prt-section SECT)
	(format #f "~6,3F * (~A, ~A)"
		(cog-count SECT)
		(prt-word (gar SECT))
		(prt-conseq (cog-outgoing-set (gdr SECT))))
)

(define (prt-cross-section XSECT)
	(format #f "~6,3F * [~A, ~A]"
		(cog-count XSECT)
		(prt-word (gar XSECT))
		(prt-shape (gdr XSECT)))
)

(define-public (prt-element ELT)
	(if (equal? (cog-type ELT) 'Section)
		(prt-section ELT)
		(prt-cross-section ELT))
)

(define-public (prt-element-list LST)
	(string-concatenate
		(map (lambda (ELT)
			(format #f "~A\n" (prt-element ELT)))
			LST))
)

; ---------------------------------------------------------------------

(define-public (accumulate-count LLOBJ ACC DONOR FRAC)
"
  accumulate-count LLOBJ ACC DONOR FRAC -- Accumulate a fraction
    FRAC of the count from DONOR into ACC.

  ACC and DONOR should be two pairs in the matrix LLOBJ.

  FRAC should be a numeric fraction, between 0.0 and 1.0.

  A fraction FRAC of the count on DONOR will be transferred to ACC.
"
	; Return #t if the count is effectively zero.
	; Use an epsilon for rounding errors.
	(define (is-zero? cnt) (< cnt 1.0e-10))

	(define moved (LLOBJ 'move-count ACC DONOR FRAC))

	; If something was transfered, save the updated counts.
	(when (not (is-zero? moved))
		(rebalance-count LLOBJ ACC (get-count ACC))
		(rebalance-count LLOBJ DONOR (get-count DONOR))
	)

	; Return how much was transferred over.
	moved
)

; ---------------------------------------------------------------------
; Example usage (none)
