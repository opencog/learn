;
; trim.scm
;
; Deprecated.
;
; XXX TODO -- kill this code.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(define-public (subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT
  Deprecated. Use
  `((add-trimmer LLOBJ) `subtotal-trim LEFT-CUT RIGHT-CUT PAIR-CUT)`
  instead.
"
	((add-trimmer LLOBJ) `subtotal-trim LEFT-CUT RIGHT-CUT PAIR-CUT)
)


(define-public (support-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  support-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT
  Deprecated. Use
  `((add-trimmer LLOBJ) `support-trim LEFT-CUT RIGHT-CUT PAIR-CUT)`
  instead.
"
	((add-trimmer LLOBJ) `support-trim LEFT-CUT RIGHT-CUT PAIR-CUT)
)
