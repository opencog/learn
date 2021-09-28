;
; trim.scm
;
; Deprecated.
;
; XXX TODO -- kill this code.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

(define-public (trim-matrix LLOBJ LEFT-PRED RIGHT-PRED PAIR-PRED)
"
  trim-matrix LLOBJ LEFT-PRED RIGHT-PRED ELEMENT-PRED
  Deprecated. Use
  `((add-trimmer LLOBJ) `generic-trim LEFT-PRED RIGHT-PRED ELEMENT-PRED)`
  instead.
"
	((add-trimmer LLOBJ) `generic-trim LEFT-PRED RIGHT-PRED PAIR-PRED)
)


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
