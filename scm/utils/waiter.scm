;
; waiter.scm
;
; Simple utilities for thread synchronization.
;
; This includes:
; * Gates -- blok execution until the gate is opened.
;
; ---------------------------------------------------------------

(use-modules (ice-9 threads))

(define-public (make-gate)
"
  make-gate -- Create a closed gate.

  This creates a closed gate. It can be opened with `open-gate`.
  Threads can wait until the gate is opened by calling `wait-gate`.
"
	(define mt (make-mutex))
	(lock-mutex mt)
	mt
)

(define-public (open-gate GATE)
"
  open-gate GATE -- open the closed GATE.

  Closed gates can be created with `make-gate`.
  Threads can wait on closed gates with `wait-gate`.
"
	(if (mutex-locked? GATE)
		(unlock-mutex GATE))
)

(define-public (wait-gate GATE)
"
  wait-gate GATE -- block this thread until GATE is opened.

  Closed gates can be created with `make-gate`.
  Gates can be opened with `open-gate`.
"
	(when (mutex-locked? GATE)
		(lock-mutex GATE)
		(unlock-mutex GATE))
)
