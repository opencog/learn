;
; common.scm
;
; Common functions shared between multpile functional units.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog persist))

; ---------------------------------------------------------------------
; Define locations where statistics will be stored.

; These are used to store/maintain counts for clique-pair counting.
; See `link-pipeline.scm` for usage.
(define pair-pred (PredicateNode "*-Sentence Word Pair-*"))
(define pair-dist (SchemaNode "*-Pair Distance-*"))

; get-count ATOM - return the raw observational count on ATOM.
(define-public (get-count ATOM) (cog-count ATOM))

; set-count ATOM CNT - Set the raw observational count on ATOM.
(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

; ---------------------------------------------------------------------

(define (count-one-atom ATM)
"
  count-one-atom ATM -- increment the count by one on ATM, and
  update storage to hold that count.

  This will also automatically fetch the previous count from storage,
  so that counting will work correctly, when picking up from a previous
  point.

  Warning: this is NOT THREAD SAFE! during rapid startup of multiple
  threads, each thread could fetch the same count, and increment
  that, leading to a loss of counts!  Once the system is up and running,
  things should be fine (i.e. it is \"eventually thread-safe\"); but
  there's a race-window during startup.

  Warning: this is NOT SAFE for distributed processing! That is
  because this does NOT grab the count from the database every time,
  so if some other process updates the database, this will miss that
  update. Multiple distributed counters will continue to clobber
  each-other indefinitely; the system will NOT settle-down long-term.
"
	(define (incr-one atom)
		; If the atom doesn't yet have a count TV attached to it,
		; then its probably a freshly created atom. Go fetch it
		; from storage. Otherwise, assume that what we've got here,
		; in the atomspace, is the current copy.  This works if
		; there is only one process updating the counts.
		(if (not (cog-ctv? (cog-tv atom)))
			(fetch-atom atom)) ; get from storage
		(cog-inc-count! atom 1) ; increment
	)
	(begin
		(incr-one ATM) ; increment the count on ATM
		(store-atom ATM)) ; save to storage
)

; ---------------------------------------------------------------------

(define-public (get-all-words)
"
  get-all-words - return a list holding all of the observed words
  This does NOT fetch the words from the backing store.
"
	(cog-get-atoms 'WordNode)
)

; ---------------------------------------------------------------------
