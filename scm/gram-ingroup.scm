;
; gram-ingroup.scm
;
; Merge N items at a time into a new cluster.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` and `gram-projective.scm` for an overview.
;
; make-merge-ingroup
; ------------------
; Merge N items into a brand new cluster.  See also `make-merge-pair`
; (not in this file) which merges two items at a time, possibly into
; existing clusters.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

(define-public (make-merge-ingroup LLOBJ QUORUM MRG-CON)
"
  make-merger-ingroup LLOBJ QUORUM MRG-CON --
  Return a function that will merge a list of words into one class.
  The disjuncts that are selected to be merged are those shared by
  the majority of the given words, where `majority` is defined as
  a fraction that is greater or equal to QUORUM.

  LLOBJ is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  QUORUM is a floating point number indicating the fraction of
  sections that must share a given disjunct, before that disjunct is
  merged into the cluster.

  MRG-CON is #t if Connectors should also be merged.  This requires
  that the LLOBJ object have shapes on it.
"
	; WLIST is a list of WordNodes and/or WordClassNodes
	; Return a WordClassNode that is the result of the merge.
	(define (merge WLIST)
		(for-each
			(lambda (WRD)
				(if (equal? (cog-type WRD) 'WordClassNode)
					(throw 'not-implemented 'make-merge-ingroup
						"Not done yet")))
			WLIST)

		; The minimum number of sections that must exist for
		; a given disjunct.
		(define vote-thresh
			(inexact->exact (round (* QUORUM (length WLIST)))))

		; Return #t if the DJ is shared by the majority of the
		; sections. Does the count exceed the threshold?
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						(if (nil? (LLOBJ 'get-pair WRD DJ)) 0 1))
					0
					WLIST)))

		; Merge the particular DJ, if it is shared by the majority.
		; CLUST is identical to cls, defined below. Return zero if
		; there is no merge.
		(define (clique LLOBJ CLUST SECT ACC-FUN)
			(define DJ (LLOBJ 'right-element SECT))

			(if (vote-to-accept? DJ)
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ)  SECT 1.0)
				0))

		; We are going to control the name we give it. We could also
		; delegate this to `add-gram-class-api`, but for now, we're
		; going to punt and do it here. Some day, in a generic framework,
		; this will need to be cleaned up.
		(define cls-name (string-join (map cog-name WLIST)))
		(define cls-type (LLOBJ 'cluster-type))
		(define cls-typname
			(if (cog-atom? cls-type) (cog-name cls-type) cls-type))
		(define cls (cog-new-node cls-typname cls-name))

		(for-each
			(lambda (WRD) (assign-to-cluster LLOBJ cls WRD clique))
			WLIST)

		(when MRG-CON
			(for-each
				(lambda (WRD) (merge-connectors LLOBJ cls WRD clique))
				WLIST)
		)

		; Cleanup after merging.
		; The LLOBJ is assumed to be just a stars object, and so the
		; intent of this clobber is to force it to recompute it's left
		; and right basis.
		(define e (make-elapsed-secs))
		(LLOBJ 'clobber)
		(for-each
			(lambda (WRD) (remove-empty-sections LLOBJ WRD))
			WLIST)
		(remove-empty-sections LLOBJ cls)

		; Clobber the left and right caches; the cog-delete! changed things.
		(LLOBJ 'clobber)

		(format #t "------ merge-ingroup: Cleanup `~A` in ~A secs\n"
			(cog-name cls) (e))

		cls
	)

	; Return the above function
	merge
)

; ---------------------------------------------------------------
; Example usage  (none)
