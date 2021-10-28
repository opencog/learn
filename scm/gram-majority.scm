;
; gram-majority.scm
;
; Merge N vectors at a time into a new cluster. Merge basis elements by
; majority democratic vote.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` and `gram-projective.scm` for an overview.
;
; Given N vectors that have been selected to form a cluster, one can
; determine what basis elements should be a part of that cluster by
; looking to see what all the vectors have in common. If the majority
; of the vectors share a particular basis element, then all of them
; should contribute that element to the cluster.
;
; This is termed "democratic voting" since a majority concept is used,
; and each vector gets one vote. (Future extensions might consider
; proportional votes?) This idea really only works if N>2 as voting
; between two contributors does not make really make sense.
;
; TODO:
; * Reintroduce FRAC for those disjuncts not shared by the majority.
;   Overall seems like a bad idea, but the unit tests do test it.
;   (i.e. its needed to replace `make-merge-pair` code with this code.
; * Maybe reintroduce NOISE for minimum counts, if this cannot be
;   handled in other ways.
;
; make-merge-majority
; -------------------
; Merge N items into a brand new cluster.  See also `make-merge-pair`
; (not in this file) which merges two items at a time, possibly into
; existing clusters.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

; TODO: we can very easily re-introduce FRAC here, and thus
; provide compatibility with the older merge methods. Just
; modify `clique` below to do this.

(define-public (count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST)
"
  count-shared-conseq LLOBJ QUORUM NOISE WORD-LIST -- Return a count
  of the number of connector sequences which are shared by a majority
  of the words in WORD-LIST. Actually, return a list of two numbers:
  this count and the total connector sequences appearing in common in
  all of the words.  Dividing these two numbers gives a generalized form
  of the Jaccard similarity between all of the words in the WORD-LIST.

  The majority is determined by QUORUM, which should be a floating-
  point number between 0.0 and 1.0.

  ConnectorSeq's with a count of less than NOISE are ignored.

  This function is a kind-of Jaccard distance between multiple words
  (two or more).  The conventional (unweighted) Jaccard distance is
  defined only for pairs of items. The generalization is done by
  counting to see if a fraction QUORUM is shared. Setting QUORUM to
  1.0, and appplying the function to two items returns the conventional
  Jaccard distance.
"
	; The threshold, the minimum number of sections that must exist
	; for a given disjunct. For a list of length two, both must share
	; that disjunct (thus giving the traditional overlap merge).
	(define wlen (length WORD-LIST))
	(define vote-thresh
		(if (equal? wlen 2) 2
			(inexact->exact (round (* QUORUM wlen)))))

	; The group-similarity function uses a strictly-less-than compare.
	; So, to be shared by two, its enough to set low-bnd to one.
	(define low-bnd (- vote-thresh 1))

	; ((make-group-similarity LLOBJ) 'mutual-col-supp low-bnd WORD-LIST)
	((make-group-similarity LLOBJ) 'noise-col-supp low-bnd NOISE WORD-LIST)
)

(define-public (make-merge-majority LLOBJ QUORUM NOISE MRG-CON)
"
  make-merger-majority LLOBJ QUORUM MRG-CON --
  Return a function that will merge a list of words into one class.
  The disjuncts that are selected to be merged are those shared by
  the majority of the given words, where `majority` is defined as
  a fraction that is greater or equal to QUORUM.

  LLOBJ is the object holding the disjuncts. For example, it could
  be (add-dynamic-stars (make-pseudo-cset-api))

  QUORUM is a floating point number indicating the fraction of
  sections that must share a given disjunct, before that disjunct is
  merged into the cluster.

  NOISE is a count, such that if a ConnectorSeq has a count less
  than or equal to this, it will always be merged, irrespective of
  the majority vote. In short, small differences between members
  are ignored, and lumped up into the quirkness of the group.

  MRG-CON is #t if Connectors should also be merged.  This requires
  that the LLOBJ object have shapes on it.
"
	; WLIST is a list of WordNodes and/or WordClassNodes that will be
	; merged into one WordClass.
	; Return a WordClassNode that is the result of the merge.
	(define (merge WLIST)
		(for-each
			(lambda (WRD)
				(if (equal? (cog-type WRD) 'WordClassNode)
					(throw 'not-implemented 'make-merge-majority
						"Not done yet")))
			WLIST)

		; The minimum number of sections that must exist for
		; a given disjunct. For a list of length two, both
		; must share that disjunct (thus giving the traditional
		; overlap merge).
		(define wlen (length WLIST))
		(define vote-thresh
			(if (equal? wlen 2) 2
				(inexact->exact (round (* QUORUM wlen)))))

		; Return #t if the DJ is shared by the majority of the
		; sections. Does the count exceed the threshold?
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						; XXX TODO thise should be either
						; (if (< 0 (LLOBJ 'pair-count WRD DJ)) ...)
						; or it should be
						; (if (< NOISE (LLOBJ 'pair-count WRD DJ)) ...)
						; to be fully compatible with `count-shared-conseq`
						; but right now, I'm going to ignore this...
						(if (nil? (LLOBJ 'get-pair WRD DJ)) CNT (+ 1 CNT)))
					0
					WLIST)))

		; Merge the particular DJ, if it is shared by the majority.
		; CLUST is identical to cls, defined below. Return zero if
		; there is no merge.
		(define (clique LLOBJ CLUST SECT ACC-FUN)
			(define DJ (LLOBJ 'right-element SECT))

			(if (or (<= (LLOBJ 'get-count SECT) NOISE)
					(vote-to-accept? DJ))
				(ACC-FUN LLOBJ (LLOBJ 'make-pair CLUST DJ) SECT 1.0)
				0))

		; We are going to control the name we give it. We could also
		; delegate this to `add-gram-class-api`, but for now, we're
		; going to punt and do it here. Some day, in a generic framework,
		; this will need to be cleaned up. XXX FIXME.
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

		(format #t "------ merge-majority: Cleanup `~A` in ~A secs\n"
			(cog-name cls) (e))

		cls
	)

	; Return the above function
	merge
)

; ---------------------------------------------------------------
; Example usage  (none)
