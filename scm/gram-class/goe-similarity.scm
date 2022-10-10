;
; goe-similarity.scm
;
; Bulk computation of gaussian orthogonal vector similarities.
; Sept 2022
; ---------------------------------------------------------------------
xxxxxxxx
under construction

; ---------------------------------------------------------------------

; ---------------------------------------------------------------------

(define-public (compute-goe-similarity LLOBJ)
"
  compute-goe-similarity LLOBJ -- provide methods for working with
  Gaussian Orthogonal vectors.

  LLOBJ

"
	; TODO filter the top lists
	; (define (filter the top list...

	; TODO check if LLOBJ is a similrity object

	; Wrap similarity, to create a new base object.
	(define sob (add-pair-stars LLOBJ))

	; Counts on smi are FloatValues of two floats.
	; First float is mi-sim
	; Second float is ranked-mi-sim
	; That is, (smi 'get-count PR) returns a FloatValue.
	; So, unwrap it.
	(define (add-mi-sim LLOBJ IDX)
		(define (get-ref PR IDX)
			; Expect either a FloatValue or #f if absent.
			(define flov (LLOBJ 'get-count PR))
			(if flov (cog-value-ref flov IDX) -inf.0))

		(lambda (message . args)
			(case message
				((get-count)  (get-ref (car args) IDX))
				(else      (apply LLOBJ (cons message args))))
		))

	(define ami (add-mi-sim sob 0))

	; -------------------------------------
	; Look at dot products

	; goe provides the 'get-count method that returns a renormalized
	; version of whatever the underlying 'get-count returns.
	(define goe (add-gaussian-ortho-api ami))
	(goe 'mean-rms)

; XXX todo store mean-rms on any-node.

	(define gos (add-similarity-api ami #f "goe"))
	(define goec (add-similarity-compute goe))

	(define (do-compute A B)
		(define simc (goec 'left-cosine A B))
		(format #t "cos=~7F for (\"~A\", \"~A\")\n"
			simc (cog-name A) (cog-name B))
		(store-atom
			(gos 'set-pair-similarity
				(gos 'make-pair A B)
				(FloatValue simc))))

	(define (dot-prod A B)
		(define have-it (gos 'pair-count A B))
		(if (not have-it) (do-compute A B)))

	(define (redo-mi-sims WRDLIST)
	; Recompute marginals after merge.
	(define touched-words (recompute-marginals LLOBJ (cons wclass in-grp)))
	(format #t "------ Recomputed MMT marginals in ~A secs\n" (e))

	(recomp-all-sim SIM-API compute-sim touched-words)
	

		; Optional; compute similarity between this and all other
		; classes. This is used to compute and log the orthogonality
		; of the classes. It provides an intersting statistic.
		(for-each (lambda (WC) (simmer wclass WC))
			(LLOBJ 'get-clusters))


)

(define allwo (rank-words pcs))
(loop-upper-diagonal dot-prod allwo 0 250)

; ---------------------------------------------------------------------
#! ========
;
; Example usage.

; Ingest data
(define pca (make-pseudo-cset-api)) ; shapes not needed to fetch sims.
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

==== !#
