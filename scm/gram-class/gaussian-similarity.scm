;
; gaussian-similarity.scm
;
; Bulk computation of gaussian orthogonal vector similarities.
; Sept 2022
; -------------------------------------
xxxxxxxx

; Ingest data
(define pca (make-pseudo-cset-api)) ; shapes not needed to fetch sims.
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

; -------------------------------------

(define*-public (compute-goe-similarity LLOBJ)
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
(define (add-mi-sim LLOBJ)
	(define (get-ref PR IDX)
		; Expect either a FloatValue or #f if absent.
		(define flov (LLOBJ 'get-count PR))
		(if flov (cog-value-ref flov IDX) -inf.0))

	(lambda (message . args)
		(case message
			((get-mi)  (get-ref (car args) 0))
			((get-rmi) (get-ref (car args) 1))
			(else      (apply LLOBJ (cons message args))))
	))

(define ami (add-mi-sim sob))

; -------------------------------------
; Look at dot products

; goe provides the 'get-count method that returns a renormalized
; version of whatever 'get-mi returns.
(define goe (add-gaussian-ortho-api ami 'get-mi))
(define gor (add-gaussian-ortho-api ami 'get-rmi))
(goe 'mean-rms)
(gor 'mean-rms)

; XXX todo store mean-rms on any-node.

(define gos (add-similarity-api ami #f "goe"))

(define goec (add-similarity-compute goe))
(define gorc (add-similarity-compute gor))

(define (do-compute A B)
	(define simc (goec 'left-cosine A B))
	(define simr (gorc 'left-cosine A B))
	(format #t "cos=~7F rcos=~7F for (\"~A\", \"~A\")\n"
		simc simr (cog-name A) (cog-name B))
	(store-atom
		(gos 'set-pair-similarity
			(gos 'make-pair A B)
			(FloatValue simc simr))))

(define (dot-prod A B)
	(define have-it (gos 'pair-count A B))
	(if (not have-it) (do-compute A B)))

(define allwo (rank-words pcs))
(loop-upper-diagonal dot-prod allwo 0 250)

; -------------------------------------
