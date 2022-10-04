;
; ortho-compute.scm
;
; Bulk computation of gaussian orthogonal ensembles.
; Streamlined version of `orthogonal-ensemble.scm`
; Part of experiments/run-15, described in diary part eight.
; Copied over and formalized at `scm/gram-class/gaussian-similarity.scm`
;
; Sept 2022
; -------------------------------------
; Ingest data
(define pca (make-pseudo-cset-api)) ; shapes not needed to fetch sims.
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(smi 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

; -------------------------------------
; TODO filter the top lists
; (define (filter the top list...

; Wrap similarity, to create a new base object.
(define sob (add-pair-stars smi))

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
; Again, this time to second order.

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define smi (add-similarity-api pcs #f "shape-mi"))

(smi 'fetch-pairs)
(define sob (add-pair-stars smi))

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

; gos will fish out the pre-computed goe-mi and goe-rmi similarities
(define gos (add-similarity-api ami #f "goe"))

; At this time, there are only 1K precomputed similarities available.
(define allwo (rank-words pcs))
(define kay-oh (take allwo 1000))
(define gob (add-keep-filter gos kay-oh kay-oh #t))

(define (add-goe-sim LLOBJ)
   (define (get-ref PR IDX)
      ; Expect FloatValue always IDX=0 is the MI sims, and 1 is the RMI
      (cog-value-ref (LLOBJ 'get-count PR) IDX))

   (lambda (message . args)
      (case message
         ((get-count)  (get-ref (car args) 0))
         (else      (apply LLOBJ (cons message args))))
   ))

(define goc (add-goe-sim gob))
(define eft (add-gaussian-ortho-api goc))
(eft 'mean-rms)

(define efc (add-similarity-compute eft))
(define efs (add-similarity-api gob #f "goe f-2"))


(define (f2-compute A B)
   (define f2 (efc 'left-cosine A B))
   (format #t "F2=~7F for (\"~A\", \"~A\")\n"
      f2 (cog-name A) (cog-name B))
   (store-atom
      (efs 'set-pair-similarity
         (efs 'make-pair A B)
         (FloatValue f2))))

(define (f2-dot-prod A B)
   (define have-it (efs 'pair-count A B))
   (if (not have-it) (f2-compute A B)))

(f2-dot-prod (WordNode "the") (WordNode "the"))

(loop-upper-diagonal f2-dot-prod allwo 0 50)
(loop-upper-diagonal f2-dot-prod allwo 0 250)


; -------------------------------------
