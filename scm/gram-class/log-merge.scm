;
; log-merge.scm
;
; Create a log, in the atomspace, of assorted data collected during 
; the merge.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Logged data is attached to the wild-card on the indicated matrix.
; Thus, it will travel with the matrix, if it is ever copied.
;
; ---------------------------------------------------------------------

(define-public (get-merge-iteration LLOBJ)
"
  get-merge-iteration LLOBJ -- return the number of merges done so far.
"
	(define log-anchor (LLOBJ 'wild-wild))
	(define count-location (Predicate "merge-count"))
	(define count-log (cog-value log-anchor count-location))

	; Return the logged value.
	(inexact->exact
		(if (nil? count-log) 0 (cog-value-ref count-log 0)))
)

(define-public (update-merge-iteration LLOBJ N)
"
  update-merge-iteration LLOBJ N -- Set the number of merges to N.
"
	(define log-anchor (LLOBJ 'wild-wild))
	(define count-location (Predicate "merge-count"))

	(cog-set-value! log-anchor count-location (FloatValue N))
	(store-atom log-anchor)
)

; ---------------------------------------------------------------------

(define (make-merge-logger LLOBJ)
"
  make-merge-logger LLOBJ -- create logger to record assorted dataset info
     as the merge progresses. The data is anchored on the wild-card of the
     LLOBJ.

     This is always called before the merge is performed; it records the
     state of affairs at that point.
"
	(define *-log-anchor-* (LLOBJ 'wild-wild))

	(define log-mmt-q (make-data-logger *-log-anchor-* (Predicate "mmt-q")))
	(define log-mi (make-data-logger *-log-anchor-* (Predicate "top-pair mi")))
	(define log-ranked-mi (make-data-logger *-log-anchor-* (Predicate "top-pair ranked-mi")))
	(define log-sparsity (make-data-logger *-log-anchor-* (Predicate "sparsity")))
	(define log-entropy (make-data-logger *-log-anchor-* (Predicate "mmt-entropy")))
	(define log-left-dim (make-data-logger *-log-anchor-* (Predicate "left dim")))
	(define log-right-dim (make-data-logger *-log-anchor-* (Predicate "right dim")))
	(define log-left-cnt (make-data-logger *-log-anchor-* (Predicate "left-count")))
	(define log-right-cnt (make-data-logger *-log-anchor-* (Predicate "right-count")))
	(define log-size (make-data-logger *-log-anchor-* (Predicate "total entries")))
	(define log-left-entropy (make-data-logger *-log-anchor-* (Predicate "left-entropy")))
	(define log-right-entropy (make-data-logger *-log-anchor-* (Predicate "right-entropy")))
	(define log-total-entropy (make-data-logger *-log-anchor-* (Predicate "total-entropy")))
	(define log-nclasses (make-data-logger *-log-anchor-* (Predicate "num classes")))
	(define log-nsimil (make-data-logger *-log-anchor-* (Predicate "num sim pairs")))
	(define log-ortho (make-data-logger *-log-anchor-* (Predicate "orthogonality")))

	(define (log2 x) (if (< 0 x) (/ (log x) (log 2)) -inf.0))

	(define sup (add-support-api LLOBJ))
	(define tsr (add-transpose-api LLOBJ))
	(define sap (add-gram-mi-sim-api LLOBJ))
	(define rpt (add-report-api LLOBJ))

	(define (get-sparsity)
		(define nrows (sup 'left-dim))
		(define ncols (sup 'right-dim))
		(define tot (* nrows ncols))
		(define lsize (sup 'total-support-left)) ; equal to total-support-right
		(log2 (/ tot lsize)))

	(define (get-mmt-entropy)
		(define mmt-support (tsr 'total-mmt-support))
		(define mmt-count (tsr 'total-mmt-count))
		(- (log2 (/ mmt-count (* mmt-support mmt-support)))))

	(define (get-mi PAIR)
		(define miv (sap 'get-count PAIR))
		(if miv (cog-value-ref miv 0) -inf.0))

	(define (get-ranked-mi PAIR)
		(define miv (sap 'get-count PAIR))
		(if miv (cog-value-ref miv 1) -inf.0))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; What fraction of all similarities between all word-classes
	; are zero? Returns a fraction between zero and one.  The
	; fraction is one, if all similarities are vanishing.
	(define (orthogonality)
		(define northo 0)
		(define ntot 0)

		; Loop over all N(N-1)/2 similarities for N classes.
		; i.e. not counting self-similarities.
		(define (count-ortho WLI)
			(define WCL (car WLI))
			(define REST (cdr WLI))
			(when (not (nil? REST))
				(for-each
					(lambda (RC)
						(set! ntot (+ 1 ntot))
						(if (< (mi-sim WCL RC) -1000)
							(set! northo (+ 1 northo))))
					REST)
				(count-ortho REST)))

		(define all-cls (LLOBJ 'get-clusters))
		(if (not (nil? all-cls)) (count-ortho all-cls))

		(if (< 0 ntot)
			(exact->inexact (/ northo ntot))
			1.0))

	; Log some maybe-useful data...
	(lambda (top-pair)
		(log-mmt-q ((add-symmetric-mi-compute LLOBJ) 'mmt-q))
		(log-ranked-mi (get-ranked-mi top-pair))
		(log-mi (get-mi top-pair))

		(log-sparsity (get-sparsity))
		(log-entropy (get-mmt-entropy))

		; The left and right count should be always equal,
		; and should never change.  This is a sanity check.
		(log-left-cnt (sup 'total-count-left))
		(log-right-cnt (sup 'total-count-right))

		; left and right dimensions (number of rows, columns)
		(log-left-dim (sup 'left-dim))
		(log-right-dim (sup 'right-dim))

		; Total number of non-zero entries
		(log-size (sup 'total-support-left))

		; word-dj entropies. Logged only when available.
		(when TRACK-ENTROPY
			(log-left-entropy (rpt 'left-entropy))
			(log-right-entropy (rpt 'right-entropy))
			(log-total-entropy (rpt 'total-entropy)))

		(log-nclasses (cog-count-atoms 'WordClassNode))
		(log-nsimil (cog-count-atoms 'SimilarityLink))
		(log-ortho (orthogonality))

		; Save to the DB
		(store-atom *-log-anchor-*)
	)
)

(define (make-class-logger LLOBJ)
"
  make-class-logger LLOBJ -- create logger to record merge details.
"
	(define *-log-anchor-* (LLOBJ 'wild-wild))

	; Record the classes as they are created.
	(define log-class (make-data-logger *-log-anchor-* (Predicate "class")))
	(define log-class-name (make-data-logger *-log-anchor-* (Predicate "class name")))
	(define log-class-size (make-data-logger *-log-anchor-* (Predicate "class size")))
	(define log-self-mi (make-data-logger *-log-anchor-* (Predicate "class self-mi")))
	(define log-self-rmi (make-data-logger *-log-anchor-* (Predicate "class self-ranked-mi")))
	(define log-support (make-data-logger *-log-anchor-* (Predicate "class support")))
	(define log-count (make-data-logger *-log-anchor-* (Predicate "class count")))
	(define log-logli (make-data-logger *-log-anchor-* (Predicate "class logli")))
	(define log-entropy (make-data-logger *-log-anchor-* (Predicate "class entropy")))
	(define log-cluster (make-data-logger *-log-anchor-* (Predicate "class cluster entropy")))

	; General setup of things we need
	(define sup (add-support-api LLOBJ))
	(define frq (add-pair-freq-api LLOBJ))
	(define sap (add-gram-mi-sim-api LLOBJ))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	; sum_i p_i log p_i = sum_i n_i/N log n_i/N
	; = (1/N) sum_i n_i (log n_i - log N)
	; = (1/N) sum_i n_i log n_i - ((log N) /N) sum_i n_i
	; = (1/N) sum_i n_i log n_i -  log N
	(define (cluster-entropy WCLASS)
		(define tot (fold (lambda (MEMB SUM) (+ SUM (cog-count MEMB)))
			0 (cog-incoming-by-type WCLASS 'MemberLink)))
		(define nlg (fold (lambda (MEMB SUM)
					(define cnt (cog-count MEMB))
					(if (< 0 cnt)  ; avoid taking log of zero.
						(+ SUM (* cnt (log cnt)))
						0))
				0 (cog-incoming-by-type WCLASS 'MemberLink)))
		(/ (- (log tot) (/ nlg tot)) (log 2)))

	(lambda (WCLASS)
		(log-class WCLASS)
		(log-class-name (cog-name WCLASS))
		(log-class-size (cog-incoming-size-by-type WCLASS 'MemberLink))
		(log-self-mi (mi-sim WCLASS WCLASS))
		(log-self-rmi (ranked-mi-sim WCLASS WCLASS))
		(log-support (sup 'right-support WCLASS))
		(log-count (sup 'right-count WCLASS))
		(when TRACK-ENTROPY
			(log-logli (frq 'right-wild-logli WCLASS))
			(log-entropy (frq 'right-wild-fentropy WCLASS)))
		(log-cluster (cluster-entropy WCLASS))
		(store-atom *-log-anchor-*)
	)
)

; ---------------------------------------------------------------

(define (print-params LLOBJ PORT)
"
  print-params LLOBJ PORT -- print parameters header.
"
	(define *-log-anchor-* (LLOBJ 'wild-wild))

	(define params (cog-value->list
		(cog-value *-log-anchor-* (Predicate "quorum-comm-noise"))))
	(define quorum (list-ref params 0))
	(define commonality (list-ref params 1))
	(define noise (list-ref params 2))
	(define nrank (list-ref params 3))

	(format PORT "# quorum=~6F commonality=~6F noise=~D nrank=~A\n#\n"
		quorum commonality noise nrank)

	(define in-grp-sim (cog-value-ref
		(cog-value *-log-anchor-* (Predicate "in-group-sim")) 0))

	(format PORT "# in-group-sim=~A\n" in-grp-sim)
)

; If nothing, then print zero.
(define (zlist-ref LST N) (if LST (list-ref LST N) 0))
(define (fcog-value->list VAL) (if VAL (cog-value->list VAL) #f))

(define-public (print-log LLOBJ PORT)
"
  print-log LLOBJ PORT -- Dump log contents as CSV
  Set PORT to #t to get output to stdout
"
	(define key-mmt-q (Predicate "mmt-q"))
	(define key-top-ranked-mi (Predicate "top-pair ranked-mi"))
	(define key-top-mi (Predicate "top-pair mi"))
	(define key-sparsity (Predicate "sparsity"))
	(define key-mmt-entropy (Predicate "mmt-entropy"))
	(define key-left-dim (Predicate "left dim"))
	(define key-right-dim (Predicate "right dim"))
	(define key-left-cnt (Predicate "left-count"))
	(define key-right-cnt (Predicate "right-count"))
	(define key-size (Predicate "total entries"))
	(define key-left-entropy (Predicate "left-entropy"))
	(define key-right-entropy (Predicate "right-entropy"))
	(define key-total-entropy (Predicate "total-entropy"))
	(define key-nclasses (Predicate "num classes"))
	(define key-nsimil (Predicate "num sim pairs"))

	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(define rows (cog-value->list (cog-value *-log-anchor-* key-left-dim)))
	(define cols (cog-value->list (cog-value *-log-anchor-* key-right-dim)))
	(define lcnt (cog-value->list (cog-value *-log-anchor-* key-left-cnt)))
	(define rcnt (cog-value->list (cog-value *-log-anchor-* key-right-cnt)))
	(define size (cog-value->list (cog-value *-log-anchor-* key-size)))
	(define lent (fcog-value->list (cog-value *-log-anchor-* key-left-entropy)))
	(define rent (fcog-value->list (cog-value *-log-anchor-* key-right-entropy)))
	(define tent (fcog-value->list (cog-value *-log-anchor-* key-total-entropy)))
	(define spar (cog-value->list (cog-value *-log-anchor-* key-sparsity)))
	(define ment (cog-value->list (cog-value *-log-anchor-* key-mmt-entropy)))
	(define rami (cog-value->list (cog-value *-log-anchor-* key-top-ranked-mi)))
	(define tomi (cog-value->list (cog-value *-log-anchor-* key-top-mi)))
	(define mmtq (cog-value->list (cog-value *-log-anchor-* key-mmt-q)))
	(define ncla (cog-value->list (cog-value *-log-anchor-* key-nclasses)))
	(define nsim (cog-value->list (cog-value *-log-anchor-* key-nsimil)))

	(define len (length rows))

	(format PORT "#\n# Log of dataset merge statistics\n#\n")
	(print-params LLOBJ PORT)
	(format PORT "# N,rows,cols,lcnt,rcnt,size,left-entropy,right-entropy,total-entropy,sparsity,mmt-entropy,top-pair-ranked-mi,top-pair-mi,mmt-q,nclasses,nsimilaries\n")
	(for-each (lambda (N)
		(format PORT "~D\t~A\t~A\t~A\t~A\t~A\t~9F\t~9F\t~9F\t~9F\t~9F\t~9F\t~9F\t~9F\t~D\t~D\n"
			(+ N 1)
			(inexact->exact (list-ref rows N))
			(inexact->exact (list-ref cols N))
			(inexact->exact (list-ref lcnt N))
			(inexact->exact (list-ref rcnt N))
			(inexact->exact (list-ref size N))
			(zlist-ref lent N)
			(zlist-ref rent N)
			(zlist-ref tent N)
			(list-ref spar N)
			(list-ref ment N)
			(list-ref rami N)
			(list-ref tomi N)
			(list-ref mmtq N)
			(inexact->exact (list-ref ncla N))
			(inexact->exact (list-ref nsim N))
		))
		(iota len))

	; Flush port to disk
	(force-output PORT)
)

(define-public (print-merges LLOBJ PORT)
"
  print-merges LLOBJ PORT -- Dump merge log contents as CSV
  Set PORT to #t to get output to stdout
"
	(define key-class (Predicate "class"))
	(define key-class-name (Predicate "class name"))
	(define key-class-size (Predicate "class size"))
	(define key-self-mi (Predicate "class self-mi"))
	(define key-self-rmi (Predicate "class self-ranked-mi"))
	(define key-support (Predicate "class support"))
	(define key-count (Predicate "class count"))
	(define key-logli (Predicate "class logli"))
	(define key-entropy (Predicate "class entropy"))
	(define key-cluster (Predicate "class cluster entropy"))

	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(define classes (cog-value->list (cog-value *-log-anchor-* key-class)))
	(define class-name (cog-value->list (cog-value *-log-anchor-* key-class-name)))
	(define class-size (cog-value->list (cog-value *-log-anchor-* key-class-size)))
	(define self-mi (cog-value->list (cog-value *-log-anchor-* key-self-mi)))
	(define self-rmi (cog-value->list (cog-value *-log-anchor-* key-self-rmi)))
	(define support (cog-value->list (cog-value *-log-anchor-* key-support)))
	(define count (cog-value->list (cog-value *-log-anchor-* key-count)))
	(define logli (fcog-value->list (cog-value *-log-anchor-* key-logli)))
	(define entropy (fcog-value->list (cog-value *-log-anchor-* key-entropy)))
	(define cluster (cog-value->list (cog-value *-log-anchor-* key-cluster)))

	(define len (length classes))

	; Frick-n-frack adjust for stumbles with quotes
	(define (esc-q STR)
		(string-concatenate
			(map (lambda (CHAR)
				(cond
					((equal? CHAR #\") "U+0022")
					((equal? CHAR #\\) "U+005C")
					(else (list->string (list CHAR)))))
				(string->list STR))))

	(format PORT "#\n# Log of class-related merge statistics\n#\n")
	(print-params LLOBJ PORT)
	(format PORT "# N,words,class-size,self-mi,self-rmi,support,count,logli,entropy,cluster\n")
	(for-each (lambda (N)
		(define cls (list-ref classes N))
		(format PORT "~D\t\"~A\"\t~D\t~9F\t~9F\t~D\t~D\t~9F\t~9F\t~9F\n"
			(+ N 1)

			; It commonnly happens that the class has been deleted,
			; whenever it was fully merged into another class.
			; Thus, we cannot rely on it being there.
			; (if (cog-atom? cls) (cog-name cls) "#f")
			(esc-q (list-ref class-name N))
			(list-ref class-size N)
			(list-ref self-mi N)
			(list-ref self-rmi N)
			(list-ref support N)
			(list-ref count N)
			(zlist-ref logli N)
			(zlist-ref entropy N)
			(list-ref cluster N)
		))
		(iota len))

	; Flush port to disk
	(force-output PORT)
)

; ---------------------------------------------------------------

(define-public (dump-log LLOBJ PREFIX-DIR PRINTER)
"
  dump-log LLOBJ PREFIX-DIR PRINTER -- print log file to PREFIX-DIR.

  The PREFIX-DIR is used to specify the directory and also the file
  prefix.  Appended to this will be a string recording the quorum,
  commonality and noise for the run.

  The PRINTER must be either `print-log` or `print-merges`.

  Example: (dump-log star-obj \"/tmp/foo\" print-log)
  Result: \"/tmp/foo-q0.5-c0.2-n4.dat\" where 0.5 is the quorum,
  0.2 the commonality and 4 is the noise.
"
	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(define params (cog-value->list
		(cog-value *-log-anchor-* (Predicate "quorum-comm-noise"))))
	(define quorum (list-ref params 0))
	(define commonality (list-ref params 1))
	(define noise (list-ref params 2))
	(define FILENAME (format #f "~A-q~A-c~A-n~D.dat"
		PREFIX-DIR quorum commonality (inexact->exact noise)))
	; (define port (open FILENAME (logior O_CREAT O_WRONLY O_TRUNC)))
	(define port (open-output-file FILENAME))
	(PRINTER LLOBJ port)
	(close port)
)

; ---------------------------------------------------------------
#! ========
;
; Example usage

(print-log star-obj #t)
(print-merges star-obj #t)

==== !#
