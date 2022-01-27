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
; ---------------------------------------------------------------

(define (make-merge-logger LLOBJ)
"
  make-merge-logger LLOBJ -- create logger to record assorted dataset info
     as the merge progresses. The data is anchored on the wild-card of the
     LLOBJ.
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

	(define (log2 x) (if (< 0 x) (/ (log x) (log 2)) -inf.0))

	(define sup (add-support-api LLOBJ))
	(define tsr (add-transpose-api LLOBJ))
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
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

		; word-dj entropies
		(log-left-entropy (rpt 'left-entropy))
		(log-right-entropy (rpt 'right-entropy))
		(log-total-entropy (rpt 'total-entropy))

		(log-nclasses (cog-count-atoms 'WordClassNode))
		(log-nsimil (cog-count-atoms 'SimilarityLink))

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
	(define sap (add-similarity-api LLOBJ #f SIM-ID))

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
		(define tot (fold (lambda (MEMB SUM) (+ SUM (cog-count MEMB))) 0
			(cog-incoming-by-type WCLASS 'MemberLink)))
		(define nlg (fold (lambda (MEMB SUM)
				(define cnt (cog-count MEMB))
				(+ SUM (* cnt (log cnt)))) 0
			(cog-incoming-by-type WCLASS 'MemberLink)))
		(/ (- (log tot) (/ nlg tot)) (log 2)))

	(lambda (WCLASS)
		(log-class WCLASS)
		(log-class-name (cog-name WCLASS))
		(log-class-size (cog-incoming-size-by-type WCLASS 'MemberLink))
		(log-self-mi (mi-sim WCLASS WCLASS))
		(log-self-rmi (ranked-mi-sim WCLASS WCLASS))
		(log-support (sup 'right-support WCLASS))
		(log-count (sup 'right-count WCLASS))
		(log-logli (frq 'right-wild-logli WCLASS))
		(log-entropy (frq 'right-wild-fentropy WCLASS))
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
	(define lent (cog-value->list (cog-value *-log-anchor-* key-left-entropy)))
	(define rent (cog-value->list (cog-value *-log-anchor-* key-right-entropy)))
	(define tent (cog-value->list (cog-value *-log-anchor-* key-total-entropy)))
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
			(list-ref lent N)
			(list-ref rent N)
			(list-ref tent N)
			(list-ref spar N)
			(list-ref ment N)
			(list-ref rami N)
			(list-ref tomi N)
			(list-ref mmtq N)
			(inexact->exact (list-ref ncla N))
			(inexact->exact (list-ref nsim N))
		))
		(iota len))
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
	(define logli (cog-value->list (cog-value *-log-anchor-* key-logli)))
	(define entropy (cog-value->list (cog-value *-log-anchor-* key-entropy)))
	(define cluster (cog-value->list (cog-value *-log-anchor-* key-cluster)))

	(define len (length classes))

	(format PORT "#\n# Log of class-related merge statistics\n#\n")
	(print-params LLOBJ PORT)
	(format PORT "# N,words,class-size,self-mi,self-rmi,support,count,logli,entropy,cluster\n")
	(for-each (lambda (N)
		(define cls (list-ref classes N))
		(format PORT "~D\t\"~A\"\t~D\t~9F\t~9F\t~D\t~D\t~9F\t~9F\t~9F\n"
			(+ N 1)

			; In extremely rare circumstances, the class may have
			; been deleted, if it was fully merged into another class.
			; Well, not so rare ...
			; (if (cog-atom? cls) (cog-name cls) "#f")
			(list-ref class-name N)
			(list-ref class-size N)
			(list-ref self-mi N)
			(list-ref self-rmi N)
			(list-ref support N)
			(list-ref count N)
			(list-ref logli N)
			(list-ref entropy N)
			(list-ref cluster N)
		))
		(iota len))
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
	(define FILENAME (format #f "~A-q~A-c~A-n~A.dat"
		PREFIX-DIR quorum commonality noise))
	(define port (open FILENAME (logior O_CREAT O_WRONLY)))
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
