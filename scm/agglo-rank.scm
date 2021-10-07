;
; agglo-rank.scm
;
; Loop over all words, merging them into grammatical categories.
; Agglomerative clustering.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file manages the top-most loop for traversing over all words,
; and assigning them to grammatical clusters. This file does not
; provide tools for judging similarity, nor does it provide the
; low-level merge code.  It only manages the top loop.
;
; This is basically the general concept of "agglomerative clustering",
; which is what is (in effect) implemented in this file.
;
; There are other styles of doing agglomerative clustering, implemented
; in `agglo-loops.scm`. They work but are more complicated and don't
; work as well.  (I think they don't work as well, but this has not
; been double-checked experimentally.)
;
; Agglomerative clustering
; ------------------------
; This file implements a form of ranked clustering. It assumes that
; there is a pair-ranking function that will report the next pair to
; be merged together. That pair may be a  pair of words, a word and
; an existing cluster, or a pair of clusters.
;
; This is basic, the `cliques/democratic voting` thing is next, but its
; more complicated, so we do this first.
;
; Assumptions:
; * This assumes that shapes are being used. This is a fundamental
;   requirement for performing connector merges, so we are not going
;   to try to pretend to support anything else.
; * This assumes that support marginals have been computed, and have
;   been loaded into RAM. it will keep support marginals updated, as
;   words are merged.
;
; Notes:
; * make sure WordClassNodes and the MemberLinks are loaded
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; Where the simiarity scores will be stored
(define SIM-ID "shape-mi")

; ---------------------------------------------------------------

(define (rank-words LLOBJ)
"
  rank-words LLOBJ -- Return a list of all words, ranked by count.
  If counts are equal, then rank by support. This may take half-a-
  minute to run.  Assumes that supports have been computed and are
  available.
"
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))

	(define wrds (LLOBJ 'left-basis))
	(sort wrds
		(lambda (ATOM-A ATOM-B)
			(define na (nobs ATOM-A))
			(define nb (nobs ATOM-B))
			(if (equal? na nb)
				(> (nsup ATOM-A) (nsup ATOM-B))
				(> na nb))))
)

; ---------------------------------------------------------------

(define (make-simmer LLOBJ)
"
  make-simmer LLOBJ -- return function that computes and stores MI's.

  This computes and stores both the MI and the Ranked-MI scores.
"
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define smi (add-symmetric-mi-compute LLOBJ))

	(define ol2 (/ 1.0 (log 2.0)))
	(define (log2 x) (if (< 0 x) (* (log x) ol2) -inf.0))

	(define mmt-q (smi 'mmt-q))
(format #t ">>>> Make-simmer mmt-q =~9F\n" mmt-q)

	; Compute ans save both the fmi and the ranked-MI.
	; The marginal is sum_d P(w,d)P(*,d) / sum_d P(*,d)P(*,d)
	; The mmt-q is sum_d P(*,d)P(*,d) =
	;              sum_d N(*,d)N(*,d) / [ sum_d N(*,d) ]^2
	(define (compute-sim WA WB)
		(define fmi (smi 'mmt-fmi WA WB))
		(define mwa (smi 'mmt-marginal WA))
		(define mwb (smi 'mmt-marginal WB))
		(define rmi (+ fmi (* 0.5 (log2 (* mwa mwb))) mmt-q))

		; Print something, so user has something to look at.
		(if (< 4 fmi)
			(format #t "\tMI(`~A`, `~A`) = ~6F  rank-MI = ~6F\n"
				(cog-name WA) (cog-name WB) fmi rmi))
		(store-atom
			(sap 'set-pair-similarity
				(sap 'make-pair WA WB)
				(FloatValue fmi rmi))))

	; Return the function that computes the MI for pairs.
	compute-sim
)

; ---------------------------------------------------------------

(define-public (compute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH)
"
  compute-diag-mi-sims LLOBJ WORDLI START-RANK DEPTH - compute MI.

  This will compute the MI similarity of words lying around a diagonal.
  The width of the diagonal is DEPTH. The diagonal is defined by the
  the ranked words. Computations start at START-RANK and proceed to
  DEPTH.  If the Similarity has already been recorded, it will not
  be recomputed.

  Think of a tri-diagonal matrix, but instead of three, its N-diagonal
  with N given by DEPTH.

  WORDLI is a list of word, presumed sorted by rank.

  Examples: If START-RANK is 0 and DEPTH is 200, then the 200x200
  block matrix of similarities will be computed. Since similarities
  are symmetric, this is a symmetric matrix, and so 200 x 201 / 2
  grand total similarities are computed.

  If START-RANK is 300 and DEPTH is 200, then computations start at
  the 300'th ranked word. This results in a total of 200x200
  similarities, as 200 rows are computed, out to 200 places away from
  the diagonal.

"
	; Create a new simmer each time, so we get the updated
	; mmt-q value for this session.
	(define do-compute-sim (make-simmer LLOBJ))

	; Don't recompute similarity, if we've already got it.
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (compute-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if (not miv) (do-compute-sim WA WB)))

	; Perform similarity computations for one row.
	(define (batch-simlist ITEM ITEM-LIST)
		(for-each
			(lambda (item) (compute-sim ITEM item))
			ITEM-LIST))

	; Take the word list and trim it down.
	(define nwords (length WORDLI))
	(define start (min START-RANK nwords))   ; avoid overflow
	(define depth (min DEPTH (- nwords start)))  ; avoid overflow
	(define row-range (take (drop WORDLI start) depth)) ; list of words to do
	(define (col-start off) (max 0 (- (+ start off) depth))) ;  column start
	(define (col-end off) (min (+ start off) depth)) ;  column end
	(define (col-range off)   ; reverse, so we go from diagonal outwards
		(reverse (take (drop WORDLI (col-start off)) (col-end off))))

	(define e (make-elapsed-secs))
	(define (do-one-row off)
		(define pone (+ 1 off))
		(batch-simlist (list-ref row-range off) (col-range pone))
		(if (equal? 0 (modulo pone 10))
			(format #t "Diag: Finished ~D rows in ~D secs\n" pone (e))))

	; Perform the similarity calculations, looping over the fat diagonal.
	(for-each (lambda (n) (do-one-row n)) (iota depth))
)

; ---------------------------------------------------------------

(define (get-ranked-pairs LLOBJ MI-CUTOFF)
"
  get-ranked-pairs LLOBJ MI-CUTOFF - get a ranked list of word pairs

  This returns a list of word-pairs sorted by rank-MI, from greatest
  to least.  All pairs in the list will have an MI of greater than
  MI-CUTOFF.  An MI-CUTOFF of 4 is recommended, maybe down to 2.
  Setting this too low will provide poor merge suggestions, in addition
  to making it take more time (because setting it low will admit more
  pairs, which take more time to sort.)
"
	; General setup of things we need
	(define sap (add-similarity-api LLOBJ #f SIM-ID))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	; Get all the similarities. We're going to just hack this, for
	; now, because we SimilarityLinks with both WordNode and WordClassNode
	; in them.
	(define all-sim-pairs (cog-get-atoms 'SimilarityLink))

	; Exclude pairs with a low MI, and also self-similar pairs.
	(define good-sims
		(filter
			(lambda (SIM)
				(define WA (gar SIM))
				(define WB (gdr SIM))
				(and (< MI-CUTOFF (mi-sim WA WB)) (not (equal? WA WB))))
			all-sim-pairs))

	;; Create a word-pair ranking function
	(define (rank-pairs PRLI FUN)
		(sort PRLI
			(lambda (ATOM-A ATOM-B)
				(> (FUN ATOM-A) (FUN ATOM-B)))))

	;; Now sort all of the available pairs according to ranked-MI
	(rank-pairs good-sims (lambda (SIM) (ranked-mi-sim (gar SIM) (gdr SIM))))
)

; ---------------------------------------------------------------

(define (prt-sorted-pairs LLOBJ LST START N)
"
  prt-sorted-pairs START NUM - print list of word pairs and similarities

  Handy-dandy debug utility.
"
	(define sap (add-similarity-api LLOBJ #f SIM-ID))

	; The MI similarity of two words
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	(define len (length LST))
	(define start (min START len))   ; start is just START unless too large.
	(define num (min N (max 0 (- len START))))  ; num is just N unless too large

	(for-each
		(lambda (PR)
			(format #t "ranked-MI = ~6F MI = ~6F (`~A`, `~A`)\n"
				(ranked-mi-sim (gar PR) (gdr PR))
				(mi-sim (gar PR) (gdr PR))
				(cog-name (gar PR))
				(cog-name (gdr PR))))
		(take (drop LST start)num))
)

; ---------------------------------------------------------------

; Unfinished rough draft.
(define-public (do-stuff LLOBJ)

	(define e (make-elapsed-secs))

	; Start by getting the ranked words.  Note that this may include
	; WordClass nodes as well as words.
	(define ranked-words (rank-words LLOBJ))
	(format #t "Done ranking words in ~A secs\n" (e))

	; Create similarities for the initial set.
	(define NRANK 200)
	(compute-diag-mi-sims LLOBJ ranked-words 0 NRANK)
	(format #t "Done computing MI similarity in ~A secs\n" (e))

	; Get rid of all MI-similarity scores below this cutoff.
	(define MI-CUTOFF 4.0)

	; The fraction to merge -- zero.
	(define (none WA WB) 0.0)

	; When to merge -- always.
	(define (always WA WB) #t)

	; Recompute the support for merged words and word-classes.
	; Although the core merge routine recomputes some of the
	; marginals, that is not enough to handle MM^T correctly.
	; So we do more, here.
	(define (store-mmt WRD)

		; Get fresh copies, so that stars objects are up to date.
		(define asc (add-support-compute LLOBJ))
		(define atc (add-transpose-compute LLOBJ))

		; This first for-each loop accounts for 98% of the CPU time
		; in typical cases.
		; 'right duals returns both connector seqs and shapes.
		(for-each
			(lambda (DJ) (store-atom (asc 'set-left-marginals DJ)))
			(LLOBJ 'right-duals WRD))
		(store-atom (asc 'set-right-marginals WRD))
		(store-atom (atc 'set-mmt-marginals WRD))
	)

	(define (store-final)

		; Get fresh copies, so that stars objects are up to date.
		(define asc (add-support-compute LLOBJ))
		(define atc (add-transpose-compute LLOBJ))

		; Computing the 'set-left-totals takes about 97% of the total
		; time in this function, and about 8% of teh grand-total time.
		; I suspect it is not used anywhere.
		(store-atom (asc 'set-left-totals))   ;; is this needed? Its slow.
		(store-atom (asc 'set-right-totals))  ;; is this needed?
		(store-atom (atc 'set-mmt-totals))
	)

	(define mrg (make-merger LLOBJ
		always none 0 0 store-mmt store-final #t))

	; Recompute all existing similarities to word WX
	(define (recomp-all-sim WX)
		(define e (make-elapsed-secs))
		(define compute-sim (make-simmer LLOBJ))
		(define sap (add-similarity-api LLOBJ #f SIM-ID))
		(define sms (add-pair-stars sap))
		(define wrd-list (sms 'left-duals WX))
		(define existing-list
			(filter (lambda (WRD) (not (nil? (sap 'get-pair WRD WX))))
				wrd-list))

		(for-each (lambda (WRD) (compute-sim WRD WX)) existing-list)

		(format #t "Recomputed ~3D sims for `~A` in ~A secs\n"
			(length existing-list) (cog-name WX) (e)))

	; Compute brand-new sims for brand new clusters.
	(define (comp-new-sims WX)
		(define e (make-elapsed-secs))
		(define compute-sim (make-simmer LLOBJ))
		(define wli (take ranked-words NRANK))
		(for-each (lambda (WRD) (compute-sim WRD WX)) wli)

		(format #t "Computed ~3D sims for `~A` in ~A secs\n"
			(length wli) (cog-name WX) (e)))

	(define (do-merge N WA WB)
		(format #t "Start merge ~D of `~A` and `~A`\n"
			N (cog-name WA) (cog-name WB))
(if (and (equal? (cog-type WA) 'WordClassNode)
(equal? (cog-type WB) 'WordClassNode))
(throw 'not-implemented 'do-stuff "both are word classes"))

		(define e (make-elapsed-secs))
		(define wclass (mrg 'merge-function WA WB))

		(format #t "------ Merged `~A` and `~A` into `~A` in ~A secs\n"
			(cog-name WA) (cog-name WB) (cog-name wclass) (e))

		; After merging, recompute similarities for the words
		; that were touched.
		(recomp-all-sim WA)
		(recomp-all-sim WB)
		(if (and (not (equal? wclass WA)) (not (equal? wclass WB)))
			(comp-new-sims wclass))

		(if (and (not (equal? wclass WA)) (not (equal? wclass WB)))
			(format #t "------ Computed MI for `~A` `~A` and `~A` in ~A secs\n"
				(cog-name WA) (cog-name WB) (cog-name wclass) (e))
			(format #t "------ Computed MI for `~A` and `~A` in ~A secs\n"
				(cog-name WA) (cog-name WB) (e)))

		; Expand the size of the universe by two.
		(define ranked-words (rank-words LLOBJ))
		(for-each (lambda (WRD)
				(format #t "Top-ranked word: ~A\n" WRD))
			(take ranked-words 12))
		(compute-diag-mi-sims LLOBJ ranked-words 0 (+ (* 2 N) NRANK))
		(format #t "------ Extended the universe in ~A secs\n" (e))
	)

	; Unleash the fury
	(for-each
		(lambda (N)
			(define sorted-pairs (get-ranked-pairs LLOBJ MI-CUTOFF))
			(define top-pair (car sorted-pairs))
(prt-sorted-pairs LLOBJ sorted-pairs 0 12)
			(do-merge N (gar top-pair) (gdr top-pair))
		)
		(iota 1000))  ; loop forever, in practical terms...
)

; ---------------------------------------------------------------
#! ========
;
; Example usage

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

; If this hasn't been done, then it needs to be!
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

(define sap (add-similarity-api sha #f "shape-mi"))
(define asm (add-symmetric-mi-compute sha))

==== !#
