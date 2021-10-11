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
(format #t ">>>> Make-simmer mmt-q = ~9F\n" mmt-q)

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

(define (recomp-all-sim LLOBJ WX)
"
  Recompute all existing similarities to word WX
"
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
		(length existing-list) (cog-name WX) (e))
)

; ---------------------------------------------------------------
(define (setup-initial-similarities LLOBJ NRANK)
"
  Compute a block matrix of similarities between the top-ranked words.
"
	(define e (make-elapsed-secs))

	; Start by getting the ranked words.  Note that this may include
	; WordClass nodes as well as words.
	(define ranked-words (rank-words LLOBJ))
	(format #t "Done ranking words in ~A secs\n" (e))

	; Load similarity-pairs; pointless to recompute if we have them!
	((add-similarity-api LLOBJ #f SIM-ID) 'fetch-pairs)

	; Create similarities for the initial set.
	(compute-diag-mi-sims LLOBJ ranked-words 0 NRANK)
	(format #t "Done computing MI similarity in ~A secs\n" (e))
)

; ---------------------------------------------------------------

(define (main-loop LLOBJ MERGE-FUN NRANK LOOP-CNT)
"
  Unleash the fury. Inside of a loop, apply the MERGE-FUN to the
  top-ranked word-pair, for LOOP-CNT iterations. After each
  iteration, the similarities for a few more words are computed,
  so that, no matte the LOOP-CNT, there is a suitably deep set of
  word-pair similarities to rank and consider.
"
	; Get rid of all MI-similarity scores below this cutoff.
	(define MI-CUTOFF 4.0)

	; Offset on number of simularities to compute
	(define NSIM-OFFSET (length (cog-get-atoms 'WordClassNode)))

	; How many more similarities to compute each step.
	(define GRO-SIZE 3)

	; Range of similarities to compute.
	(define (diag-start N) (+ N NSIM-OFFSET))
	(define (diag-end N) (+ NRANK (* GRO-SIZE N) NSIM-OFFSET))

	(for-each
		(lambda (N)
			(define e (make-elapsed-secs))
			(define sorted-pairs (get-ranked-pairs LLOBJ MI-CUTOFF))
			(format #t "------ Round ~A Next in line:\n" (diag-start N))
			(prt-sorted-pairs LLOBJ sorted-pairs 0 12)

			(define top-pair (car sorted-pairs))
			(MERGE-FUN (diag-start N) (gar top-pair) (gdr top-pair))

			(format #t "------ Completed merge in ~A secs:\n" (e))

			; Expand the size of the universe
			(define ranked-words (rank-words LLOBJ))
; (format #t "Skipping:")
; (for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
; (take ranked-words (diag-start N)))
; (format #t "\n")
; (format #t "Head of sim-pair list:")
; (for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
; (take (drop ranked-words (diag-start N)) 12))
; (format #t "\n")
			; (compute-diag-mi-sims LLOBJ ranked-words (diag-start N) (diag-end N))
			(compute-diag-mi-sims LLOBJ ranked-words 0 (diag-end N))
			(format #t "------ Extended the universe in ~A secs\n" (e))
		)
		(iota LOOP-CNT))
)

; ---------------------------------------------------------------

(define-public (pair-wise-cluster LLOBJ NRANK LOOP-CNT)
"
  pair-wise-cluster LLOBJ NRANK LOOP-CNT - perform clustering.

  This is the main entry point for clustering similar words. It uses
  a relatively simple algorithm for determining when words should be
  merged into clusters. As of this writing, it works quite well,
  apparently as well as any earlier attempts.

  This starts by computing similarities between pairs of words among
  the NRANK most frequent words. It is recommended that NRANK be set
  to between 100 and 200. This initial calculation takes hours,
  depending on the dataset and the CPU.

  The similarity that is computed is the symmetric-MI similarity and
  the `ranked-MI` similarity. This is computed between word-vectors
  that include both Sections and CrossSections. There is no option
  to use a different similarity function; others seem to offer little
  improvement, and seveal downsides, so we've hard-coded this.

  There is no option to use anything other than the Sections (+)
  CrossSections word-vector. This is because the CrossSections are
  required to perform a coherent connector merge, and so we use that
  throughout. This is all automated, under the covers.

  LOOP-CNT is the number of grammatical classes to create. This will
  loop through the merge step this many times.

  The merge step picks the top-two most similar words (or word and
  word-class) and merges them, including merging the connectors.
  After the merge is performs, marginals are recomputed, so that they
  remain accurate. After each merge, the list of most frequent
  words/word-classes is recomputed, and similarities are computed for
  the next three words (hard-coded at 3, for now). Thus, by the end,
  thwere will be similarities for `NRANK + 3*LOOP-CNT` most frequent
  words/word-classes.

  This can be contrasted to the clique-merge algorithm, which computes
  an in-group of the most similar pairs, and merges them in each merge
  step. The clique-merge algo is more complex, because it has to find
  this in-group. It is also more powerful, since it generalizes the
  overlaps of the word-vectors, which this algo is fundamentally
  incapable of doing.

  Status: more-or-less finished. Works. Prints a lot of diagnostics.
  Easy-to-use, simple, not a lot of user-adjustable parameters.

  Recommended usage:
  ```
      (define pca (make-pseudo-cset-api))
      (define pcs (add-pair-stars pca))
      (define sha (add-covering-sections pcs))
      (sha 'fetch-pairs)
      (sha 'explode-sections)

      (if (forgot-to-do-the-mmt-marginals-yet?)
          ((batch-transpose sha) 'mmt-marginals)
      )

      (pair-wise-cluster sha 200 500)
  ```
"
	(setup-initial-similarities LLOBJ NRANK)

	; ------------------------------

	; The fraction to merge -- zero.
	(define (none WA WB) 0.0)

	(define (store-mmt WRD) (recompute-mmt LLOBJ WRD))

	(define (store-final) (recompute-mmt-final LLOBJ))

	(define (accum LLOBJ CLUST SECT WEIGHT)
		(accumulate-count LLOBJ CLUST SECT WEIGHT))
	(define merge-them (make-mergefn LLOBJ
		none accum store-mmt store-final #t))

	; ------------------------------
	; The workhorse, the function that does the work.

	(define (do-merge N WA WB)
		(format #t "------ Start merge ~D of `~A` and `~A`\n"
			N (cog-name WA) (cog-name WB))

		; If we are merging two classes into one, then the
		; second class will be depopulated. We need to trash
		; the SimilarityLinks to it to avoid an error.
		(if (and (equal? (cog-type WA) 'WordClassNode)
				(equal? (cog-type WB) 'WordClassNode))
			(for-each cog-delete!
				(cog-incoming-by-type WB 'SimilarityLink)))

		(define e (make-elapsed-secs))
		(define wclass (merge-them  WA WB))

		(format #t "------ Merged `~A` and `~A` into `~A` in ~A secs\n"
			(cog-name WA) (cog-name WB) (cog-name wclass) (e))

		; After merging, recompute similarities for the words
		; that were touched. We don't need to explicitly handle
		; the new class, because the grow-universe code will do that.
		(recomp-all-sim LLOBJ WA)
		(recomp-all-sim LLOBJ WB)

		(format #t "------ Recomputed MI for `~A` and `~A` in ~A secs\n"
			(cog-name WA) (cog-name WB) (e))
	)

	; --------------------------------------------
	; Unleash the fury
	(main-loop LLOBJ do-merge NRANK LOOP-CNT)
)

; ---------------------------------------------------------------

(define-public (in-group-cluster LLOBJ NRANK LOOP-CNT)
"
  in-group-cluster LLOBJ NRANK LOOP-CNT - perform clustering.

Unfinished prototype
"
	(setup-initial-similarities LLOBJ NRANK)

	; The ranked MI similarity of two words
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (ranked-mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 1) -inf.0))

	; ------------------------------
	; The fraction to merge -- always zero.
	(define (none WA WB) 0.0)

	; Recompute marginals after merge.
	(define (store-mmt WRD) (recompute-mmt LLOBJ WRD))
	(define (store-final) (recompute-mmt-final LLOBJ))

	; ------------------------------
	; Vote for the disjuncts that will be included in the merge group
	(define (vote-for-disjuncts IN-GRP VOTE-THRESH)
		(define (foo . args)
			'stuff)
		(define tupe (add-tuple-math LLOBJ foo 'right-element))
		#f
	)

	; ------------------------------
	; Main workhorse function
	(define (do-merge N WA WB)
		(define e (make-elapsed-secs))
		(format #t "------ Start merge ~D with seed pair `~A` and `~A`\n"
			N (cog-name WA) (cog-name WB))

		(define ranked-words (rank-words LLOBJ))
		; Approximation to number of words with sims.
		; This is overkill; NRANK is more than enough!
		(define n-to-take
			(min (length ranked-words) (+ NRANK (* 3 N))))
		(define words-with-sims (take ranked-words n-to-take))
		(define in-grp
			(optimal-in-group ranked-mi-sim WA WB words-with-sims))
		(format #t "In-group size=~A:" (length in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD))) in-grp)
		(format #t "\n")

		; When to merge -- depends on how the clique voted.
; xxxxxx not done
(define (voter LLOBJ CLUST SECT WEIGHT)
; This is not the thing yet.
   (accumulate-count LLOBJ CLUST SECT WEIGHT))

		; We need a new merge object per in-group, because the votes
		; depend on the in-group.
		(define merge-them (make-mergefn LLOBJ
			none voter store-mmt store-final #t))

		; Merge the first two manually, so that wclass is always
		; a WordClass.
		(define wclass (merge-them WA WB))

		; Merge the rest of them. WA an WB are always at the tail
		; of the in-group list, so drop them.
		(for-each
			(lambda (WRD)
				(set! wclass (merge-them wclass WRD)))
			(drop-right in-grp 2))

		(format #t "------ Merged into `~A` in ~A secs\n"
			(cog-name wclass) (e))

		; After merging, recompute similarities for the words
		; that were touched.
		(for-each (lambda (WRD) (recomp-all-sim LLOBJ WRD)) in-grp)

		(format #t "------ Recomputed MI in ~A secs\n" (e))
	)

	; --------------------------------------------
	; Unleash the fury
	(main-loop LLOBJ do-merge NRANK LOOP-CNT)
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
