;
; agglo-goe.scm
;
; Loop over all words, merging them into grammatical categories.
; Agglomerative clustering, using GOE similarity to determine
; membership.
;
; Copyright (c) 2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file manages the top-most loop for traversing over all words,
; and assigning them to grammatical clusters using GOE similarity.
; This file does not provide tools for judging similarity, nor does
; it provide the low-level merge code.  It only manages the top loop.
;
; Under construction.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
;
(define (add-goe-sim LLOBJ)
"
  add-goe-sim LLOBJ -- add wrapper that returns GOE similarity with the
  'get-count and 'pair-count methods. Specifically, these two methods
  will return floating point numbers (and NOT a FloatValue!)
"
	(define (get-ref PR IDX)
		; Expect FloatValue always. IDX=0 is the MI sims, and 1 is the RMI
		(cog-value-ref (LLOBJ 'get-count PR) IDX))

	(define (get-pair-count WA WB)
		(get-ref (Similarity WA WB) 0))

	(lambda (message . args)
		(case message
			((get-count)   (get-ref (car args) 0))
			((pair-count)  (apply get-pair-count args))
			(else          (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------

(define*-public (goe-cluster LLOBJ LOOP-CNT
	#:key
		(QUORUM 0.7)
		(COMMONALITY 0.2)
		(NOISE 4)
		(PUSH-FRAMES #t)
)
"
  goe-cluster LLOBJ LOOP-CNT -- perform GOE-based clustering.

  LLOBJ needs to have the 'cluster-type method on it.
  LLOBJ should probably be `(add-covering-sections (csets))`

  LOOP-COUNT is number of times to run the loop.

  Keyed paramters are #:QUORUM #:COMMONALITY #:NOISE #:PUSH-FRAMES
  The first three are the same as described elsewhere; they control the
  generation of the in-group. The #:PUSH-FRAMES defaults to #t and
  determines whether frames are used protect earlier data.

  The initial in-group is selected based on GOE similarity. This is done
  with some hard-coded paramters; these should probably be exposed in the
  API.

  Upon completion, this returns a list of all words that were touched
  (merged). The list includes the WordClasses that were created.

  Neither MI similarities, nor GOE similarities are recomputed; these
  must be done elsewhere, based on the retruned word-list.
"
	; General structure:
	; * Run merge loop for N cycles
	; * Track all words that were touched, this is in `donelist`

	; Does the same thing:
	; (define gram-mi-api (add-similarity-api LLOBJ #f "shape-mi"))
	(define gram-mi-api (add-gram-mi-sim-api LLOBJ))
	(define goe-api (add-similarity-api LLOBJ #f "goe"))
	(define goe-sim (add-goe-sim goe-api))

	; Pair-wise simiarity. Use arccosine to get a better spread, to
	; decompress the area where things are similar.  Flip the sign,
	; because the clustering code assumes more-positive == more similar.
	(define (theta-sim WA WB)
		(- (acos (goe-sim 'pair-count WA WB))))

	(define e (make-elapsed-secs))

	; Log the parameters that were supplied.
	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(cog-set-value! *-log-anchor-* (Predicate "goe-quorum-comm-noise")
		(FloatValue QUORUM COMMONALITY NOISE))

	; Record the classes as they are created.
	(define log-class (make-class-logger LLOBJ))
	(define log-dataset-stuff (make-merge-logger LLOBJ))

	; Create the function that determines group membership.
	(define jaccard-select (make-jaccard-selector LLOBJ
		QUORUM COMMONALITY NOISE))

	; Create the function that performs the merge.
	(define merge-majority (make-merge-majority LLOBJ QUORUM NOISE #t))

	; ------------------------------
	; Main workhorse function
	(define (perform-merge N WA WB AVAILABLE-WORDS)
		(define e (make-elapsed-secs))
		(format #t "------ Start GOE merge ~D with seed pair `~A` and `~A`\n"
			N (cog-name WA) (cog-name WB))

		; Chop down the list to a more manageable size.
		; max-epsi is how far below the top pair we want to go.
		; seems like 0.3 should be OK.
		; The absolute lower bound should be 0.5 for N=2500 basis
		; but should be 0.55 for the N=6000 basis...
		(define initial-in-grp
			(optimal-in-group theta-sim WA WB AVAILABLE-WORDS
				#:epsi-step 0.01
				#:win-size 0.02
				#:max-epsi 0.3  ; for theta sim
				#:lower-bound -0.55  ; for theta sim
				#:max-jump 2.5))

		(format #t "Initial in-group size=~D:" (length initial-in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
			initial-in-grp)
		(format #t "\n")

		(define in-grp (jaccard-select initial-in-grp))
		(format #t "In-group size=~A:" (length in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD))) in-grp)
		(format #t "\n")

		(define wclass (make-class-node LLOBJ in-grp))
		(merge-majority wclass in-grp)

;xxxxxxxxxx
		(format #t "------ Merged into `~A` in ~A secs\n"
			(cog-name wclass) (e))

		; Return the in-group
		(cons wclass in-grp)
	)

	; --------------------------------------------
	; Get the word-pairs sorted by GOE cosine.
	(define (get-sorted-goe-pairs)
		(define e (make-elapsed-secs))
		(define all-cosi (goe-api 'get-all-elts))
		(define uniq-cosi
			(remove (lambda (PR) (equal? (gar PR) (gdr PR))) all-cosi))
		(define (lessi A B)
			(> (cog-value-ref (goe-api 'get-count A) 0)
				(cog-value-ref (goe-api 'get-count B) 0)))

		(define spr (sort uniq-cosi lessi))
		(format #t "Sorted ~A GOE pairs in ~A secs\n" (length spr) (e))
		; return the sort.
		spr
	)

	(define (prt-sorted-pairs LST)
		(define sap (add-gram-mi-sim-api LLOBJ))
		(define (mi-sim PR)
			(define miv (sap 'get-count PR))
			(if miv (cog-value-ref miv 0) -inf.0))

		(for-each
			(lambda (PR)
				(define gcos (goe-sim 'get-count PR))
				(format #t "goe-theta= ~6F cos= ~6F mi= ~6F (`~A`, `~A`)\n"
					(acos gcos) gcos
					(mi-sim PR)
					(cog-name (gar PR))
					(cog-name (gdr PR))))
			LST))

	(define (get-words-with-sims PRLIST)
		(define basis-set (make-atom-set))
		(for-each (lambda (PR) (basis-set (gar PR)) (basis-set (gdr PR)))
			PRLIST)
		(basis-set #f))

	; --------------------------------------------------------
	; Setup the initial lists

	; Get all similarity pairs that have a goe on them, and sort them.
	(define all-sorted-pairs (get-sorted-goe-pairs))

	; Get all words with GOE sims
	(define words-with-sims (get-words-with-sims all-sorted-pairs))

	; Place them in frequency-ranked order.
	(define ranked-words (rank-words LLOBJ words-with-sims))

	(format #t "Found ~D words with GOE sims\n" (length ranked-words))

	(define wordlist words-with-sims)
	(define sorted-pairs all-sorted-pairs)
	(define donelist '())

	(define base-done-count (get-merge-iteration LLOBJ))

	; Main loop
	(define (loop-step N)
		(define iter-count (+ 1 N base-done-count))

		(when PUSH-FRAMES
			(let* ((as-name (format #f "GOE-merge layer ~D" iter-count))
					(as-frame (cog-new-atomspace as-name (cog-atomspace))))
				(cog-set-atomspace! as-frame)
				(store-frames as-frame)))

		(define e (make-elapsed-secs))
		(format #t "------ Round ~A Next in line:\n"
			(get-merge-iteration LLOBJ))
		(prt-sorted-pairs (take sorted-pairs 12))

		(define top-pair (car sorted-pairs))

		; Log some maybe-useful data...
		(log-dataset-stuff top-pair)

		; Perform the actual merge
		(define in-grp (perform-merge N
			(gar top-pair) (gdr top-pair) wordlist))

;xxxxxxxxxx
		; Remove the merged words from further consideration.
		(set! wordlist (lset-difference equal? wordlist in-grp))
		(set! sorted-pairs (remove (lambda (PR)
			(or
				(any (lambda (WRD) (equal? WRD (gar PR))) in-grp)
				(any (lambda (WRD) (equal? WRD (gdr PR))) in-grp)))
			sorted-pairs))

		(set! donelist (append in-grp donelist))
		(format #t "------ Completed merge in ~A secs\n" (e))

		; Store the new count last. Thus, if something fails,
		; the atomspace will have the new count, but the log
		; will not yet have been updated.
		(update-merge-iteration LLOBJ iter-count)
	)

	(for-each loop-step (iota LOOP-CNT))

	; Return the words that were touched.
	donelist
)

; ---------------------------------------------------------------------

(define*-public (foo)
"
TODO:
-- recompute all similarities every so often.
   * recomp all marginals on them & the DJ's.
   * recomp all MI's for those words.
   * comp new MI's for the gram classes.
   * recomp all goe sims, all the way down.  Ugh.
"
	#f
)

; ---------------------------------------------------------------------
#! ========
;
; Example usage.

; Load disjuncts into RAM, and create the matching shapes.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

(fetch-atom (AnchorNode "data logger"))

; This should have been done much much earlier, and stored!
; It's needed for grammatical-MI similarity computations.
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

; Also, grammatical-MI similarities should have been computed and
; stored! Fetch these!
(define smi (add-similarity-api pcs #f "shape-mi"))
(define e (make-elapsed-secs))
(smi 'fetch-pairs)
(load-atoms-of-type 'Similarity)
(format #t "Fetched in ~A secs\n" (e))

; Here's where the GOE similarities are stored.
(define gos (add-similarity-api smi #f "goe"))
(gos 'pair-count (Word "she") (Word "he"))
(gos 'get-count (Similarity (Word "she") (Word "he")))

(define layer-zero (cog-atomspace))
(define layer-one (cog-new-atomspace "layer one" layer-zero))
(cog-set-atomspace! layer-one)

(goe-cluster sha 1000 50)
(goe-cluster covr-obj 1000 5)

==== !#
