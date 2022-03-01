;
; similarity-graphs.scm
;
; Ad hoc scripts for greating assorted graphs and datasets comparing
; different similarity measures. This is for diary Part Three, Sept
; 2021. Also in the Diary Part Four, "Miscellanesous" section.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))


; Ad hoc globals.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sup (add-support-api pcs))
(define sap (add-similarity-api pcs))
(define smi (add-similarity-api pcs #f "mi"))
(define sov (add-similarity-api pcs #f "overlap"))
(define scj (add-similarity-api pcs #f "condjacc"))

; Need to fetch all pairs, because the similarity object doesn't
; automate this.
(pca 'fetch-pairs)
(sap 'fetch-pairs) ;;; same as (load-atoms-of-type 'Similarity)

(define ol2 (/ 1.0 (log 2.0)))
(define (log2 x)
	(if (< 0 x) (* (log x) ol2) (- (inf))))

; ---------------------------------------

; Return a list of all words, ranked by count.
; If counts are equal, then rank by support.
(define (rank-words LLOBJ)
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

; Same as above, but ranked by support first, and if equal,
; only then obs.
(define (sup-rank-words LLOBJ)
	(define sup (add-support-api LLOBJ))
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))
	(define wrds (LLOBJ 'left-basis))
	(sort wrds
		(lambda (ATOM-A ATOM-B)
			(define na (nsup ATOM-A))
			(define nb (nsup ATOM-B))
			(if (equal? na nb)
				(> (nobs ATOM-A) (nobs ATOM-B))
				(> na nb))))
)

(define ranked-words (rank-words pcs))
(define sup-ranked-words (sup-rank-words pcs))

; Return a sorted list of the NTOP most frequent words.
(define (top-ranked LLOBJ NTOP)

	(define short-list (take ranked-words NTOP))
	(format #t "After sorting, kept ~A words out of ~A\n"
		(length short-list) (LLOBJ 'left-basis-size))
	short-list
)

(define wli (top-ranked pcs 1200))

(list-ref wli 101)

; ---------------------------------------
; Dump datafile -- Self-similary rank vs MI scatterplot

(chdir "/home/ubuntu/experiments/run-6/data")

(define (rank-mi-scatter WORD-LIST TITLE FILENAME)
	(define csv (open FILENAME (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# ~A\n#\n" TITLE)
	(format csv "#\n# rank\tword\tcount\tself-mi\tsupport\n")
	(for-each
		(lambda (WRD)
			(define fv-mi (smi 'pair-count WRD WRD))
			(set! cnt (+ 1 cnt))
			(format csv
				"~A\t~A\t~D\t~6F\t~D\n" cnt (cog-name WRD)
				(inexact->exact (sup 'right-count WRD))
				(cog-value-ref fv-mi 0)
				(inexact->exact (sup 'right-support WRD))
			)
		)
		WORD-LIST
	)
	(close csv)
)

(rank-mi-scatter ranked-words
	"Observation-Count Rank words vs. self-MI"
	"rank-mi-scatter.dat")

; Same as above, but ranked by support.
(rank-mi-scatter sup-ranked-words
	"Support Rank words vs. self-MI"
	"sup-rank-mi-scatter.dat")

; ---------------------------------------
; How many words share a common connector sequence?

(define fi (sup 'right-duals (Word "fishermen")))
(map (lambda (CONSEQ) (length (sup 'left-stars CONSEQ))) fi)

; ---------------------------------------
; Take a look at connector sequences on specific words...
;

; Compute average connectorseq length for a word.
; Unweighted -- every conseq counted once.
(define (avg-conlen WORD)
	(define conseqs (sup 'right-duals WORD))
	(exact->inexact (/
		(fold + 0 (map (lambda (CONSEQ) (cog-arity CONSEQ)) conseqs))
		(length conseqs))))

; Compute weighted average connectorseq length for a word.
; Average is weighted by number of words that conseq is in.
(define (avg-weighted-conlen WORD)
	(define conseqs (sup 'right-duals WORD))
	(exact->inexact (/
		(fold + 0
			(map
				(lambda (CONSEQ) (*
					(cog-arity CONSEQ)
					(length (sup 'left-stars CONSEQ))))
				conseqs))
		(fold + 0
			(map
				(lambda (CONSEQ) (length (sup 'left-stars CONSEQ)))
				conseqs)))))

; Scatterplot of MI vs average connector lengths.
; This takes an hour or two to run, because we failed to use the cached
; support values on disjunct lengths in the above sum. That's just dumb,
; the above should have been rewritten to do that.
; Late-night fuzzy-brainedness.
(define (conlen-mi-scatter WORD-LIST)
	(define csv (open "conlen-mi-scatter.dat" (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# ConnectorSeq Averge Lenghts\n#\n")
	(format csv "#\n# rank\tword\tself-mi\tAvg\tWeighted\n")
	(for-each
		(lambda (WRD)
			(define fv-mi (smi 'pair-count WRD WRD))
			(set! cnt (+ 1 cnt))
			(format csv
				"~A\t~A\t~6F\t~6F\t~6F\n" cnt (cog-name WRD)
				(cog-value-ref fv-mi 0)
				(avg-conlen WRD)
				(avg-weighted-conlen WRD)
			)
			(force-output csv)
		)
		WORD-LIST
	)
	(close csv)
)

(conlen-mi-scatter ranked-words)

; ---------------------------------------
; Bin-count of self-MI

(define self-mi-hist
	(bin-count ranked-words 34
		(lambda (WRD) (cog-value-ref (smi 'pair-count WRD WRD) 0))
		(lambda (WRD) 1)
		0 34))

(define (prt-self-mi-hist)
	; (define csv (open "self-mi-hist.dat" (logior O_WRONLY O_CREAT)))
	(define csv (open "self-mi-hist-tsup.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv self-mi-hist csv)
	(close csv))

; ---------------------------------------
; Bin-count of word similarities.

(define wli (top-ranked pcs 1200))

; Construct long list of simlinks.
; Huh? Why? Why not just call ((add-pair-stars sap) 'get-all-elts) ?
; that would have been easier...
; or even `(cog-get-atoms 'Similarity)`
(define (get-simlinks WLI)
	(filter-map
		(lambda (WPR)
			(define sim (cog-link 'Similarity (car WPR) (cdr WPR)))
			(if (nil? sim) #f sim))
		(concatenate!   ; a list of all word-pairs
			(map
				(lambda (N)
					(define tli (drop WLI N))
					(define head (car tli))
					(map (lambda (WRD) (cons head WRD)) tli))
				(iota (length WLI))))))

(define all-sims (get-simlinks wli))
(length all-sims) ; 386380  ; or 384548 when retrimmed

(define nbins 100)
(define width 50)
(define wmi (/ 2.0 (length all-sims)))
(define wmi (/ 2.0 (length uniq-sims)))
(define mi-dist
	(bin-count uniq-sims 100 ;  all-sims 100
		(lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))
		(lambda (SIM) wmi)
		-25 25))

(define (prt-mi-dist)
	; (define csv (open "mi-dist.dat" (logior O_WRONLY O_CREAT)))
	; (define csv (open "mi-dist-tsup.dat" (logior O_WRONLY O_CREAT)))
	(define csv (open "mi-dist-uniq.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv mi-dist csv)
	(close csv))

; ------
(define wov (/ nbins (* 20.0 (length all-sims))))
(define ov-dist
	(bin-count all-sims 100
		(lambda (SIM) (cog-value-ref (sov 'get-count SIM) 0))
		(lambda (SIM) wov)
		-20 0))

(define (prt-ov-dist)
	(define csv (open "overlap-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv ov-dist csv)
	(close csv))

; ------
(define wcj (/ nbins (* 20.0 (length all-sims))))
(define cj-dist
	(bin-count all-sims 100
		(lambda (SIM) (cog-value-ref (scj 'get-count SIM) 0))
		(lambda (SIM) wcj)
		-20 0))

(define (prt-cj-dist)
	(define csv (open "condjacc-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv cj-dist csv)
	(close csv))

; ---------------------------------------
;
; Scatterplots of MI vs condjacc vs overlap

(define (mi-ov-cj-scatter)
	(define csv (open "mi-ov-cj-scatter.dat" (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# MI vs Overlap vs CondJacc\n#\n")
	(format csv "#\n# idx\tmi\toverlap\tcondjacc\n")
	(for-each
		(lambda (SIM)
			(set! cnt (+ 1 cnt))
			(format csv
				"~D\t~6F\t~6F\t~6F\n" cnt
				(cog-value-ref (smi 'get-count SIM) 0)
				(cog-value-ref (sov 'get-count SIM) 0)
				(cog-value-ref (scj 'get-count SIM) 0)
			)
		)
		all-sims
	)
	(close csv)
)


; ---------------------------------------
; List of lists, used for inverted-MI exploration.

(define wli (top-ranked pcs 1200))

; Create a sorted list by sim(WRD, word in WLIST)
; LLOBJ should be smi sov or scj
(define (sorted-by-sim LLOBJ WRD WLIST)
	(define valid-wlist
		(filter (lambda (ATOM) (LLOBJ 'pair-count ATOM WRD)) WLIST))
	(stable-sort valid-wlist
		(lambda (ATOM-A ATOM-B)
			(define sa (LLOBJ 'pair-count ATOM-A WRD))
			(define sb (LLOBJ 'pair-count ATOM-B WRD))
			(> (cog-value-ref sa 0) (cog-value-ref sb 0)))))

; (define x (sorted-by-sim smi (WordNode "of") wli))
; (any (lambda (wrd) (equal? wrd (WordNode "of"))) x)
; (map (lambda (wrd) (smi 'pair-count wrd (WordNode "of"))) x)

% list of ranked lists, sorted by MI.
% Acutally, cons pairs: (word . sorted-list)
(define sim-ranked-pairs
	(map (lambda (WRD) (cons WRD (list (sorted-by-sim smi WRD wli)))) wli))

; ---------------------------------------
; Scripts for MI inversion

; A subset of the (word . sorted-list) where word is not the first word
; in the sorted list. This is an inverted-pair situation.
(define inverted-ranked-pairs
	(filter
		(lambda (SLI) (not (equal? (car SLI) (caadr SLI))))
		sim-ranked-pairs))

(length inverted-ranked-pairs)  ; 135 out of 700 and 286 out of 1200

(WordNode "in") ; le
(smi 'pair-count (WordNode "in") (WordNode "in")) ; 4.6861
(smi 'pair-count (WordNode "in") (WordNode "le")) ; 5.0586
(smi 'pair-count (WordNode "le") (WordNode "le")) ; 16.088

; Given SLI a (word . sorted-list) pair, return the leading part of
; the sorted list that is above the word.
(define (take-upr SLI)
	(define head-word (car SLI))
	(take-while
		(lambda (WRD) (not (equal? WRD head-word)))
		(cadr SLI)))

; Print.
(for-each
	(lambda (SLI)
		(format #t "~A has ~A\n" (car SLI) (take-upr SLI)))
	inverted-ranked-pairs)

(define (prt-sim WA WB)
	(format #t "~5F ~5F ~5F\n"
		(cog-value-ref (smi 'pair-count (Word WA) (Word WB)) 0)
		(cog-value-ref (smi 'pair-count (Word WA) (Word WA)) 0)
		(cog-value-ref (smi 'pair-count (Word WB) (Word WB)) 0))
	*unspecified*)

(WordNode "I")
 has ((WordNode "I’ll")
 (WordNode "You")
 (WordNode "We")
 (WordNode "I’ve")
)

(WordNode "will")
 has ((WordNode "cannot")
 (WordNode "may")
 (WordNode "must")
 (WordNode "would")
 (WordNode "can")
 (WordNode "can’t")
)

(WordNode "back")
 has ((WordNode "straight")
 (WordNode "down")
 (WordNode "forth")
 (WordNode "forward")
 (WordNode "up")
 (WordNode "out")
 (WordNode "off")
)

(WordNode "mouth")
 has ((WordNode "Majesty")
 (WordNode "sister")
 (WordNode "own")
 (WordNode "husband")
 (WordNode "brother")
 (WordNode "friend")
)

(WordNode "daughter")
 has ((WordNode "Majesty")
 (WordNode "sister")
 (WordNode "own")
 (WordNode "brother")
 (WordNode "husband")
 (WordNode "friend")
 (WordNode "mother")
 (WordNode "father")
 (WordNode "hair")
)

; Dump of all of them
(for-each
	(lambda (SLI)
		(define head-word (car SLI))
		(define bigs (take-upr SLI))
		(format #t "~A has ~A\n" head-word bigs)
		(for-each (lambda (SWD)
			(format #t "its: ~A -- ~A : ~6F vs ~6F\n"
				(cog-name head-word) (cog-name SWD)
				(cog-value-ref (smi 'pair-count SWD head-word) 0)
				(cog-value-ref (smi 'pair-count SWD SWD) 0)))
			bigs)
		(format #t "its: ~A -- ~A : ~6F\n"
			(cog-name head-word) (cog-name head-word)
			(cog-value-ref (smi 'pair-count head-word head-word) 0))
	)
	inverted-ranked-pairs)


; Compute MI(a,b) - 0.5 MI(a,a) - 0.5 MI(b,b)
(define (delta-MI WA WB)
	(-
		(cog-value-ref (smi 'pair-count (Word WA) (Word WB)) 0)
		(* 0.5 (+
			(cog-value-ref (smi 'pair-count (Word WA) (Word WA)) 0)
			(cog-value-ref (smi 'pair-count (Word WB) (Word WB)) 0)))))

; print a triangle from a list.
(define (prt-tri FUN LST)
	(define (prt-row ROW)
		(for-each (lambda (ITEM) (format #t "~6F " (FUN (car ROW) ITEM))) ROW))
	(for-each
		(lambda (N) (prt-row (drop LST N)) (newline))
		(iota (length LST))))

(prt-tri delta-MI (list "I’ll" "You" "We" "I’ve" "I"))
(prt-tri delta-MI (list "cannot" "may" "must" "would" "can" "can’t" "will"))

; ------
; Distribution of delta-MI
(define (delta-WMI WA WB)
	(-
		(cog-value-ref (smi 'pair-count WA WB) 0)
		(* 0.5 (+
			(cog-value-ref (smi 'pair-count WA WA) 0)
			(cog-value-ref (smi 'pair-count WB WB) 0)))))

(define wdt (/ nbins (* 25.0 (length all-sims))))
(define dt-dist
	(bin-count all-sims 100
		(lambda (SIM) (delta-WMI (gar SIM) (gdr SIM)))
		(lambda (SIM) wdt)
		-25 0))

(define (prt-dt-dist)
	(define csv (open "delta-mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv dt-dist csv)
	(close csv))

; ------
; scatterplot of MI vs delta-MI:

(define (mi-delta-scatter)
	(define csv (open "mi-delta-scatter.dat" (logior O_WRONLY O_CREAT)))
	(define cnt 0)
	(format csv "#\n# MI vs Delta-MI\n#\n")
	(format csv "#\n# idx\tmi\tdelta-mi\n")
	(for-each
		(lambda (SIM)
			(if (not (equal? (gar SIM) (gdr SIM)))
				(begin
					(set! cnt (+ 1 cnt))
					(format csv
						"~D\t~6F\t~6F\n" cnt
						(cog-value-ref (smi 'get-count SIM) 0)
						(delta-WMI (gar SIM) (gdr SIM))
					))))
		all-sims)
	(close csv)
)

; ---------------------------------------
; Ranked word-pairs. Rank them in various ways, to see which ones give
; the best merge recommendations.

; uniq-sims are just all sims with the self-sim (diagonal) entries
; removed.
(define uniq-sims
	(filter (lambda (SIM) (not (equal? (gar SIM) (gdr SIM)))) all-sims))

(define (rank-pairs FUN)
	(sort uniq-sims
		(lambda (ATOM-A ATOM-B)
			(> (FUN ATOM-A) (FUN ATOM-B))))
)

(define pairs-mi-sort
	(rank-pairs (lambda (SIM) (cog-value-ref (smi 'get-count SIM) 0))))
(define pairs-delta-sort
	(rank-pairs (lambda (SIM) (delta-WMI (gar SIM) (gdr SIM)))))

(define trp (add-transpose-api pcs))

; (trp 'mmt-count) is sum_y N(x,d) N(*,d)
; (trp 'total-mmt-count) is sum_y N(*,d) N(*,d)

(define tot-mmt (trp 'total-mmt-count))
(define ltot-mmt (log2 tot-mmt))

(define (marg-mmt WRD)
	(- (log2 (trp 'mmt-count WRD)) ltot-mmt))

(define pfq (add-pair-freq-api pcs))
; (pfq 'right-wild-logli)   -log_2 P(x,*)
; But above has not been computed, prints the
; "Run `((make-compute-freq LLOBJ) 'cache-all)` " error mesage.

(define tot-cnt (sup 'total-count-right))
(define ltot-cnt (log2 tot-cnt))
(define (right-freq WRD)
	(- (log2 (sup 'right-count WRD)) ltot-cnt))

(define (common-MI WA WB)
	(+ (cog-value-ref (smi 'pair-count WA WB) 0)
		(* 0.5 (+ (marg-mmt WA) (marg-mmt WB)))))

(define pairs-common-mi-sort
	(rank-pairs (lambda (SIM) (common-MI (gar SIM) (gdr SIM)))))

(define (marg-MI WA WB)
	(+ (cog-value-ref (smi 'pair-count WA WB) 0)
		(* 0.5 (+ (right-freq WA) (right-freq WB)))))

(define pairs-marg-mi-sort
	(rank-pairs (lambda (SIM) (marg-MI (gar SIM) (gdr SIM)))))

; And how is this distributed?

(define wcom (/ nbins (* 25.0 (length all-sims))))
; (define wcom 1)
(define com-dist
	(bin-count uniq-sims 100
		(lambda (SIM) (common-MI (gar SIM) (gdr SIM)))
		(lambda (SIM) wcom)
		-25 0))

(define (prt-com-dist)
	; (define csv (open "common-mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(define csv (open "common-mi-tsup.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv com-dist csv)
	(close csv))

(define wmarg (/ nbins (* 30.0 (length all-sims))))
; (define wmarg 1)
(define marg-dist
	(bin-count uniq-sims 100
		(lambda (SIM) (marg-MI (gar SIM) (gdr SIM)))
		(lambda (SIM) wmarg)
		-30 0))

(define (prt-marg-dist)
	(define csv (open "marg-mi-dist.dat" (logior O_WRONLY O_CREAT)))
	(print-bincounts-tsv marg-dist csv)
	(close csv))

(define (marg-overlap WA WB)
	(+ (cog-value-ref (sov 'pair-count WA WB) 0)
		(* 0.5 (+ (right-freq WA) (right-freq WB)))))

(define pairs-marg-ov-sort
	(rank-pairs (lambda (SIM) (marg-overlap (gar SIM) (gdr SIM)))))

(define (marg-condjacc WA WB)
	(+ (cog-value-ref (scj 'pair-count WA WB) 0)
		(* 0.5 (+ (right-freq WA) (right-freq WB)))))

(define pairs-marg-cj-sort
	(rank-pairs (lambda (SIM) (marg-condjacc (gar SIM) (gdr SIM)))))

; ----------
; Given a word, what is it's ranking?
(define (rank-of-word WRD)
	(list-index (lambda (RW) (equal? WRD RW)) wli))


; Just print out the top 100 ranked pairs, so we can look at them
; visually.
(define (prt-csv LST N)
	(define cnt 0)
	(for-each
		(lambda (SIM)
			(define ra (rank-of-word (gar SIM)))
			(define rb (rank-of-word (gdr SIM)))
			(set! cnt (+ cnt 1))
			(format #t "~D,~A,~A rank=~D ~D mind=~D ~D  cmi=~6F\n"
				cnt (cog-name (gar SIM)) (cog-name (gdr SIM))
				ra rb  (min ra rb) (abs (- ra rb))
				(common-MI (gar SIM) (gdr SIM))
			))
		(take LST N)))

(prt-csv pairs-common-mi-sort 100)
(prt-csv pairs-marg-mi-sort 100)
(prt-csv pairs-marg-ov-sort 100)
(prt-csv pairs-marg-cj-sort 100)

; ---------------------------------------
; Graphs for diary part-4 "miscellanous" section

(chdir "/home/ubuntu/experiments/run-7/data")
(pcs 'right-basis-size)

; Disjunct lengths
(define djl (/ 1.0 (pcs 'right-basis-size)))
(define djlen-dist
	(bin-count
		(pcs 'right-basis) ; all disjuncts
		9  ; max expected length of 10
		(lambda (DJ) (cog-arity DJ)) ; length of that disjunct
		(lambda (DJ) djl) ; Just count.
		0.5 9.5))

(define (prt-djlen-dist)
	(define csv (open "djlen-dist.dat" (logior O_WRONLY O_CREAT)))
	(format csv "#\n# Number of disjuncts with given length\n")
	(format csv "# Divived by total of ~A\n" (pcs 'right-basis-size))
	(format csv "#\n# idx\tlen\tcount\n")
	(print-bincounts-tsv djlen-dist csv)
	(close csv))

; ---------------------------------------

; Number of sections with given disjunct length
(define djs (sup 'total-support-left))
(define djlen-sect-dist
	(bin-count
		(pcs 'right-basis) ; all disjuncts
		9  ; max expected length of 10
		(lambda (DJ) (cog-arity DJ)) ; length of that disjunct
		(lambda (DJ) (/ (sup 'left-support DJ) djs))
		0.5 9.5))

(define (prt-djlen-sect-dist)
	(define csv (open "djlen-sect-dist.dat" (logior O_WRONLY O_CREAT)))
	(format csv "#\n# Number of sections with disjuncts with given length\n")
	(format csv "# Divived by total of ~A\n" (sup 'total-support-left))
	(format csv "#\n# idx\tlen\tcount\n")
	(print-bincounts-tsv djlen-sect-dist csv)
	(close csv))

; ---------------------------------------

; Number of sections with given disjunct length
(define djs (sup 'total-count-left))
(define djlen-count-dist
	(bin-count
		(pcs 'right-basis) ; all disjuncts
		9  ; max expected length of 10
		(lambda (DJ) (cog-arity DJ)) ; length of that disjunct
		(lambda (DJ) (/ (sup 'left-count DJ) djs))
		0.5 9.5))

(define (prt-djlen-count-dist)
	(define csv (open "djlen-count-dist.dat" (logior O_WRONLY O_CREAT)))
	(format csv "#\n# Count of sections with disjuncts with given length\n")
	(format csv "# Divived by total of ~A\n" (sup 'total-count-left))
	(format csv "#\n# idx\tlen\tcount\n")
	(print-bincounts-tsv djlen-count-dist csv)
	(close csv))

; ---------------------------------------

; Number of uniq words with given disjunct length

(define sets (map (lambda (N) (make-atom-set)) (iota 20)))

(define (get-set N) (list-ref sets N))

; Assign each word to a set of words having that particular length
(for-each
	(lambda (DJ)
		(define aset (get-set (cog-arity DJ)))
		(for-each
			(lambda (WORD) (aset WORD))  ; add word to set
			(pcs 'left-duals DJ)))     ; words on that disjunct
	(pcs 'right-basis))  ; all disjuncts

; how many words are there, with that length?
(define sizes
	(map
		(lambda (CNT) (length ((get-set CNT) #f)))
		(iota 19)))

; Total number of observations.
; Obviously, any given word may have djs of several different lengths.
(define total-wrds (fold + 1e-30 sizes))

(define djlen-word-dist
	(bin-count
		(iota 9 1)
		9  ; max expected length of 9
		(lambda (N) N) ; length of that disjunct
		(lambda (N) (/ (list-ref sizes N) total-wrds))
		0.5 9.5))

(define (prt-djlen-word-dist)
	(define csv (open "djlen-word-dist.dat" (logior O_WRONLY O_CREAT)))
	(format csv "#\n# Count of unique number of words with disjuncts with given length\n")
	(format csv "# Divived by total of ~A\n" total-wrds)
	(format csv "#\n# idx\tlen\tword-count\n")
	(print-bincounts-tsv djlen-word-dist csv)
	(close csv))

(define twos ((get-set 2) #f))

(define not-twos (atoms-subtract (pcs 'left-basis) twos))

(define (rank-wordlist LLOBJ WORD-LIST)
	(define sup (add-support-api LLOBJ))

	; nobs == number of observations
	(define (nobs WRD) (sup 'right-count WRD))
	(define (nsup WRD) (sup 'right-support WRD))

	(sort WORD-LIST
		(lambda (ATOM-A ATOM-B)
			(define na (nobs ATOM-A))
			(define nb (nobs ATOM-B))
			(if (equal? na nb)
				(> (nsup ATOM-A) (nsup ATOM-B))
				(> na nb))))
)

(define rnot-twos (rank-wordlist pcs not-twos))

(define (rank WORD)
	(+ 1 (length (take-while (lambda (WR) (not (equal? WR WORD))) ranked-words))))

(for-each
	(lambda (WRD)
		(format #t "~A   count=~D rank= ~D\n" (cog-name WRD)
			(sup 'right-count WRD) (rank WRD)))
	(take rnot-twos 20))


; ---------------------------------------
