;
; gram-optim.scm
;
; Merge words into word-classes by grammatical similarity.
; Maximum-entropy style merges.
;
; UNFINISHED. Actually unstarted.
; This might be a mirage.
;
; Copyright (c) 2017, 2018, 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` for an overview.
;
; This file implements the "linear programming merge" style.
;
; Linear Programming merge
; ------------------------
; Here, one searches for a vector `s` that maximizes some some
; (information-theoretic) criterion for merging. This criterion takes
; the form of a set of real numbers {a(d) |d is a disjunct} so that
; N(s,d) = a(d) N(w,d). To obey non-negativity, one must have
; 0 =< a(d) =< 1 for each `d`. This turns the problem into a linear
; programming problem (or rather, a convex optimization problem).
; If a(d) is either zero, or one, then this is a (binary) integer
; programming problem.
;
; In the current context, there are two information-theoretic criteria
; that seem reasonable to pursue. One is to maximize
;
;     S = MI(g,s) - MI(g,t)
;
; with MI as defined in `gram-classification.scm` (and elsewhere).
; That is, we want to decompose `w=s+t` such that the `s` component has
; the greatest possible MI with `g`, and the remainder has the
; least-possible.
;
; A second possibility is to maximize
;
;     H = p(s) MI(g,w) - p(t) MI(g,t)
;
; where p(w) = dot(w,*) / dot(*,*) with dot(,) as defined above.
;
; In principle, integer optimization problems are NP-hard (NP-complete).
; It is not entirely clear if that is the case here. There seems to be
; at least one perhaps-hacky-but-linear-time algo:
;  1. Create a sorted list of disjuncts `d` according to N(*,d)
;  2. Create the empty set S
;  3. For each `d`, from highest N(*,d) to lowest, create a vector `s`
;     setting N(s,b)=N(w,b) if b is in S or if b==d; else N(s,b)=0.
;  4. If the vector `s` is accepted by the criteria (i.e. it is larger)
;     then define S = S union d.
;  5. Loop to step 3 until done.
;
;
; Parameter choices
; -----------------
; Gut-sense intuition suggests these possible experiments:
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------
; Is it OK to merge WORD-A and WORD-B into a common vector?
;
; Return #t if the two should be merged, else return #f
; WORD-A might be a WordClassNode or a WordNode.
; WORD-B should be a WordNode.
;
; MIOBJ must offer the 'mmt-fmi method.
;
; This uses information-similarity and a cutoff to make the ok-to-merge
; decision. Uses the information-similarity between the
; disjunct-vectors only (for now!?), and not between the shapes.
;
; Example usage
;     (define pca (make-pseudo-cset-api))
;     (define psa (add-pair-stars pca))
;     (define mio (add-symmetric-mi-compute psa))
;     (is-info-similar? mio 4.0 (Word "he") (Word "she"))
;
(define (is-info-similar? MIOBJ CUTOFF WORD-A WORD-B)

	(define (get-info-sim wa wb) (MIOBJ 'mmt-fmi wa wb))

	; To print an ad hoc progress report, call is-similar?
	; (is-similar? get-info-sim CUTOFF WORD-A WORD-B)
	(define sim (get-info-sim WORD-A WORD-B))
	(< CUTOFF sim)
)

; ---------------------------------------------------------------

(define (foo LLOBJ)

	(let* ((stars-obj (add-pair-stars LLOBJ))
			(suppt-obj (add-support-api stars-obj))
			(trans-obj (add-transpose-api stars-obj))
			(symio-obj (add-symmetric-mi-compute trans-obj))
		)

	; Obtain a sorted list of all disjuncts, ordered from highest
	; to lowest counts.
	(define sorted-djs
		(sort (stars-obj 'right-basis)
			(lambda (da db)
				(> (suppt-obj `left-count da) (suppt-obj `left-count db)))))

(format #t "foo")
	)
)

; ---------------------------------------------------------------
; Example usage
;
; (define pca (make-pseudo-cset-api))
; (define psa (add-pair-stars pca))
; (define mio (add-symmetric-mi-compute psa))
; (is-info-similar? mio 4.0 (Word "he") (Word "she"))
