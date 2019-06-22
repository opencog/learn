;
; gram-optim.scm
;
; Merge words into word-classes by grammatical similarity.
; Maximum-entropy style merges.
;
; Copyright (c) 2017, 2018, 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm`
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
; with MI as defined above. That is, we want to decompose `w=s+t` such
; that the `s` component has the greatest possible MI with `g`, and the
; remainder has the least-possible.
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
; Initial cluster formation
; -------------------------
; The above described what to do to extend an existing grammatical class
; with a new candidate word.  It does not describe how to form the
; initial grammatical class, out of the merger of two words. Several
; strategies are possible. Given two words `u` and `v`, These are:
;
; * Simple sum: let `g=u+v`. That's it; nothing more.
; * Overlap and union merge, given below.
;
; Overlap merge
; -------------
; A formal (i.e. mathematically dense) description of overlap merging is
; given here. One wishes to compute the intersection of basis elements
; (the intersection of "disjuncts" aka "sections") of the two words, and
; then sum the counts only on this intersected set. Let
;
;   {e_a} = set of basis elements in v_a with non-zero coefficients
;   {e_b} = set of basis elements in v_b with non-zero coefficients
;   {e_overlap} = {e_a} set-intersection {e_b}
;   pi_overlap = unit on diagonal for each e in {e_overlap}
;              == projection matrix onto the subspace {e_overlap}
;   v_a^pi = pi_overlap . v_a == projection of v_a onto {e_overlap}
;   v_b^pi = pi_overlap . v_b == projection of v_b onto {e_overlap}
;
;   v_cluster = v_a^pi + v_b^pi
;   v_a^new = v_a - v_a^pi
;   v_b^new = v_b - v_b^pi
;
; The idea here is that the vector subspace {e_overlap} consists of
; those grammatical usages that are common for both words a and b,
; and thus hopefully correspond to how words a and b are used in a
; common sense. Thus v_cluster is the common word-sense, while v_a^new
; and v_b^new are everything else, everything left-over.  Note that
; v_a^new and v_b^new are orthogonal to v_cluster. Note that v_a^new
; and v_b^new are both exactly zero on {e_overlap} -- the subtraction
; wipes out those coefficients. Note that the total number of counts
; is preserved.  That is,
;
;   ||v_a|| + ||v_b|| = ||v_cluster|| + ||v_a^new|| + ||v_b^new||
;
; where ||v|| == ||v||_1 the l_1 norm aka count aka Manhattan-distance.
;
; If v_a and v_b have several word-senses in common, then so will
; v_cluster.  Since there is no a priori way to force v_a and v_b to
; encode only one common word sense, there needs to be some distinct
; mechanism to split v_cluster into multiple word senses, if that is
; needed.
;
; Union merging can be described using almost the same formulas, except
; that one takes
;
;   {e_union} = {e_a} set-union {e_b}
;
;
; Zipf Tails
; ----------
; The distribution of disjuncts on words is necessarily Zipfian. That
; is, the vectors could be called "Zipf vectors", in that the vector
; coefficients follow a Zipfian distribution.  There are many reasons
; why this is so, and multiple effects feed into this.
;
; It seems plausible to treat extremely-low frequency observations as
; a kind of noise, but which might also contain some signal. Thus,
; during merge, all of a Zipfian tail should be merged in. If its noise,
; it will tend to cancel during merge; if its signal, it will tend to be
; additive.
;
; That is, during merge, low-frequency observation counts should be
; merged in their entirety, rather than split in parts, with one part
; remaining unmerged.  For example, if a word is to be merged into a
; word-class, and disjunct d has been observed 4 times or less, then
; all 4 of these observation counts should be merged into the word-class.
; Only high-frequency disjuncts can be considered to be well-known
; enough to be distinct, and thus suitable for fractional merging.
;
;
; Disjunct merging
; ----------------
; Disjunct merging is the second step in creating grammatical classes.
; The idea here is to replace individual connectors that specify words
; with connectors that specify word-classes. This step is examined in
; greater detail in `cset-class.scm`.
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
(define (is-info-similar? MIOBJ CUTOFF WORD-A WORD-B)

	(define (get-info-sim wa wb) (MIOBJ 'mmt-fmi wa wb))
	(is-similar? get-info-sim CUTOFF WORD-A WORD-B)
)

; ---------------------------------------------------------------
; Example usage
