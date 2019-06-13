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
; When a pair of words are judged to be grammatically similar, they can
; be combined to create a "grammatical class", containing both the words,
; and behaving as their "average".  Similarly, a word can be compared to
; an existing grammatical class, to see if it belongs to that class.
;
; Recall that words are represented as vectors, so that both comparison
; and merging are comparisons of vectors and merges of vectors.
; This file implements several different comparison and merge strategies:
;
; For comparison:
; * Cosine similarity
; * MI similarity
;
; For theoretical reasons, MI should work better than cosine, primarily
; because the vector space is not Euclidean space, but a probability
; simplex.
;
; For merging:
; * Orthogonal decomposition into parallel & perpendicular components.
; * Union and overlap of basis elements
; * Binary optimization (integer programming)
;
; For theoretical reasons, the binary-optimization strategy should
; provide the best results. This is because the merge decision is
; determined by maximum entropy principles.
;
;
; Representation
; --------------
; A grammatical class is represented as
;
;     MemberLink
;         WordNode "wordy"      ; The word itself.
;         WordClassNode "noun"  ; The grammatical class of the word.
;
; Word classes have a designated grammatical behavior, using Sections,
; behaving just like the pseudo-connectors on single words. Thus, either
; a WordNode or a WordClassNode can appear in a Connector link, as
; shown below.
;
;     Section
;         WordClassNode "noun"
;         ConnectorSeq
;             Connector
;                WordClassNode "verb" ; or possibly a WordNode
;                LgConnDirNode "+"
;             Connector
;                ....
;
; The TV on the MemberLink holds a count value; that count equals the
; total number of section-counts that were transferred from the word, to
; the word-class, when the word was merged into the class. The sum over
; all of these counts (on the MemberLinks) should exactly equal the sum
; over the counts on all Sections for that WordClassNode.  Thus, it can
; be used to determine what fraction the word contributed to the class.
; Since merge strategies are generally non-linear, this value is at best
; a crude indicator, rather than a way of reconstructing the components.
;
; Basic assumptions
; -----------------
; It is assumed that grammatical classes are stepping stones to word
; meaning; that meaning and grammatical class are at least partly
; correlated. It is assumed that words can have multiple meanings, and
; thus can belong to multiple grammatical classes. It is assumed that
; the sum total number of observations of a word is a linear combination
; of the different ways that the word was used in the text sample.
; Thus, the task is to decompose the observed counts on a single word,
; and assign them to one of several different grammatical classes.
;
; The above implies that each word should be viewed as a vector; the
; disjuncts form the basis of the vector space, and the count of
; observations of different disjuncts indicating the direction of the
; vector. It is the linearity of the observations that implies that
; such a vector-based linear approach is correct.
;
; The number of grammatical classes that a word might belong to can
; vary from a few to a few dozen; in addition, there will be some
; unknown amount of "noise": incorrect sections due to incorrect parses.
;
; It is assumed that when a word belongs to several grammatical classes,
; the sets of disjuncts defining those classes are not necessarily
; disjoint; there may be significant overlap. That is, different
; grammatical classes are not orthogonal, in general.
;
; A Note about "Vectors"
; ----------------------
; Unfortunately, the word "vector" is not quite the correct term, as it
; suggests a vector space with rotational symmetry and basis independence.
; This is very much NOT the case: the space has NOT rotational symmetry
; at all, and is very basis-dependent.  The concept of a "matroid" moves
; in the right direction, but does not quite capture the idea. The space
; is actually a probability space; the preservation of counts is the
; same as the preservation of probability.
;
; A different problem is that the bases of the vectors are not actually
; "independent"; they contain words, themselves. One can consider
; Sections where each word in a Connector is replaced by a variable (by
; a wild-card): these are called "Shapes" below, and elsewhere, and
; provide basis elements for a different, but related vector space.
; These subtleties regarding vectors and shapes are ignored here; the
; word "vector" will be used only because of the lack of a better word.
;
; Semantic disambiguation
; -----------------------
; The correct notion of a grammatical class is not so much as a
; collection of words, but rather as a collection of word-senses.
; Consider the word "saw": it can be the past tense of the verb
; "to see", or it can be the cutting tool, a noun.  Thus, the word
; "saw" should belong to at least two different grammatical classes.
; The actual word-sense is "hidden", only the actual word is observed.
; The "hidden" word-sense can be partly (or mostly) discerned by looking
; at how the word was used: nouns are used differently than verbs.
; The different usage is reflected in the collection of sections
; ("disjuncts") that are associated with the word-sense.
;
; Thus, the vector associated to the word "saw" is the (linear) sum
; for a noun-vector (the cutting tool) and two different verb-vector
; (observing; cutting).  This section describes how the cosine-distance
; can be used to distinguish between these different forms, how to
; factor the vector of observation counts into distinct classes.
;
;
; Word Similarity
; ---------------
; There are several different means of comparing similarity between
; two words.  A traditional one is cosine distance: if the cosine of two
; word-vectors is greater than a threshold, they should be merged.
;
; The cosine distance between the two words w_a, w_b is
;
;    cos(w_a, w_b) = v_a . v_b / |v_a||v_b|
;
; Where, as usual, v_a . v_b is the dot product, and |v| is the length.
;
; If N(w,d) is the count of the number of observations of word w with
; disjunct d, the dot product is
;
;    dot(w_a, w_b) = v_a . v_b = sum_d N(w_a,d) N(w_b,d)
;
; The minimum-allowed cosine-distance is a user-tunable parameter in
; the code below; it is currently hard-coded to 0.65.
;
; A fundamental problem with cosine distance is that it is built on an
; assumption of the rotational invariance of Euclidean space. However,
; the "vectors" here are not actually vectors, they are points in a
; probability space that has no rotational symmetry. Acknowledging this
; leads to the contemplation of probabilistic distance functions.
;
; A better judge of similarity is the information-theoretic divergence
; between the vectors (the Kullback-Lielber divergence). If N(w,d) is
; the count of the number of observations of word w with disjunct d,
; the divergence is:
;
;    MI(w_a, w_b) = log_2 [dot(w_a, w_b) dot(*,*) / ent(w_a) ent(w_b)]
;
; where
;
;    ent(w) = sum_d N(w,d) N(*,d) = dot(w, *)
;
; so that log_2 ent(w) is the entropy of word w (Up to a factor of
; N(*,*) squared. That is, we should be using p(w,d) = N(w,d) / N(*,*)
; in the definition. This and other considerations are covered in much
; greater detail in the supporting PDF's.)
;
;
; Merge Algos
; -----------
; There are several ways in which two words might be merged into a
; word-class, or a word added to a word-class.  Some of these are
; described below.  Additional kind of merges can be imagined; an
; adequate theoretical foundation remains unclear.  However, based
; on the gut-sense intuition that information-theoretic techniques
; are primal, then a merge strategy based on MI/entropy maximization
; appears to be the most promising. This is described last, as it
; is the most complex.
;
; Given a word (a word-vector) `w`, we wish to decompose it into a
; component `s` that will be merged into the grammatical class `g`,
; and a component `t` that will be left over. The preservation of
; counts (the preservation of probabilities) requires that
; `w = s + t`; that is, that N(w,d) = N(s,d) + N(t,d) for fixed d.
;
; Merging means that `g_new = g_old + s`; that is,
;
;      N(g_new, d) = N(g_old, d) + N(s,d)
;
; for each disjunct `d`. To keep the number of vectors that need to be
; track small, the vector `g_old` is immediately discarded. Likewise,
; the vector `s` is also immediately discarded. The remainder-vector `t`
; is relabelled as the new `w`; that is, `w_new = t` and `w_old` is
; discarded. (This is a subtraction that preserves the total counts:
; if `w_old = w`, then `w_new = w_old - s`.)
;
; This is what is meant by "merging" in what follows. The different
; algos are all about different ways of figuring out what the vector
; `s` should be.
;
; All of these different merge algos suffer from a certain set of
; problems. These are:
;
; A) The number of vectors being tracked in the system is increasing:
;    merge decisions include the decision to combine two words to form
;    a new grammatical class. The `t` remnants of each word remain in
;    the system, available for further classification into other
;    word-senses.  At some point, the remainders `t` are likely to get
;    small, and consist entirely of noise, and so require pruning.
;
; B) There is a hysteresis effect: when `g_new` is obtained, it is in
;    general no longer parallel to `g_old`, and future merge decisions
;    will be based on `g_new` rather than on `g_old`. Furthermore, the
;    earlier merge decisions are not recomputed, and so there is a
;    gradual drift of each grammatical class `g` as words are assigned
;    to it. The drift is history-dependent: it depends on the sequence
;    by which words are added in.
;
; C) The replacement of `w` by `w_new` means that the original word
;    vectors are "lost", and not available for some other alternative
;    processing. This could be avoided by caching the original
;    word-vectors somewhere. However, doing this would increase the
;    size of the dataset (which is already unmanageably large), and
;    at this time, there does not seem to be any reason to access the
;    original counts.
;
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
