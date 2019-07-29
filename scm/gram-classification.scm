;
; gram-classification.scm
;
; Explanation of grammatical similarity, and how it's computed.
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
;                ConnectorDir "+"
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
; word-class, or a word added to a word-class.  All of these involve
; the transfer of counts from individual word-vectors to the word-class
; vector.  One generic style of merging uses concepts from Eucliden
; geometry, and involve taking parallel and perpendicular components
; of vectors. This is implemented in `gram-projective.scm`.  Another
; style suggests that information-theoretic techniques are primal, and
; a merge strategy based on MI/entropy maximization is best. This is
; implemented in the `gram-optim.scm` file.
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
; Broadening
; ----------
; The issue described in a) is an issue of broadening the known usages
; of a word, beyond what has been strictly observed in the text.  There
; are two distinct opportunities to broaden: first, in the union vs.
; overlap merging above, and second, in the merging of disjuncts. That
; is, the above merging did not alter the number of disjuncts in use:
; the disjuncts on the merged class are still disjuncts with single-word
; connectors. At some point, disjuncts should also be merged, i.e. by
; merging the connectors on them.
;
; If disjunct merging is performed after a series of word mergers have
; been done, then when a connector-word is replaced by a connector
; word-class, that class may be larger than the number of connectors
; originally witnessed. Again, the known usage of the word is broadened.
;
;
; Disjunct merging
; ----------------
; Disjunct merging is the second step in creating grammatical classes.
; The idea here is to replace individual connectors that specify words
; with connectors that specify word-classes. This step is examined in
; greater detail in `cset-class.scm`.
;
; ---------------------------------------------------------------
; This file currently contains no code!  It just documents the code!
