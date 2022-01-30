
Grammatical similarity, and how it's computed
=============================================
The code in this directory can be used to form "grammatical classes", based
on "grammatical similarity". This file explains what these are, and how the
general process of clustering works.

Although the below keeps talking about words and word-classes, the
actual code is (almost) entirely generic, and can merge (cluster)
anything. There are only a handful of places where its not generic;
these are slowly being cleaned up. The generic merge is possible
because what to merge, and where to put the merger results, are
defined by `LLOBJ`.

OVERVIEW
--------
When a pair of words are judged to be grammatically similar, they can
be combined to create a "grammatical class", containing both the words,
and behaving as their "average".  Similarly, a word can be compared to
an existing grammatical class, to see if it belongs to that class.

Recall that words are represented as vectors, so that comparison is a
comparison of vectors, and merging merges vectors. Although the word
"vector" is used, many of the operations are **NOT** linear; this is
not just some linear algebra, and there are some rather complicated
non-linear operations involved!

The code in this directory implements several different comparison and
merge strategies:

For comparison:
* Cosine similarity
* MI similarity
* Ranked-MI similarity

Ranked-MI works best for merging; it has the best properties of MI but also
includes the word frequency as part of the similarity score.  Otherwise,
MI works great, as long as one merges high-frequency word pairs first.
Cosine similarity is popular in the machine-learning industry, but works
poorly, based on  experimental results. This is no surprise: cosine
distance is a Casimir invariant for Euclidean space, but probability
space is not Euclidean! Its a simplex!

For merging:
* Orthogonal decomposition into parallel & perpendicular components.
* Union and overlap of basis elements.
* In-group cliques with majority voting.
* Binary optimization (integer programming).

For theoretical reasons, the binary-optimization strategy should
provide the best results. This is because the merge decision is
determined by maximum entropy principles. This has not yet been
verified experimentally, and the code has not yet been written.
In fact, it might be a mirage. (See `gram-optim.scm` for more.)


Representation
--------------
A grammatical class is represented as

    MemberLink
        WordNode "wordy"      ; The word itself.
        WordClassNode "noun"  ; The grammatical class of the word.

Word classes have a designated grammatical behavior, using Sections,
behaving just like the pseudo-connectors on single words. Thus, either
a WordNode or a WordClassNode can appear in a Connector link, as
shown below.

    Section
        WordClassNode "noun"
        ConnectorSeq
            Connector
               WordClassNode "verb" ; or possibly a WordNode
               ConnectorDir "+"
            Connector
               ....

The TV on the MemberLink holds a count value; that count equals the
total number of section-counts that were transferred from the word, to
the word-class, when the word was merged into the class. The sum over
all of these counts (on the MemberLinks) should exactly equal the sum
over the counts on all Sections for that WordClassNode.  Thus, it can
be used to determine what fraction the word contributed to the class.
Since merge strategies are generally non-linear, this value is at best
a crude indicator, rather than a way of reconstructing the components.

Basic assumptions
-----------------
It is assumed that grammatical classes are stepping stones to word
meaning; that meaning and grammatical class are at least partly
correlated. It is assumed that words can have multiple meanings, and
thus can belong to multiple grammatical classes. It is assumed that
the sum total number of observations of a word is a linear combination
of the different ways that the word was used in the text sample.
Thus, the task is to decompose the observed counts on a single word,
and assign them to one of several different grammatical classes.

The above implies that each word should be viewed as a vector; the
disjuncts form the basis of the vector space, and the count of
observations of different disjuncts indicating the direction of the
vector. It is the linearity of the observations that implies that
such a vector-based linear approach is correct.

The number of grammatical classes that a word might belong to can
vary from a few to a few dozen; in addition, there will be some
unknown amount of "noise": incorrect sections due to incorrect parses.

It is assumed that when a word belongs to several grammatical classes,
the sets of disjuncts defining those classes are not necessarily
disjoint; there may be significant overlap. That is, different
grammatical classes are not orthogonal, in general.

A Note about "Vectors"
----------------------
Unfortunately, the word "vector" is not quite the correct term, as it
suggests a vector space with rotational symmetry and basis independence.
This is very much NOT the case: the space has NOT rotational symmetry
at all, and is very basis-dependent.  The concept of a "matroid" moves
in the right direction, but does not quite capture the idea. The space
is actually a probability space; the preservation of counts is the
same as the preservation of probability.

A different problem is that the bases of the vectors are not actually
"independent"; they contain words, themselves. One can consider
Sections where each word in a Connector is replaced by a variable (by
a wild-card): these are called "Shapes" below, and elsewhere, and
provide basis elements for a different, but related vector space.
These subtleties regarding vectors and shapes are ignored here; the
word "vector" will be used only because of the lack of a better word.

Semantic disambiguation
-----------------------
The correct notion of a grammatical class is not so much as a
collection of words, but rather as a collection of word-senses.
Consider the word "saw": it can be the past tense of the verb
"to see", or it can be the cutting tool, a noun.  Thus, the word
"saw" should belong to at least two different grammatical classes.
The actual word-sense is "hidden", only the actual word is observed.
The "hidden" word-sense can be partly (or mostly) discerned by looking
at how the word was used: nouns are used differently than verbs.
The different usage is reflected in the collection of sections
("disjuncts") that are associated with the word-sense.

Thus, the vector associated to the word "saw" is the (linear) sum
for a noun-vector (the cutting tool) and two different verb-vector
(observing; cutting).  This section describes how the cosine-distance
can be used to distinguish between these different forms, how to
factor the vector of observation counts into distinct classes.


Word Similarity
---------------
There are several different means of comparing similarity between
two words.  A traditional one is cosine distance: if the cosine of two
word-vectors is greater than a threshold, they should be merged.

The cosine distance between the two words w_a, w_b is

   cos(w_a, w_b) = v_a . v_b / |v_a||v_b|

Where, as usual, v_a . v_b is the dot product, and |v| is the length.

If N(w,d) is the count of the number of observations of word w with
disjunct d, the dot product is

   dot(w_a, w_b) = v_a . v_b = sum_d N(w_a,d) N(w_b,d)

The minimum-allowed cosine-distance is a user-tunable parameter in
the code below; it is currently hard-coded to 0.65.

A fundamental problem with cosine distance is that it is built on an
assumption of the rotational invariance of Euclidean space. However,
the "vectors" here are not actually vectors, they are points in a
probability space that has no rotational symmetry. Acknowledging this
leads to the contemplation of probabilistic distance functions.

A better judge of similarity is the information-theoretic divergence
between the vectors (the Kullback-Lielber divergence). If N(w,d) is
the count of the number of observations of word w with disjunct d,
the divergence is:

   MI(w_a, w_b) = log_2 [dot(w_a, w_b) dot(*,*) / ent(w_a) ent(w_b)]

where

   ent(w) = sum_d N(w,d) N(*,d) = dot(w, *)

so that log_2 ent(w) is the entropy of word w (Up to a factor of
N(*,*) squared. That is, we should be using p(w,d) = N(w,d) / N(*,*)
in the definition. This and other considerations are covered in much
greater detail in the supporting PDF's.)


Merge Algos
-----------
There are several ways in which two words might be merged into a
word-class, or a word added to a word-class.  All of these involve
the transfer of counts from individual word-vectors to the word-class
vector.  One generic style of merging uses concepts from Eucliden
geometry, and involve taking parallel and perpendicular components
of vectors. This is implemented in `gram-projective.scm`.  Another
style suggests that information-theoretic techniques are primal, and
a merge strategy based on MI/entropy maximization is best. This is
implemented in the `gram-optim.scm` file.

Given a word (a word-vector) `w`, we wish to decompose it into a
component `s` that will be merged into the grammatical class `g`,
and a component `t` that will be left over. The preservation of
counts (the preservation of probabilities) requires that
`w = s + t`; that is, that N(w,d) = N(s,d) + N(t,d) for fixed d.

Merging means that `g_new = g_old + s`; that is,

     N(g_new, d) = N(g_old, d) + N(s,d)

for each disjunct `d`. To keep the number of vectors that need to be
track small, the vector `g_old` is immediately discarded. Likewise,
the vector `s` is also immediately discarded. The remainder-vector `t`
is relabelled as the new `w`; that is, `w_new = t` and `w_old` is
discarded. (This is a subtraction that preserves the total counts:
if `w_old = w`, then `w_new = w_old - s`.)

This is what is meant by "merging" in what follows. The different
algos are all about different ways of figuring out what the vector
`s` should be.

All of these different merge algos suffer from a certain set of
problems. These are:

A) The number of vectors being tracked in the system is increasing:
   merge decisions include the decision to combine two words to form
   a new grammatical class. The `t` remnants of each word remain in
   the system, available for further classification into other
   word-senses.  At some point, the remainders `t` are likely to get
   small, and consist entirely of noise, and so require pruning.

B) There is a hysteresis effect: when `g_new` is obtained, it is in
   general no longer parallel to `g_old`, and future merge decisions
   will be based on `g_new` rather than on `g_old`. Furthermore, the
   earlier merge decisions are not recomputed, and so there is a
   gradual drift of each grammatical class `g` as words are assigned
   to it. The drift is history-dependent: it depends on the sequence
   by which words are added in.

C) The replacement of `w` by `w_new` means that the original word
   vectors are "lost", and not available for some other alternative
   processing. This could be avoided by caching the original
   word-vectors somewhere. However, doing this would increase the
   size of the dataset (which is already unmanageably large), and
   at this time, there does not seem to be any reason to access the
   original counts.


Zipf Tails
----------
The distribution of disjuncts on words is necessarily Zipfian. That
is, the vectors could be called "Zipf vectors", in that the vector
coefficients follow a Zipfian distribution.  There are many reasons
why this is so, and multiple effects feed into this.

It seems plausible to treat extremely-low frequency observations as
a kind of noise, but which might also contain some signal. Thus,
during merge, all of a Zipfian tail should be merged in. If its noise,
it will tend to cancel during merge; if its signal, it will tend to be
additive.

That is, during merge, low-frequency observation counts should be
merged in their entirety, rather than split in parts, with one part
remaining unmerged.  For example, if a word is to be merged into a
word-class, and disjunct d has been observed 4 times or less, then
all 4 of these observation counts should be merged into the word-class.
Only high-frequency disjuncts can be considered to be well-known
enough to be distinct, and thus suitable for fractional merging.


Broadening
----------
The issue described in a) is an issue of broadening the known usages
of a word, beyond what has been strictly observed in the text.  There
are two distinct opportunities to broaden: first, in the union vs.
overlap merging above, and second, in the merging of disjuncts. That
is, the above merging did not alter the number of disjuncts in use:
the disjuncts on the merged class are still disjuncts with single-word
connectors. At some point, disjuncts should also be merged, i.e. by
merging the connectors on them.

If disjunct merging is performed after a series of word mergers have
been done, then when a connector-word is replaced by a connector
word-class, that class may be larger than the number of connectors
originally witnessed. Again, the known usage of the word is broadened.


Disjunct merging
----------------
Disjunct merging is the second step in creating grammatical classes.
The idea here is to replace individual connectors that specify words
with connectors that specify word-classes. This step is examined in
greater detail in `gram-majority.scm`.


Orthogonal merging
------------------
In this merge strategy, `w` is decomposed into `s` and `t` by
orthogonal decomposition, up to a clamping constraint, so as to keep
all counts non-negative. That is, start by taking `s` as the component
of `w` that is parallel to `g`, and `t` as the orthogonal complement.
In general, this will result in `t` having negative components; this
is clearly not allowed in a probability space. Thus, those counts are
clamped to zero, and the excess is transferred back to `s` so that the
total `w = s + t` is preserved.

Note the following properties of this algo:
a) The combined vector `g_new` has exactly the same support as `g_old`.
   That is, any disjuncts in `w` that are not in `g_old` are already
   orthogonal. This may be undesirable, as it prevents the broadening
   of the support of `g`, i.e. the learning of new, but compatible
   grammatical usage. See discussion of "broadening" below.

b) The process is not quite linear, as the final `s` is not actually
   parallel to `g_old`.


Union merging
-------------
Here, one decomposes `w` into components that are parallel and
perpendicular to `g + w`, instead of `g` as above.  Otherwise, one
proceeds as above.

Note that the support of `g + w` is the union of the support of `g`
and of `w`, whence the name.  This appears to provide a simple
solution to the broadening problem, mentioned above.  Conversely, by
taking the union of support, the new support may contain elements
from `w` that belong to other word-senses, and do NOT belong to `g`
(do not belong to the word sense associate with `g`).

Initial cluster formation
-------------------------
The above described what to do to extend an existing grammatical class
with a new candidate word.  It does not describe how to form the
initial grammatical class, out of the merger of N words. Several
strategies are possible. Given words `u`, `v`, `w`, ... one may:

* Simple sum: let `g=u+v+w+...`. That's it; nothing more.
* Overlap and union merge, described below.
* Democratic voting: merge those basis elements shared by a majority.

Overlap merge
-------------
A formal (i.e. mathematically dense) description of overlap merging is
given here. One wishes to compute the intersection of basis elements
(the intersection of "disjuncts" aka "sections") of the two words, and
then sum the counts only on this intersected set. Let

  {e_a} = set of basis elements in v_a with non-zero coefficients
  {e_b} = set of basis elements in v_b with non-zero coefficients
  {e_overlap} = {e_a} set-intersection {e_b}
  pi_overlap = unit on diagonal for each e in {e_overlap}
             == projection matrix onto the subspace {e_overlap}
  v_a^pi = pi_overlap . v_a == projection of v_a onto {e_overlap}
  v_b^pi = pi_overlap . v_b == projection of v_b onto {e_overlap}

  v_cluster = v_a^pi + v_b^pi
  v_a^new = v_a - v_a^pi
  v_b^new = v_b - v_b^pi

The idea here is that the vector subspace {e_overlap} consists of
those grammatical usages that are common for both words a and b,
and thus hopefully correspond to how words a and b are used in a
common sense. Thus v_cluster is the common word-sense, while v_a^new
and v_b^new are everything else, everything left-over.  Note that
v_a^new and v_b^new are orthogonal to v_cluster. Note that v_a^new
and v_b^new are both exactly zero on {e_overlap} -- the subtraction
wipes out those coefficients. Note that the total number of counts
is preserved.  That is,

  ||v_a|| + ||v_b|| = ||v_cluster|| + ||v_a^new|| + ||v_b^new||

where ||v|| == ||v||_1 the l_1 norm aka count aka Manhattan-distance.

If v_a and v_b have several word-senses in common, then so will
v_cluster.  Since there is no a priori way to force v_a and v_b to
encode only one common word sense, there needs to be some distinct
mechanism to split v_cluster into multiple word senses, if that is
needed.

Union merging can be described using almost the same formulas, except
that one takes

  {e_union} = {e_a} set-union {e_b}

accumulate-count, assign-to-cluster
-----------------------------------
The above merge methods are implemented in the `accumulate-count`
and `assign-to-cluster` functions. The first does the math for
one basis element, the second loops over the basis elts in a vector.

The first takes, as an argument, a fractional weight which is
used when the disjunct isn't shared between both words. Setting
the weight to zero gives overlap merging; setting it to one gives
union merging. Setting it to fractional values provides a merge
that is intermediate between the two: an overlap, plus a bit more,
viz some of the union.  This is sometimes called "fuzzy merging"
in other places.

That is, the merger is given by the vector

  v_merged = v_overlap + FRAC * (v_union - v_overlap)

If v_a and v_b are both words, then the counts on v_a and v_b are
adjusted to remove the counts that were added into v_merged. If one
of the two is already a word-class, then the counts are simply moved
from the word to the class.

Majority Voting
---------------
Better merge results can be obtained by merging two or more vectors
at the same time.

Connector merging
-----------------
When merging  vectors, it is best to merge the connectors that appear
in the basis elements of the vectors (the basis elements are connector
seqeunces).  Performing this connector merge is easiest if "shapes" are
used, (see `shape-project.scm` for details) and if the inner loop is
the loop over the words to be merged, for a fixed basis element.
Earlier code reversed the inner and outer loops (see the code in the
`attic` directory) and doing it the other way creates a number of
difficult issues for connector merging.

---------------------------------------------------------------------
This file currently contains no code!  It just documents the code!
Copyright (c) 2017, 2018, 2019 Linas Vepstas
