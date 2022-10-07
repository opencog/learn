
Grammatical Similarity, and how it's computed
=============================================
The code in this directory can be used to form "grammatical classes", based
on "grammatical similarity". This file explains what these are, and how the
general process of clustering works.

Although the below keeps talking about words and word-classes, the
actual code is (almost) entirely generic, and can merge (cluster)
anything. There are only a handful of places where its not generic;
these are slowly being cleaned up. The generic merge is possible
because what to merge, and where to put the merger results, are
defined by the `LLOBJ` object, passed as an argument to the assorted
functions and methods.

OVERVIEW
--------
When a pair of words are judged to be grammatically similar, they can
be combined to create a "grammatical class", containing both the words,
and behaving as their "average".  Similarly, a word can be compared to
an existing grammatical class, to see if it belongs to that class.

Words are represented as vectors. There are at least three completely
different kinds of word-vector representations being used:

* Vector components are given by pair-MI to other words.  The pair-MI
  was obtained by pair counting. This is mostly NOT used in this
  directory; this was obtained in erlier processing stages. Word pairs
  with high pair-MI are words that commonly occur together.

* Vector components are given by frequency of word-disjunct pairs. The
  word-disjunct pair counts were obtained by MST parsing, in an earlier
  stage.

* Vector components are given by grammatical-MI, summing over disjuncts.
  The grammatical-MI between a pair of words indicates how grammatically
  similar they are.  The grammatical-MI is distributed roughly as a
  Gaussian; thus, these vectors belong to a Gaussian Orthogonal Ensemble
  (GOE). The dot-product of two GOE vectors is an even stronger indicator
  of grammatical similarity.

Although words are represented as vectors, the merge operations are
highly non-linear. This is because disjuncts are composed of
"half-words" (connectors), and when words are merged into a class, the
connectors must be merged as well ("detailed balance"), resulting in
a very non-linear transformation on the vectors.

The code in this directory implements several different comparison and
merge strategies:

For comparison:
* Cosine similarity
* Grammatical-MI similarity
* Ranked-MI similarity
* GOE similarity

The first three work with word-disjunct vectors; the GOE similarity
works with word-word vectors, where the vector components are given by
the grammatical-MI similarity.

GOE similarity works best for merging; it measures not only the local
syntactic environment for two words, but also how similar they are in
relation to other (3rd party) words. It resembles a layer "one deeper",
than the original vectors from which it is built. The downside is that
GOE similarity is expensive to compute.

Ranked-MI works second-best for merging; it has the best properties of
MI but also includes the word frequency as part of the similarity score.
Otherwise, MI works great, as long as one merges high-frequency word
pairs first.  Cosine similarity works poorly, based on  experimental
results. This is no surprise: cosine distance is a Casimir invariant
for Euclidean space, but probability space is not Euclidean! Its a simplex!

For merging:
* Orthogonal decomposition into parallel & perpendicular components.
* Union and overlap of basis elements.
* In-group cliques with majority voting.
* Binary optimization (integer programming).

Orthogonal decomposition, and union, overlap merging were explored
experimentally in detail; see the 'Diary Part One' and 'Part Two'.
They worked OK, but not great, and the code that implements them has
been moved to the `attic` directory (its obsolete and not used any
more.)  They are still explained below, as understanding these helps
with the understanding of general principles.

The current merge algo forms in-groups or 'private clubs' of closely
related words, and uses a majority voting scheme to determine which
disjuncts are admitted into the club. The code for this can be found in
`gram-majority.scm`. This algo seems to work well, and is being explored
experimentally.

There is a vague idea that some kind of binary optimization merge
algorithm might be "even better", especially if it is well-founded on
principles of information theory. At this time, it remains a bit of a
daydream, and maybe a mirage.  See `gram-optim.scm` for more.


Broadening
----------
Prior to the start of the merge, the collection of word-disjunct pairs
corresponds precisely to what was observed in the training corpus. When
word-classes are formed, they inevitably broaden the allowed use of words
beyond what was observed in the text.

That is, when word `a` and word `b` are merged to form class `c`, there
will be, in general, disjuncts `d` and `e` such that the pair `(a,d)` was
observed, and `(b,e)` was observed, but not `(a,e)` and not `(b,d)`. If
the merge result contains both `(c,d)` and `(c,e)`, then effectively the
grammatical usage of `a` and `b` has been broadened into a bigger context.

The entire goal of merging is to perform this broadening, but not too
much of it. One wishes to extrapolate from the particular to the general,
but not so much that one loses the ability to discriminate between
important particulars. Exactly how this can be done best is the grand
mystery, the grand question, quest of the code here.

As noted above, something called 'union and overlap' merging was already
explored, and found wanting. An 'in-group, private club' algo seems to
work quite well, and is the primary merge algo implemented here, in this
directory.  The generic information-theoretic foundations of merging
remain opaque and unknown (but I'm trying to figure them out.)


Representation
--------------
A grammatical class is represented as

    MemberLink
        WordNode "wordy"      ; The word itself.
        WordClassNode "noun"  ; The grammatical class of the word.

Word classes have a designated grammatical behavior, using Sections,
behaving just like the pseudo-connectors on single words. Thus, either
a `WordNode` or a `WordClassNode` can appear in a `Connector` link, as
shown below.

    Section
        WordClassNode "noun"
        ConnectorSeq
            Connector
               WordClassNode "verb" ; or possibly a WordNode
               ConnectorDir "+"
            Connector
               ....

The TV on the `MemberLink` holds a count value; that count equals the
total number of section-counts that were transferred from the word, to
the word-class, when the word was merged into the class. The sum over
all of these counts (on the `MemberLink`s) should exactly equal the sum
over the counts on all `Section`s for that `WordClassNode`.  Thus, it can
be used to determine what fraction the word contributed to the class.
Since merge strategies are generally non-linear, this value is at best
a crude indicator, rather than a way of reconstructing the components.


Terminology
-----------
The `LLOBJ` or matrix of word-disjunct pairs appears in the AtomSpace in
the form of

    Section
        WordNode "foo"
        ConnectorSeq
            Connector
               WordNode "bar"
               ConnectorDir "+"
            Connector
               ....

which can be shortened to `(foo, bar+ & ...)` or even further to a pair
$(w,d)$ for word $w$ and disjunct $d$.  Here, the disjunct $d$ is taken as
a synonym for `ConnectorSeq`. Further below, the concept of `Shape` is
introduced; Shapes are built from `ConnectorSeq` by substituting a
variable for a `Connector`. That is, disjuncts may also be Shapes, but
this detail can be safely ignored for the remainder of the text below.

Associated to each Section is a count, stored as a `CountTruthValue`
on the `Section`. Abstractly, this is written as $N(w,d)$.  This
abstraction is thus the matrix: the count matrix $N$ having rows
and columns $(w,d)$. Each row and each column is a vector.

Holding the word constant, the row vector $N(w,\*)$ is written simply
as $w$ below. In this case, the $d$ are the basis elements of that
vector.


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


Goals
-----
The goal of obtaining grammatical classes is to enable grammatical
parsing of sentences. It is hoped/beleived that such parsing will be
more accurate than MST parsing.  Thus, after determining grammatical
classes, a second round of parsing becomes possible. This second round
can then provide the foundation for discerning entities, and the
properties of entities (e.g. that dogs have tails, legs, eyes; that IBM
is a specific, named corporation) In this second round, there will be
correlations across sentences. In this second round, there will again be
similarities, but this time, the similarities will not be grammatical,
but similarities in properties (e.g. that dogs, cats and squirrels have
tails, legs, eyes.).


A Note about "Vectors"
----------------------
As noted earlier, there are multiple kinds of vectors appearing in this
code base.  The two primary ones in this directory is the word-disjunct
vector (where the vector basis consists of disjuncts, and the vector
coordinates are the frequency counts of word-disjunct pairs) and the GOE
vector (where the basis consists of other words, and the coordinates are
the grammatical-MI to those words.)

In the word-disjunct case, the term "vector" is misleading, and does not
convey the correct idea. The word "vector" suggests a vector space with
rotational symmetry and basis independence.  This is very much NOT the
case: the word-disjunct space has NO rotational symmetry at all, and is
strongly basis-dependent.  The concept of a "matroid" moves in the right
direction, but still does not quite capture the idea. The word-disjunct
"vectors" live in a probability space: that is, the coordinates are
probabilities; the sum of the probabilities must equal one. The
word-disjunct "vectors" are actually just points on the face of a
very high-dimensional simplex.

A different problem is that the basis elements of the word-disjunct
vectors are not actually "independent" of one-another. The basis
elements are disjuncts, which are composed of "half-words" (connectors).

Singling out any one connector, one can ask what other disjuncts have
that connector on it, and then ask what words have those disjuncts. This
give a different "vector": it is the "shape" of the connector, and
these vectors are called "Shapes". Again, these shapes are probability
vectors. Since they are constructed from the word-disjunct "vectors",
the are not independent of them. The construction is very highly
non-linear *in the space of disjuncts*, as it tangles in *all* other
disjuncts having that connector, and all other words having that
disjunct. Yet, this construction must necessarily preserve the
grand-total counts; it is linear in a certain "covering space" of
fragmentary connectors. This idea is called "detailed balance", in
analogy to the concept of detailed balance in chemistry and
thermodynamics. The total number of connectors, the total number of
words and the total number of disjuncts are all preserved.

It is tempting to think of "shapes" as being kind-of-like "moieties" in
chemistry, but the analogy is misleading and fails at multiple levels.
The only commonality is that detailed balance remains: no matter how one
splits and rearranges a collection of molecules, the total number of
atoms remains unchanged. So also here: the observation counts on
connectors are preserved, no matter how they are re-arranged for form
vector spaces.


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

Thus, prior to classification/categorization, the vector associated to
the word "saw" is the (linear) sum for a noun-vector (the cutting tool)
and two different verb-vectors (observing; cutting).  The next few
paragraphs describe how the cosine-distance (or mutual information,
or another kind of distance metric) can be used to distinguish between
these different forms, how to factor the vector of observation counts
into distinct classes.


Grammatical-MI Similarity
-----------------------
There are several different means of comparing similarity between
two words.  A traditional one is cosine distance: if the cosine of two
word-vectors is greater than a threshold, they should be merged.

The cosine distance between the two words $w_a$, $w_b$ is

$$  \cos(w_a, w_b) = \frac{v_a \cdot v_b}{|v_a||v_b|} $$

Where, as usual, $v_a \cdot v_b$ is the dot product, and $|v|$ is the length.

If $N(w,d)$ is the count of the number of observations of word $w$ with
disjunct $d$, the dot product is

$$    \mbox{dot}(w_a, w_b) = v_a \cdot v_b = \sum_d N(w_a,d) N(w_b,d) $$

A fundamental problem with cosine distance is that it is built on an
assumption of the rotational invariance of Euclidean space. However,
the "vectors" here are not actually vectors, they are points in a
probability space that has no rotational symmetry. Acknowledging this
leads to the contemplation of probabilistic distance functions.

A better judge of similarity is the information-theoretic divergence
between the vectors (the Kullback-Lielber divergence). If $N(w,d)$ is
the count of the number of observations of word $w$ with disjunct $d$,
the divergence is:

$$   MI(w_a, w_b) = \log_2 \frac {\mbox{dot}(w_a, w_b)\ \mbox{dot}(\*,\*)}{\mbox{ent}(w_a) \ \mbox{ent}(w_b)} $$


where

$$   \mbox{ent}(w) = \sum_d N(w,d) N(\*,d) = \operatorname{dot}(w, \*) $$

so that $\log_2 \operatorname{ent}(w)$ is the entropy of word $w$
(Up to a factor of $N(\*,\*)$ squared. That is, we should be using
$p(w,d) = N(w,d) / N(\*,\*)$ in the definition. This and other
considerations are covered in much greater detail in the supporting
PDF's and Diaries.)

An experimental examination of cosine vs. various different Jaccard
and overlap distances vs. MI can be found in the 'Diary Part Three'.

Note that this MI is symmetric: $MI(w_a, w_b) = MI(w_b, w_a)$. Here
and elsewhere, it is sometimes called the "symmetric-MI", although
"grammatical-MI" is a better name for it.  These terms are meant to
distinguish it from the word-pair-correlation MI (which is not
symmetric).


GOE Similarity
--------------
The grammatical-MI is roughly distributed as a Gaussian (both for
English and Chinese, see Diary Part Three and Part Five). Thus, the
values $MI(w_a, w_b)$ can be taken as the coordinates of a vector
$\vec w_a$. For a perfect Gaussian with mean zero, these vectors
would be uniformly randomly scattered around the origin of
$N$-dimensional space, where $N$ is the size of the vocabulary (the
dimension of the vector space).  That this is so, experimentally,
is explored and confirmed in Diary Part Eight. When normalized to unit
length, these vectors are uniformly distributed on the unit sphere
$S_{N-1}$.

This allows the definition and use of GOE vectors.  Let

$$ \mu = \langle MI \rangle = \frac{MI(\*,\*)} {N^2} = \frac{1}{N^2} \sum_{w_a,w_b} MI(w_a, w_b) $$

and

$$ \langle MI^2 \rangle = \frac{1}{N^2} \sum_{w_a,w_b} MI^2(w_a, w_b) $$

so that

$$ \sigma = \sqrt{\langle MI^2 \rangle - \langle MI \rangle^2} $$

is the standard deviation of the distribution of grammatical-MI. Define
the normal Gaussian distribution as

$$ G(w_a, w_b) = \frac {MI(w_a, w_b) - \mu} {\sigma} $$

This allows the unit word-vector $\widehat{w}_a$ to be defined: this is the
vector whose coordinates are $G(w_a, w_b)$.  By construction, it is of
unit length: $|\widehat{w}_a|=1$ and thus lies on the (high-dimensional)
unit sphere.

The GOE Similarity between two words $u$ and $w$ is then just the dot
product:

$$ \mbox{GOE-sim}(u, w) = \cos\theta(u, w) = \widehat{u} \cdot \widehat{w} = \sum_{z} G(u, z) G(w, z)$$

Experimentally, it appears that words with $\theta(w, u) \lesssim 0.5$
or $\cos\theta(w, u) \gtrsim 0.88$ are (very) grammmatically similar,
and otherwise not.


Merge Algos
-----------
There are several ways in which two words might be merged into a
word-class, or a word added to a word-class.  All of these involve
the transfer of counts from individual word-vectors to the word-class
vector.  One generic style of merging uses concepts from Euclidean
geometry, and involves taking parallel and perpendicular components
of vectors. This is implemented in `gram-projective.scm` (in the `attic`
directory. It has been moved to the attic because it is not used any
more.) Another style suggests that information-theoretic techniques are
primal, and a merge strategy based on MI/entropy maximization is best.
A general idea for this is sketched in the `gram-optim.scm` file, but
remains vague, unformed, unimplemented: it is just a sketch.

The merge code that is in current use can be found in `gram-majority.scm`.
It forms "in-groups" (or "clubs" or "cliques": exclusive groups whose
members share common traits.) The in-groups are formed by nominating a
set of similar words (per the similarity metric) and then determining
the membership of specific disjuncts by majority voting (so that the
majority of the club members have that disjunct in common.) This algorithm
is not based on information theory principles, but is relatively easy to
implement, and seems to work well.


Detailed Balance
----------------
Given a word (a word-disjunct vector) $w$, we wish to decompose it into
a component $s$ that will be merged into the grammatical class $g$, and
a component $t$ that will be left over. The preservation of counts
(the preservation of probabilities) requires that (before merging)
the vector $w$ decomposes as $w = s + t$, so that

$$   N(w,d) = N(s,d) + N(t,d) $$

for each 'basis element' (disjunct) $d$.

Merging is performed so that 'detailed balance' is preserved. That means
that, in forming the new class $g_{new}$ from $g_{old}$ and $s$, one has
that $g_{new} = g_{old} + s$, and this holds on component-by-component

$$   N(g_{new}, d) = N(g_{old}, d) + N(s,d) $$

for each disjunct $d$.

Note that this second equation is not so much an equation, as it is a
directive to transfer the counts from $s$ and $g_{old}$ to $g_{new}$. Thus,
before the transfer, $N(g_{new}, d)$ was zero, and becomes non-zero during
the transfer. Similarly, both $N(g_{old}, d)$ and $N(s,d)$ are set to zero.

As a practical matter, to avoid exploding the size of the dataset, all
matrix entries for which $N(x,d)$ becomes zero after the count transfer
are then deleted.

The different merge algos differ in how they decompose the vector $w$
into the part $s$ to be transferred, and the part $t$ to be kept. However,
they all obey the detailed-balance requirement.


Detailed Balance, Part Two
--------------------------
The above describes detailed balance for merging words, while holding
the set of disjuncts constant. In fact, the merge algos also create new
disjuncts, and destroy old ones. Detailed balance must be preserved here,
as well.

A given disjunct has the general form:

        ConnectorSeq
            Connector
               WordNode "a"
               ConnectorDir "+"
            Connector
               ....

which can be written in short-hand as `(a+ & ...)`.  Suppose one wishes
to merge the words `a` and `b` into a wordclass `g`.  If these appear
in a disjunct (ConnectorSeq), they must be merged as well. For example,
`(a+ & f+ & h-)` together with `(b+ & f+ & h-)` are merged to create
`(g+ & f+ & h-)`.

Counts on these three must remain balanced, as well. Thus, if disjunct
$d_a$ and $d_b$ are merged to form $d_g$, then, for any fixed word $w$,
one must have

$$   N(w, d_g) = N(w, d_a) + N(w, d_b) $$


Merge Issues
------------
The generic process of merging raises some issues that must be dealt
with (or ignored, or explained away) These are:

#### Dust
As portions of word vectors are merged into classes, other portions
remain unassigned, ready for later merge decisions. These remaining
portions may get small, and risk becoming 'dust' that need to be swept
up. Some of the merge algos deal with this by automatically merging
all disjuncts with counts below a given threshold, thus avoiding the
creation of dust.

#### Hysteresis
There is a hysteresis effect: when $g_{new}$ is obtained, it is in
general no longer parallel to $g_{old}$, and future merge decisions
should be based on $g_{new}$ rather than on $g_{old}$. Furthermore, the
earlier merge decisions are not recomputed, and so there is a
gradual drift of each grammatical class $g$ as words are assigned
to it. The drift is history-dependent: it depends on the sequence
by which words are added in.

When merge decisions are based on the grammatical-MI, then it is
feasible to recompute, after each merge, the MI between all of the words
that were affected by the merge. This is not practical for the
GOE-similarity, and so batching is required: the top GOE-similar words
can be merged; but then there must be a full-stop, and the GOE
similarities must be recomputed across the board. It's currently unclear
if this can be somehow optimized.

#### Loss of History
The replacement of $w$ by $w_{new}$ means that the original word
vectors are "lost", and not available for some other alternative
processing. This can be avoided by using AtomSpace "frames". That is,
at each merge step, a new AtomSpace layer is created; it holds all of
the deltas (differrences) between this and the AtomSpace underneath.
This allows for pre-merge counts and statistics to be accessed. It seems
that this is (will be) useful for "lifelong learning", to get access to
earlier data as new vocabulary is encountered. "Will be", as this is not
implemented yet, and the need for this remains to be experimentally
determined.


Zipf Tails
----------
The distribution of disjuncts on words is necessarily Zipfian. That
is, the word-disjunct vectors could be called "Zipf vectors", in that
the vector coefficients follow a Zipfian distribution.  There are many
reasons why this is so, and multiple effects feed into this.

It seems plausible to treat extremely-low frequency observations as
a kind of noise, but which might also contain some signal. Thus,
during merge, all of a Zipfian tail should be merged in. If its noise,
it will tend to cancel during merge; if its signal, it will tend to be
additive.

That is, during merge, low-frequency observation counts should be
merged in their entirety, rather than split in parts, with one part
remaining unmerged.  For example, if a word is to be merged into a
word-class, and disjunct $d$ has been observed 4 times or less, then
all 4 of these observation counts should be merged into the word-class.
Only high-frequency disjuncts can be considered to be well-known
enough to be distinct, and thus suitable for fractional merging.

More generally, the correct way to treat these tails remains rather
mysterious. Despite the fact that each individual count is small, almost
all of the grand-total count is in the tail: the tail itself is bigger
than the body itself. Although it feels like "noise", it appears to
also contain a lot of "signal", and disentangling this signal from the
noise remains profoundly confusing.


Simple Merge Strategies
=======================
A few of the simplest merge strategies are described below. They give
a general sense of what is being done, and provide a language and
terminology for the more complex merge algos.


Orthogonal merging
------------------
In this merge strategy, a vector $w$ is decomposed into $s$ and $t$ by
orthogonal decomposition. A clamping constraint is then applied, so as
to keep all counts non-negative.

Start by taking $s$ as the component of $w$ that is parallel to $g$,
and $t$ as the orthogonal complement.  In general, this will result in
$t$ having negative components; this is clearly not allowed in a
probability space. Thus, those counts are clamped to zero, and the
excess is transferred back to $s$ so that the total $w = s + t$ is
preserved (so that detailed balance holds).

Note the following properties of this algo:

1) The combined vector $g_{new}$ has exactly the same support as
   $g_{old}$.  That is, any disjuncts in $w$ that are not in $g_{old}$
   are already orthogonal. This may be undesirable, as it prevents the
   broadening of the support of $g$, i.e. the learning of new, but
   compatible grammatical usage. See discussion of "broadening" at above.

2) The process is not quite linear, as the final $s$ is not actually
   parallel to $g_{old}$.


Union merging
-------------
Here, one decomposes $w$ into components that are parallel and
perpendicular to $g + w$, instead of $g$ as above.  Otherwise, one
proceeds as above.

Note that the support of $g + w$ is the union of the support of $g$
and of $w$, whence the name.  This appears to provide a simple
solution to the broadening problem, mentioned above.  Conversely, by
taking the union of support, the new support may contain elements
from $w$ that belong to other word-senses, and do NOT belong to $g$
(do not belong to the word sense associate with $g$).


Initial cluster formation
-------------------------
The above describe what to do to extend an existing grammatical class
with a new candidate word.  It does not describe how to form the
initial grammatical class, out of the merger of N words. Several
strategies are possible. Given words $u$, $v$, $w$, ... one may:

* Simple sum: let $g=u+v+w+\ldots$ . That's it; nothing more.
* Overlap and union merge, described below.
* Democratic voting: merge those basis elements shared by a majority.

Democratic voting appears to work the best, so much so that the other
methods have been removed from the code-base (and moved to the `attic`
directory, when possible.) They are still explained, so as to orient the
concepts.


Overlap merge
-------------
A formal (i.e. mathematically dense) description of overlap merging is
given here. One wishes to compute the intersection of basis elements
(the intersection of "disjuncts" via "sections") of the two words, and
then sum the counts only on this intersected set. Let

* $\\{e_a\\}$ = set of basis elements in $v_a$ with non-zero coefficients
* $\\{e_b\\}$ = set of basis elements in $v_b$ with non-zero coefficients
* $\\{e_{overlap}\\} = \\{e_a\\} \cap \\{e_b\\}$ where $\cap$ is
  set-intersection.
* $\pi_{overlap}$ = unit on diagonal for each $e \in \\{e_{overlap}\\}$
  This is the projection matrix onto the subspace $\\{e_{overlap}\\}$
* $v_a^\pi = \pi_{overlap} \cdot v_a$ is the projection of $v_a$ onto
  $\\{e_{overlap}\\}$.
* $v_b^\pi = \pi_{overlap} \cdot v_b$ is the projection of $v_b$ onto
  $\\{e_{overlap}\\}$.

* $v_{cluster} = v_a^\pi + v_b^\pi$
* $v_a^{new} = v_a - v_a^\pi$
* $v_b^{new} = v_b - v_b^\pi$

The idea here is that the vector subspace $\\{e_{overlap}\\}$ consists of
those grammatical usages that are common for both words $a$ and $b$,
and thus hopefully correspond to how words $a$ and $b$ are used in a
common sense. Thus $v_{cluster}$ is the common word-sense, while
$v_a^{new}$ and $v_b^{new}$ are everything else, everything left-over.
Note that $v_a^{new}$ and $v_b^{new}$ are orthogonal to $v_{cluster}$.
Note that $v_a^{new}$ and $v_b^{new}$ are both exactly zero on
$\\{e_{overlap}\\}$ -- the subtraction wipes out those coefficients. Note
that the total number of counts is preserved.  That is,

$$    ||v_a|| + ||v_b|| = ||v_{cluster}|| + ||v_a^{new}|| + ||v_b^{new}|| $$

where $||v|| = ||v||_1$ the $l_1$ Banach norm aka count aka
Manhattan-distance.

If $v_a$ and $v_b$ have several word-senses in common, then so will
$v_{cluster}$.  Since there is no a priori way to force $v_a$ and $v_b$ to
encode only one common word sense, there needs to be some distinct
mechanism to split $v_{cluster}$ into multiple word senses, if that is
needed.

Union merging can be described using almost the same formulas, except
that one takes

$$  \\{e_{union}\\} = \\{e_a\\} \cup \\{e_b\\} $$

where $\cup$ is set-union.


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

$$   v_{merged} = v_{overlap} + FRAC * (v_{union} - v_{overlap}) $$

If $v_a$ and $v_b$ are both words, then the counts on $v_a$ and $v_b$ are
adjusted to remove the counts that were added into $v_{merged}$. If one
of the two is already a word-class, then the counts are simply moved
from the word to the class.


Majority Voting
---------------
Better merge results can be obtained by merging two or more vectors
at the same time. See `gram-majority.scm` for details.


Connector merging
-----------------
Words appear not only as vectors, but also in the connectors that form
a disjunct. When words are merged into a class, care must be taken to
handle the connectors as well.

A given disjunct has the general form:

        ConnectorSeq
            Connector
               WordNode "a"
               ConnectorDir "+"
            Connector
               ....

If a word `a` is merged into a class `c`, the above connector sequence
(disjunct) must be replaced by

        ConnectorSeq
            Connector
               WordClassNode "c"
               ConnectorDir "+"
            Connector
               ....

wherever it appears.  This makes the merging algorithm 'non-linear', in
that the number of disjuncts is not constant: the number of basis elements
are changing, the dimension of the space is changing. The basis elements
themselves mutate. The overall process does remain linear in that detailed
balance must still be maintained; failure to maintain detailed balance
would prevent the ability to interpret the entire process in terms of
probabilities.

The term 'detailed balance' is meant to invoke the same idea from
chemistry. During a chemical reaction, the total number of atoms remains
the same, even as the total number of molecules is changing.

There are a number of analogies that can be made to chemistry: a
`ConnectorSeq` is a list of the chemical bonds bonds that can be
formed.  Thus, a pair $(w,d)$ is a listing of an atom $w$ and the
bonds $d$ it can form. Because any given atom can form bonds in
several different ways, there are several different $d$ that can
be associated with $w$. The act of classification is like saying
that chlorine, fluorine and bromine are all very similar in the
bonds that they form. The act of connector merging is like saying
that the halides all bond similarly to the alkalis.

As a practical matter for tracking the merge of connectors, the
concept of a `Shape` is introduced, together with the concept of
a `CrossSection`. This is described in detail in `shape-project.scm`.

A quick example. Given a `Section`

    Section
        WordNode "foo"
        ConnectorSeq
            Connector
               WordNode "bar"
               ConnectorDir "+"
            Connector
               WordNode "nim"
               ConnectorDir "-"

one can define two `CrossSection`s and two `Shape`s, one for each of the
`Connector`s. One of them is

    CrossSection
        WordNode "bar"
        Shape
            WordNode "foo"
            Connector
               VariableNode "$x"
               ConnectorDir "+"
            Connector
               WordNode "nim"
               ConnectorDir "-"

The other one is analogous, replacing 'nim' by the variable. The original
`Section` can be uniquely and unambiguously reconstructed by plugging the
word 'bar' in for the variable, and moving the head of the shape to the
head of the `Section`.  Thus, the `CrossSection`s are just `Section`s
rotated around, to place each of the words in the `Connector`s into the
head position. This rotation of the `Connector`s into head position
makes it easier to apply the assorted vector-space concepts described
above, to maintain detailed balance, to compute word-similarity and
in general, to track what needs to be merged with what.

Note that a word vector with `CrossSection`s in it really is a different
vector than one without it; thus, including `CrossSection`s really does
alter the similarity between word-vectors. The similarities between
word-vectors with `CrossSection`s on them does appear to be more
accurate than those without, but this has not been firmly established
experimentally, nor is there any theoretical foundation for supporting
this.

Given a set of `Section`s, the set of `CrossSection`s and `Shape`s
can be uniquely and unambiguously determined. The counts on the
`CrossSection`s are exactly equal to the counts on the corresponding
`Section`s.

Performing this connector merge is easiest if "shapes" are used,
(see `shape-project.scm` for details) and if the inner loop is
the loop over the words to be merged, for a fixed basis element.
Earlier code reversed the inner and outer loops (see the code in the
`attic` directory) and doing it the other way creates a number of
difficult issues for connector merging.


Code Overview
=============
A quick sketch of which files implement what.

* `agglo-mi-rank.scm` -- Contains the main entry point `in-group-mi-cluster`.
* xxx -- same but for GOE
* `mi-similarity.scm` -- Compute grammatical MI similarities between words.
* `similarity.scm` -- Generic similarity tools.

-------------------------------------------------------------------

Copyright (c) 2017, 2018, 2019, 2022 Linas Vepstas
