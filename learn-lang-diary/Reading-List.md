
Things to read:

* Elena Di Lavore, Giovanni de Felice, Mario Román,
  Monoidal Streams for Dataflow Programming
  https://arxiv.org/pdf/2202.02061.pdf
  Appendix contains strong description of monoidal categories.

* Noam Zeilberger:
  "Parsing as a lifting problem and the Chomsky-Schützenberger representation theorem"
  https://www.youtube.com/watch?v=AX8tpQSi8v8

  Colloquim.

* Michel TALAGRAND, Mean Field Models for Spin Glasses Volume I:
  Basic Examples (2010) Book, 490 pages, Springer-Verlag

  Provides overview of high-dimensional statstics from a mathematicians
  viewpoint. Sherrington-Kirkpatrick model, Perceptron, Hopfield,
  and more.

* Gao et al,
  The Pile: An 800GB Dataset of Diverse Text for Language Modeling
  https://arxiv.org/abs/2101.00027

* Leo Gao,
  An Empirical Exploration in Quality Filtering of Text Data
  https://arxiv.org/abs/2109.00698
  We find that aggressive filtering can in fact lead to a decrease
  in model quality on a wide array of downstream tasks for a
  GPT-like language model.

* Paul Cisek - papers on neural evolution.

* Tobias Fritz
  A synthetic approach to Markov kernels, conditional independence
  and theorems on sufficient statistics.
  https://arxiv.org/abs/1908.07021v8

* Tailin Wu, Max Tegmark
  Toward an AI Physicist for Unsupervised Learning
  https://arxiv.org/abs/1810.10525
  Unsupervised learning of theories and their domain of validity.

* Multimodal Neurons in Artificial Neural Networks
  https://openai.com/blog/multimodal-neurons/
  This is an important foot-in-the-door for symbolic reasoning.

* Geoffrey Hinton
  "How to represent part-whole hierarchies in a neural network"
  25 Feb 2021
  https://arxiv.org/abs/2102.12627

* Robert Haas
  Symbolic regression with Atomese code in OpenCog
  https://robert-haas.github.io/g3p/media/notebooks/atomese_symbolic_regression.html
  Python notebook, doing moses-like symbolic regression.

* Henry W. Lin, Max Tegmark, and David Rolnick
  "Why does deep and cheap learning work so well?"
  3 Aug 2017
  https://arxiv.org/pdf/1608.08225.pdf

  Short answer: they can represent multiplication and addition easily.
  Making them deep makes the representation more compact and simpler to
  learn.

* Harmen Prins
  "Matching ontologies with distributed word embeddings"
  July 7, 2016
  http://www.ru.nl/publish/pages/769526/z_harmen_prins.pdf

* David I. Spivak, Nelson Niu
  "Polynomial Functors: A General Theory of Interaction"
  2021
  https://topos.site/poly-book.pdf

  Knee-jerk reaction after a 30-second skim: yes, exactly! Polynomials in
  N variables lead naturally to the idea of jets and sheaves. So the sheaf
  theory that is being developed in this project is what you would get if
  you took a polynomial, and threw away the addition and multiplication,
  and replaced them by concatenation and composition.

* Grant Sanderson
  "Puzzling through exact sequences"
  11 Nov 2021 3Blue1Brown blog
  https://www.3blue1brown.com/blog/exact-sequence-picturebook

  Jigsaw puzzle pieces! This time, they occur in algebraic topology!

* AMR "Parsing with Action-Pointer Transformer" - 24 Nov 2020
  https://openreview.net/forum?id=X9KK-SCmKWn

* Mark Newman, Albert-László Barabási, and Duncan J Watts.
  "The structure and dynamics of networks."
  Princeton University Press, 2006.

* John Baez
  "Toplogical Crystals"
  2016
  This is interesting because it discusses the treatment of covering
  spaces with vectors. This is similar to what is being done in this
  project.  However, in this project, its not clear that deck
  transformations are meaningful or carry something important, although
  perhaps the exchange of synonymous phrases can be treated as a deck
  transformation.

* Russ Harmer and Eugenia Oshurko "Reversibility and composition of
  rewriting in hierarchies", (2020)
  https://hal.archives-ouvertes.fr/hal-02869865

  The idea here is that sequences of rules can be applied, and then they
  can be reversed. This enables back-tracking on a rule system.

* D. A Turner, "Functional programs as executable specifications"
  (1984) Phil. Trans R. Soc. Lond. A 312, pp. 363-388.

  Functional programming languages as a collection of rewrite rules
  applied to program expressions. Background theory for the Clean
  programming langauge. See
  https://en.wikipedia.org/wiki/Clean_(programming_language)
  See also the Wikipedia description of the ABC machine.

  The relevance for the AtomSpace is this: Deep inside a query, we
  need to be able to call out to functions, passing arguments. For
  opaque, black-box functions, we need to eager-evaluate all arguments.
  There are two alternatives to eager evaluation. One is to do a $vau
  trick, and eager-evaluate only the first arg, and pass the rest
  unevaluated. The other is to evaluate none of them, and pass a context
  which contains all necessary symbol groundings are present, so that
  the callee can evaluate, as needed.

  I dunno. Feels like we're reinventing... Hmm.

* HodgeNet -- Justin Solomon -- learning of the Hodge star operator
  via neural nets.  Specifically, for dealing with sparse matrices.

Actually useful in practical applications
-----------------------------------------
* Jesse O. Wrenn, Peter D. Stetson, Stephen B. Johnson
  An Unsupervised Machine Learning Approach to Segmentation of Clinician-Entered Free Text
  AMIA Annu Symp Proc. 2007; 2007: 811–815.
  PMC2655800
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2655800/

  Lists the following prior work:
  * Zellig Harris (1967) -- Compute conditional probability of transitions
    from character to character. Minimize entropy to obtain morpeheme boundaries.

  * John Goldsmith (2001) -- Minimum Description Length

  * Mollah & Johnson (2003) -- Use Harris algo to get morphemes, then prune
    based on MI.

  Current work:
  * Compute the conditional probablity of observing a character, given a
    preceeding string.  Cap at length seven. Go forwards and backwards.
  * "Freedom at character transitions" == number of distinct characters that
    can follow a given string. There's a forward and backward version of this.
  * "Peak freedom" is a second difference; its the sum of the increase in
    freedom of the prior transition, and the decrease in freedom of the
    subsequent transtion.   There are both forward and backward peak freedoms.
  * Two paramters control tokenization: substring length (its a priori) and
    cutoff for peak freedom.
  * Characterization of results is conventional b.s. mumbo-jumbo tables
    showing sensitivity, specificity, and area under ROC curve. This is
    infuriating, because this kind of conventional analysis completely
    obscures what is actually happening! Argh!

  Suggested clarifications to above work:
  * A precise mathematical, symbolic definition of peak freedom is needed.
    The informal definition is imprecise, prone to misunderstanding, and
    obscures relationships to other similar mathematical formulas.
  * Peak freedom appears to be defined as a second difference, as the
    difference between the prior and the subsequent transition freedom.
    What if, instead, one worked with log2 of transition freedoms, so
    that a peak entropic freedom was a ratio instead of a difference?
  * Given this variant, what other variants of freedom are possible, and
    how are they related to more traditional entropic definitions?
  * Show distribution of peak freedom. That is, there are hundreds of
    thousands of transitions; what's the freedom of each? What's the peak
    freedom of each?
  * How do things change, if one considers not just the next character, but
    the next pair of characters, or the next triple of characters?

Suggestive but Important
------------------------
Don't quite provide what is immediately needed, but is very interesting
anyways:

* Pentti Kanerva, "Hyperdimensional Computing: An Introduction to Computing
  in Distributed Representation with High-Dimensional Random Vectors"
  Cognitvie Compututing (2009) 1:139–159 DOI 10.1007/s12559-009-9009-8

  Provides a good, general description of hypervector memory and
  hypervector ALU operations. Informal (few formulas) but mostly
  accurate (I spotted a few mistakes).

  Basic ideas: high-dimensional binary vectors (hypervectors), general
  stochastic properties of hyperspace, content-addressable memory, ALU
  ops as XOR and vector addition. Collision-avoidance via permutations.

  Examples: encoding sets as hypervectors, encoding pairs, encoding
  tables, encoding sequences. (and reconstructing these, given the
  hypervectors)

  Practical examples: hypervectors as alternative to LSA (Latent
  Semantic Anslysis) overcomes difficulties of PCA (Principle Component
  Analysis). Also, document-vectors look like context vectors, i.e.
  the two approaches become unified with hypervectors.

  Author recognizes the role of grammar, without saying the word
  "grammar" and suggests word contexts are impoverished. (and this is
  correct; so he punts.)

  Practical example: encoding of logical inference. Appears to be a
  variant of encoding a table. Points out that naive representations
  struggle with certain kinds of relationships (transitivity,
  membership.) Implies more complicated representations can over come this?
  (without showing how).

  Practical example: learning by example.  Not this is actually
  interesting.  Shows how basic relationships can be infered from
  a handful(?) of examples.  Given the difficulty of inferencing
  described above, this is a crude approximation to a more fully
  formal, symbolic relational description. However, I'm thinking
  that this is enough to get started, to provide the needed rough
  draft that other kinds of systems can take over and elaborate.
  Wow! That's actually pretty cool! I'm psyched!

  Concludes with a nice summary of historical developments of
  various vector-encoding schemes.


Interesting, but maybe less useful
----------------------------------
* Jerry R. Hobbs "Chapter 6: Word meaning and world knowledge"
  (2019) in book "Semantics - Theories", de Gruyter
  https://doi.org/10.1515/9783110589245-006

  A fairly general overview of the relationship between the meaning
  of words, and world models. Taken as a general overview of the
  general problem, the text seems adequate.

  I'm worried that it is suggesting perhaps exactly the wrong direction?
  More precisely, the goal of the unsupervised learning project to
  "learn common sense", and to "learn logical thinking", and so it
  is fundamentally wrong to start with a model of semantics that is
  trying to jam meaning into a pre-existing logical framework.  Notions
  such as scalars, magnitudes, causality, changes of state are to be
  *learned* and *not* programmed in. This includes ontological
  structure: is-a, has-a relationships: these are to be *learned*,
  and not hard-coded into the theory.

* Mati Kilp, Ulrich Knauer, Alexander V. Mikhalev.
  "Monoids, acts, and categories : with applications to wreath products
  and graphs : a handbook for students and researchers"
  (2000) W. de Gruyter,

  Provides a handbook on the basic defintions of state machines, and
  how one talks about them in a category-theoretic language.  Useful,
  in that it provides a general background in category theory to core
  concepts regarding monoids.

* Alberto Speranzon, David I. Spivak, Srivatsan Varadarajan
  "Abstraction, Composition and Contracts: A Sheaf Theoretic Approach"
  8 Feb 2018
  https://arxiv.org/abs/1802.03080

  The keyword in the title is "contracts". Although the paper uses
  sheaf theory as the abstact foundation for composition, the primary
  application of the abstract is to glue together computational models
  of complex systems-of-systems, with particular attention paid to
  discrete and continuous (dynamical) systems. This is all very nice, but
  does not appear to be relevant to the present concerns: that of
  discerning meaningful structure from probabilistic observations.

  The primary astraction presented in the paper is the sheaf of continuous
  intervals on the real number line. These are then used to glue together
  continuous-time dynamical systems and discrete-time decision systems;
  the primary example is an aircraft collision-avoidance system, where the
  continous parameters include height, speed and heading, and the discrete
  decisions to be made include banking left or right, or changing
  altitidue.

  There is no obvious way to convert that discussion into one that glues
  together probabilistic relationships to graphical representations. The
  keyword here is "obvious": clearly, information-theoretic quantities are
  continuous, and symbolic reprsentations are discrete. How can
  sheaf-theoretic framing provide insight? Not obvious. Would take
  significant effort.

  One of the more interesting claims (theorems) is that the sheaf of
  integer-length intervals is equivalent to the category of graphs.
  The sheaf axioms instruct how intervals can be glued to one-another in a
  coherent fashion.  The intervals, so glued, correspond to paths on a
  graph.  This is an interesting equivalence, and symbolic representations
  of meaning certainly seem to be kinds of graphs. However, nothing in the
  symbolic representations seem to correspond to paths on graphs, the
  linear nature of sentences notwithstanding.  The grammatical structure
  of a sentence is not overtly a walk of a graph; it is a flattening, a
  serialzation of a more complex structure.  It is not obvious that the
  equivalence of the sheaf of intervals and the caegory of graphs offers
  any articular insight into either grammatical sructure, or into the more
  general setting of Curry-Howard correspondance.

* Scott Garrabrant, Tsvi Benson-Tilsen, Andrew Critch, Nate Soares, and Jessica Taylor
  "Logical Induction" (2016)
  https://arxiv.org/abs/1609.03543
  "Logical Induction (Abridged)" (2016)
  https://intelligence.org/files/LogicalInductionAbridged.pdf

  "We present a computable algorithm that assigns probabilities to
  every logical statement in a given formal language, and refines
  those probabilities over time."

  Interesting, but some of the claims appear to be false.  Page 12 states
  (seems to state, when I read it) that theorems that can be efficiently
  enumerated will be assigned a high price in relatively short order.
  But surely this cannot be the case, or I misunderstand? If I can
  efficiently enumerate a sequence of sentences `S`, then for any given
  sentence `s` I can propose that `s is true` and `s is false` at the same
  time. But both of these cannot be simultaneously assigned a high price.
  Do I misunderstand something? (Probably.)

  The example of Ramanujan and Hardy clarifies the intent: We now
  understand that Ramanujan could do long division in his head, and thus
  numerically calculate sequences of numbers, and thereby find patterns.
  As a pattern recognizer, he could generate "true" theorems faster than
  Hardy could prove them.  That is, Ramanujan is not just enumerating
  theorems (as that would require enumerating both the theorem and its
  negation), but is instead spotting patterns.

  At any rate, betting on mathematical theorems seems like a dubious
  activity. When is it actually useful to do so?  For example, number
  theory is filled with black swan events: things that appear to follow
  a predictable pattern, until they do not. I can see why rational
  traders would assign these a high price until such time when a
  counter-example is found, but it seems a bit dubious to try to assign
  a price to such statements, unless it was a "bet your life" situation.

  Thus, the algorithm they propose is interesting not so much because it
  provides a betting pool for mathematical theorems, than it is a means of
  discerning structure in (formal) languages.  Kind of like what this
  project (`opencog/learn`) is trying to do.
