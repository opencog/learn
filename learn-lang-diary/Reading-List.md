
Things to read:

* Geoffrey Hinton
  "How to represent part-whole hierarchies in a neural network"
  25 Feb 2021
  https://arxiv.org/abs/2102.12627

* Henry W. Lin, Max Tegmark, and David Rolnick
  "Why does deep and cheap learning work so well?"
  3 Aug 2017
  https://arxiv.org/pdf/1608.08225.pdf

* Harmen Prins
  "Matching ontologies with distributed word embeddings"
  July 7, 2016
  http://www.ru.nl/publish/pages/769526/z_harmen_prins.pdf

* Alberto Speranzon, David I. Spivak, Srivatsan Varadarajan
  "Abstraction, Composition and Contracts: A Sheaf Theoretic Approach"
  8 Feb 2018
  https://arxiv.org/abs/1802.03080

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
  can be reversed. This enableds back-tracking on a rule system.

* Scott Garrabrant, Tsvi Benson-Tilsen, Andrew Critch, Nate Soares,
  Jessica Taylor "Logical Induction" (2016)
  https://arxiv.org/abs/1609.03543

  "We present a computable algorithm that assigns probabilities to
  every logical statement in a given formal language, and refines
  those probabilities over time."


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
  how one talks about them in a category-teheroetic language.  Useful,
  in that it provides a general background in category theory to core
  concepts regarding monoids.

* Scott Garrabrant, Tsvi Benson-Tilsen, Andrew Critch, Nate Soares, and Jessica Taylor
  "Logical Induction (Abridged)"
  2016
  https://intelligence.org/files/LogicalInductionAbridged.pdf
  Interesting, but some of the claims appear to be false.  Page 12 states
  (seems to state, when I read it) that theorems can be efficiently
  enumerated will be assigned a high price in relatively short order.
  But surely this cannot be the case, or I misunderstand? If I can
  efficiently enumerate a sequence of sentences `S`, then for any given
  sentence `s` I can propose that `s is true` and `s is false` at the same
  time. But both of these cannot be simultaneously assigned a high price.
  Do I misunderstand something?

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

