
Things to read:

AMR Parsing with Action-Pointer Transformer - 24 Nov 2020
https://openreview.net/forum?id=X9KK-SCmKWn


Geoffrey Hinton
How to represent part-whole hierarchies in a neural network
25 Feb 2021
https://arxiv.org/abs/2102.12627

Henry W. Lin, Max Tegmark, and David Rolnick
Why does deep and cheap learning work so well?
3 Aug 2017
https://arxiv.org/pdf/1608.08225.pdf

John Baez
Toplogical Crystals
2016
This is interesting because it discusses the treatment of covering
spaces with vectors. This is similar to what is being done in this
project.  However, in this project, its not clear that deck
transformations are meaningful or carry something important, although
perhaps the exchange of synonymous phrases can be treated as a deck
transformation.

Harmen Prins
Matching ontologies with distributed word embeddings
July 7, 2016
http://www.ru.nl/publish/pages/769526/z_harmen_prins.pdf


Alberto Speranzon, David I. Spivak, Srivatsan Varadarajan
Abstraction, Composition and Contracts: A Sheaf Theoretic Approach
8 Feb 2018
https://arxiv.org/abs/1802.03080


Scott Garrabrant, Tsvi Benson-Tilsen, Andrew Critch, Nate Soares, and Jessica Taylor
Logical Induction (Abridged)
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
