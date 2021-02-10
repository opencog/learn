
Unsupervised Language Learning
==============================
* Linas Vepstas December 2013
* Updated May 2017
* Updated June 2018
* Updated February 2021

Ongoing project, continuing activity.  See the
[language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview.

Table of Contents
------------------
1. [Project Summary](#project-summary)
2. [Processing Overview](#processing-overview)
3. [Computational Pre-requisites](#computational-pre-requisites)
4. [Setting up the AtomSpace](#setting-up-the-atomspace)
5. [Bulk Pair Counting](#bulk-pair-counting)
6. [Mutual Information of Word Pairs](#mutual-information-of-word-pairs)
7. [The Vector Structure Encoded in Pairs](#the-vector-structure-encoded-in-pairs)
8. [Maximum Spanning Trees](#maximum-spanning-trees)
9. [MST Disjunct Counting](#mst-disjunct-counting)
10. [Disjunct Marginal Statistics](#disjunct-marginal-statistics)
11. [Determining Grammatical Classes](#determining-grammatical-classes)
12. [Creating Grammatical Classes](#creating-grammatical-classes)
13. [Exporting a Lexis](#exporting-a-lexis)
14. [Clustering](#clustering)
15. [Measuring Quality](#measuring-quality)
16. [Precomputed LXC containers](#precomputed-lxc-containers)


Project Summary
---------------
The goal of this project is to create a system that is capable of
learning the grammar and some of the semantics of natural language.
The fundamental goal is to do this in an unsupervised fashion, with
no training data beyond that of un-annotated raw text.

An early draft of how this can be done is presented in the paper
"Language Learning", B. Goertzel and L. Vepstas (2014) on ArXiv; see
[ArXiv abs/1401.3372](https://arxiv.org/abs/1401.3372). Some of this
is repeated on the
[language learning wiki](http://wiki.opencog.org/w/Language_learning).
A very important update, describing how to move past state-of-the-art
results, is presented in the
"[Sheaf Theory](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/sheaves.pdf)"
paper. This important step is simply summarized in a later section,
below.

The key process is a sequence of steps that can be used to extract the
structure of the "language graph", with each step being noisey and
error-prone, but the subsequent step averaging over this structure in
such a way as to cancel noise, and allow the actual graphical structure
to emerge. Central to this process is extracting the actual structure of
the graph, as opposed to the extraction of vector spaces, as is currently
done with neural-net techniques or other styles of clustering.

The core insight is that although vector spaces can be layered onto
language data (thus explaining the success of modern-day neural net
techniques), ther is also a way of "stitching together" these vector
spaces in such a way that they extract the underlying graphical
structure.  The appearance of vector spaces is not an accident: the
axioms of algebraic linguistics (such as those found in categorial
grammar, pregroup grammar, link grammar) are similar to the axioms that
define a vector space. The similarity accounts for why techniques such
as Word2Vec are successful. The dis-similarity is why these same
tecniques are incomplete.  The road-block can be overcome by viewing
the language graph (and graphs in general) as being constructed out
of connected parts, with explicitly-named connectors. This
connector-based approach to the definition of a graph just so happens
to obey the axioms of a sheaf (as commonly understood in sheaf theory).
This connector-based approach also indicates how vector spaces can be
assembled, connected together to form  the graph from which they are
being sampled.  The code here is an exploration and implementation of
this insight.

An very early and yet pretty presentation of algebraic linguistics,
as seen axiomatically, is given by Solomon Marcus,
“[Algebraic Linguistics; Analytical Models](https://monoskop.org/images/2/26/Marcus_Solomon_editor_Algebraic_Linguistics_Analytical_Models_1967.pdf)”,
(1967), Elsevier.

A feels-right explanation of how one extracts semantics from disjuncts is
given by EA Nida,
[“The Molecular Level of Lexical Semantics”](https://www.academia.edu/36534355/The_Molecular_Level_of_Lexical_Semantics_by_EA_Nida),
(1997) *International Journal of Lexicography*, **10**(4): 265–274.
What Nida is saying can, in fact, be measured, by correlating, for
example, disjuncts with WordNet word-senses. The correlation is real and
is measureable (and has been measured). The goal of this project is to
move beyond this.

Status
------
In 2019 we realized that training on English corpora does not offer
sufficient control to measure the quality of the learning algorithm.
Thus, we've devised a new approach: create a random grammar, create
a corpus of sentences from that random grammar, learn a grammar from
that corpus, and validate that the learned grammar accurately matches
the input grammar.  Doing this will allow the learning algorithm to
be correctly calibrated for grammars of different sizes and
complexities, and for corpora of different sizes. We will be able to
measure how accuracy scales as a function of training time, how well
different training algorithms perform, how large a corpus is neeed to
get good results, and other related questions.

As of 2021, we are still setting up the infrastruture to do the above.
Once this is done (real soon now?) the project can resume training runs.
Please contact via email or discord opencog chat for details.

The instructions below still assume training is done on an English
corpus, and will need to be modified to describe the above.  The work
on English seemed to work "pretty well", in that it was converging to
what seemed to be the right answers. However, controlling the rate
of convergence was a challenge, since measuring the accuracy of the
results was impossible. We could "eyeball" the accuracy and it "looked
OK". But "eyeballing it" is not very scientific.

Note also: The instructions below refer to storing the data in
PostgreSQL. However, the RocksDB is much faster, and will be used
for the next-gen work. They both use the same API, so conversion
will not be hard.

Processing Overview
-------------------
See the [README-Natural](README-Natural.md) file for a description of
the "old" (uncalibrated) processing. It describes the processing steps
in detail.

See the [README-Calibration](README-Calibration.md) file for the "new"
(next-gen) process of generating artficifical languages with bounded
statistical properties, learning them, and then measuring the accuracy
of the learned language vs. the generated artificial language.


That's all for now!
-------------------
THE END.
