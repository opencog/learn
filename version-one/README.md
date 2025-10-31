Learning
========
The "big ideas" describe how this project thinks about the world.
The "medium ideas" describe the basic algorithm being used.
The "small ideas" describe what has been implemented and is doable
(has been done) with the current technology and code base. Current focus
is on language; we're looking for collaborators on image recognition
and/or movement classification.


The Symbol Grounding Problem
----------------------------
Today, AI (AGI) theory stands divided into two camps: the neural net
approaches, and symbolic approaches. Both approaches have important
results and shed light on the key problem of intelligence. In the
middle, between them is the symbol grounding problem. A common complaint
about symbolic reasoning over common sense knowledge is the problem of
knowing what a symbol is: when a chatbot talks about chairs and tables,
sports teams and favorite colors, how can it know what a "chair" is?
A "chair" is a symbol, but what does it "mean"?  Conventional neural net
approaches have to opposite problem: they don't have or use symbols,
and so are not amenable to leveraging the power of logical inference.

The aim of this project is to solve the symbol grounding problem. The
proposed solution is that symbols are simply names for large, complex,
***sparse*** networks of hierarchical relationships. Thus, the goal is
to discover the  large, complex, sparse networks of hierarchical
relationships in unstructured data. Symbols then become coherent
networks of relationships. Thus, a "chair" is both a collection of all
"facts" one might know about chairs, as well as an English word
participating in the complex grammatical network of the English
language.  The word "chair" is not so much a "symbol" as it is a bridge
between these complex networks. Reasoning can be done "sub-symbolically",
on the network of relationship; the result of reasoning can be
articulated verbally, adhereing to the rules of grammar. Reasoning itself
is not (inherently) logical; rather, reasoning is powered by a set of
"valid" manipulations applied to the network, the validity of which is
also learned and encoded in a complex network. Thus, the goal of this
project is to not only learn what a "chair" is from environmental
stimulous, and not only learn how to use the word "chair" in a
grammatical sentence, but also to learn the rules of logic, and the
rules of common sense reasoning, out of thing air.  The rest of this
file (and other documents in this and other repos) expand in detail
how the above can be accomplished.

BTW: to be clear: the aim of this project is to create a system that can
interpret vision, and sound, and language, and sensory data in general,
from a single, unified theoretical framework. The proof of concept for
some of these domains is underway, albeit proceeding slowly. Helpers
needed.


The Big Idea - Knowledge
------------------------
Here's the big idea: ***everything is a graph*** -- all knowledge is linked
information. For example, knowledge-bases describe relations between
*things* - entities, ideas, concepts.

Graphs are described by local connectivity: what is connected to what,
nearby. Different problem domains call these local connections by various
different names: They might be called assertions, statements, rules, facts,
axioms, ontologies, abstract syntax trees, directed acyclic graphs,
decision trees, decision forests, ProLog statements, and so on.

Many local neighborhoods of a graph look alike, and connect in similar
ways. Such similarities also have many different names: an "instance of
a class", an "example of this type", a "template", a "general rule", an
"axiom schema", and so on.

The way in which regions of graphs look locally similar can be described
by a *grammar*.  The way they connect is a *syntax*. Valid graphs have
shapes that follow from the syntax/grammar. The *meaning* or the
*semantics* of the graph lies in the connections themselves.

This project takes the above to be the structure of ***knowledge*** and
***meaning***. Stronger and more detailed arguments for why this is the
correct definition of "knowledge" and "meaning", together with concrete
definitions, details and examples are provided by the PDF's in the
[AtomSpace "sheaf" directory](https://github.com/opencog/atomspace/tree/master/opencog/sheaf/docs).


The Big Idea - Learning and Understanding
-----------------------------------------
If you want to describe something, some idea, some concept, some
situation or event, you have several choices: draw a picture, make a
movie, write some text, dance about it, build a machine.

If you are writing text, what you are "actually doing" is taking the
network of interconnected facts/ideas, and serializing them into a
sequence of words. A sequential, time-like order, one word after
another.  You *serialize* the *graph of ideas.*

When you read, and try to understand and learn, you try to *deserialize*
the words, and reconstruct the graph-network of ideas in your mind.

The goal of this project is to build a ***serializer/deserializer pair***.
More narrowly, to convert natural language into a graph of ideas, and
conversely, to express the knowledge-graph as a sequence of sentences.
Narrower still, the goal is to extract the grammar and syntax from
a corpus of natural language text.

This narrow focus is the starting point. Thus, most of what follows will
talk about grammar. That focus is necessary to make forward progress,
although the vision above hints at a far more general, far more powerful
possibility for working with knowledge. This vision should be portable
to (bio-)chemical structures, the 3D-shapes of physical objects, the
correlation of images to text, the correlation of movement and action
to sound and text.  But to keep things focused, for now its just text.

The "graph of knowledge" sketched above is assumed to be a ***sparse
graph***, or olde-school "symbolic AI". This is in contrast to neural nets
and deep learning, where knowledge is encoded in a dense graph, the
network connectivity graph of weight matrices, vectors and the like.
The deep learning industry has plenty of folks working on it. It's
a multi-billion-dollar industry. We are not competing with that behemoth.
The goal of this project is to move forward on sparse-graph knowledge
representation. (Note however: some parts of sparse graphs are densely
connected, and, for those parts, deep-learning type structures may be
ideal.  This idea is explored in detail in several PDF's here and
elsewhere. The heading for that discussion is "matrix factorization",
or rather, "sparse matrix factorization.")

This is an ongoing project, with continuing if sometimes sporadic activity.


The Big Idea - World Models
---------------------------
In a text, there is both a small-scale structure, and a large scale
structure. The small scale structure consists of grammatically-correct
sentences written in some natural language. The large-scale structure
is determined by the type of the text: is it a short story?
A dissertation? An owner's manual? The large scale flows from sentence
to sentence, paragraph to paragraph, as repeating ideas, characters,
topics, themes reoccur throughout the text.

There is also an intermediate-scale structure that has been
well-developed by linguists; a particularly apt one is Mel'čuk's
"Meaning-Text Theory", which describes ideas like "Deep Syntax"
and "Lexical Functions".

The goal of this project is to recursively extract deeper and deeper
layers of structure, starting from the surface structure of a sequence
of words. The author believes this can be done, so perhaps one could
say the goal of this project is to prove that it can be done.

Effectively, this means that the goal is really to produce an
***autonomous agent***.  This agent observes its environment (a
time-series of sensory inputs, such as sight, sound, and, to keep it
simple, plain text). From these observations, it builds a
***world model***, that encapsulates what it has learned.  Finally,
based on it's world model, it generates actions. Something gets done,
some action is performed (for example, a physical movement of a robot,
a change of facial expression in a robot, or, to keep things simple,
generation of some text.)

In the narrowest sense, this agent can be imagined to be a chatbot,
perhaps answering questions based on pattern matches on it's world-model.
More generally, the goal is that the agent should be able to conduct
entertaining conversations when called upon, or to write erudite
paragraphs (of grammatically correct English, in appropriate narrative
form) articulating some particular portion of the World Model. As the
World Model necessarily includes a self-model, this implies a certain
degree of "self-awareness".  In this sense, the agent is imagined to be
an AGI, in the complete sense.

Obviously, no such thing has been created. However, the current methods
developed here appear to be promising, and there is no apparent
impediment in sight, other than perhaps the general scale of the project
itself.  Plainly, there's a lot of code to write, test, debug, and a
lot of research and development to be performed.


### The Medium Idea
Part-Whole Hierarchies
======================
The above sketch is sufficiently abstract that, while it may be hard to
disagree with, it may also be hard to figure out how to write code for
it. Below is a sketch of what is actually done. It involves a recurring
process of generating vectors (of several different kinds), applying
similarity measures to those vectors, and then forming clusters or
classes (or "islands") of similar vectors.  Distance information can be
used to extract a graphical structure, which is decomposed into nearest
neighbors, which can then be formed again into vectors. This is used
to ratchet up a part-whole hierarchy, where parts are distinctly
identified as "those things that are similar" and the whole-relationship
is given by the local graphical structure. As icing on the cake, all
of the above is done with maximum-entropy principles in mind.

The general process is as follows:
* Make pair-wise observations, counting frequency of occurrences.
* Interpret pairs as the rows & columns of a matrix: each row and column
  is a vector. These vectors are extremely high-dimensional and sparse.
* Obtain pair-wise mutual information (MI). Any distance measure will do,
  but the information-theoretic ground of maximum entropy principles
  seems best.
* Re-observe data, this time using the pair-wise distances to form
  a spanning tree or spanning graph. The only edges allowed in this
  graph are those with high MI (i.e. small distance, those that are
  close to one-another.)
* Factor the graph into vertices and their nearest neighbors. This
  factorization resembles an N-gram or skip-gram: the list of neighbors
  is that skip-gram.
* Just as skip-grams can be interpreted as vectors, the same is possible
  here. The pair (vertex, nearest-neighbors) can be taken as the row
  and column addresses of a matrix, and the numeric value of that matrix
  entry is just the number of times that (vertex, neighbor) combination
  has been observed.
* Apply similarity measures to the row-vectors of this matrix, and
  cluster together or agglomerate similar vectors.
* These clusters are then the desired outcome: these are the classes of
  similar or identical observations. The next time some sequence of
  events is observed, it can be classed into one of these classes.
* Because the classes have (vertex, neighbor) information, they encode
  a graph. That is, they explicitly encode part-whole relationships.
  Any given vertex is a "part"; its neighbors determine how it's
  connected to the "whole".

To obtain hierarchical relationships, one notes that the dimensional
reduction obtained through clustering can then be used to tackle
combinatoric explosions and sparse data "at the next level". Thus,
for example, N-grams/skip-grams are famously high dimensional (e.g.
for a vocabulary of V = 10K words, there are then (10K)^3 = trillion
possible 3-grams) By the graphical agglomeration procedure given
above, this can be reduce to P=100 different "parts of speech"
(grammatical classes: nouns, adjectives, verbs, transitive verbs, etc.)
with graphical structure: the encoded graph is much much smaller.
Now that the data size is manageable, again, the learning process
can be restarted, this time looking at correlations between sentences,
between paragraphs, as opposed to correlations between words.  So,
for example, if a paragraph contains N=100 words, there gives a
practical for treating that N-gram with N=100, as the (graphical)
structure of smaller units in that paragraph are now available.

Unsupervised training
---------------------
A key difference between the code here, and conventional neural net
techniques is that all training here is "unsupervised". There are no
"training sets"; the learning is done entirely by extracting statistical
regularity from the input stream. In the parlance, it is not "turtles
all the way down"; but rather "the buck stops here".

This refers to the problem of training in neural nets: the training sets
are created by expert human markup, usually by grad students. In effect,
the neural net is only as smart as the grad student who provided the
training data.  Politically charged examples of this includes facial
recognition suites that mistreat people of color, as opposed to white
people. This effect has been termed "turtles all the way down", in
reference to the idea that the Earth sits on the back of a turtle:
but what does the turtle sit on? The neural net sits on the back of
a grad student.

Agglomeration
-------------
The current code base implements more-or-less traditional
high-dimensional agglomerative clustering. It does NOT use 3rd-party
systems or libraries, for three reasons:
* Other systems do not handle extremely sparse data efficiently.
* The high-dimensional data produced here is already in the form of a
  graph, and is stored in a graph format in the AtomSpace. Copying
  this data out, and back in again, is wasteful of CPU. It also
  requires complicated import/export code. The complexity of
  import/export is about equal to do-it-yourself clustering, so
  nothing is gained by using 3rd-party tools.
* The formation of clusters here is not quite as simple as throwing
  similar labels into buckets.  The vectors have to be merged,
  basis-element by basis-element, and those vectors encode graphical
  structure. For example, when one merges (vertex-A, neighbors-of-A)
  with (vertex-B, neighbors-of-B), it is not enough to merge just
  A and B, one must also merge (vertex-X, neighbors-of-X) whenever
  a neighbor-of-X includes A or B. This would require a callback from
  the clustering code to perform that graphical merge. 3rd-party
  tools are incapable of such callbacks.

Vectors
-------
There are two key differences between the vectors being formed here,
and the vectors that are commonplace in neural-net learning systems.
First is that the vectors here are extremely high-dimensional and
sparse. Second is that vector similarity is NOT judged by cosine angles,
but by information-theoretic measures.

A short tirade: the neural-net people are probably making a mistake by
using cosine angles. The cosine is great when a vector lives in a
Euclidean space, because Euclidean spaces have rotational symmetry.
But there is no reason to expect neural-net vectors to live in Euclidean
space, and it is rather silly to force a rotational symmetry onto a
problem that has none. The problem here is the word "vector".

In conventional mathematics, a "vector" is a collection of (number,
basis-element) pairs, and exists "naturally" in Euclidean space. But,
for neural nets, and for the current code base, this collection is
should be interpreted as a (number-between-zero-and-one, basis-element),
that is, as a frequency or probability: one should write P(A) as
the number (probability) at basis element A (event A). The "vectors"
are actually points in a simplex; and have the structure of a
sigma-algebra. Whenever a vector has the structure of a sigma algebra,
it should be interpreted as a probability, and the similarity of two
vectors is then given by (take your pick) the conditional probability,
the Kullback-Leibler divergence, the conditional or mutual entropy,
the mutual information, etc. It is *NOT* given by the dot-product or
the cosine angle! Note, BTW, that the mutual information does include
a dot-product inside of it; it just has different normalization factors
that break Euclidean rotational symmetry, and replace it by maximum
entropy principles.

N-grams, skip-grams, graph decomposition, grammar
-------------------------------------------------
Another major difference between the code here, and conventional
neural net theory, is that the explicit graphical structure is extracted
from the data, in the form of graphical nearest-neighbors. Thus, a
(word, neighboring-words) is the conventional definition of an N-gram.
If some neighbors are excluded, this is a skip-gram. The present case
is similar, but with several key distinctions.

**First:** neighbors are determined not by the physical proximity of one
word to another (how close they are in a sentence), but rather by the
mutual information between the words. (Here, a "word" can be literally
a "word", or more generically "some time-series event observed in
nature".)

**Second:** it is not the raw MI-proximity that counts, but rather, the
spanning tree or spanning graph that connects vertices (words). That
is, in formulating this "graphical skip-gram", one does not just grab
the nearest neighbors as measured by the distance function. One instead
attempts to minimize/maximize the distance measure (the entropy) for
the entire local graph.  This has the effect of discarding (not forming)
some edges despite those edges being short: it is more important to
minimize/maximize the local, collective graph, the local connectivity of
all vertices, and not just pairs of them.

**Third:** the use of "graphical skip-grams", as described here, provides
a natural bridge to the concept of syntax and grammar. That is, a
"graphical skip-gram" is a syntactic element, in a very real and formal
sense of languages and grammars. This is a foundational, key
observation, the details of which can be found in 1991 Link Grammar
papers: Sleator, Temperley, [*"Parsing English with a Link Grammar"*](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/link/pub/www/papers/ps/tr91-196.pdf).
The earliest mention of these concepts seems to be in a book:
[*"Algebraic Linguistics; Analytical Models"*](https://monoskop.org/images/2/26/Marcus_Solomon_editor_Algebraic_Linguistics_Analytical_Models_1967.pdf),
from 1967, by Solomon Marcus (published by Elsevier).

**Fourth:** the result of the agglomerative clustering on feature vectors
can be interpreted as "types", in the formal type-theoretical sense.
The Link Grammar "link types" really are types. They are short-hand
for ideas like S\NP and S/VP found in pregroup grammars, or more
generally in phrase-structure grammars.  The "learning" being performed
here does not "just" glom together identical feature vectors; it does
more: it explains exactly how those feature vectors should be interpreted
as graphs, and how that graph structure has regularities defined by a
grammar.

**Fifth:** meaning. There are frequent arguments made that neural nets
extract "meaning", or that weight vectors can be interpreted as
"meaning". These arguments carry over into the present case. They
are strengthened, because the feature vectors are no long "just
vectors", they are also components of a graph, components of a grammar.


### The Small Idea

System Architecture
===================
There are many -- many dozens -- of different NLP projects and
frameworks -- written in Python, Java, Perl, and another half-dozen
programming ecosystems.  The code here uses none of these. Why?

The answer is illustrated by a a simple, practical problem encountered
when one tries to perform scientific research withing these frameworks.
It is illustrated in the [README-Architecture](README-Architecture.md)
file.

Unsupervised Image Learning and Recognition (Vision)
====================================================
A simple sketch of how the above can be applied to vision (2D images) is
[presented here](README-Vision.md). No code has been written. Very
little theoretical exploration has been done.

Unsupervised Language Learning
==============================
(See also: [Language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview.)

In order to make forward progress, the current actual scope of the
project is limited to what is reasonably achievable with a relatively
small amount of work. The current primary focus is on unsupervised
language learning.

The goal of this project is to create a system that is capable of
learning the grammar and some of the semantics of natural language.
The fundamental goal is to do this in an unsupervised fashion, with
no training data beyond that of un-annotated raw text.

An early draft of how this can be done is presented in the paper
"Language Learning", B. Goertzel and L. Vepstas (2014) on ArXiv; see
[ArXiv abs/1401.3372](https://arxiv.org/abs/1401.3372). Some of this
is repeated on the
[language learning wiki](http://wiki.opencog.org/w/Language_learning).
Updates, describing how to move past state-of-the-art results, are
presented in the
"[Sheaf Theory](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/sheaves.pdf)"
paper and in the
"[Neural-Net vs. Symbolic Machine Learning](learn-lang-diary/skippy.pdf)"
paper. Other ideas and details are sketched in
"[Stitching Together Vector Spaces](learn-lang-diary/stitching.pdf)" and
"[Messaging](learn-lang-diary/messaging.pdf)".

The key process is a sequence of steps that can be used to extract the
structure of the "language graph", with each step being noisy and
error-prone, but the subsequent step averaging over this structure in
such a way as to cancel noise, and allow the actual graphical structure
to emerge. Central to this process is extracting the actual structure of
the graph, as opposed to the extraction of vector spaces, as is currently
done with neural-net techniques or other styles of clustering.

The core insight is that although vector spaces can be layered onto
language data (thus explaining the success of modern-day neural net
techniques), there is also a way of "stitching together" these vector
spaces in such a way that they extract the underlying graphical
structure.  The appearance of vector spaces is not an accident: the
axioms of algebraic linguistics (such as those found in categorial
grammar, pregroup grammar, link grammar) are similar to the axioms that
define a vector space. The similarity accounts for why techniques such
as Word2Vec are successful. The dis-similarity is why these same
techniques are incomplete.  The road-block can be overcome by viewing
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
is measurable (and has been measured). The goal of this project is to
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
different training algorithms perform, how large a corpus is need to
get good results, and other related questions.

As of May 2021, a basic infrastructure for doing the above has been set
up and mostly automated. Some initial calibration runs have been
performed. A multitude of difficult theoretical questions were promptly
exposed, see [README-Calibration](README-Calibration.md) for these.
The primary issue is that it seems like it's very easy to generate
complex and chaotic grammars which produce ergodic corpora. There is no
structure to something that is ergodic, so attempting to extract a
grammar is like trying to find structure in white noise. These
artificial grammars do not resemble those of natural human languages,
unless the parameters are very carefully chosen. Judging this and
evaluating results remains challenging.

Please contact via email or discord opencog chat for details.


Image recognition
-----------------
It seems like the above concepts (from the "medium idea" section) should
also work "just fine" for images (pixels in images). That's because a
2D image is "just like" a 1-D stream of words, in that there are obvious
neighbors, and nearby pixels are correlated. The pixel-pair-MI is a
stand-in for an edge detector; the minimum-spanning-tree just picks out
all of the nearby, inter-related image-parts (areas of the image that
frequently co-occur), and the grammar provides the part-whole hierarchy
for the image. Thus, it "seems like it should work".

The existing code would choke and be unbearably slow; thus, lots of
brand-new coded is needed to handle pixels. Interested collaborators
need to raise their hands!  This task is suitable for programmers/coders
familiar with image processing techniques; the more abstract parts of
the theory should not interfere with the more mundane tasks.  Note,
however: this is a risky endeavor: it seems like it should work, but
there is no guarantee.  Competing with conventional neural-net
technology is hard.

A different problem that this framework should be able to tackle is the
understanding coordinated motion. Consider, for example, the movement of
football players on a field: based on their movements, one can predict
what kind of play might be made. Similarly, the movements of a ballerina
- the hand, elbow, hip, knee movements are also correlated and can be
classified into "dance moves". This problem is unlike image recognition,
because one is given the positions of the players on the field, or the
positions of the dancers limbs, rather than being given a 2D pixelated
image.  Despite these differences, the overall algorithm "should still
work" on this data. (It seems like it should work! But what do we know?)


Mechanical Engineering
----------------------
The basic rules of mechanical engineering -- inclined planes, levers,
screws, rope and pulleys -- assemble in a form of a grammar. Can these
be discovered from observation?

Consider children's toy blocks: blocks and cylinders, columns, pyramids
and arches. These have rules for stacking, for example, cylinders roll,
and cannot be stacked unless upright. These can be considered to be
grammatical rules, describing the range of allowed, allowable
combinations that give stable structures.  The "corpus" is the
collection of all allowed, stable structures. Can the grammar of block
stacking, the grammar of mechanical engineering be learned from the
corpus of stable structures?

Can it be learned incrementally? Can it be learned by playing? How might
this be done?  Is the current theory suitable for this, and, if not, why
not? Can the current theory be amended so that it is suitable for
learning mechanical engineering?


Processing Overview
-------------------
See the [README-Natural](README-Natural.md) file for a description of
the "open-loop" (un-calibrated) processing system. It describes the
processing steps in detail.  Getting good results requires tuning
a variety of parameters, and so careful monitoring is required.

See the [README-Natural-v2](README-Natural-v2.md) file for the new
"integrated", "continuous learning" pipeline. Under development.

The [README-Calibration](README-Calibration.md) proposed a technique
for calibrating the learning pipeline, by generating artificial languages
with known, bounded statistical properties, then learning them, and then
measuring the accuracy of the learned language vs. the generated artificial
language. This approach is currently abandoned. Its not a bad idea, just
that naive conception won't work.

Directories
-----------
A quick overview:

* [download](download) - code for downloading sample corpora off the intertubes.
* [fake](fake) - code for generating artificial grammars.
* [learn-lang-diary](learn-lang-diary) - diary and notes and papers
  describing results and theory.
* [run](run) - scripts for running the learning pipeline.
* [run-config](run-config) - configuration parameters for the learning pipeline.
* [run-common](run-common) - generic scripts used in multiple different steps.
* [scm](scm) - the code that actually does all the work.
* [tests](tests) - unit tests. These should pass. Run `make test`.

* [attic](attic) - old datasets and old experimental runs.
* [scm/attic](scm/attic) - old source code and instruments.

Architecture Overview
---------------------
All the "heavy lifting" is done in the OpenCog
[AtomSpace](https://github.com/opencog/atomspace). The AtomSpace is a
graph database for working with typed (hyper-)graphs. (Typed hypergraphs
are ideal for storing very abstract kinds of knowledge.) The AtomSpace
can be manipulated through Atomese, Python, C++, Haskell and Scheme
bindings.  This project glues all of the parts together with Scheme
([guile](https://www.gnu.org/software/guile/)). If you are a Python fan,
sorry! The goal here is rapid prototyping, easy experimentation, rapid
reconfiguration/redesign. For that, Scheme is just simpler, better,
faster. (At least, for me.)

Long-term, the best and finest algorithms will probably be re-written
in C++ (for speed), and exported with Atomese, Python, Haskell, Scheme
(pick your favorite) bindings. This migration process should happen as
we gain understanding of what the problem is, and what reasonable
solutions look like.

This is a ***science project***. The goal is to determine how things work,
run experiments, create and refine new algorithms. Thus, the code in
this repo is organized like a science lab: stuff laying around in
a bit of a jumble, sometimes connected and working, and sometimes not.
Sometimes with instructions and an operating manual, and sometimes not.
If you're  a scientist, you're used to this. If you're a software
engineer, you might find all this to just be a vast incomprehensible
mess of cryptic code. Sorry about that!  This is a permanent
construction site, evolving and changing regularly.

Yes, the code "works". It works the same way any science experiment
works: you turn it on, things happen, and its up to you to figure out
what that means. What to do with the results. If you were expecting
an intelligent AI that you can crack jokes with, sorry, we don't have
that here.

That's all for now!
-------------------
THE END.
