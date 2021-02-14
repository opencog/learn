Learning
========

The Big Idea - Knowledge
------------------------
Here's the big idea: "everything is a graph" -- all knowledge is linked
information. For example, knowledge-bases describe relations between
"things" - entities, ideas, concepts.

Graphs are described by local connectivity: what is connected to what,
nearby. These local connections are given many different names:
assertions, statements, rules, facts, axioms.

Many local neighborhoods look alike, and connect in similar ways. Such
similarities also have many different names: an "instance of a class",
an "example of this type", a "template", a "general rule", an "axiom
schema".

The way in which regions of graphs look locally similar can be described
by a "grammar".  The way they connect is a "syntax". Valid graphs have
shapes that follow from the syntax/grammar. The "meaning" or the
"semantics" of the graph lies in the connections themselves.

This project takes the above as the structure of "knowledge" and
"meaning". For now, this is just metaphysics; more concrete details
are provided below and in related PDF's in github, and on OpenCog wiki
pages.

The Big Idea - Learning and Understanding
-----------------------------------------
If you want to describe something, some idea, some concept, some
situation or event, you have several choices: draw a picture, make a
movie, write some text, dance about it, build a machine.

If you are writing text, what you are "actually doing" is taking the
network of interconnected facts/ideas, and serializing them into a
sequence of words. A sequential, time-like order, one word after
another.  You "serialize" the "graph of ideas."

When you read, and try to understand and learn, you try to "deserialize"
the words, and reconstruct the graph-network of ideas in your mind.

The goal of this project is to build a serializer/deserializer pair.
More narrowly, to convert natural language into a graph of ideas, and
conversely, to express the knowledge-graph as a sequence of sentences.
Narrower still, the goal is to extract the grammar and syntax from
a corpus of natural language text.

This narrow focus is the starting point. Thus, most of what follows will
talk about grammar. That focus is necessary to make forward progress,
although the vision above hints at a far more general, far more powerful
possibility for working with knowledge. This vision should be portable
to (bio-)chemical structures, the 3D-shapes of physical objects, the
correlation of images to text, the correlation of movement to text.
But to keep things focused, for now its just text.

The "graph of knowledge" sketched above is assumed to be a "sparse
graph", or olde-school "symbolic AI". This is in contrast to neural nets
and deep learning, where knowledge is encoded in a dense graph, the
network connectivity graph of weight matrices, vectors and the like.
The deep learning industry has plenty of folks working on it. The goal
of this project is to move forward on sparse-graph knowledge
representation. (Note however: some parts of sparse graphs are densely
connected, and, for those parts, deep-learning type structures may be
ideal.  This idea is explored in detail in several PDF's here and
elsewhere. The heading for that discussion is "matrix factorization",
or rather, "sparse matrix factorization.")

This is an ongoing project, with continuing if sometimes sporadic activity.

Unsupervised Language Learning
==============================
(See also: [Language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview.)

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

As of 2021, we are still setting up the infrastructure to do the above.
Once this is done (real soon now?) the project can resume training runs.
Please contact via email or discord opencog chat for details.

The instructions below still assume training is done on an English
corpus, and will need to be modified to describe the above.  The work
on English seemed to work "pretty well", in that it was converging to
what seemed to be the right answers. However, controlling the rate
of convergence was a challenge, since measuring the accuracy of the
results was impossible. We could "eyeball" the accuracy and it "looked
OK". But "eyeballing it" is not very scientific.

Processing Overview
-------------------
See the [README-Natural](README-Natural.md) file for a description of
the "open-loop" (uncalibrated) processing system. It describes the
processing steps in detail.  Getting good results requires tuning
a variety of parameters, and so calibration needs to be run first.

See the [README-Calibration](README-Calibration.md) file for the "new"
(next-gen) process of generating artificial languages with bounded
statistical properties, learning them, and then measuring the accuracy
of the learned language vs. the generated artificial language.

Directories
-----------
A quick overview:

* [download](download) - code for downloading sample corpora off the intertubes.
* [fake](fake) - code for generating artificial grammars.
* [learn-lang-diary](learn-lang-diary) - diary and notes and papers
  describing results and theory.
* [run](run) - scripts for running the learning pipeline.
* [scm](scm) - the code that actually does all the work.
* [tests](tests) - unit tests. Currently unmaintained and broken.

Architecture Overview
---------------------
All the "heavy lifting" is done in the OpenCog
[AtomSpace](https://github.com/opencog/atomspace). The AtomSpace is a
graph database for working with typed (hyper-)graphs. (Typed hypergraphs
are ideal for storing very abstract kinds of knowledge.) The AtomSpace
can be manipulated through Atomese, python, C++, haskel and scheme
bindings.  This project glues all of the parts together with scheme
([guile](https://www.gnu.org/software/guile/)). If you are a python fan,
sorry! The goal here is rapid prototyping, easy experimentation, rapid
reconfiguration/redesign. For that, scheme is just simpler, better,
faster. (At least, for me.)

Long-term, the best and finest algorithms will probably be re-written
in C++ (for speed), and exported with Atomese, python, haskel, scheme
(pick your favorite) bindings. This migration process happens as we gain
understanding of what the problem is, and what reasonable solutions look
like.

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
