
Language Processing Code
------------------------
A short description of each file. Listed roughly in pipeline order.
As a general rule, later files not only depend on earlier files,
but the also require, as input, data that was produced by earlier files.
Some of these files contain extensive documentation. Those are linked
below.

* __learn.scm__           This is the main guile module.
* __common.scm__          Common code.
* __utilities.scm__       Utilities, e.g. for looping.

* [__pair-count__](pair-count) Count word pairs extracted from large
                          blocks of natural language text.

* [__parse__](parse)      Perform "unified", "mixed-mode" generic
                          parsing that combines MST/MPG parsing with
                          LG parsing. The parse results are used to
                          update counts on the Sections that were used
                          (observed).


In the `gram-class` subdirectory:

* [__README.md__](gram-class/README.md) Provides
                          a general overview of the idea of clustering,
                          classification. This includes a discussion of
                          "grammatical similarity" and other relevant topics.

* [__shape-vec.scm__](gram-class/shape-vec.scm) Provides a matrix API
                          to cset-shapes. These are csets (connector
                          sequences), where the wild-card is one of the
                          connectors, rather than being the root-word
                          of the disjunct. These "shapes" allow vectors
                          to have specific connectors in their basis.
                          Shapes are a central concept used to simplify
                          the "flattening" (projection) of sheaves down
                          the base space.

* [__shape-project.scm__](gram-class/shape-project.scm) Provides a
                          detailed explanation of how vectors that have
                          shapes in them are merged. That is, how sheaves
                          are projected down to their base space.

* __gram-class-api.scm__  Public API classes for merging words into
                          grammatical classes.
* __agglo-rank.scm__      Agglomerative clustering. Loops over all words,
                          creating clusters.
* __in-group.scm__        Selection of a group of words to cluster. The
                          group is selected to have many traits in common.
* __gram-majority.scm__   Implementation of the actual merge of sections
                          into a cluster.
* __gram-filters.scm__    Assorted filters for picking subsets of the full
                          grammatical matrix.

In the `concepts` subdirectory:

* Code for the automatic discovery of concepts and their attributes.

The `attic` subdirectory contains code that is obsolete and is no longer
used. It is kept as a reference, because the code therein is described
and experimentally measured in the 'Diary', and it did seem at one time
to be important, or was the best way to do something. That is, it did
have some kind of appealing theory behind it, or at least the appearance
of a coherent theory. The code there should mostly work, but may be
bit-rotted.


Data Structures
---------------
All code uses a repetitive set of data structures called "vectors",
"germs", "sections" and "disjuncts". The "vector" is similar to a
conventional vector, as defined in mathematics, in that it associates
a number to a basis element. More precisely, the vectors here are just
sets, with an associated floating-point number for each member of the
set. Elements of the set are Atoms stored in the AtomSpace.

The 2D version of a "vector" is a "matrix". Again, this is similar to
to the conventional mathematical definition of a matrix. The primary
difference is that here, the matrix is just a number associated to pairs
of things (pairs of Atoms). In general, the matrices are extremely
sparse, which is why they are stored as pairs - it is much more memory
(RAM/storage) efficient. In the code here, the matrix objects are just
called "objects" or `LLOBJ` or "low-level objects": the matrix API is
an object-oriented class providing methods to obtain marginal sums,
conditional probabilities, mutual information and many other quantities
that can be obtained from a matrix.  The full documentation for the API
can be found in a different GitHub repo, the
[matrix](https://github.com/opencog/atomspace/tree/master/opencog/matrix)
directory in the AtomSpace repo.

Sections and disjuncts (connector sets) are concepts that arise
naturally when working with subsets of graphs. Roughly speaking, a
section is what one gets if one takes a graph, and clips some edges in
half.  Each half-edge becomes a "connector": it signifies the
possibility of connecting to some appropriate mating connector. The
"disjunct" is the same thing as a "connector set". (It gets the name
"disjunct" because it is typically disjoined with other connector sets.
For example: 'connect (A and B) or (C and D)'. The 'and', 'or' here are
not Boolean and/or, they are linear-logic and/or, because linear logic
is the "internal language" of a "monoidal category".) The terms
"disjunct" and "connector set" are used synonymously throughout the
code. Sections, connector sets and more are explained in detail in a
different github repo, in the
[sheaf](https://github.com/opencog/atomspace/tree/master/opencog/sheaf)
directory, and the various PDF's in the
[sheaf/docs](https://github.com/opencog/atomspace/tree/master/opencog/sheaf/docs)
directory.
