
Language Processing Code
------------------------
A short description of each file. Listed roughly in pipeline order.
As a general rule, later files not only depend on earlier files,
but the also require, as input, data that was produced by earlier files.

* __learn.scm__           This is the main guile module.
* __common.scm__          Common code
* __utilities.scm__       Utilities, e.g. for looping.
* __link-pipeline.scm__   Word-pair counting. Given raw input text, this
                          accumulates frequencies of word-pairs.
* __singletons.scm__      Single-word counting. Unused.
* __batch-word-pair.scm__ Batch-compute the marginals and mutual
                          information of word-pairs.
* __mst-parser.scm__      MST Parser. Uses word-pair MI to generate
                          a spanning tree. Busts up tree into
                          pseudo-csets. Accumulates frequencies of
                          pseudo-csets.
* __pseudo-csets.scm__    Provides a matrix API to the csets. Rows
                          (left index) are words, columns (right index)
                          are pseudo-csets.
* __shape-vec.scm__       Provides a matrix API to cset-shapes. These
                          are csets, where the wild-card is one of the
                          connectors (rather than the root-word of the
                          disjunct)
* __summary.scm__         Print a summary report for a dataset.
* __vectors.scm__         Vector arithmetic.
* __gram-class.scm__      Merge words into grammatical classes.  If two
                          words are similar, they are merged into one
                          class.  If a word is similar to a class, the
                          word is merged into the class. Several metrics
                          are provided.
* __gram-agglo.scm__      Agglomerative clustering. Uses abve merge
                          strategies to actually create clusters.
* __cset-class.scm__      Merge connector sets.
* __export-disjuncts.scm__ Export grammatical classes to Link Grammar.

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
that can be obtained from a matrix.  The full documentation for teh API
can be found in a different github repo, the
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


Documentation
-------------
The above files contain documentation embeded within them. Particularly
notable:

* [gram-classification.scm](gram-classification.scm) Explains the
                          process of clustering/classification in detail.

* [shape-vec.scm](shape-vec.scm) Explains the concept of "shapes". These
                          are vectors where one of the connectors has
                          been made the base of the vector.

* [cset-merge.scm](cset-merge.scm) Reviews how connector sets are to be
                          merged. The merge decision is made elsewhere;
                          this only goes through the mechanics of merging.

TODO
----
Review and probably delete:

* singletons.scm - stale rancid unfinished code.
