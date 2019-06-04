
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
