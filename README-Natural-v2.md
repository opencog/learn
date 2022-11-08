
Integrated Natural Language System
==================================
* November 2022.

This is a revised version of the earlier [Unsupervised Natural Language
Learning README](./README-Natural.md), updated to reflect a new integrated
process that enables continuous learing.

This integration is in development; the instructions below are incomplete.


Table of Contents
------------------
1. [Processing Overview](#processing-overview)
2. [Preliminaries](#preliminaries)

Processing Overview
-------------------
Most of this README concerns the practical details of configuring and
operating the system, as it stands, today.  A diary of scientific notes
and results is in the [learn-lang-diary](learn-lang-diary) subdirectory.

The basic algorithmic steps, as implemented so far, are as follows:

* **A)** Ingest a lot of raw text, such as novels and narrative
         literature, and count the occurrence of nearby word-pairs.
         Anything that has lots of action-verbs will do.

* **B)** Compute the mutual information (mutual entropy or MI) between
         the word-pairs. The MI gives the affinity between words.

* **C)** Use a Minimum-Spanning-Tree algorithm to obtain provisional
         parses of sentences.  This requires ingesting a lot of raw
         text, again. (Independently of step **A**) The resulting
         parses are "provisional": medium accuracy, a lot better than
         random chance, but still very imperfect.

* **D)** Extract linkage disjuncts from the parses, and count their
         frequency. Linkage disjuncts capture how words connect to
         their neighbors.  By counting the frequencies, errors in
         the MST parses get washed out by the law of averages, while
         the likely-correct disjuncts accumulate large counts. The
         resulting dictionary is *more* accurate than any single
         MST parse!

* **E)** Use agglomerative clustering to merge similar disjuncts,
         while simultaneously merging similar words.  This will
         result in a set of word-classes (grammatical classes)
         with disjuncts on them that use word-class connectors.
         By law of averages, random errors get further washed out,
         while recurring frequent patterns get large counts.

* **F)** Place word-classes and word-class connectors into a
         link-grammar dictionary.

* **G)** Parse a large quantity of raw text, using the parser
         constructed in the previous step. Using maximum-entropy-style
         techniques, attempt to extract higher-order relations
         (resolve anaphora and other referents, determine rheme+theme,
         learn "lexical functions").

Currently, the software implements steps **A**, **B**, **C**, **D**,
**F** and much of step **E**, although this step remains a topic of
current research and experimentation.  Its quite unclear how to
determine the quality of lexis that falls out of step **F**.  Some
initial experiments on step **G** have been performed, and look promising.

Steps **A-C** are "well-known" in the academic literature, with results
reported by many researchers over the last two decades. The results
from Steps **D** & **E** are new, and have never been published before.
Results from Step **D** can be found in the PDF file
[Connector Sets](learn-lang-diary/drafts/connector-sets.pdf)
in the diary subdirectory.

It is important to understand that Step **E** is ***very different***
from commonly-reported algorithms used in the published literature,
such as using K-means clustering and dimensional reduction to obtain
word classes (grammatical categories). The reason for this is that the
disjuncts provide connectivity information, and that one must **not**
(merely) cluster over a vector space, but that one must instead cluster
over a "sheaf".  That is, the basis-elements of the vector space
themselves have non-trivial structure (which must not be ignored).
Sheaves resemble vector spaces, but are not the same.  They capture
and describe the connectivity information in the "basis elements".
This connectivity information is absent from ordinary approaches to
the machine-learning of grammatical classes.

As a result, ***none*** of the industry-standard classifiers and
clustering algorithms can be applied to step **E**: they all assume that
the input data can be structured as a vector space.  The axioms of
algebraic linguistics can resemble the axioms of a vector space, which
is why vector-space techniques can work pretty well in the machine-
learning of linguistic structure. However, ultimately, the axioms of
algebraic linguistics are ***not*** the axioms of a vector space.
In practical terms, this means that clustering/classification algorithm
used in step **E** is completely different than anything else available in
the industry.

The complete novelty of step **E**, coupled to the need to perform
experiments in earlier and later stages means that industry-standard,
generic big-data, machine-learning tools are wholly inadequate for the
task. It is for thus reason that all data processing is being done in
the AtomSpace, and not in some other machine learning or big-data
framework. Existing industry tools are not adequate for proper
linguistic analysis. This is a very important reason for developing
the AtomSpace, and other OpenCog infrastructure: to get past tool
limitations.

All of the statistics gathering is done within the OpenCog AtomSpace,
where counts and other statistical quantities are associated with various
different hypergraphs.  The contents of the AtomSpace are saved to an
SQL (Postgres) server for storage.  The system is fed with raw text
using assorted ad-hoc scripts, which include link-grammar as a central
component of the processing pipeline. Most of the data analysis is
performed with an assortment of scheme scripts.

Thus, operating the system requires three basic steps:
* Setting up the AtomSpace,
* Configuring scripts to feed in raw text, and
* Processing the data after it has been collected.

Each of these is described in greater detail in separate sections below.

Preliminaries
-------------
The setup of the integrated pipeline requires many prequisites and
preliminaries. These are given in the earlier

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


That's all for now!
-------------------
THE END.
