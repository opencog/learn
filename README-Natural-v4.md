
Integrated Natural Language System
==================================
* November 2022.

This is a revised version of the earlier [Unsupervised Natural Language
Learning README Version 3](./attic/run-v3/README-Natural-v3.md), updated
to reflect a new integrated process that enables continuous learing.

A general overview, including pre-requistes, is provided by the
[Unsupervised Natural Language Learning README](./README-Natural.md) file.

This integration is in development; the instructions below are incomplete.
Look at the older version for general guidance.

There are technical challenges to fully implementing continuous learning.
The instructions below will be a hybrid of the older-style batch process,
and the newer style.


Table of Contents
------------------
1. [Processing Overview](#processing-overview)
2. [Preliminaries](#preliminaries)

Preliminaries
-------------
The setup of the integrated pipeline requires many prequisites and
preliminaries. These are given in the earlier

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
16. [Precomputed LXC containers](#precomputed-lxc-containers)

Mutual Information of Word Pairs
--------------------------------
The goal is to have MI be computed dynamically, on the fly. The code
to get this working is half written, but incomplete. So, for now, do
it as before, as a batch process. Run the code in
`run-common/marginals-pair.scm`. It works.

MST Parsing Demo
----------------
As a demo of what is about to happen, aim the `link-parser` at a
running instance of the CogServer, containing word-pairs (and
word-pair MI data.) Type in any sentence, and then patiently wait
(about 5-10 seconds) for data to fly over the net. The resulting
parses will be maximal planar graphs (MPG), which are similar to
maximal spanning trees (MST), but contain loops. What's being
maximized is the sum-total of all of the MI of the links between
words.

Use the dictionary in `run-config/dict-combined` after adjusting
the URL in it. Like so:
```
link-parser run-config/dict-combined
```

MST Disjunct Counting
---------------------
As before, but with modernized infrastucture.
(This is not yet the "continuous learning" design...)

* Edit `run-config/3-mpg-conf.sh` and modify as needed,
* Start CogServer with `run/3-mst-parsing/run-mst-cogserver.sh`
  or simply `guile -l run-common/cogserver-mst.scm`.
* Place text data into `$CORPORA_DIR` as configured in `3-mpg-conf.sh`
* Run `./run/3-mst-parsing/mst-submit.sh`

That's all for now!
-------------------
THE END.
