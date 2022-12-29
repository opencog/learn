
Unsupervised Natural Language Learning
======================================
* Linas Vepstas December 2013
* Updated May 2017
* Updated June 2018
* Updated February 2021
* Updated November 2022

This README provides general information needed for setting up the
natural language learning processing pipeline. It details pre-requisites
and outlines some general concepts. These are shared by two sub-projects:

* [Integrated Language Learning](./README-Natural-v4.md) -- the current
  focus of activity.
* [Calibrated Learning](README-Calibration.md) -- Mothballed. Some good
  (great?) ideas for calibrating the pipeline, but not really workable at
  the present time.

A diary of scientific notes and results is in the
[learn-lang-diary](learn-lang-diary) subdirectory.
Earlier versions of the processing scripts can be found in the
[attic directory](./attic).

Table of Contents
------------------
1. [Processing Overview](#processing-overview)
2. [Computational Pre-requisites](#computational-pre-requisites)
3. [Bulk Pair Counting](#bulk-pair-counting)
4. [The Vector Structure Encoded in Pairs](#the-vector-structure-encoded-in-pairs)

Processing Overview
-------------------
The basic algorithmic steps, as implemented so far, are as follows:

* **A)** Ingest a lot of raw text, such as novels and narrative
         literature, and count the occurrence of nearby word-pairs.
         Anything that has lots of action-verbs will do.

* **B)** "Sleep". In the first step, above, the system is "awake"
         and ingesting data from the environment. This is followed
         by a sleep cycle, where MI's are computed, similarities are
         computed, and clustering is done. The sleep cycle consists
         of the following:

* **C)** Compute the mutual information (mutual entropy or MI) between
         the word-pairs. The MI gives the affinity between words.

* **D)** Perform agglomerative clustering, to assign similar words
         word-classes. This helps keep the size of datasets manageable.
         Similarity is judged by Gaussian Orthogonal Ensemble (GOE)
         techniques.

* **E)** Wake cycle. Resume ingestion of data. This include more pair
         counting, as in step A), plus, in addition, the following:

* **F)** Use a Minimum-Spanning-Tree (MST) algorithm to obtain
         provisional parses of sentences.  This requires ingesting a
         lot of raw text, again. (Independently of step **A**) The
         resulting parses are "provisional": medium accuracy, a lot
         better than random chance, but imperfect.

* **G)** Extract linkage disjuncts from the parses, and count their
         frequency. Linkage disjuncts capture how words connect to
         their neighbors.  By counting the frequencies, errors in
         the MST parses get washed out by the law of averages, while
         the likely-correct disjuncts accumulate large counts. The
         resulting dictionary is *more* accurate than any single
         MST parse!

* **H**) Sleep cycle two. Perform the same sleep tasks as the first time,
         plus also: Compute GOE similarities from disjuncts (instead of
         word-pairs). Use these to perform clustering, to revise existing
         clusters. By law of averages, random errors in pair counting
         are washed out, while recurring frequent patterns get large
         counts (*i.e.* are amplified).

* **I)** Wake cycle three. Perform the same pair counting and disjunt
         counting as in the earlier wake cycles, but also perform LG
         parsing. Also extract long-range data.

* **J**) The disjunct dictionary, obtained in the earlier wake cycle
         (**E-F-G**) is Link Grammar compatible and the conventional
         LG parser can be used to obtin parses. So parse again, this
         time counting which disjuncts are actually used.

* **K**) Perform long-distance pair-counting. This time, instead of
         tracking word-pairs, track disjunct pairs. The goal here is
         entity detection, automatic extraction of intensional,
         extensional entity properties. This, pus discovery of Lexical
         Functions (LF's) and Anaphora Resulution (AR) or Reference
         Resolution (RR).  So, if the *same* word occurs in two different
         sentences, create a pair from the two different disjuncts that
         were observed. This indicates a possible entity; it also
         enumerates possible entity properties. If the *same* disjunct
         occurs in two different sentences, pair the words together.
         This pair indicates a possible anaphora or reference.

* **L**) Sleep cycle three. Repeat sleep cycles 1 &amp; 2, plus also
         analysis of the next level of pairs.

The distinction between the wake and sleep cycles is because the sleep
cycles compute marginals and similarities, and these cannot be done at
the same time that data is being ingested, as all of the counts would be
thrown off.

Currently, the software implements steps **A**, through **J** but not yet
in a fully integrated fashion. That is, the second wake cycle does not
include the counting from the first; and so on, for the sleep cycles.
This is being fixed, right now, as a fully integrated pipeline is needed
for successful steps **J**, **K**, **L**.

Steps **A-C** and **F** are "well-known" in the academic literature, with
results reported by many researchers over the last two decades. The steps
**D** is novel, and has never been published before (obviously,
agglomerative clustering is not new; however, agglomerative clustering of
*disjuncts* has never been reported before.)

Steps **G** and after are all new, and have not been attempted anywhere,
by anyone else.  Results from Step **G** can be found in the PDF file
[Connector Sets](learn-lang-diary/drafts/connector-sets.pdf)
in the diary subdirectory.

It is important to understand that the agglomerative culstering done in
Step **H** is ***very different*** from commonly-reported algorithms used
in the published literature, such as using K-means clustering and
dimensional reduction to obtain word classes (grammatical categories).
The reason for this is that the disjuncts provide connectivity information,
and that one must **not** (merely) cluster over a vector space, but that
one must instead cluster over a "sheaf".  That is, the basis-elements of
the vector space themselves have non-trivial structure (which must not
be ignored).  Sheaves resemble vector spaces, but are not the same.
They capture and describe the connectivity information in the "basis
elements".  This connectivity information is absent from ordinary
approaches to the machine-learning of grammatical classes.

As a result, ***none*** of the industry-standard classifiers and
clustering algorithms can be applied to step **H**: they all assume that
the input data can be structured as a vector space.  The axioms of
algebraic linguistics can resemble the axioms of a vector space, which
is why vector-space techniques can work pretty well in the machine-
learning of linguistic structure. However, ultimately, the axioms of
algebraic linguistics are ***not*** the axioms of a vector space.
In practical terms, this means that clustering/classification algorithm
used in step **H** is completely different than anything else available in
the industry.

The use of GOE similarity also appears to be a completely novel
innovation, from what I can tell. Note that the use of GOE does mean
that classification is superficially similar to that seen in
deep-learning neural nets, and to some degree, the transformers, as all
of these models work on extremely high-dimensional vectors.

The complete novelty of step **H**, coupled to the need to perform
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
RocksDB database for storage.  The system is fed with raw text using
assorted ad-hoc scripts, which include link-grammar as a central
component of the processing pipeline. Most of the data analysis is
performed with an assortment of scheme scripts.

Thus, operating the system requires three basic steps:
* Setting up the AtomSpace,
* Configuring scripts to feed in raw text during the wake cycles,
* Processing the data during the sleep cycles.

Each of these is described in greater detail in separate sections below.

Computational Pre-requisites
----------------------------
This section describes minimum mandatory hardware and software
requirements. Failing to take these seriously will result in an
unhappy, painful experience.

* **0.1)** Optional, but strongly recommended! Consider investing
           in an uninterruptible power supply (UPS) to avoid data
           loss/data corruption in the event of a power outage.
           The scripts below can take weeks to run, and unexpected
           power outages can corrupt databases, resulting in weeks
           or months of lost work.

   ***You have been warned! This is not a joke!***

* **0.2)** Optional, but strongly recommended!  Consider investing in
           a pair of solid-state disk drives (SSD), configured as a
           RAID-1 (mirroring) array, to hold the database.
           Accumulating observation statistics, as well as other data
           processing operations, is database intensive, and the writes
           to disk are often the main bottleneck to performance.

   When running English-language learning, 1TB (one terabyte)
   devoted to just the databases is just barely enough to
   perform the work described here. Database copies quickly
   chew up disk space.

   Individual databases are typically in the 10GB to 150GB in
   size, and not very many copies of a 150GB database will fit
   onto a 1TB disk.  2TB is a more comfortable size to work with.

* **0.3)** Get a machine with 4 or more CPU cores, and a minimum of
           64GB RAM.  128GB or 256GB or more is preferrable.
           Otherwise one is faced with the agony of struggling
           with datasets that don't fit.

   Many of the scripts are beig modernized to fetch data "on demand",
   and to flush RAM when that data is no longer needed. However, this
   has not been acheived in general, and sometimes, especially during
   the sleep cycles, the entire dataset must be paged in.

* **0.4)** Optional but recommended. If you plan to run the pipeline
           on multiple different languages, it can be convenient, for
           various reasons, to run the processing in an LXC container.
           If you already know LXC, then do it. If not, or this is
           your first time, then don't bother right now. Come back to
           this later.

   LXC containers are nice because:

   * Each container can have a different config, different
     datasets, and be executing different steps of the pipeline.
     This is great, for juggling multiple jobs.

   * The system install in one container won't corrupt the other
     containers; you can experiment, without impacting stable
     systems.

   * LXC containers can be stopped and moved to other system with
     more RAM (or less RAM), more disk, or less disk. Handy for
     load-balancing, and/or easily moving to a bigger system.

   * Do not confuse LXC with Docker! They are similar, but LXC
     is superior for the current task! Why, you ask? Because
     when you reboot a Docker container, you lose all your work!
     Dohhh! You can probably configure Docker to also work in this
     way, but this requires more fiddling.

* **0.5)** Optional but strongly recommended. Install system shutdown
           scripts.  This will help protect your data in case of an
           unexpected power loss.  These scripts, and the instructions
           for them, are located in the `run/manual` directory. They
           need to be adjusted and manually installed. Several of them
           hold login credentials that have to be adjusted.

* **0.6)** Guile version 3.0 or newer is required. Most recent distros
           include this version. Otherwise, guile must be built from
           source, which can be obtained from the Guile ftp repo
           https://ftp.gnu.org/gnu/guile/ or with `git`, by doing
```
         git clone git://git.sv.gnu.org/guile.git
```

   Note `par-for-each` hangs:
            https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26616

* **0.7)** Mandatory (!)  You'll work with large datasets; default
           guile is not equipped to deal with the sizes encountered here.
           Skipping this step will lead to the error
           `Too many heap sections: Increase MAXHINCR or MAX_HEAP_SECTS`.
           Fix this with:
```
      git clone https://github.com/ivmai/bdwgc
      cd bdwgc
      git checkout v8.0.4     # (or newer)
      ./autogen.sh
      mkdir build; cd build;
      ../configure --enable-large-config
      make -j; sudo make install
```

* **0.8)** Create/edit the `~/.guile` file and add the content below.
           This makes the arrow keys work, and prints nicer stack traces.
```
      (use-modules (ice-9 readline))
      (activate-readline)
      (debug-enable 'backtrace)
      (read-enable 'positions)
      (add-to-load-path ".")
```

* **0.9)** Install assorted build tools. At this time, the following
           are required:
```
      sudo apt install git cmake g++
      sudo apt install libboost-filesystem-dev libboost-system-dev libboost-thread-dev libboost-program-options-dev
      sudo apt install guile-3.0-dev librocksdb-dev libuuid-dev
      sudo apt install autoconf-archive flex byobu rlwrap telnet
      sudo apt install automake libpcre2-dev libedit-dev
```

* **0.10)** The following AtomSpace and OpenCog components are needed:
            `cogutils`, `atomspace`, `atomspace-rocks`, `cogserver`.
            Each of these follows this generic pattern:
```
      git clone https://github.com/opencog/cogutils
      cd cogutils; mkdir build; cd build
      cmake ..
      make -j
      sudo make install
```
   Repeat the above for each of the other github repos.

* **0.11)** Link-grammar version 5.12.0 or newer is required. It must be
           built after the AtomSpace, as it has a dependency on it. It must
           be built before `lg-atomese`, because `lg-atomese requires
           Link Grammar.

           The currently installed version can be displayed by running
```
      link-parser --version
```

   Newer versions are available at:
           https://www.abisource.com/downloads/link-grammar/

   The main link-grammar project page is here:
           https://www.abisource.com/projects/link-grammar/

   Install as:
```
      tar -zxf link-grammar-5.x.x.tar.gz
      cd link-grammar-5.x.x
      mkdir build; cd build
      ../configure
      make -j
      sudo make install
```

* **0.12)** The `lg-atomese` component. It follws the same generic
      pattern as the other OpenCog components:
```
      git clone https://github.com/opencog/lg-atomese
      cd lg-atomese; mkdir build; cd build
      cmake ..
      make -j
      sudo make install
```

Bulk Pair Counting
------------------
The first major stage of data processing is the generation of word-pair
statistics. Later stages depend on high-quality word-pair statistics,
and this is best obtained by processing a large number of text files;
typically thousands or tens of thousands of them, for run-time of a
few days to a week or two.  For a practice run, half-an-hour is
sufficient, but a half-hours worth of data will be quite low-quality.

Performance: it takes days/weeks because the processing pipeline has
been optimized for experimentation, and not for speed. We are playing
with different algos. Once the "best" algo is selected, hard-coding
it in C++ *will* run 100x faster!

Natural Language Corpora
------------------------
The best text for "natural" English is narrative literature, adventure
and young-adult novels, and newspaper stories. These contain a good
mix of common nouns and verbs, which is needed for conversational
natural language.

It turns out that Wikipedia is a poor choice for a dataset. That's
because the "encyclopedic style" means it contains very few pronouns,
and very few action-verbs (hit, jump, push, take, sing, love). Most
Wikipedia articles state facts, describe objects, ideas and events
(primarily using the verbs is, has, was). It also contains large
numbers of product names, model numbers, geographical place names,
and foreign language words, which do little or nothing for learning
grammar. Finally, it has large numbers of tables and lists of dates,
awards, ceremonies, locations, sports-league names, battles, etc.
that get mistaken for valid sentences (they are not; they are lists),
and leads to unusual deductions of grammar.  Thus, it turns out
Wikipedia is a bad choice for learning text.

There are various scripts in the `download` directory for downloading
and pre-processing texts from Project Gutenberg, Wikipedia, and the
"Archive of Our Own" fan-fiction website.

*  Explore the scripts in the `download` directory. Download or
   otherwise obtain a collection of texts to process. Put them in
   some master directory, and also copy them to the `beta-pages`
   directory.  The name `beta-pages` is not mandatory, but some of
   the default configurations look for files there. During processing,
   files are moved to the `split-articles` directory and finally to the
   `submitted-articles` directory. Processing continues until the scripts
   are interrupted, or until the `beta-pages` directory is empty.

   Little or no pre-processing of the text files are needed. Mostly,
   one just needs to remove HTML markup or other binary crud. The
   pipeline expects all files to be in UTF-8 format.  The pipeline
   does not support any other format; if you have other formats,
   learn how to use the `iconv` command.

   There is no need to perform sentence splitting or other normalization
   (such as down-casing of initial words).  In fact, just about any
   pre-processing will lower the quality of the input data;
   pre-processing is strongly discouraged.

   It is best to split texts into files containing a few hundred to
   at most ten-thousand sentences each; larger files cause trouble
   if the counting needs to be restarted.

   However, if the intent is to scrape web pages or PDF documents,
   take a look at "Ingestum", https://gitlab.com/sorcero/community/ingestum


The Vector Structure Encoded in Pairs
-------------------------------------
Note that any kind of pair `(x,y)` of things `x,y` that have a number
`N(x,y)` associated with the pair can be though of as a matrix from
linear algebra.  That is, `N` is a number, `x` is a row-index, and `y`
is a column-index, for the `(x,y)`'th entry of the matrix `N`.

In the current case, both `x` and `y` are `WordNode` Atoms, and `N(x,y)`
is an observation count, for how often the word-pair `x,y` was observed.
Note that this matrix is **extremely sparse**: if there are 10K words,
there are in principle 10K x 10K = 100M entries; of these, fewer than
a million will have been observed just once, and half of that twice.
(following Zipf's law - a quarter of those observed 4 times, etc.).

Given `N(x,y)`, one can compute the marginal sums `N(x,*)`, `N(*,y)`
and `N(*,*)` (with `*` denoting the wild-card).  The observed frequency
is then given by `p(x,y) = N(x,y)/N(*,*)` -- this is the so-called
"frequentist probability" -- a probability obtained from counting how
often something actually occurred.

Given the `p(x,y)`, one can go on to compute marginal entropies, such
as `H(x,*) = -sum_y p(x,y) log_2 p(x,y)` and so on. Mutual information
is defined similarly.  Please be aware that none of these quantities
are symmetric: in general, `N(x,y)` does NOT equal `N(y,x)`, and that
likewise, `N(x,y)` does NOT equal `N(y,x)`, and thus, in general, the
mutual information will also not be symmetric: `MI(x,y)` does NOT equal
`MI(y,x)`.  This is because the order of the words in a word-pair is
important: words do not come randomly distributed. This can lead to a
considerable amount of confusion, when comparing to the definition of
mutual information given in textbooks, references and Wikipedia:
usually, those references only provide a symmetric definition, suitable
only when events are not sequentially ordered. That definition is **not
the same** as that being used here, although it looks quite similar.
Failure to make note of this can lead to a lot of confusion!


Next Steps
----------
The above provided a basic overview. Detailed steps for actually
running the pipeline are in the [Integrated Language Learning
README](./README-Natural-v4.md).  Go there next.

That's all for now!
-------------------
THE END.
