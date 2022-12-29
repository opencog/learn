
Unsupervised Natural Language Learning
======================================
* Linas Vepstas December 2013
* Updated May 2017
* Updated June 2018
* Updated February 2021
* Updated November 2022

*** OBSOLETE ***

This is a snapshot of the processing scripts as the were in October 2022.
This featured a discrete pipeline, with distinct processing steps. This
pipeline is no longer tenable, as it has trouble reaching into higher
stages.  The snapshot is here for reference.  Notable changes include:

* Calibrated AKA "closed" learning is mothballed/abandoned, it's not
  feasible as envisioned. The scripts for this processing can be found
  in this directory; they've been removed from the main directory.
* The Postrgres backend is abandoned; its slow, awkward, inflexible.
* Link Grammar takes over the MST parsing from the earlier scheme scripts.
* Many of the TODO items listed below have been done.

See the directory containing this README file for the assorted scripts
and tools that were part of this process.

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
This README describes the overall process flow for "open" natural
language learning. It is more-or-less identical to the "closed-loop"
or "calibrated" artificial-language learning, except that the input
corpus is a sample of natural language text, instead of an artificial
corpus.

To get good results, you need to use a calibrated learner.  That is,
there are many tunable/adjustable parameters in the learning process,
and if you don't pick some good settings, the result will be mediocre
quality. See [README-Calibration](README-Calibration.md) for calibration
details.

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

   Databases for the artificial-language pipeline might be
   smaller, so this is perhaps less of a concern.

* **0.3)** Get a machine with 4 or more CPU cores, and a minimum of
           64GB RAM.  For English-language learning, it will be less
           agonizing and infuriating if you have 128 GB RAM or more.
           Smaller RAM sizes are enough for artificial-language
           learning experiments.

   For English-langauge work, the datasets do get large, and
   although many of the scripts are designed to only fetch from
   disk "on demand", the in-RAM sizes can get large. As I write
   this, my grammatical-clustering process is using 53GB
   resident-in-RAM working-set size (57GB virtual size).
   You need additional RAM for the Postgres server -- 24GB is
   reasonable -- and so 53+24=78GB is the current bare-minimum.
   More, if you want to e.g. run a web-browser or something.

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
     Dohhh!

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
      sudo apt install postgresql-client postgresql libpq-dev pgtop
      sudo apt install autoconf-archive flex byobu rlwrap telnet
      sudo apt install automake libsqlite3-dev libedit-dev
```

* **0.10)** Link-grammar version 5.6.1 or newer is required.
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

   The artificial-language-learning code requires an unpublished prototype
   Get this as
```
      git clone https://github.com/opencog/link-grammar
      cd link-grammar
      git checkout generate
```
This will eventually appear in version 5.9.0.

* **0.11)** The following AtomSpace and OpenCog components are needed:
            `cogutils`, `atomspace`, `atomspace-rocks`, `cogserver`,
            `lg-atomese`. Each of these follows this generic pattern:
```
      git clone https://github.com/opencog/cogutils
      cd cogutils; mkdir build; cd build
      cmake ..
      make -j
      sudo make install
```

   Repeat the above for each of the other github repos.

* **0.12)** Install the guile bindings for sqlite3. This is needed
            so as to export the resulting dictionary to Link Grammar.
```
      git clone https://github.com/opencog/guile-dbi
      cd guile-dbi/guile-dbi
      ./autogen.sh --no-configure
      mkdir build; cd build; ../configure; make
      sudo make install

      cd ../guile-dbd-sqlite3
      ./autogen.sh --no-configure
      mkdir build; cd build; ../configure; make
      sudo make install
```

Setting up the AtomSpace
------------------------
This section describes how to set up the AtomSpace to collect
statistics. Statistics are collected in a database; you have a choice
of two: RocksDB and PostgreSQL. Pick one.

* Although RocksDB seems to be 2x or 3x faster in synthetic benchmarks,
  it is in fact 10% or 20% slower for the current pipeline.

* Postgres requires a more complicated setup. There are more
  instructions to read and follow. If you are new to Postgres, this
  will be confusing and error-prone.

The instructions below describe both databases; skip the parts that
don't apply to you.

These instructions go through a basic "sniff test" to make sure
everything works. It's a small orientation demo.

* **1)** (Postgres only) Set up and configure Postgres, as described in
         [`atomspace/opencog/persist/sql/README.md`](https://github.com/opencog/atomspace/tree/master/opencog/persist/sql/README.md)

* **2)** (Postgres only) Create and initialize a database. Pick any
         name you want; here it is `learn_pairs`.  Later on, you will
         have to place this name into a config file (see further below).
```
      (use-modules (opencog) (opencog persist) (opencog persist-sql))
      (sql-create "postgres:///learn_pairs")
```

* **3)** Start the OpenCog server.  Later on, the batch processing
         instructions (below) indicate how to automate this. However,
         for the practice run, it is better to do all this by hand.

   First, review the contents of
         `run-config/2-cogserver/cogserver-pairs-en.conf`.
         This simply declares the prompts that the cogserver will use;
         most importantly, it declares the port number for the cogserver.
         It's currently coded to be 17005.

   Edit the `run-config/0-pipeline.sh` and `run-config/2-pair-conf-en.sh`
         and hard-code your database credentials into it.

   Finally, start the cogserver by
```
     $ source run-config/0-pipeline.sh
     $ cd run/2-word-pairs
     $ ./run-cogserver.sh
```
   This will start a guile REPL shell, and will have the cogserver
   running in the background. The database will have been opened too.

* **4)** Verify that the pair-counting pipeline works. In a second
         terminal, try this:
```
      rlwrap telnet localhost 17005
      opencog-en> (observe-text "this is a test")
```
   The port number 17005 was from the above-mentioned config file.

   Better yet:
```
      echo -e "(observe-text \"this is a another test\")" |nc localhost 17005
      echo -e "(observe-text \"Bernstein () (1876\")" |nc localhost 17005
      echo -e "(observe-text \"Lietuvos žydų kilmės žurnalistas\")" |nc localhost 17005
```

   This should result in activity in the cogserver. (If running
         Postgres, then the postgres server should be active.)
         The `observe-text` scheme function sends the text for parsing,
         counts the returned word-pairs, and stores them in the
         database.

   If you are curious, go to the guile shell, and type in
         `(cog-report-counts)` to see a summary of the diffent Atom
         types in the AtomSpace.  Database stats are printed by
         `(cog-rocks-stats)` for RocksDB and by `(sql-stats)` for
         Postgres.  Both database reports are highly technical,
         and are mostly useful for debugging, only.

* **5)** (Postgres only) Verify that the above resulted in data sent
         to the SQL database.  Log into the database, and check:
```
      psql en-pairs
      en-pairs=# SELECT * FROM atoms;
      en-pairs=# SELECT COUNT(*) FROM atoms;
      en-pairs=# SELECT * FROM valuations;
```
   The above shows that the database now contains word-counts for
         pair-wise linkages for the above sentences. If the above are
         empty, something is wrong. Go to back to step zero and try again.

* **6)** Halt the cogserver either by exiting the guile shell (with
         control-D or with `(quit)` or with `(exit)`) or by killing
         the guile process.

That's it for the practice run. If stuff is showing up in the database,
then processing is proceeding as expected.

The next step is to set up bulk text processing. There are three general
stages: **(I)** collection of word-pair statistics **(II)** collection of
disjunct statistics **(III)** clustering.  These are described next.

*Note Bene*: Please do NOT spend any time at all trying to figure out
what is stored in the SQL tables.  There are "no user-serviceable parts
inside" and there is "risk of electrical shock". The SQL tables are a
very twisted, distorted and awkward representation of the AtomSpace
contents. Its kind-of like reading assembly code: you will not learn
how the AtomSpace works by reading the SQL code.  In fact, doing this
will probably make you anti-learn, by damaging your brain, and then
planting incorrect ideas into it.  Don't look in there. *You have been
warned!* (This warning applies equally to the RocksDB backend.)


Bulk Pair Counting
------------------
The first major stage of data processing is the generation of word-pair
statistics. Later stages depend on high-quality word-pair statistics,
and this is best obtained by processing a large number of text files;
typically thousands or tens of thousands of them, for run-time of a
few days to a week or two.  For a practice run, half-an-hour is
sufficient, but a half-hours worth of data will be quite low-quality.

There are two basic pipelines: "fake" and "natural". "Fake" uses
artificially generated grammars and text corpora, and is essential for
calibrating and adjusting the learning algorithms to produce the best
results. This is a closed-loop process, where the accuracy of the final
result can be compared to input grammar. "Natural" uses natural-language
corpora downloaded from the web. Since the algorithms are currently
uncalibrated (untuned) you ... get what you get: it will be of uncertain
quality, and there is no practical way to measure the accuracy. There
are instructions below for both pipelines.

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

* **7)** Explore the scripts in the `download` directory. Download or
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


Fake Language Corpora
---------------------
See the instructions in [README-Calibration.md](README-Calibration.md).
Return to here when done.


Running Bulk Pair-counting
--------------------------
Do this.

* **8)** (Beginners should skip this step; needed only for experiments.)
   Review the `observe-text` function in `link-pipeline.scm`. The
   default, as it is, is fine, and this is almost surely what you want.
   (And so you can skip this step).

   The code in `link-pipeline.scm` is written to collect a several
   kinds of information, using several different approaches. Most
   of these variants remain unexplored.

   Caution, though: enabling some of these variants will create a
   deluge of data. Some unsuspecting users were frustrated by the
   explosion in RAM and disk use.

   The `observe-text` function in 'link-pipeline.scm` collects
   observation counts on four different kinds of structures:

   * LG "ANY" word pairs -- how often a word-pair is observed.
   * Word counts -- how often a word is seen.
   * Clique pairs, and pair-lengths -- This counts pairs using the "clique
                  pair" counting method.  The max length between words
                  can be specified. Optionally, the lengths of the pairs
                  can be recorded. Caution: enabling length recording
                  will result in 6x or 20x more data to be collected,
                  if you've set the length to 6 or 20.  That's because
                  any given word pair will be observed at almost any
                  length apart, each of these is an atom with a count.
                  Watch out!
   * Disjunct counts -- how often the random ANY disjuncts are used.
                  You almost surely do not need this.  (This is for my
                  own personal curiosity.)

   The default config is to only count the LG "ANY" word pairs. None
   of the other styles are used in the current pipeline.

   Knowing the length of the pairs might be important for "understanding"
   that adjectives, adverbs, determiners and possessives modify only
   nearby words. They might be important in analyzing morphology.
   Currently, no other code examines lengths; this is an open
   experiment.

   The code was written with flexibility in mind, and not performance.
   Re-writing the word-pair counting to run in straight C++ code
   could improve performance by 100x. This is not a current priority.

* **9)** Configuration. The pipeline is controlled by a collection of
    configuration files. These specify things like the training corpora
    and the databases in which to store intermediate results. Multiple
    experiments can be run at the same time, by twiddling the port
    numbers at which the cogservers are started (being careful to pipe
    the right data to the right server!)

    The config files are located in the `run-config` directory.
    The master config is in `0-pipeline.sh`. The intent is that,
    for each distinct experiment, you can make a copy of the
    `run-config` directory, and adjust file and database locations
    for each experiment by editing the config files.

    Artificial language generation is controlled with the
    `run-config/1-*` files. See
    [README-Calibration.md](README-Calibration.md) is your are
    interested in calibration experiments.

    Pair counting is controlled by the `run-config/2-pair-conf*sh`
    files. Pick one; there are several preconfigured variants.
    Edit as required.

* **10)** Configuration planning and setup.
    Make plans for five different directories. These will be:

    + A directory for helper scripts
    + A directory for running the experiment
    + A directory for the text corpus
    + A directory for database files (if using RocksDB)
    + A directory for per-experiment config files

    The goal of this directory structure is to allow lots of
    experiments to be performed and tracked, without clobbering
    each-other, and while minimizing duplicated/shared files.
    Here is a suggested directory structure, for "Experiment 42":
```
    cp -pr run-common /home/ubuntu/run-common
    cp -pr run /home/ubuntu/run
    mkdir /home/ubuntu/text/expt-42
    mkdir /home/ubuntu/data/expt-42
    cp -pr run-config /home/ubuntu/config/expt-42
```

   + The helper scripts in `run-common` are shared by all experiments.
     They just need to be installed once.
   + The run scripts in `run` are organized by processing stages. They
     are designed so that you can `cd` into that directory, and then
     run the scripts there, as needed.  All of the different
     experiments can use the same `run` directory.
   + Each experiment gets it's own copy of the corpora. This is because
     the corpora files are moved around during processing. They are
     moved around so that progress can be monitored, and so that, if
     the processing pipeline is killed and restarted, previously-handled
     files are not reprocessed.
   + If using RocksDB, each experiment can get it's own database
     directory. This allows the same filenames to get re-used. However,
     you can configure this however you want to.
   + Each experiment gets it's own set of config files.  This is key!
     Each experiment will ... do things differently, with different
     parameters. Some of these include the directory locations above.

   The config files are numbered: `run-config/0-pipeline.sh` is the
   master config file shared by all stages. It will need to be edited
   and sourced. Conclude by exporting it's location:
```
   $ export MASTER_CONFIG_FILE=/home/ubuntu/config/expt-42/0-pipeline.sh
```

* **11)** Processing preliminaries.
    It is convenient to have lots of terminals open and ready for use;
    the `byobu+tmux` terminal server provides this, without chewing up
    a lot of screen real-estate.  The `run-shells.sh` scripts will run
    a `byobu/tmux` session, start the cogserver in one of the terminals,
    and leave open several other terminals for general use.

    Byobu cheat-sheet:
    ++ `F3` -- move left
    ++ `F4` -- move right
    ++ `F7` -- enable the page-up and page-down keys for scrolling.
    ++ `F6` -- detach from byobu. Reattach by saying `tmux attach`.
    ++ `F2` -- create new terminal
    ++ `F8` -- change terminal title

    See `man byobu` for details.

* **12)** Processing.
    Change directory to the `run/2-word-pairs` directory, and start
    tmux/byobu by running `run-shells.sh`.

    If all went well, you should find that the cogserver is running
    in the `cogsrv` tmux terminal.  It should be sitting at the guile
    prompt.

    Change to the `submit` tmux terminal, and start text processing by
    running the `./pair-submit.sh`.

    Depending on the size of the corpus, pair counting can take an hour
    or two, up to days or weeks. It runs at about 5-30 sentences per
    second, depending on the length of the sentence (and, of course,
    the CPU speed, the disk speed, and the database configuration.)

    At this time, it is not known how big a sample should be taken to
    get accurate results. It seems like a few days of processing for
    natural language (English) seems to be OK, while a few weeks is
    better. Too-large a sample will slow down some of the later stages,
    however, and the databases might get unmanagebly large.
    Experimentation is needed.

    The `pair-submit.sh` script will run until all of the text files
    have been processed, or until it is interrupted.  If it is
    interrupted, it can be restarted; it will resume with the previous
    unfinished text file. As text files are being processed, they are
    moved to the `pair-split` directory (this is configurable) and then,
    to the `pair-submitted` directory (again, configurable).   The
    input directory will gradually empty out, the `pair-submitted`
    directory will gradually fill up.  Progress can be monitored by
    saying `find |wc`.

    There are several functions to monitor parsing from scheme:
```
    (monitor-parse-rate "some message")
    (report-avg-gc-cpu-time)
```

  For a trial run, a half-hour or so is sufficient. Feel free to
  ctrl-C at this point, and move to the next step
  (covered in the section **Mutual Information** below.)

Notes:
* Segmentation for Chinese. There are two possibilities, here. One is
  to try to segment; the other is to ignore the problem, and try to
  learn the grammar on a hanzi-by-hanzi basis. In principle, the
  resulting grammar should be able to detect word boundaries by noting
  that intra-word MI is large, and inter-word MI is much lower. These
  experiments have not been run to conclusion. See the section
  'Morphology' immediately below.

  Alternately, you can try to use an off-the-shelf segmenter.  This
  gives fair accuracy, but not really all that good.

  This can be done using jieba
      [https://github.com/fxsjy/jieba](https://github.com/fxsjy/jieba)
  If you are working with Chinese texts, install:
  `pip install jieba` and then segment text:
  `run/jieba-segment.py PATH-IN PATH-OUT`. This python utility is in
  the `run` directory.  You will need to create modified versions of
  the `run/pair-one.sh` and `run/mst-one.sh` scripts to invoke jieba.

  To run this segmenter, you will also need to run the sentence-splitter
  differently.  Use the `zh-pre` or `yue-pre` languages with the
  sentence splitter -- these will split on sentence boundaries, only,
  and do no other processing.  By contrast, the `zh` or `yue` languages
  insert a blank space between Hanzi characters.

* Morphology. Some work on morphology has been done; but there are no
  pre-written scripts for the processing pipeline to handle this. You
  are on your own, for now.

  Morphology for those languages that need it (Russian, French, etc.)
  resembles the Chinese word-segmentation problem.

Bulk Pair Counting - Quickstart/Cheat Sheet
-------------------------------------------
* (Postgres only) Don't forget to perform the Postgres tuning
  recommendations found in various online Postgres performance wikis,
  or in the `atomspace/opencog/persist/sql/README.md` file.

* Use LXC containers, one for each language. Buy SSD disks. Buy an UPS.
  Install the system shutdown scripts.

* (Postgres only) Set up distinct databases, one for each language:
```
      (use-modules (opencog) (opencog persist) (opencog persist-sql))
      (sql-create "postgres:///en_word_pairs")
```

* Copy input texts to the `beta-pages` directory (or another directory,
  if you wish).

* Copy the `run/0-config` and edit to suit.

* Run `run-shells.sh`.

* Run `pair-submit.sh` in the tmux/byobu `submit` terminal. Wait until
  done.

If the above generates the error
```
      nc: invalid option -- 'N'
```
then edit `run/common/file-split-process.sh` and `file-nosplit-process.sh`
and remove the `-N` option from the `nc` command.  Some versions of `nc`
require the `-N` flag: the scripts will just hang without it. Other
versions of `nc` are unaware of the `-N` option, and don't need it.
This is an `nc` design flaw.

Mutual Information of Word Pairs
--------------------------------
After observing word-pair counts, the mutual information (mutual
entropy) between them needs to be computed.  This is a required step
before disjunct observation can be started.

Do this by running the `run/2-word-pairs/compute-marginals.sh` script.
Batch-counting might take hours or longer (maybe over a day), depending
on your dataset size. Small trial-run datasets should take no more than
5-10 minutes... or seconds, for small artificial languages. This script
prints a lot of output, including a technical summary at the end.

The above might double the size of the database, as it sits on disk --
adding frequencies and mutual-information values to word pairs will
increase the the storage size of each word-pair by a lot.  In addition,
the marginal stats are not insignificant in size.

Thus, you may want to:

* Check that you have enough storage allocated for the database.

* Make a copy of the word-pair-only database, so that you can return
  to it, if you decide that you need to return to it for some reason.
  RocksDB databases can be copied with the `cp -pr` command. Postgres
  databases can be copied with the `createdb -T` command.

It appears that trimming the word-pair database may have a small but
positive effect. Trimming to remove all words that have been observed
ten or fewer times, and all word-pairs that have been observed 4 or
fewer times will raise the average MI of the entire dataset, and cut
the number of pairs in it by about half.  As the bulk of the dataset is
word-pairs, this smaller number will reduce database size. The downside
is that trimming requires some extra CPU-intensive steps:marginals have
to be computed both before and after the trim. Trim by running
`run/2-word-pairs/trim-dataset.sh`.

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

Anyway ... the `batch-pairs` script computes all of these marginals.
The `print-matrix-summary-report` reports on the resulting matrix.
Note that both of these routines are generic: they will work for
any collection of pairs of atoms, and not just `WordNode`s. What's
more, the type of `x` and the type of `y` don't even have to be the
same; these can be two different types of Atoms.  Nor do the two atoms
have to be yoked by a single `Link`; they can exist as two distinct
atoms embedded in more complex AtomSpace patterns of any kind.  The
`matrix` subsystem slaps a two-D matrix API onto arbitrary patterns
in the AtomSpace, allowing essentially any complex graphical shape
to be viewed as a collection of vectors.

Again, this is a key insight: the axioms of graph-theory are somewhat
similar to the axioms of a vector space (but are not the same). This
similarity can be used and abused: the `matrix` subsystem allows
portions of a graph to be viewed as a collection of vectors.  The
tools then allow probabilities, entropies and mutual information to
be computed for these vectors.

Exploring Word-pair Mutual Information
--------------------------------------
After word-pairs have been counted, and the mutual information between
them has been computed, the results can be explored manually, so as to
understand the process or to verify expected results. Here's how.
Working from a guile shell that has the pair-count database open:
```
(define ala (make-any-link-api))
(define asa (add-pair-stars ala))
(define also (add-support-api asa))
(define alf (add-pair-freq-api also))

(ala 'fetch-pairs)
```
View the count on a word-pair:
```
(ala 'pair-count (Word "the") (Word "thing"))
```
View the actual pair that is actually holding the count:
```
(ala 'get-pair (Word "the") (Word "thing"))
```
View marginal counts, and the word-pair MI:
```
(alf 'left-count (Word "the"))
(alf 'wild-wild-count)
(alf 'pair-fmi (alf 'get-pair (Word "the") (Word "thing")))
(alf 'right-wild-fmi (Word "the"))
(alf 'left-wild-fmi (Word "thing"))
```
Print documentation about other methods:
```
(asa 'help)
,d add-pair-stars
(alf 'help)
,d add-pair-freq-api
,d add-support-api
```
The above API works for any kind of pairs held in the AtomSpace.
The `(make-any-link-api)` function makes an API for word-pairs.
The `(make-pseudo-cset-api)` function make an API for pairs, where
the left item is a word, and the right item is a connector-set
(aka disjunct). Other pairs that are currently available are:
`make-clique-pair-api`, `make-distance-pair-api`,
`make-gram-class-api` and `add-shape-vec-api` and there may be
more in the future.

Maximum Spanning Trees
----------------------
The next step of the processing pipeline is to determine some
provisional, plausible disjuncts that can be associated with words.
A disjunct can be thought of as "just like an N-gram" or "just like a
skip-gram", except that (unlike N-grams/skip-grams) the disjunct
contains additional connectivity information.  The disjunct indicates
which words are allowed to "connect" to which other words, and in
what order those connections can be performed.  It is the connectivity
information between words that captures the grammar of a language.

To be clear: if one has explicit and accurate information about the
connectivity between words, then it is straight-forward to convert
that information into a natural-language grammar, in any one of a
number of competing systems, such as Head-Phrase Structure Grammar
(HPSG), or Categorial Grammar (CG), or Dependency Grammar (DG). The
primary focus here is on Link Grammar (LG), which can be thought of
as a kind of dependency grammar. All of these different systems are
roughly equivalent, and grammars expressed in one of the systems can
be transformed into grammars expressed in one of the others, in a
fairly straight-forward, purely mechanical way.

Thus, the goal is to obtain reasonably-accurate connectivity information
between words, accumulating observation counts, so that, eventually,
the most likely word-usages and word-connectivity emerge from the noise.
This again requires crunching a lot of raw text.

Semi-accurate connectivity information can be obtained by MST parsing.
The result of such parsing is OK, but falls short of being great; this
has been demonstrated a number of times, most clearly and notably in
Deniz Yuret's PhD thesis.  The goal of this processing step is to
accumulate connectivity information from a very large number of MST
parses, with the hope that the incorrect/conflicting parses will average
each-other out (destructive interference) while the correct parses will
reinforce and become statistically clear (constructive interference).
(And this is indeed what actually happens!)

This is done by constructing a Maximum Spanning Tree (MST) between the
words in a sentence.  The tree contains just enough edges to connect
every word in the sentence.  The quantity being maximized is the sum of
the mutual information between word-pairs.  There is exactly one such
tree that maximizes the total MI.

After obtaining such a tree, each edge is "cut in half", leaving behind
a word, with dangling, disconnected half-edges attached to it. The
collection of dangling half-edges is called a "disjunct"; each half-
edge is called a "connector" (some parts of the code use the words
"connector set", "connector sequence" and "section" as synonyms for
"disjunct". Sometimes, they are called "pseudo-disjuncts" (and "pseudo-
connectors"), to distinguish from "real" connectors and disjuncts,
defined later.  This is a historical artifact.)

The specific disjunct that remains behind depends very strongly on the
word itself (of course) and on the MST tree for the sentence that the
word was in. The code in this part of the pipeline accumulates
observation counts on the large variety of possible disjuncts (that is,
word-disjunct pairs) that can be observed.

MST Disjunct Counting
---------------------
The overall processing is very similar to batch pair-counting. Scripts
for performing this are located in the `run/3-mst-parsing` directory.

The steps are as follows:

* Pick one of the configuration files `run-config/3-*sh` files.
  If needed, adjust the configuration. The default configuration
  should work well, without adjustment. Here `mpg` is "maximal planar
  graph" and `mst` is "maximal spanning tree". It seems that `mpg`
  works better. Maybe.

* Make a copy of the database containing word-pair mutual information.
  The disjunct observation counts will be accumulated into the database,
  further expanding it's size.  You probably want to keep a copy of the
  original, "just in case". You can copy RocksDB databases with `cp -pr`
  and Postgres databases with `createdb -T`.

* Review `run-mst-shells.sh` and (optionally) change it as desired.
  Run it.  It will start a tmux/byobu session. One of the tmux terminals
  will start a cogserver. This may stall for a while, as it loads
  word-pairs into RAM. This may take from seconds to over an hour,
  depending on the number of word-pairs and the database backend.

* If you don't want to run byobu/tmux, just start the cogserver with
  `run-mst-cogserver.sh`.

* Once the word-pair loading is done, MST parsing can be started.
  When word-pair loading finishes, a summary report will be printed,
  followed by a guile prompt. After this appears, flip to the `submit`
  tmux/byobu terminal, and run the `./mst-submit.sh` script. This
  will pass all of the files in the corpus through the MST parsing
  stage, accumulating disjunct counts as it goes.

  You can monitor progress by saying `find|wc`.  If you interrupt
  processing, you can safely resume it -- it will pick up again, with
  the last unprocessed file.

  At this time, it is not known how much data is needed to get a good
  sampling. Experiments with English have used several days worth of
  processing, and this seemed OK. The small artificial corpora can
  be processed in an hour or two. For a trial-run, run this for a
  half-hour, and kill it, and move to the next step.

Disjunct Marginal Statistics
----------------------------
The next steps require marginal statistics to be available for the
disjuncts. The previous step gathered a large number of observation
counts for disjuncts. These need to be summarized, so that one obtains
per-word statistics.  Marginals will be needed both for the
pseudo-csets and (optionally) for the cross-connector sets.

The first part is automated in `3-mst-parsing/compute-mst-marginals.sh`.
Run that script. Its a wrapper around this:
```
      (cog-rocks-open "rocks:///home/ubuntu/data/expt-42/fake_pairs.rdb")
      (define pca (make-pseudo-cset-api))
      (define psa (add-pair-stars pca))
      (psa 'fetch-pairs)
      ((add-support-compute psa) 'cache-all)
      (define btr (batch-transpose psa))
      (btr 'mmt-marginals)
```
Experimentation with cross-connectors is in progress.

To obtain the marginals for the cross-connectors, repeat the above
steps, but this time with `(add-shape-vec-api (make-pseudo-cset-api))`
as the base class.  At this time, the need for the cross-connectors
(shapes) is optional, but recommended. It seems to improve accuracy,
at the cost of a much larger AtomSpace. There are roughly 2 to 5
CrossSections created for each Section (since each connector in
a connector-sequence can be a wild-card, and a word typically has 2-5
connectors). This can double or quadruple the size of a dataset
(double or quadruple RAM usage.)

The above computations may take hours or days, depending on the size
of the disjunct set.  Be sure to make a backup copy of the resulting
database before proceeding to the next step.

(Optional but strongly recommended) Trimming Disjunct Datasets
--------------------------------------------------------------
The previous step can result in unmanagebly large datasets. These can be
(and should be) trimmed to a smaller size. More than 90% of all disjuncts
are observed only once! Thus, removing these offers a tremendous data
reduction. Removing all disjuncts that appear only once not only shrinks
datasets by a lot, but allow later steps to run much more efficiently!
(Trimming more than this might not be a good idea. See the Diary entries
in Part Two, starting in June 2021, continuing into Part Three, Sept.
2021.)

```
; Utility to perform trimming.
(define (iter-trim a b c)
   (define pca (make-pseudo-cset-api))
   (define psa (add-pair-stars pca))
	(define trm (add-trimmer psa))
   (trm 'support-trim a b c)
   (trm 'subtotal-trim a b c)
   (trim-linkage psa)
   (set! pca (make-pseudo-cset-api))
   (set! psa (add-pair-stars pca))
   ((add-support-compute psa) 'cache-all)
   (print-matrix-summary-report psa)
   (inexact->exact (round ((add-support-api psa) 'total-support-left)))
)
```
The above can leave unwanted cruft in the database, such as Connectors
that are not in any connector sequences, or WordNodes that aren't in
the matrix. These can be removed with the scripts in
`scm/gram-class/cleanup.scm`. Use at your disgression.

The following sequence will perform the trim, as well as removing
connectors that cannot connect to anything. Running this is slow:
it may take hours for mid-size datasets.

The trim used below keeps all words observed more than once, all
disjuncts observed more than once, and word-disjunct pairs observed
more than once.  The initial dataset is called `all_disjunct_dataset`.

```
   $ cp -pr full_dataset.rdb trimmed_dataset.rdb
```
Then, in guile:
```
   (cog-rocks-open "rocks:///where/ever/trimmed_dataset.rdb")
   (define pca (make-pseudo-cset-api))
   (define psa (add-pair-stars pca))
   (psa 'fetch-pairs)        ;; Load the dataset
   (define psc (add-support-compute psa))
   (psc 'cache-all)          ;; compute subtotals

   ; Now, repeatedly trim. This will settle down after 7 or 8
   ; iterations. The problem is that each trim leaves behind
   ; unconnectable words, which have to be removed, thus
   ; throwing off the counts.
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
   (iter-trim 1 1 1)
```
If you are also keeping word-word pairs in the same dataset as
word-disjunct pairs, then some additional work is needed to keep these
two in sync. The problem is that some words never appear in
word-disjunct pairs, and so were never trimmed to begin with.  Get rid
of these by saying:
```
   (load-atoms-of-type 'WordNode) ; load words that are not in disjuncts.
   (for-each
      (lambda (base)
         (if (and (cog-atom? base)
               (equal? 0 (cog-incoming-size-by-type base 'Section)))
            (cog-delete-recursive! base)))
      (cog-get-atoms 'WordNode))
```
After the above trim, the marginals for word pairs will be all wrong.
These now need to be recomputed, as described earlier.

After trimming, the word-disjunct marginals, and the MM^T marginals will
be stale.  These need to be recomputed, as well (as described earlier)
```
   ((batch-transpose psa) 'mmt-marginals) ;; Word-pair entropies
   (cog-rocks-close)
   ^D                         ;; Exit.
```

If you don't actually recompute the MM^T marginals at this stage, then
you should at least store the trimmed marginals:
```
	((make-store psa) 'store-wildcards)
```
Seriously, though, you really need the MM^T marginals for the later
stages to work.

After the above steps, a typical dataset might contain 15K words and
about a million disjuncts. The total AtomSpace size will be about 4
million atoms; this will require about 4GB of RAM. This is quite
manageable. Datasets before trimming may be ten times larger!

Determining Grammatical Classes
-------------------------------
The point of counting disjuncts is to obtain a good statistical sampling
of what words connect to what words: a big collection of these "dangling
cut-in-half edges" that make up a disjunct.  With this dataset, you now
have a pretty good idea of which words typically attach to other words,
and how many connectors they need to do this.  This dataset has captured
the syntactic structure of the language, on a word-by-word basis,
complete with statistical information about how often each disjunct
occurs, relative to all the others.

Typically, this is a large dataset, and it still contains some fair
amount of noise (although it contains less noise than a single MST
tree -- all of those bad MST parses are beginning to average out).
Further averaging and noise reduction (and thus, higher accuracy)
can be achieved by classifying individual words into grammatical
categories. Doing so, however, also requires that the disjuncts also
be classified: disjuncts consist of connectors, each connector
specifying just a single word. The words in the connectors **also**
have to be merged into grammatical categories. What's more, these
categories have to be consistent with the categories obtained from
clustering the words.

This requirement makes it difficult to apply off-the-shelf clustering
software. Thus, a number of different clustering algos have been coded
up in this project.  All of them are "agglomerative", and have three
basic steps:

1. Compute similarities between word-vectors.

2. Pick N similar words.

3. Merge these into a cluster.

4. Recompute similarities, and go to step 2.

The primary difficulties are avoiding wasting mind-boggling amounts
of CPU time computing similarities, and performing the actual merge.
The first problem is solved by only considering a limited number of
similarities, those between the top-ranked words (ranked by frequency.)

The actual merge is far more difficult. Basic concepts are sketched
below.  The paper on "Sheaf Theory", located at
https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/sheaves.pdf
explains this in greater detail.  Somwehere there is a blog
post on this. (XXX where?)

### Word-vectors

The collection of (word,disjunct) pairs can be thought of as a
collection of vectors, one per word.  The observed frequency `p(w,d)`
for word `w` and disjunct `d` can be thought of as the vector magnitude
for the basis element `d`. That is, a word `w` can be represented as
the vector
```
      v = p(w,d_1) e_1 + p(w,d_2) e_2 + ... p(w, d_n) e_n
```
The `e_k` are the basis elements of the vector space.  There is exactly
one such basis element for each disjunct `d_k`, and so one can safely
think of the `d_k` and the `e_k` as being the same thing. We use the
notation `e_k` here because this is the textbook-standard way of writing
the basis vector of a vector space.  Its the same thing. The `e_k` are
basis vectors.

There is one such vector per word. The probabilities `p(w,d)` are
experimentally determined.  Since there is a vector per word, all of the
industry-standard concepts can be applied: one can compute the cosine
angle between two words. One can compute the Jaccard distance.  One
can compute the (symmetric!) mutual information (MI) between two words
(the Kullbeck-Liebler divergence).  All of these different metrics give
a judgement of how similar or different two words might be.

Experimental work indicates that cosine-distance is terrible, and that
MI works pretty good. The Jaccard distance might be better; this is
supported by some experiments, but those are poorly designed.

### Clustering

It is tempting to apply industry-standard tools to obtain clusters.
For example, one could do K-means clustering, if one wished.  One
could take the famous, well-known Adagram or Word2Vec algorithms, and
replace the n-grams/skip-grams in these algos by the disjuncts. That is,
the disjunct-vector above is a lot like a traditional n-gram/skip-gram.
 If one applies these, or other neural-net techniques to the disjunct
vectors, one would likely get results that are very similar to what the
current n-gram/skip-gram techniques are producing.  The results would
be similar because the "statistical power" in the word-disjunct vectors
is very similar to the statistical power in a skip-gram. They really
are not all that different.

***HOWEVER...***
There is one hugely-important, critical difference.

The basis elements `e_k` are not indivisible, opaque, meaningless
boxes.  The have a structure!  They consist of sequences of connectors!
Those connectors are words! When the clustering/classification step
is performed, one must **not only** classify according to
word-similarity, as revealed by the vector, **but also** to classify
in such a fashion that the basis elements are renumbered, restructured
as well.  The vector space itself is not static: it is mutating and
changing shape, as classification proceeds, because the `e_k`
themselves **are not constants**.

This is the reason that applying K-means, PCA/SVM kernel methods on
the word-disjunct vectors is both boring and also counter-productive.
Disjuncts behave a whole lot like skip-grams (well, they might be,
maybe, a teeny-weeny bit more accurate. Maybe). Applying these
traditional algos to the word-disjunct dataset will give traditional
results, with traditional accuracies. Word-disjunct pairs, used in this
way, will not provide break-out performance or results.

The whole point of computing disjuncts is to get past these vector-based
algorithms, and to enable graph-based, connectivity-based classification
and clustering.  The point is to start with a network, and to extract
the structure of the network, accurately, by making explicit use of the
network connectivity.  Disjuncts do this.  The correct algorithm
requires that both the words be clustered, and also, at the same time,
for the connectors to be clustered, with both steps being done
simultaneously, in a coherent fashion.  During classification, portions
of the dataset resemble vector spaces, and that's OK.  This can be
leveraged and used. However, the vector spaces stitch together in a
very non-linear kind of way.  The disjuncts/connectors show you
exactly how they stitch together.

The correct determination of grammatical classes requires that the
stitching of this fabric be accounted for. The paper on 'sheaf theory'
tries to explain this in greater detail. The code here implements
these ideas.

Exploring Word-Word Distances
-----------------------------
The classic word-word distance is the cosine distance. As the above
explains, this works rather poorly. But you can play around with it,
anyway:
```
      (define pco (add-pair-cosine-compute psa))
      (pco 'right-cosine (Word "this") (Word "that"))
      (pco 'right-cosine (Word "he") (Word "she"))
```
A superior measure to the cosine-distance is the mutual information
between word-disjunct vectors. This is computed using the same vectors,
and a similar dot-product, but is weighted differently, in a way that
makes more sense for probabilities.
```
      (define pmi (add-symmetric-mi-compute psa))
      (pmi 'mmt-fmi (Word "this") (Word "that"))
```

The Diary Part One, Two and Three each explore the statistics and
structure of these vectors. The code that was used to create the
various graphs can be found scattered in the
[learn-lang-diary/utils](learn-lang-diary/utils/) directory, primarily
in [disjunct-stats.scm](learn-lang-diary/utils/disjunct-stats.scm).


Creating Grammatical Classes
----------------------------
The development of the clustering code, for isolating grammatical
classes, is winding down.  Things seem to be working. There are a few
ideas that still need to be coded. There are probably some bugs still
lurking.  The instructions here are provisional and subject to change.

* Make a copy of the table holding the disjunct statistics. This is
  critical! The clustering algorithm(s) actively alter the per-word
  statistics as they proceed. The database contents will be scrambled
  in such a way that the original word-disjunct stats will be lost.
```
    cp -pr mpg_parse.rdb gram-1.rdb
```

* At the guile prompt:
```
      (use-modules (opencog) (opencog persist) (opencog persist-rocks))
      (use-modules (opencog nlp) (opencog nlp learn))
      (use-modules (opencog matrix))
      (cog-rocks-open "rocks:///home/ubuntu/data/expt-8/gram-1.rdb")
```
  and then
```
      (use-modules (srfi srfi-1))
      (define pca (make-pseudo-cset-api))
      (define pcs (add-pair-stars pca))
      (define sha (add-covering-sections pcs))
      (sha 'fetch-pairs)
      (sha 'explode-sections)

      (if (forgot-to-do-mmt-marginals-yet?)
         ((batch-transpose sha) 'mmt-marginals)
      )

      ; Create 500 grammatical clusters
      ; This many will take days or more!  You don't have to do this many!
      ; This will print lots of diagnostics!
      (in-group-cluster sha 0.7 0.2 3 200 500)
```
  The meanings of the parameters are explained in the function
  documentation; See the [agglo-rank.scm](scm/agglo-rank.scm) file.

  To automate the above, configure the paramters in the `run-config`
  directory, and then run `cd run-common; guile -l cogserver-gram.scm`.

  This will take days to run. You can start poking at the results
  earlier, though. The file `learn-lang-diary/word-classes/word-classes.scm`
  contains an ad-hoc assortment of tools that can be used to examine
  the word-classes discovered so far.  Read it for details. See also
  Diary Part Five; there's a large number of statistics collected during
  the merge; logs of these stats can be dumped with the code in
  `scm/gram-class/log-merge.scm`.

  Note that, because the above alters word-vectors on the fly, the
  MI between words will change over time, and thus might not actually
  be what you expect. The clustering step splits words up into distinct
  word-senses based on the grammatical category to which disjuncts are
  assigned. Thus, words that can be both nouns and verbs are typically
  split into two or more clusters, at this stage, and what remains of
  the original word might be just some random noise that was unassigned.

Export to Link Grammar
----------------------
At the conclusion of clustering, the results can be exported to Link
Grammar. This can be done as follows:

* Stop the cluster process. As currently designed above, it will run
  for far longer than you will ever care to wait.  Clustering is slow.
  Copy the database, so the steps below do not accidentally corrupt
  the original.

* Make sure that frequently-occuring words that have not been assigned
  to any cluster will be included in a WordClass of their own
  (singleton word classes).  The following includes all words observed
  more than 500 times:
```
       (define pca (make-pseudo-cset-api))
       (define cvs (add-covering-sections pca))
       (cvs 'fetch-pairs)
       (cvs 'explode-sections)
       (define asc (add-singleton-classes cvs))
       (asc 'create-hi-count-singles 500)
       (cvs 'implode-sections)
```

* Compute costs. The costs are needed to tell Link Grammar how likely
  any given disjunct is. Link Grammar ranks it's parses by liklihood.
  Compute the costs as follows:
```
       (define gcf (add-class-filter cvs #t))
       (batch-all-pair-mi gcf)
```
  The above may take (many) hours or more to run, depending linearly
  on the number of observed disjuncts.

* Compute costs (alternate version). Filtering keeps the entire word-
  disjunct matrix, exposing only a subset of it through a window
  defined by the filter. An alternative is to just remove the unwanted
  matrix entries outright. This results in a smaller dataset, on which
  computations can be much faster.  Be sure to make a copy of the
  original dataset first!

  The below will delete (with `cog-delete!`) all matrix entries that
  are not word-classes.
```
       (define (is-word-class? ITEM) (eq? 'WordClassNode (cog-type ITEM)))
       (linking-trim cvs is-word-class?)
       (batch-all-pair-mi cvs)
```

* Make sure you have `libsqlite3-dev` and `dbi` installed. The `sqlite3`
  database provides a fast file format for Link Grammar dictionaries.
  The `dbi` module provides guile with database interfaces for popular
  databases.  It is needed to write out the Link Grammar dataset.
```
	sudo apt install libsqlite3-dev
   git clone https://github.com/opencog/guile-dbi
```
  Follow the instructions in the README. It's easy and fast. Build the
  sqlite3 bindings.

* Run the following:
```
      (use-modules (opencog nlp lg-export))
      (export-csets gcf "/tmp/dict.db" "EN_us")
```
  Then, in bash:
```
       cp -pr /usr/local/share/link-grammar/demo-sql ./some-place
       cp /tmp/dict.db ./some-place
       link-parser ./some-place
```

### Unclustered Dictionaries
It's vaguely entertaining to create a dictionary from the results of
the spanning-graph parses, before clustering is done. All this needs
is to rerun the export steps, on a copy of the dataset that has not
been altered by the clustering code.  Some suggestions for playing
around:
```
      (load-atoms-of-type 'WordNode)  ; Load words from database.
      (length (cog-get-atoms 'WordNode))  ; How many words are there?

      (define pca (make-pseudo-cset-api))
      (define pss (add-pair-stars pca))
      (pss 'fetch-pairs)
      (cog-report-counts)
      ; This reports  137078 WordNodes,
      ;               268592 Connectors,
      ;              6239997 ConnectorSeq,
      ;             11642010 Sections
      ; for my `mrg_tst` database.
```
  The `lang-learn-diary/disjunct-stats.scm` file contains ad-hoc code
  used to prepare the summary report.  To use it, just cut-n-paste to
  the guile prompt, to get it to do things.

  The marginal entropies and the mutual information between words and
  disjuncts can be computed in the same way that it's done for word-
  pairs, by using the `(batch-pairs ...)` function. However, that
  function does more than is strictly needed, and one can save some
  disk space and CPU time by computing only the word-similarities:
```
      (define pca (make-pseudo-cset-api))
      (define psa (add-pair-stars pca))
      (define btp (batch-transpose psa))
      (btp 'mmt-marginals)
```
  Again, `(w,d)` is just a pair. Like other pairs, its a matrix, and
  has marginal probabilities associated with it.

* The file `learn-lang-diary/word-classes/word-classes.scm` contains
  utilities for displaying word classes. See, for example
  `(prt-all-classes)` and `(prt-multi-members)`


TODO
----
Some things in the pipeline, but unfinished:

* Read
  [Graph Models vs. Gradient Descent](https://github.com/opencog/opencog/raw/master/opencog/nlp/learn/learn-lang-diary/skippy.pdf)
  document.

* The clustering algos above already perform word-sense factoring.
  Explicit word-senses can be identified by looking at multi-cluster
  membership.  These can now start to be used for re-parsing, to
  obtain word-sense pair-correlation statistics.

  Explain the above, document it more clearly.

* More unit tests are needed. I guess. Most things are tested by
  daily use. Testing things that are not stable is pointless.

TODO - Clustering
-----------------
(Finish) implementing clique clustering. See `cliques.scm`.

TODO - Export to Link Grammar
-----------------------------
Make sure the above instructions are still correct. Make sure that
everything works.

TODO - Quality Evaluation
-------------------------
A reasonable next step is to run raw text through this parser,
accumulate statistics on the individual disjuncts, and see how they
stack up against the original stats. Are some disjuncts being used far
more often? Far less often? If so, what does that mean?

Can two different dictionaries be compared? Surely, some will give
different parses than others; where do they differ? Why?

Given a stable dictionary, an obvious next step is to attempt to
perform reference resolution.

See [README-Calibration](README-Calibration.md) for more about
quality calibration.


TODO -- Measuring Quality
-------------------------
Current efforts are focused on judging the quality of the results. Manual
inspection looks pretty good. Manual inspection is not enough to allow
the fine-tuning of the algorithms and parameters. Measuring results
against linguist-created corpora and grammars is problematic: the
child-directed speech corpora is too small, and newspaper-English
grammars do not correctly describe children's speech. Full corpora
with complex grammars are hard to evaluate, as issues with synonyms,
synonymous phrases, word-sense disambiguation, quoted speech, and
reference resolution arise. How can the quality of the results be
assessed? Linguist-created grammars are insufficient to control and
evaluate results.

To bypass this deep and fundamental measurement issue, work has begun
on creating infrastructure for generating random grammars (with closely
controlled grammaical behaviors) and sample corpora from those
grammars. Thus, the known-input grammar can be closely controlled;
will the learning system reproduce this known-input grammar? This work
is located in the ["fake"](./fake) directory. It depends on the
[corpus-generation](https://github.com/opencog/generate) code, currently
under development.


Precomputed LXC containers
--------------------------
For your computing pleasure, there are some LXC containers that you
can download that have a fully-configured, functioning system on them.
The set of available containers will change over time.  The first one
actually has a bunch of bugs and other problems with it, but it's
enough to do some basic work. The steps are:

* You'll still need to satisfy at least some of the hardware
  requirements listed previously.

* Install lxc, Like so:
```
       sudo apt-get install lxc
```

* Download a container from
  https://linas.org/lxc-nlp-containers/
  *** XXX No. These are all stale and out of date. *** Contact me for
  something modern.

* The following describe root-owned containers. You can also have
  user-owned containers, if you know how to do that.

* Go to the directory `/var/lib/lxc/lxc`.  You may need to create
  this directory.  Unpack the file you downloaded above, in this
  directory. Do this as root. Do NOT change the ownership of the
  files!  Avoid changing the date-stamps on the files.

* Type in: `sudo lxc-ls -f` You should see a display similar to this:
```
      NAME           STATE   AUTOSTART GROUPS IPV4      IPV6
      simil-en       STOPPED 0         -      -         -
```

* Start the container by saying `lxc-start -n simil-en`. Give it
  5 or 10 or 30 seconds to boot up. Then `lxc-ls -f` again. You
  should see the below. It may take a minute for the IPV4 address to
  show up. You will probably get a different IP than the one shown.
```
      NAME           STATE   AUTOSTART GROUPS IPV4      IPV6
      simil-en       RUNNING 0         -      10.0.3.89 -
```

* You can now `ssh` into this container, just as if it were some
  other machine on your network.  It more-or-less is another machine
  on your network.
```
      ssh ubuntu@10.0.3.89
```
  The password is `asdfasdf`.

* The run-scripts are in the `run` directory.  The opencog sources
  are in the `src` directory. You can `git pull; cmake..; make` these
  if you wish. Or not. Its all set up already.  Pull only if you need
  to get the latest.

  (For example, this README file, that you are reading, is in
  `/home/ubuntu/src/learn/README`. Right now.)

* You can now hop directly to the section "Exploring Connector-Sets"
  above, and just go for it.  Everything should be set and done.
  Well, you do need to start guile, of course, etc.


That's all for now!
-------------------
THE END.
