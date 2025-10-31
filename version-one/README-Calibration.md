
Calibrating Unsupervised Language Learning
==========================================
* Version of May 2021
* Mothballed, late 2021

Mothballed project, halted activity. This was a good idea, but it
foundered on severeal practical and several theoretical issues.
Cannot move foreward, until these are addressed.  See below, "lessons
learned". for a summary of these issues.

See also the
[language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview. See
[Diary Part Two](learn-lang-diary/learn-lang-diary-part-two.pdf)
for a log of the experimental efforts and results.

Project Summary
---------------
In 2019 we realized that training on English corpora does not offer
sufficient control to measure the quality of the learning algorithm.
Thus, we've devised a new approach: create a random grammar, create
a corpus of sentences from that random grammar, learn a grammar from
that corpus, and validate that the learned grammar accurately matches
the input grammar.  Doing this will allow the learning algorithm to
be correctly calibrated for grammars of different sizes and
complexities, and for corpora of different sizes. We will be able to
measure how accuracy scales as a function of training time, how well
different training algorithms perform, how large a corpus is needed to
get good results, and other related questions.

Informally, the idea of calibration here is just as with any other
instrument: you measure a "known quantity", and make sure that the
instrument is reading it accurately.  In this case, the "known quantity"
is a known grammar, and the instrument is the grammar-learning system.

As of May 2021, the infrastructure to do the above is more or less
complete, and mostly automated, and some early calibration runs have
been performed.  Lessons learned so far are given below, right after a
quick overview of the processing stages.

Processing Overview
-------------------
See the [README-Natural](README-Natural.md) file for a description of
the "open-loop" (uncalibrated) processing system. It describes the
processing steps in detail.  Getting good results requires tuning
a variety of parameters, and so calibration needs to be run first.

Calibration is done by creating an artificial grammar, then creating
a text corpus from this grammar, and then attempting to learn a new
grammar from this text corpus, and then assessing accuracy by comparing
the the learned grammar to the generating grammar.

This pipeline has been more-or-less fully set up, perhaps with a few
"rough edges": some cosmetic bugs and some incomplete automation
scripts and faulty instructions. It requires a fair bit of editing
of configuration scripts, to adjust file paths, desirable parameters,
etc.  So far:

1. Build and install link-grammar.  Download the latest Link Grammar
tarball from `http://www.abisource.com/downloads/link-grammar/current`.
Then unpack it and compile it:
```
tar -zxf link-grammar-*.tar.gz
cd link-grammar-*
mkdir build; cd build; ../configure; make -j
sudo make install
```

2. Build and install `cogutils`, `atomspace`, and this project:
```
git clone https://github.com/opencog/cogutils
cd cogutils; mkdir build; cd build; cmake ..; make
sudo make install
```
Repeat the above, with `atomspace` in place of `cogutils`, and again,
with `learn` (this project).

3. Go to the [run/0-config](run/0-config) and review both the
   [run/0-config/0-pipeline.sh](run/0-config/0-pipeline.sh) and the
   [run/0-config/1-dict-conf.scm](run/0-config/1-dict-conf.scm) files.
   The first contains a directory where the generated dictionary should
   be written.  The second contains configurable parameters for
   defining a random grammar. The shell script
   [run/1-gen-dict/gen-dict.sh](run/1-gen-dict/gen-dict.sh) will
   generate the dictionary.  The output is a standard Link Grammar
   Dictionary.

```
$ cd /home/ubuntu/run/1-gen-dict
$ ./gen-dict.sh
```

4. Review [run/0-config/1-corpus-conf.scm](run/0-config/1-corpus-conf.sh)
   for the corpus generation settings. Then run `./gen-corpus.sh`.

5. Run the processing pipeline described in
   [README-Natural](README-Natural.md)

6. Measure accuracy.

So far, steps 1-6 have been automated to varying degrees. Contact us
(me) to discuss details.

Lessons Learned
---------------
The idea of calibration is a good idea, even an excellent idea, but is
considerably more subtle than it first appears.  The following issues
and questions arose fairly quickly.

* It is possible for two seemingly different grammars to generate the
  same corpus. In this case, when learning the grammar, how do we judge
  if it is "correct"? How can we prove that two different grammars are
  equivalent? Is there an algorithm for generating this proof? Is this
  even provable, or is this Turing-undecidable, in the same way that the
  equivalence of two different group presentations is famously known to
  be undecidable? Given that group presentations and groups are a
  special case of grammars and corpora, it would seem that proving the
  equivalence of grammars is also undecidable.  None-the-less, for
  simple grammars, one might hope that ad hoc algos might suffice.

* The current code for generating random grammars has a dozen different
  tunable parameters to control that grammar. They are "common sense"
  parameters, in that they directly control different steps in the
  generation. Yet it has rapidly become clear that most parameter
  settings result in wildly complex and highly chaotic grammars. The
  resulting corpora appear to be highly "mixed" or ergodic. If a corpus
  is ergodic, then, of course, it will be impossible to extract any
  structure from it. How can one measure the ergodicity of a corpus?
  How can one measure the complexity of a grammar?

* The way in which grammars are generated was motivated by a perhaps
  naive understanding of "factorization" - see the paper on sheaves,
  where an analogy is made between Ising models, matrix factorization,
  partition functions and other related concepts. The idea is that the
  word-disjunct matrix M factorizes as M=LCR where L and R are sparse
  high-dimension matrices, and C is low-dimension and dense (compact).
  The idea was to generate the grammars so that they resemble this
  factorization; yet, the generated grammars are likely to have multiple
  ambiguous factorizations. How can we tell if a grammar has multiple
  ambiguous factorizations? How can we find these? Obviously, if a
  grammar has multiple ambiguous factorizations, then the machine that
  attempts to learn that grammar is likely to come up with one of the
  equivalent factorizations. How can we characterize this?

* As noted above, most parameter settings generate complex and seemingly
  ergodic grammars. Just eyeballing these shows that they do not seem
  to resemble English or any other natural language grammar. How can we
  determine if a random grammar is "natural-language-like"? What
  parameter settings result in human-like languages? Are the "axes" of
  adjustable parameters even "aligned" with the axes of human language
  complexity? How does one even judge this?

* A serious, practical impediment is that there is no easy way to just
  "eyeball" the results. With English, one can just look at the stuff
  being generated, and its fairly clear when it's good and when its
  garbage.  But with artificial langauges, it is impossible to glance at
  it and get any idea of what's going on. In order to track what's
  happpening, one would need to build a deep, complex and throuogh set
  of tools, carefully measuring everything at every step, to see if the
  process is working or has gone off the rails.

Additional details, results, questions and head-scartching can be found
in the [Language Learning Diary, Part
Two](learn-lang-diary/learn-lang-diary-part-two.pdf).

It appears that we've stumbled into the classic trap of science: the
more we learn, the more we don't know, and the things we don't know
appear to be increasingly basic and simple. It feels like I haven't
even dented the surface of grammar--corpus correspondence. Onward
through the fog!

That's all for now!
-------------------
THE END.
