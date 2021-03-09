
Calibrating Unsupervised Language Learning
==========================================
* Version of February 2021

Ongoing project, continuing activity.  See the
[language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview. See the diary at
`learn-lang-diary/learn-lang-diary-part-two.lyx` for a progress log.

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
different training algorithms perform, how large a corpus is neeed to
get good results, and other related questions.

As of 2021, we are still setting up the infrastruture to do the above.
Once this is done (real soon now?) the project can resume training runs.
Please contact via email or discord opencog chat for details.

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

This pipeline is in the process of being set up. It has been partly
automated. Currently requires a lot of editing to adjust file paths,
etc.  So far:

1. Build the corpus generation tools, as follows:
```
git clone https://github.com/opencog/link-grammar
cd link-grammar
git checkout generate
./autogen.sh --no-configure
mkdir build; cd build; ../configure; make
sudo make install
```

2. Build and install this project:
```
mkdir build; cd build; cmake ..; make
sudo make install
```

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

So far, steps 1-5 have been partially automated. Contact us (me) to
discuss details.

The corpus generation tool is in development. See
https://github.com/opencog/link-grammar/discussions/1146
for current status.


That's all for now!
-------------------
THE END.
