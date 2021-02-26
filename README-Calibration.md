
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

This pipeline is in the process of being set up. So far:

0. Build the corpus generation tools, as follows:
```
git clone https://github.com/linas/link-grammar
cd link-grammar
git checkout generate
./autogen.sh --no-configure
mkdir build; cd build; ../configure; make
sudo make install
```

1. Build and install this project:
```
mkdir build; cd build; cmake ..; make
sudo make install
```

2. Go to the [fake](fake) directory, and review the `random-dict.scm`
   file, which describes how to create a create a Link Grammar Dictionary.
   See `0-gen-dict/gen-dict.scm` for an example usage.
   Sample usage:
```
$ guile -l 0-gen-dict/gen-dict.scm
```

3. Copy the dictionary into place:
```
$ cp -r /where/ever/link-grammar/data/gen /tmp/fake-lang
$ mv /tmp/4.0.dict /tmp/fake-lang
```

4. Generate a corpus, as shown below. This assumes the created language
   is called `fake-lang`. The below will generate 50000 random sentences
   that are 6 words long.  If the grammar does not allow this many
   sentences to be generated, fewer will be created. If the grammar
   allows more than 50000 sentences, then a random sampling of 50000
   sentences will be made.

```
$ link-generator -l /tmp/fake-lang -s 6 -c 50000 > /tmp/corpus.txt
```

5. Alternately, use `gen-corpus.sh` to perform steps 2,3,4 above.

6. Run the processing pipeline described in
[README-Natural](README-Natural.md)

7. Measure accuracy.

So far, none of this has been automated. Contact us (me) for details.


That's all for now!
-------------------
THE END.
