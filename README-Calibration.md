
Calibrating Unsupervised Language Learning
==========================================
* Version of February 2021

Ongoing project, continuing activity.  See the
[language learning wiki](http://wiki.opencog.org/w/Language_learning)
for an alternate overview.

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

1. Go to the [fake](fake) directory, load the `random-dict.scm` file,
and run it to create a Link Grammar Dictionary.

2. Copy the above and use Amir's tools to generate a corpus.

3. Run the processing pipeline described in 
[README-Natural](README-Natural.md)

4. Measure accuracy.

So far, none of this has been automated. Contact us (me) for details.


That's all for now!
-------------------
THE END.
