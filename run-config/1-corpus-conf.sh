#! /bin/bash
#
# Configuration parameters for artificial language corpus generation.
#
# Directory where dictionary will be written
export DICT_DIR=$TEXT_DIR/fake-lang

# Directory where corpora files will be written
export CORPORA_DIR=$TEXT_DIR/fake-corpus

# The file that contains the dictionary configuration information.
DICT_CONF=1-dict-conf.scm

# The length of the shortest and the longest sentences to generate.
# Sentences between these lengths (inclusive) will be generated.
SHORTEST=3
LONGEST=12

# Maximum number of sentences to generate for each fixed sentence
# length. If more sentences than this are possible, then a random
# subset will be sampled. Otherwise, all possible sentences will
# be generated.
#
# Pair counting runs at about 15 to 30 sentences per second
# on present-day desktop CPUs.
# MPG parsing runs at about 100 to 300 sentences per second.
NSENT=25000
