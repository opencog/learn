#! /bin/bash
#
# Configuration parameters for artificial language corpus generation.
#
# --------------
# Directory where dictionary will be written
export DICT_DIR=$TEXT_DIR/fake-lang

# Directory where corpora files will be written
export GEN_CORPUS_DIR=$TEXT_DIR/fake-corpus

# The file that contains the dictionary configuration information.
export $CONFIG_DIR/DICT_CONF=1-dict-conf.scm

# The length of the shortest and the longest sentences to generate.
# Sentences between these lengths (inclusive) will be generated.
export SENT_SHORTEST=3
export SENT_LONGEST=12

# Maximum number of sentences to generate for each fixed sentence
# length. If more sentences than this are possible, then a random
# subset will be sampled. Otherwise, all possible sentences will
# be generated.
#
# Pair counting runs at about 15 to 30 sentences per second
# on present-day desktop CPUs.
# MPG parsing runs at about 100 to 300 sentences per second.
export NUM_SENTENCES=25000
