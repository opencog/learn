#! /bin/bash
#
# Configuration parameters for artificial language corpus generation.
#
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
NSENT=50000
