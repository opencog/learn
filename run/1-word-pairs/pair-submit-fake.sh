#!/bin/bash
#
# Batch word-pair counting script for fake generated languages.
#
# Loop over all the files in 'data', sentence-split them
# and submit them for word-pair couting.
#
# time find beta-pages -type f -exec ./pair-nosplit-one.sh {} localhost 17008 \;
time ./pair-nosplit-one.sh fake ~/data/corpus.txt localhost 17008
