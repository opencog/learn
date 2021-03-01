#!/bin/bash
#
# Batch word-pair counting script for fake generated languages.
#
# Loop over all the files in $CORP, sentence-split them
# and submit them for word-pair couting.
#
# Adjust the value of $CORP to suit your tastes.
#
CORP=/home/ubuntu/data/fake-corpus
time find $CORP -type f -exec ./pair-nosplit-one.sh {} localhost 17008 \;
