#!/bin/bash
#
# Batch word-pair counting script for fake generated languages.
#
# Loop over all the files in $CORP, sentence-split them and submit them
# for word-pair couting.  Adjust the value of $CORP to suit your tastes.
# As files are processed, them will be moved from $CORP to the directory
# `submitted` in the current working dir.
#
# The below assumes $CORP is a directory relative to the current path.
#
CORP=fake-corpus
time find $CORP -type f -exec ./pair-nosplit-one.sh {} localhost 17008 \;
