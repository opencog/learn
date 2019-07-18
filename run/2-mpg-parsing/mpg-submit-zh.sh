#!/bin/bash
#
# Batch MPG parsing script for Mandarin Chinese.
# Loop over all the files in 'gamma-pages', sentence-split them
# and submit them for MPG parsing.
#
time find gamma-pages -type f -exec ./mpg-one.sh en {} localhost 19007 \;
