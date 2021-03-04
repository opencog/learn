#!/bin/bash
#
# Batch MST parsing script for artificial languages
# Loop over all the files in 'gamma-pages', sentence-split them
# and submit them for MST parsing.
#
time find gamma-pages -type f -exec ./mst-nosplit-one.sh {} localhost 19010 \;
