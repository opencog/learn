#! /bin/bash
#
# Partially automated corpus generation
# Before running this script, be sure to do this first:
# `cp -r /where/ever/link-grammar/data/gen /tmp/fake-lang`
#
# -----------
# Generate a dictionary and move it into place
guile -l 0-gen-dict/gen-dict.scm
mv /tmp/4.0.dict /tmp/fake-lang

link-generator -l /tmp/fake-lang -s 1 -c 150000 > /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 2 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 3 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 4 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 5 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 6 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 7 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 8 -c 150000 >> /tmp/corpus.txt
link-generator -l /tmp/fake-lang -s 9 -c 150000 >> /tmp/corpus.txt
