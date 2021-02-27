#! /bin/bash
#
# Partially automated corpus generation
# Before running this script, be sure to do this first:
# `cp -r /where/ever/link-grammar/data/gen /tmp/fake-lang`
#
# -----------
# Location where files should go. Suggest /home/user/data
BASE=/tmp

# -----------
# Generate a dictionary and move it into place
guile -l gen-dict.scm

mv $BASE/4.0.dict $BASE/fake-lang

link-generator -l $BASE/fake-lang -s 1 -c 150000 > $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 2 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 3 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 4 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 5 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 6 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 7 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 8 -c 150000 >> $BASE/corpus.txt
link-generator -l $BASE/fake-lang -s 9 -c 150000 >> $BASE/corpus.txt
