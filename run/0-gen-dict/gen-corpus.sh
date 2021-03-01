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

# Generate corpus files, containing sentences of different lengths.
link-generator -l $BASE/fake-lang -s 1 -c 150000 > $BASE/corpus-1.txt
link-generator -l $BASE/fake-lang -s 2 -c 150000 > $BASE/corpus-2.txt
link-generator -l $BASE/fake-lang -s 3 -c 150000 > $BASE/corpus-3.txt
link-generator -l $BASE/fake-lang -s 4 -c 150000 > $BASE/corpus-4.txt
link-generator -l $BASE/fake-lang -s 5 -c 150000 > $BASE/corpus-5.txt
link-generator -l $BASE/fake-lang -s 6 -c 150000 > $BASE/corpus-6.txt
link-generator -l $BASE/fake-lang -s 7 -c 150000 > $BASE/corpus-7.txt
link-generator -l $BASE/fake-lang -s 8 -c 150000 > $BASE/corpus-8.txt
link-generator -l $BASE/fake-lang -s 9 -c 150000 > $BASE/corpus-9.txt
