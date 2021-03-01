#! /bin/bash
#
# Partially automated corpus generation
# Before running this script, be sure to do this first:
# `cp -r /where/ever/link-grammar/data/gen /tmp/fake-lang`
#
# -----------
# Location where files should go. Suggest /home/user/data
DICT=/home/ubuntu/data/fake-lang
CORP=/home/ubuntu/data/fake-corpus

# -----------
# Generate a dictionary and move it into place
guile -l gen-dict.scm

# Above writes the file to /tmp
mv /tmp/4.0.dict $DICT

# Generate corpus files, containing sentences of different lengths.
link-generator -l $DICT -s 1 -c 150000 > $CORP/corpus-1.txt
link-generator -l $DICT -s 2 -c 150000 > $CORP/corpus-2.txt
link-generator -l $DICT -s 3 -c 150000 > $CORP/corpus-3.txt
link-generator -l $DICT -s 4 -c 150000 > $CORP/corpus-4.txt
link-generator -l $DICT -s 5 -c 150000 > $CORP/corpus-5.txt
link-generator -l $DICT -s 6 -c 150000 > $CORP/corpus-6.txt
link-generator -l $DICT -s 7 -c 150000 > $CORP/corpus-7.txt
link-generator -l $DICT -s 8 -c 150000 > $CORP/corpus-8.txt
link-generator -l $DICT -s 9 -c 150000 > $CORP/corpus-9.txt
