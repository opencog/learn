#! /bin/bash
#
# Automated corpus generation. There are no configurable parameters
# in this file. All configuration is located in `gen-dict.scm`.
#
# -----------
# Generate a dictionary. The return string is either an error message,
# or its the configured directory.
EXPERIMENT_DIR=`./gen-dict.scm`

if [ $? -ne 0 ]; then
	echo $EXPERIMENT_DIR
	exit -1
fi

DICT=$EXPERIMENT_DIR/fake-lang
CORP=$EXPERIMENT_DIR/fake-corpus

if [[ -d $CORP ]]; then
	echo Corpus directory exists: $CORP
	echo Delete or move this directory and try again
	exit -1
fi

echo Dictionary is located in $DICT
echo Corpus is in $CORP

mkdir $CORP

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
