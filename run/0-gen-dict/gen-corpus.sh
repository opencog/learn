#! /bin/bash
#
# Automated corpus generation. Most configurable parameters are located
# in `gen-dict.scm`. A few more are below.
#
# The length of the shortest and the longest sentences to generate.
# Sentences between these lengths (inclusive) will be generated.
SHORTEST=3
LONGEST=12

# Maximum number of sentences to generate for each fixed sentence
# length. If more sentences than this are possible, then a random
# subset will be sampled. Otherwise, all possible sentences will
# be generated.
NSENT=50000

# No user-configurable parameters below this point.
# ===================================================================
# No user-configurable parameters below this point.
#
# Generate a dictionary. The return string is either an error message,
# or its the configured directory.
EXPERIMENT_DIR=`./gen-dict.scm`

if [ $? -ne 0 ]; then
	echo $EXPERIMENT_DIR
	exit -1
fi

# Hard-coded grammar and corpus directories
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
# For example:
# link-generator -l $DICT -s 4 -c 150000 > $CORP/corpus-4.txt

for (( n=$SHORTEST; n<=$LONGEST; n++)); do
	echo Generating sentences of length $n
	link-generator -l $DICT -s $n -c $NSENT > $CORP/corpus-$n.txt
done

exit 0
