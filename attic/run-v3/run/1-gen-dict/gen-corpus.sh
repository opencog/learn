#! /bin/bash
#
# Automated corpus generation. Configurable parameters are located
# in `run-config/1-corpus-conf.sh` and in `1-dict-conf.scm`. Edit those
# files as desired.
#
# ---------

# Load config parameters
if [ -z $MASTER_CONFIG_FILE ]; then
	echo "MASTER_CONFIG_FILE not defined!"
	exit -1
fi

if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi

if [ -r $GEN_CONF_FILE ]; then
	. $GEN_CONF_FILE
else
	echo "Cannot find corpus configuration file!"
	exit -1
fi

DICT=$DICT_DIR
CORP=$GEN_CORPUS_DIR

if [[ -d $CORP ]]; then
	echo Corpus directory exists: $CORP
	echo Delete or move this directory and try again
	exit -1
fi

echo "Using dictionary found in $DICT"
echo "Placing generated corpus in $CORP"

mkdir $CORP

# Generate corpus files, containing sentences of different lengths.
# For example:
# link-generator -l $DICT -s 4 -c 150000 > $CORP/corpus-4.txt

for (( n=$SENT_SHORTEST; n<=$SENT_LONGEST; n++)); do
	echo "Generating sentences of length $n"
	link-generator -l $DICT -s $n -c $NUM_SENTENCES > $GEN_CORPUS_DIR/corpus-$n.txt
done

exit 0
