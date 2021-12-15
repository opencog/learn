#!/bin/bash
#
# pair-submit.sh
#
# Batch pair counting script.
#
# Loop over all of the corpora files (all the files in $CORPORA_DIR),
# and then (optionally) sentence-split them and submit them for
# pair counting.
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

if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# Verify that the input corpus can be found, and is not empty.
if [ ! -d $CORPORA_DIR ]; then
	echo "Cannot find a text corpus at $CORPORA_DIR"
	exit -1
fi

if [ 0 -eq `find $CORPORA_DIR -type f |wc -l` ]; then
	echo "Empty text corpus directory at $CORPORA_DIR"
	exit -1
fi

${COMMON_DIR}/process-corpus.sh $PAIR_CONF_FILE
