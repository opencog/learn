#!/bin/bash
#
# mst-submit.sh
#
# Batch MST parsing.
#
# Loop over all of the corpora files (all the files in $CORPORA_DIR),
# and then (optionally) sentence-split them and submit them for
# word-pair counting.
#
# ---------

if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi

../common/process-corpus.sh $MST_CONF_FILE
