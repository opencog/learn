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

# Load config parameters
if [ -z $MASTER_CONFIG_FILE ]; then
	echo "MASTER_CONFIG_FILE not defined!"
	exit -1
fi

if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file at MASTER_CONFIG_FILE"
	env |grep CONF
	exit -1
fi

if [ -r $MST_CONF_FILE ]; then
	echo "Start MST/MPG parsing"
else
	echo "Cannot find MST/MPG configuration file at MST_CONF_FILE"
	env |grep CONF
	exit -1
fi

${COMMON_DIR}/process-corpus.sh $MST_CONF_FILE
