#!/bin/bash
#
# process-corpus.sh <config>
#
# Batch process a collection of corpora files.
#
# Loop over all of the corpora files (all the files in $CORPORA_DIR),
# and then (optionally) pre-process them and submit them for counting
# by the cogserver. Pre-processing usually means sentence-splitting.
#
# As files are processed, them will be moved from $CORP to the directory
# `submitted` in the current working dir.
#
# Assorted environment variables will be fetched from the <config> file.
# An example config file is in `~/run-config/2-pair-conf.sh`
#
# XXX TODO Why are we using a config file here? Why aren't we just
# grabbing everything we need from the env?
# ---------

CONF_FILE=$1

if [ -r $CONF_FILE ]; then
	. $CONF_FILE
else
	echo "Cannot find configuration file!"
	exit -1
fi

export HOSTNAME
export PORT
export OBSERVE
export IN_PROCESS_DIR
export COMPLETED_DIR
export MSG

cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Always do block processing by default. All of the other modes
# are deprecated, and will be removed eventually.
if $BLOCK_SUBMIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
elif $SENTENCE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-split-process.sh $SPLIT_LANG {} $CORPORA_DIR \;
elif $XFORM_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-xform-process.sh {} $CORPORA_DIR $XFORM_CMD \;
elif $LINE_SPLIT; then
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-line-process.sh {} $CORPORA_DIR \;
else
	time find $CORPORA_DIR -type f \
		-exec $cwd/file-block-process.sh {} $CORPORA_DIR \;
fi
