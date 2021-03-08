#! /bin/bash
#
# run-cogserver.sh
#
# Run everything needed for the language-learning word-pair counting
# pipeline. Starts the CogServer and opens the storage database.
#
# ----------------------
# Load config parameters
if [ -r ../0-config/0-pipeline.sh ]; then
	. ../0-config/0-pipeline.sh
else
	echo "Cannot find master configuration file!"
	exit -1
fi

if [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

exec guile -l ../common/cogserver.scm
