#! /bin/bash
#
# run-mst-cogserver.sh
#
# Run everything needed for the language-learning disjunct-counting
# pipeline. Starts the CogServer, opens the database, loads the
# word-pairs in the database (which can take an hour or more!).
#
# ----------------------
# Load config parameters
if [ -r ../0-config/0-pipeline.sh ]; then
	. ../0-config/0-pipeline.sh
else
	echo "Cannot find master configuration file!"
	exit -1
fi

if [ -r ${MST_CONF_FILE} ]; then
	. ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	exit -1
fi

exec guile -l ../common/cogserver-mst.scm
