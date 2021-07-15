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

if ! [ -z ${MST_CONF_FILE} ] && [ -r ${MST_CONF_FILE} ]; then
	. ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file at MST_CONF_FILE"
	env |grep CONF
	exit -1
fi

exec guile -l ${COMMON_DIR}/cogserver-mst.scm
