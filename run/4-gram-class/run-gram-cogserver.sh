#! /bin/bash
#
# run-gram-cogserver.sh
#
# Run everything needed for the language-learning grammatical class
# clustering pipeline. Starts the CogServer, opens the database, loads
# the disjuncts in the database (which can take an hour or more!).
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
	echo "Cannot find master configuration file!"
	exit -1
fi

if ! [ -z ${GRAM_CONF_FILE} ] && [ -r ${GRAM_CONF_FILE} ]; then
	. ${GRAM_CONF_FILE}
else
	echo "Cannot find grammatical class clustering configuration file!"
	exit -1
fi

exec guile -l ${COMMON_DIR}/cogserver-gram.scm
