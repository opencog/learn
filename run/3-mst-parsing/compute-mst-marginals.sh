#! /bin/bash
#
# compute-mst-marginals.sh
#
# Start the cogserver, open the database and compute the disjunct
# marginal statistics.  It is safe to run this multiple times.
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

if [ -r ${MST_CONF_FILE} ]; then
	. ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	exit -1
fi

guile -s ${COMMON_DIR}/marginals-mst.scm
