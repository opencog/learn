#! /bin/bash
#
# compute-marginals.sh
#
# Start the cogserver, open the database and compute the word-pair
# marginal statistics.  It is safe to run this multiple times, but
# it is wasteful: it will give the same results each time.
#
# Instead of running this, `trim-dataset.sh` can be run to trim the
# dataset, and then compute the marginals for the smaller dataset.
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

if [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

guile -s ${COMMON_DIR}/marginals-pair.scm
