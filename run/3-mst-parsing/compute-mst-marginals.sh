#! /bin/bash
#
# compute-mst-marginals.sh
#
# Start the cogserver, open the database and compute the disjunct
# marginal statistics.  It is safe to run this multiple times.
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

guile -s ../common/marginals-mst.scm
