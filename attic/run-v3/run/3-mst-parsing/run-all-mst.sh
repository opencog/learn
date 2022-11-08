#! /bin/bash
#
# run-all-mst.sh
#
# Run everything needed for the MST-parsing and disjunct-counting
# stage of the language-learning pipeline.
#
# ----------------------

# Load master config parameters
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

# ----------------------
# The pair-conf file holds the pairs DB name.
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# ------------------------
# Step three - MST parsing and disjunct counting
if ! [ -z ${MST_CONF_FILE} ] && [ -r ${MST_CONF_FILE} ]; then
	. ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	exit -1
fi

# Copy the database, to provide isolation between stages.
if [[ $STORAGE_NODE = "(RocksStorageNode"* ]]; then
	if [ -d ${MST_DB} ]; then
		echo "The MST database ${MST_DB} already exists; not copying!"
		exit -1
	fi
	cp -pr ${PAIRS_DB} ${MST_DB}
elif [[ $STORAGE_NODE = "(PostgresStorageNode"* ]]; then
	createdb -T ${PAIRS_DB} ${MST_DB}
	retVal=$?
	if [ $retVal -ne 0 ]; then
		echo "Postgres database create failed; not continuing!"
		exit -1
	fi
else
	echo "Unknown storage medium!"
	exit -1
fi

# Run the MST cogserver
guile -l ${COMMON_DIR}/cogserver-mst.scm -c "(sleep 150000000)" &

# Wait for the cogserver to initialize.
sleep 3
echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null

# Batch-process the corpus.
${COMMON_DIR}/process-corpus.sh $MST_CONF_FILE

# Shut down the server.
echo Done MST parsing
echo "(exit-server)" | nc $HOSTNAME $PORT >> /dev/null

# Wait for the shutdown to complete.
sleep 1

# Compute the disjunct marginals.
echo "Start computing the disjunct marginals"
guile -s ${COMMON_DIR}/marginals-mst.scm
echo "Finish computing the disjunct marginals"
echo -e "\n\n\n"

echo Done MST parsing and disjunct counting
# ------------------------
