#! /bin/bash
#
# run-all-gram.sh
#
# Run everything needed for grammatical classification.
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
# The MST config is needed for the MST database location.
if ! [ -z ${MST_CONF_FILE} ] && [ -r ${MST_CONF_FILE} ]; then
	. ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	exit -1
fi

# ------------------------
# Step four - Clustering of grammatical classes
if ! [ -z ${GRAM_CONF_FILE} ] && [ -r ${GRAM_CONF_FILE} ]; then
	. ${GRAM_CONF_FILE}
else
	echo "Cannot find grammatical class clustering configuration file!"
	exit -1
fi

# Copy the database, to provide isolation between stages.
if [[ $STORAGE_NODE = "(RocksStorageNode"* ]]; then
	cp -pr ${MST_DB} ${GRAM_DB}
elif [[ $STORAGE_NODE = "(PostgresStorageNode"* ]]; then
	createdb -T ${MST_DB} ${GRAM_DB}
else
	echo "Unknown storage medium!"
	exit -1
fi

# Run the classification cogserver
guile -l ${COMMON_DIR}/cogserver-gram.scm -c "(sleep 150000000)" &

# Wait for the cogserver to intialize.
sleep 3

# Perform the desired clustering.
# The trailing newline-dots exit the cogserver shell,
# as otherwise the netcat willl hang, waiting for completion.
# We avoid "nc -q 0" because we want to see the output.
echo -e "$GRAM_CLUSTER\n.\n." | nc $HOSTNAME $PORT

# Shut down the server.
echo Done clustering
echo "(exit-server)" | nc $HOSTNAME $PORT >> /dev/null

# Wait for the shutdown to complete.
sleep 1

echo Done
# ------------------------
