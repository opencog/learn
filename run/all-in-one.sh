#! /bin/bash
#
# all-in-one.sh
#
# Run everything needed for the language-learning pipeline.
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
# Step two - pair counting
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# Verify that the input corpus can be found, and is not empty.
if [ ! -d $CORPORA_DIR ]; then
	echo "Cannot find a text corpus at $CORPORA_DIR"
	exit -1
fi

if [ 0 -eq `find $CORPORA_DIR -type f |wc -l` ]; then
	echo "Empty text corpus directory at $CORPORA_DIR"
	exit -1
fi

# Run the pair-counting cogserver
guile -l ${COMMON_DIR}/cogserver.scm -c "(sleep 150000000)" &

# Wait for the cogserver to intialize.
sleep 3

# Batch-process the corpus.
${COMMON_DIR}/process-corpus.sh $PAIR_CONF_FILE

# Shut down the server.
echo Done pair counting
echo "(exit-server)" | nc $HOSTNAME $PORT >> /dev/null

# Wait for the shutdown to complete.
sleep 1

# Compute the pair marginals.
echo "Start computing the pair marginals"
guile -s ${COMMON_DIR}/marginals-pair.scm
if [ $? -ne 0 ]; then
	echo "Failure computing the pair marginals!"
	exit -1
fi
echo "Finish computing the pair marginals"
echo -e "\n\n\n"

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
	cp -pr ${PAIRS_DB} ${MST_DB}
elif [[ $STORAGE_NODE = "(PostgresStorageNode"* ]]; then
	createdb -T ${PAIRS_DB} ${MST_DB}
else
	echo "Unknown storage medium!"
	exit -1
fi

# Run the MST cogserver
guile -l ${COMMON_DIR}/cogserver-mst.scm -c "(sleep 150000000)" &

# Wait for the cogserver to intialize.
sleep 3

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
if [ $? -ne 0 ]; then
	echo "Failure computing the disjunct marginals!"
	exit -1
fi
echo "Finish computing the disjunct marginals"
echo -e "\n\n\n"

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

# ------------------------
# Step five - Export to Link Grammar

if ! [ -z ${EXPORT_CONF_FILE} ] && [ -r ${EXPORT_CONF_FILE} ]; then
	. ${EXPORT_CONF_FILE}
else
	echo "Cannot find grammatical class export configuration file!"
	exit -1
fi

echo "Exporting grammar to $LG_DICT_EXPORT"

# Create the export directory, if its not already there.
# Add the Link Grammar boilerlate files.
EXPORT_DIR=$(dirname $LG_DICT_EXPORT)
mkdir -p $EXPORT_DIR
cp -p /usr/local/share/link-grammar/demo-sql/4.0.* $EXPORT_DIR

guile -s ${COMMON_DIR}/export-dictionary.scm
if [ $? -ne 0 ]; then
	echo "Failure exporting the dictionary!"
	exit -1
fi

# ------------------------
echo Done
# ------------------------
