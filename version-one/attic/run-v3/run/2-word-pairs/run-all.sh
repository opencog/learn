#! /bin/bash
#
# run-all.sh
#
# Run a fully-automated word-pair counting pipeline.
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
# Pair-counting config
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# Run the pair-counting cogserver
guile -l ${COMMON_DIR}/cogserver.scm -c "(sleep 150000000)" &

# Wait for the cogserver to initialize.
sleep 3
echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null

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
echo "Finish computing the pair marginals"
echo -e "\n\n\n"

echo Done processing word-pairs
# ------------------------
