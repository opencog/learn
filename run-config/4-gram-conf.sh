#! /bin/bash
#
# Configuration parameters for grammatical class clustering.
# Under construction. Temporary scaffolding.
# --------------
#
# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=19008
export PROMPT="scheme@(gram-class)"
export COGSERVER_CONF=${CONFIG_DIR}/4-cogserver/cogserver-gram-fake.conf

# Location of the database where disjunct counts will be accumulated
export GRAM_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${GRAM_DB}\")"

# Scheme function that will perform classification
export GRAM_CLUSTER="(gram-classify-greedy-discrim 0.5 4)"
export GRAM_CLUSTER="(gram-classify-greedy-fuzz 0.65 0.3 4)"
