#! /bin/bash
#
# Configuration parameters for artificial-language word-pair counting.
# See `2-pair-conf.sh` for documentation of these parameters.
# ------------

# Directory where corpora files can be found
export CORPORA_DIR=$TEXT_DIR/fake-corpus

# Disable sentence splitting.
export SENTENCE_SPLIT=false

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17008
export PROMPT="scheme@(fake-pairs)"
export COGSERVER_CONF=${CONFIG_DIR}/2-cogserver/cogserver-pairs-fake.conf

# Scheme function name for word-pair counting
export OBSERVE="observe-text"

# Location of the database where pair counts will be accumulated
export PAIRS_DB=${ROCKS_DATA_DIR}/fake_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"

# File processing grunge
export MSG="Splitting and word-pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
