#! /bin/bash
#
# Configuration parameters for English word-pair counting.
# See `2-pair-conf.sh` for documentation of these parameters.
# ------------

# Directory where corpora files can be found
export CORPORA_DIR=$TEXT_DIR/beta-pages

# Enable sentence splitting.
export SENTENCE_SPLIT=true
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17005
export PROMPT="scheme@(en-pairs)"
export COGSERVER_CONF=${CONFIG_DIR}/2-cogserver/cogserver-pairs-en.conf

# Scheme function name for word-pair counting
export OBSERVE="observe-text"

# Location of the database where pair counts will be accumulated

# For Postgres, use this. (The quotes are important!)
export PAIRS_DB=en_pairs
export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"

# For RocksDB, use this.
export PAIRS_DB=${ROCKS_DATA_DIR}/en_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"

# File processing grunge.
export MSG="Splitting and word-pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
