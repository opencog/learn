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
export PROMPT="scheme@(en-pairs)> "
export COGSERVER_CONF=config/opencog-pairs-en.conf

# Scheme function name for word-pair counting
export OBSERVE="observe-text"

# URL for the database where pair counts will be accumulated
ROCKS_DB_URL=rocks://${DATA_DIR}/en_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"${ROCKS_DB_URL}\")"

# File processing grunge
export MSG="Splitting and word-pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
