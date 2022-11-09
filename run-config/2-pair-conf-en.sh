#! /bin/bash
#
# Configuration parameters for English word-pair counting.
# See `2-pair-conf.sh` for documentation of these parameters.
# ------------

# Directory where corpora files can be found
export CORPORA_DIR=$TEXT_DIR/beta-pages

# Files will be moved from the above, to the below, as they
# get processed. This allows interrupted processes to be restarted,
# without repeating earlier work.
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
export MSG="Splitting and word-pair counting"

# Enable sentence splitting.
export SENTENCE_SPLIT=true
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17005
export PROMPT="\x1b[0;34mscheme@(en-pairs) \x1b[0m"
export OCPROMPT="\x1b[0;32mcogserv@(en-pairs) \x1b[0m"
export LOGFILE=/tmp/cogserver-pairs-en.log

# Scheme function name for word-pair counting
export OBSERVE="observe-text"

# Location of the database where pair counts will be accumulated

# For Postgres, use this. (The quotes are important!)
# (Except don't use this: the Postgres backend is deprecated.)
# export PAIRS_DB=en_pairs
# export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"

# For RocksDB, use this.
export PAIRS_DB=${ROCKS_DATA_DIR}/en_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
