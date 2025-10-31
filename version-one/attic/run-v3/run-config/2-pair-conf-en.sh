#! /bin/bash
#
# Deprecated; example only.
# Example configuration parameters for English word-pair counting.
# See `2-pair-conf.sh` for documentation of these parameters.
#
# This is deprecated, because it uses external perl scripts for
# sentence segmentation. That was OK for initial bringup, but doing
# segmentation externally to the processing generates artifacts,
# due to .. bad segmentation algos.  Everything is now handled by the
# pipeline itself.
# ------------

# Directory where corpora files can be found
export CORPORA_DIR=$TEXT_DIR/beta-pages

# Files will be moved from the above, to the below, as they
# get processed. This allows interrupted processes to be restarted,
# without repeating earlier work.
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
export MSG="Splitting and word-pair counting"

# Enable sentence splitting. The `observe-text` function expects
# one sentence per line, and splitting is required to feed it
# sentences.
export SENTENCE_SPLIT=true
export SPLIT_LANG=en

# Scheme function name for word-pair counting
export OBSERVE="observe-text"

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17005
# Colorized prompt. Sadly, this breaks command-line editing.
# export PROMPT="[0;34mscheme@(en-pairs) [0m"
export PROMPT="scheme@(en-pairs)"
export OCPROMPT="[0;32mcogserv@(en-pairs) [0m"
export LOGFILE=/tmp/cogserver-pairs-en.log

# Location of the database where pair counts will be accumulated

# For Postgres, use this. (The quotes are important!)
# (Except don't use this: the Postgres backend is deprecated.)
# export PAIRS_DB=en_pairs
# export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"

# For RocksDB, use this.
export PAIRS_DB=${ROCKS_DATA_DIR}/en_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
