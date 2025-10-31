#! /bin/bash
#
# Configuration parameters for (non-planar) MST (Max Spanning Tree)
# parsing.
#
# This is an example config file; see `3-mpg-conf.sh` for proper
# documentation.
# --------------
#
# Directory where corpora files can be found.
export CORPORA_DIR=$TEXT_DIR/pair-counted

# Enable or disable sentence splitting.
export SENTENCE_SPLIT=false

# If splitting is enabled, then specify the splitting language.
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17001
export PROMPT="scheme@(mst-parse)"
export COGSERVER_CONF=""

# Scheme function name for MST parsing.
export OBSERVE="observe-mst"

# Location of the database where disjunct counts will be accumulated
export MST_DB=${ROCKS_DATA_DIR}/mst_parse.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${MST_DB}\")"

# Directories where in-process and completed files will be moved.
export IN_PROCESS_DIR=mst-split
export COMPLETED_DIR=mst-done

# Message printed for each processed file
export MSG="MST-Processing"
