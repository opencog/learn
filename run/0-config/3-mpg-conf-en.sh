#! /bin/bash
#
# Configuration parameters for Planar MST parsing.
#
export SENTENCE_SPLIT=true
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=19005
export PROMPT="scheme@(mpg-parse)> "
export COGSERVER_CONF="config/opencog-mst-en.conf"

# Scheme function name for planar MST parsing.
export OBSERVE="observe-mpg"

# URL for the database where disjunct counts will be accumulated
export ROCKS_DB_URL=rocks://${DATA_DIR}/mpg_parse.rdb

# Directories where in-process and completed files will be moved.
export IN_PROCESS_DIR=mpg-split
export COMPLETED_DIR=mpg-done

# Message printed for each processed file
export MSG="MPG-Processing"
