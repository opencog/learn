#! /bin/bash
#
# Configuration parameters for MPG (Max Planar Graph) parsing - English.
#
export CORPORA_DIR=$TEXT_DIR/pair-counted

export SENTENCE_SPLIT=true
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=18005
export PROMPT="\x1b[0;34mscheme@(mpg-parse) \x1b[0m"
export OCPROMPT="\x1b[0;32mcogserv@(mpg-parse) \x1b[0m"
export LOGFILE=/tmp/cogserver-mpg-en.log

# Scheme function name for planar MST parsing.
export OBSERVE="observe-mpg"

# Location of the database where disjunct counts will be accumulated
export MST_DB=${ROCKS_DATA_DIR}/mpg_parse.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${MST_DB}\")"

# Directories where in-process and completed files will be moved.
export IN_PROCESS_DIR=mpg-split
export COMPLETED_DIR=mpg-done

# Message printed for each processed file
export MSG="MPG-Processing"
