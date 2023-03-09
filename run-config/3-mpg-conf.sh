#! /bin/bash
#
# Configuration parameters for MPG (Max Planar Graph) parsing.
#
# This is an example config file; you might want to use one of the
# preconfigured files, e.g. `3-mpg-conf-en.sh`
# --------------
#
# Directory where corpora files can be found. This can be anywhere;
# however, it is most convenient to make it the same as the output
# directory configured in the previous (pair-counting) stage. This is
# the setting of the `$COMPLETED_DIR` of the earlier stage.
export CORPORA_DIR=$TEXT_DIR/pair-counted

# Scheme function name for planar MST parsing. This is a scheme function
# that will be called to process each sentence.  For example, if the corpus
# contains "Some sentence." then the cogserver will receive
#   (observe-block-mpg "Some sentence.")
#
# Use `observe-block-mpg` to get planar MST/MPG parsing.
export OBSERVE="observe-block-mpg"

# Directories where in-process and completed files will be moved.
export IN_PROCESS_DIR=mpg-split
export COMPLETED_DIR=mpg-done

# Message printed for each processed file
export MSG="MPG-Processing"

# --------------
# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17003
# Colorized prompt. Sadly, this wrecks command-line editing.
# export PROMPT="[0;34mscheme@(mpg-parse) [0m"
export PROMPT="scheme@(mpg-parse) "
export OCPROMPT="[0;32mcogserv@(mpg-parse) [0m"
export LOGFILE=/tmp/cogserver-mpg-en.log

# Location of the database where disjunct counts will be accumulated
export MST_DB=rocks://${ROCKS_DATA_DIR}/mpg-parse.rdb
export STORAGE_NODE="(RocksStorageNode \"${MST_DB}\")"

# --------------
