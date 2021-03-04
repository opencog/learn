#! /bin/bash
#
# Configuration parameters for Planar MST parsing.
#
SENTENCE_SPLIT=false

# IPv4 hostname and port number of where the cogserver is running.
HOSTNAME=localhost
PORT=19010
PROMPT="scheme@(mpg-parse)> "
COGSERVER_CONF="config/opencog-mpg-fake.conf"

# Scheme function name for planar MST parsing.
OBSERVE="observe-mpg"

# URL for the database where disjunct counts will be accumulated
ROCKS_DB_URL=rocks://${DATA_DIR}/mpg_parse.rdb

# Directories where in-process and completed files will be moved.
IN_PROCESS_DIR=mpg-split
COMPLETED_DIR=mpg-done

# Message printed for each processed file
MSG="MPG-Processing"
