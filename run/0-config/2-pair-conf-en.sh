#! /bin/bash
#
# Configuration parameters for English word-pair counting.
#
# Enable sentence splitting.
SENTENCE_SPLIT=true
SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
HOSTNAME=localhost
PORT=17005
PROMPT="scheme@(en-pairs)> "
COGSERVER_CONF=config/opencog-pairs-en.conf

# Scheme function name for word-pair counting
OBSERVE="observe-text"

# URL for the database where pair counts will be accumulated
ROCKS_DB_URL=rocks://${DATA_DIR}/en_pairs.rdb

# File processing grunge
MSG="Splitting and word-pair counting"
IN_PROCESS_DIR=pair-split
COMPLETED_DIR=pair-counted
