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

# URL for the database where pair counts will be accumulated
PAIR_DB_URL=rocks://${DATA_DIR}/en_pairs.rdb
