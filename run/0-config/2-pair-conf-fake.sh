#! /bin/bash
#
# Configuration parameters for artificial-language word-pair counting.
#
# Disable sentence splitting.
SENTENCE_SPLIT=false

# IPv4 hostname and port number of where the cogserver is running.
HOSTNAME=localhost
PORT=17008
PROMPT="scheme@(fake-pairs)> "
COGSERVER_CONF=config/opencog-pairs-fake.conf

# Scheme function name for word-pair counting
OBSERVE="observe-text"

# URL for the database where pair counts will be accumulated
PAIR_DB_URL=rocks://${DATA_DIR}/fake_pairs.rdb

# File processing grunge
MSG="Splitting and word-pair counting"
IN_PROCESS_DIR=pair-split
COMPLETED_DIR=pair-counted
