#! /bin/bash
#
# Configuration parameters for grammatical class clustering.
# --------------
#
# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=19008
export PROMPT="scheme@(gram-class)"
export COGSERVER_CONF=${CONFIG_DIR}/4-cogserver/cogserver-gram-fake.conf

# Location of the database where grammatical classes will be formed.
export GRAM_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${GRAM_DB}\")"

# Scheme function that will perform classification.
# Pick one. Tune as desired.
# API="(define gsa (add-cluster-gram (make-pseudo-cset-api)))"
# API="${API} (define psa (add-pair-stars gsa))"
API="(define psa star-obj)"
export GRAM_CLUSTER="${API} (gram-classify-greedy-discrim psa 0.5 4)"
export GRAM_CLUSTER="${API} (gram-classify-greedy-fuzz psa 0.65 0.3 4)"
export GRAM_CLUSTER="${API} (gram-classify-greedy-disinfo psa 3.0 4)"
