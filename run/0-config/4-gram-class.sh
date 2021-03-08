#! /bin/bash
#
# Configuration parameters for grammatical class clustering.
# Under construction. Temporary scaffolding.
# --------------
#
# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=20010
export PROMPT="scheme@(gram-class)> "
export COGSERVER_CONF="config/opencog-gram-fake.conf"

# URL for the database where disjunct counts will be accumulated
ROCKS_DB_URL=rocks://${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"${ROCKS_DB_URL}\")"
