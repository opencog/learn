#! /bin/bash
#
# Configuration parameters for exporting grammatical classes to Link
# Grammar dictionaries.
#
# --------------
#
# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=20008
export PROMPT="scheme@(export)"
export COGSERVER_CONF=${CONFIG_DIR}/5-cogserver/cogserver-export-fake.conf

# Location of the database where the grammatical classes are held
export EXPORT_DB=${ROCKS_DATA_DIR}/gram-2.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${EXPORT_DB}\")"

# Path to the file that will hold the Link Grammar dicationary.
# The file itself *must* be named `dict.db`, or else LG will fail.
export LG_DICT_EXPORT=${TEXT_DIR}/learned-2/dict.db

# The locale to apply to the dictionary.
export LG_DICT_LOCALE=en_US
