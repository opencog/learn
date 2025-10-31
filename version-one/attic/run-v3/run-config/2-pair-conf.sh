#! /bin/bash
#
# Configuration parameters for word-pair counting.
#
# This is an example config file; you might want to use one of the
# preconfigured files, e.g. `2-pair-conf-fake.sh` or `2-pair-conf-en.sh`
# ------------
#
# Directory where corpora files can be found
export CORPORA_DIR=$TEXT_DIR/beta-pages

# Enable or disable sentence splitting.
# If the text corpora have one sentence per line, then splitting is not
# needed. If the corpora are arranged into paragraphs (as conventional
# for natural language), then the paragraphs must be split into distinct
# sentences.
export SENTENCE_SPLIT=false

# If splitting is enabled, then specify the splitting language. Choices
# include `en`, `fr`, `pl` and many more; see the splitter directory for more.
export SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17001
export PROMPT="scheme@(count-pairs)"
export COGSERVER_CONF=""

# Scheme function name for word-pair counting. This is a scheme function
# that will be called to process each sentence.  For example, if the corpus
# contains "Some sentence." then the cogserver will receive
#   (observe-text "Some sentence.")
#
export OBSERVE="observe-text"

# Location of the database where pair counts will be accumulated
export PAIRS_DB=${ROCKS_DATA_DIR}/word_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"

# For Postgres, use this. (The quotes are important!)
export PAIRS_DB=word_pairs
export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"

# Directories where in-process and completed files will be moved.
# This avoids double-processing of corpus files, if the pair-processing
# scripts are interrupted and restarted.
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted

# Message printed for each processed file.
export MSG="Splitting and word-pair counting"
