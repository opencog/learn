#! /bin/bash
#
# Configuration parameters for word-pair counting.
#
# This is an example config file; copy and edit to suit your needs.
# ------------
#
# Directory where corpora files can be found
# export CORPORA_DIR=$TEXT_DIR/beta-pages
export CORPORA_DIR=$TEXT_DIR/input-pages

# Directories where in-process and completed files will be moved.
# This avoids double-processing of corpus files, if the pair-processing
# scripts are interrupted and restarted.
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted

# Submit each file as a single large block to the cogserver.
# The pipeline code will deal with all segmentation issues.
export OBSERVE="observe-block-pairs"

# Message printed for each processed file.
export MSG="Block word-pair counting"

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17002
# Colorized prompt. Sadly, this breaks command-line editing.
# export PROMPT="[0;34mscheme@(count-pairs) [0m"
export PROMPT="scheme@(count-pairs) "
export OCPROMPT="[0;32mcogserv@(count-pairs) [0m"
export LOGFILE=/tmp/cogserver-pairs-en.log

# Location of the database where pair counts will be accumulated.
export PAIRS_DB=${ROCKS_DATA_DIR}/word-pairs.rdb
# export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"
export STORAGE_NODE="(MonoStorageNode \"monospace://${PAIRS_DB}\")"

# For Postgres, use this. (The quotes are important!)
# (Except don't use it -- the Postgres Backend is deprecated.)
# export PAIRS_DB=word_pairs
# export STORAGE_NODE="(PostgresStorageNode \"postgres:///${PAIRS_DB}\")"

# =================================================================
# Deprecated settings. These settings allow the text to be processed
# using an older processing model. It suffers from various theoretical
# issues having to do with segmentation into words and sentences, and
# thus is not used any more. Segmentation is henceforth handled in the
# in the AtomSpace processing pipeline.
#
# Enable or disable sentence splitting.
# If the text corpora have one sentence per line, then splitting is not
# needed. If the corpora are arranged into paragraphs (as conventional
# for natural language), then the paragraphs must be split into distinct
# sentences.
# export SENTENCE_SPLIT=true

# If splitting is enabled, then specify the splitting language. Choices
# include `en`, `fr`, `pl` and many more; see the splitter directory for more.
# export SPLIT_LANG=en

# Scheme function name for word-pair counting. This is a scheme function
# that will be called to process each sentence.  For example, if the corpus
# contains "Some sentence." then the cogserver will receive
#   (observe-text "Some sentence.")
#
# export OBSERVE="observe-text"

# Enable or disable the application of a transform before text
# submission. See run-common for details.
# export XFORM_SPLIT=false

# Enable observation of text on a line-by-line basis. The file is
# assumed to contain lines of text; each line of text will be submitted,
# one at a time, via the OBSERVE function above.
# export LINE_SPLIT=false
