#! /bin/bash
#
# Configuration parameters for MST parsing.
#
# This is an example config file; you might want to use one of the
# preconfigured files, e.g. `3-mst-conf-fake.sh` or `3-mst-conf-en.sh`
#
# Enable or disable sentence splitting.
# If the text corpora have one sentence per line, then splitting is not
# needed. If the corpora are arranged into paragraphs (as conventional
# for natural language), then the paragraphs must be split into distinct
# sentences.
SENTENCE_SPLIT=false

# If splitting is enabled, then specify the splitting language. Choices
# include `en`, `fr`, `pl` and many more; see the splitter directory for more.
SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
HOSTNAME=localhost
PORT=17001
PROMPT="scheme@(mst-parse)> "
COGSERVER_CONF=""

# Scheme function name for MST parsing. This is a scheme function
# that will be called to process each sentence.  For example, if the corpus
# contains "Some sentence." then the cogserver will receive
#   (observe-text "Some sentence.")
#
OBSERVE="observe-mst"

# URL for the database where disjunct counts will be accumulated
ROCKS_DB_URL=rocks://${DATA_DIR}/mst_parse.rdb

# Directories where in-process and completed files will be moved.
IN_PROCESS_DIR=mst-split
COMPLETED_DIR=mst-done

# Message printed for each processed file
MSG="MST-Processing"
