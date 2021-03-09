#! /bin/bash
#
# 0-pipeline.sh
#
# Master file for configuration parameters for langugage learning.
# This particular file is aimed at artificial-language/corpus
# generation and processing.
#
# Directory roots. One for text data (the corpus, and assorted text files)
# the other for RocksDB databases. This allows the text files to live on a
# spinning disk (where speed is not important) and databases to live on a
# smaller SSD (where speed is important). Set these any way you want.
# If you are using Postgres, just delete `ROCKS_DATA_DIR`

export TEXT_DIR=/home/ubuntu/run/expt-42
export ROCKS_DATA_DIR=/home/ubuntu/data/expt-42

# Directory in which configuration parameters (including this file)
# are located.
export CONFIG_DIR=$TEXT_DIR/0-config

# File containing artificial-languaage generation configuration
export GEN_CONF_FILE=$CONFIG_DIR/1-corpus-conf.sh

# File containing pair-counting configuration
export PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-en.sh
export PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-fake.sh

# File containing MST-parsing configuration
export MST_CONF_FILE=$CONFIG_DIR/3-mst-conf.sh
export MST_CONF_FILE=$CONFIG_DIR/3-mpg-conf-fake.sh
