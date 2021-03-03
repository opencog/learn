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
#
TEXT_DIR=/home/ubuntu/run/expt-42
DATA_DIR=/home/ubuntu/data/expt-42

# Directory in which configuration parameters (including this file)
# are located.
CONFIG_DIR=$TEXT_DIR/0-config

# Directory where dictionary will be written
DICT_DIR=$TEXT_DIR/fake-lang

# Directory where corpora files will be written
CORPORA_DIR=$TEXT_DIR/fake-corpus

# File contianing pair-counting configuration
PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-en.sh
PAIR_CONF_FILE=$CONFIG_DIR/2-pair-conf-fake.sh
