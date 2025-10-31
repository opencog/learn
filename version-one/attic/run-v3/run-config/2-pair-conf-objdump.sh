#! /bin/bash
#
# Configuration parameters for `objdump -d --no-show-raw-insn` pair
# counting. This treats disassembled binaries as plain-text files,
# and performs statistical counting of pairs of white-space separated
# items in that text: these will be instructions, opcode, register
# names, and memory addresses. This is "interesting" because it makes no
# a priori assumptions about what this assembly code means, or how to
# interpret it.  Of course, one could tune the pipeline, and built-in
# (by hand) base knowledge about assembly, instructions, registers, etc.
# But that would miss the point: we want to see how much this system can
# figure out on it's own, without human help and guidance.
#
# See `2-pair-conf.sh` for documentation of these parameters.
# ------------

# Directory where binaries can be found
export CORPORA_DIR=$TEXT_DIR/binaries

# Disable sentence splitting.
export SENTENCE_SPLIT=false

# Enable disassembly
export XFORM_SPLIT=true
export XFORM_CMD=${COMMON_DIR}/split-objdump.pl

# IPv4 hostname and port number of where the cogserver is running.
export HOSTNAME=localhost
export PORT=17009
export PROMPT="scheme@(objdump)"
export COGSERVER_CONF=${CONFIG_DIR}/2-cogserver/cogserver-pairs-objdump.conf

# Scheme function name for pair counting.
# Uses a window size of 30. Why 30? Because each insn splits into 2-6
# tokens after running through the `split-objdump.pl` script, and so
# this window size is about 4-7 insns long. Which seems like a
# reasonable size for assembly correlations. (Maybe it should be
# smaller? Maybe larger? Why?)
export OBSERVE="observe-window 30"

# Location of the database where pair counts will be accumulated
export PAIRS_DB=${ROCKS_DATA_DIR}/objdump_pairs.rdb
export STORAGE_NODE="(RocksStorageNode \"rocks://${PAIRS_DB}\")"

# File processing grunge
export MSG="Objdump and pair counting"
export IN_PROCESS_DIR=pair-split
export COMPLETED_DIR=pair-counted
