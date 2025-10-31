#!/bin/bash
#
# file-split-process.sh <lang> <file> <base-path>
#
# Support script for processing of files containing paragraphs of text.
#
# Sentence-split each paragraph into individual files, and submit it,
# via perl script, to the cogserver. When done, move the file over to
# the $COMPLETED_DIR directory.
#
# This script is handles the case for text files that are organized into
# conventional paragraphs, with multiple sentences per paragraph. Such
# paragraphs have to be split into distinct sentences before further
# processing. If the text file already has one sentence per line, then
# sentence-splitting is not needed, and `file-nosplit-process.sh` can be
# used.
#
# <lang> is the language to use for determining sentence boundaries.
# <file> is the file to process
# <base-path> is the directory in which the text corpora are located.
#
# Example usage:
#    ./file-split-process.sh en foo.txt
#    ./file-split-process.sh en /home/data/dir/foo.txt /home/data/
#
# This code is almost identical to `file-xform-process.sh`

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
lang=$1
filename="$2"
basepath="$3"

splitter=$COMMON_DIR/split-sentences.pl

coghost=$HOSTNAME
cogport=$PORT
observe=$OBSERVE

# Punt if the cogserver has crashed.  Use netcat to ping it.
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Unable to ping cogserver; not processing file."
	exit 1
fi

# Split the filename into two parts
alen=${#basepath}
blen=$(($alen+2))
rest=`echo $filename | cut -c$blen-500`

echo "$MSG file >>>$rest<<<"

# Remove everything after the last slash in the basepath.
base=`echo ${basepath%/*}`
splitdir=${base}/${IN_PROCESS_DIR}
subdir=${base}/${COMPLETED_DIR}

# Create directories if missing
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")

# Sentence split the article itself
cat "$filename" | $splitter -l $lang >  "$splitdir/$rest"

# Submit the split article
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cat "$splitdir/$rest" | $cwd/submit-one.pl $coghost $cogport "$observe"

# Punt if the cogserver has crashed (second test,
# before doing the mv and rm below)
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Failed to ping cogserver after processing $rest"
	exit 1
fi

# Move article to the done-queue
mv "$splitdir/$rest" "$subdir/$rest"
rm "$basepath/$rest"
