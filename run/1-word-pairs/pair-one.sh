#!/bin/bash
#
# pair-one.sh <lang> <filename> <cogserver-host> <cogserver-port>
#
# Support script for word-pair counting of plain-text files.
# Sentence-split one file, submit it, via perl script, to the cogserver.
# When done, move the file over to the `submitted-pages` directory.
#
# This script is handles the case for text files that are organized into
# conventional paragraphs, with multiple sentences per paragraph. Such
# paragraphs have to be split into distinct sentences before further
# processing. If the text file already has one sentence per line, then
# sentence-splitting is not needed, and `pair-nosplit-one.sh` can be used.
#
# Example usage:
#    ./pair-one.sh en Barbara localhost 17001
#

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
lang=$1
filename="$2"
# coghost="localhost"
# cogport=17002
coghost="$3"
cogport=$4

#splitter=/usr/local/bin/split-sentences.pl
splitter=./split-sentences.pl

splitdir=split-articles
subdir=submitted-articles
observe="observe-text"

# Punt if the cogserver has crashed.  Use netcat to ping it.
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	exit 1
fi

# Split the filename into two parts
base=`echo $filename | cut -d \/ -f 1`
rest=`echo $filename | cut -d \/ -f 2-6`

echo "Word-paring counting file >>>$rest<<<"

# Create directories if missing
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")

# Sentence split the article itself
cat "$filename" | $splitter -l $lang >  "$splitdir/$rest"

# Submit the split article
cat "$splitdir/$rest" | ../submit-one.pl $coghost $cogport $observe

# Punt if the cogserver has crashed (second test,
# before doing the mv and rm below)
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	exit 1
fi

# Move article to the done-queue
mv "$splitdir/$rest" "$subdir/$rest"
rm "$base/$rest"
