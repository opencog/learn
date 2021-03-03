#!/bin/bash
#
# pair-split-one.sh <lang> <file> <cog-host> <cog-port> <base-path>
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
# <lang> is the language to use for determining sentence boundaries.
# <file> is the file to process
# <cog-host> and <cog-port> are teh cogserver ipv4 hostname and port
#    number where the processing will be done.
# <base-path> is the directory in which the test corpora are located.
#
# Example usage:
#    ./pair-one.sh en foo.txt localhost 17001
#    ./pair-one.sh en /home/data/dir/foo.txt localhost 17001 /home/data/
#

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
lang=$1
filename="$2"
coghost="$3"
cogport=$4
basepath="$5"

#splitter=/usr/local/bin/split-sentences.pl
splitter=./split-sentences.pl

observe="observe-text"

# Punt if the cogserver has crashed.  Use netcat to ping it.
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	exit 1
fi

# Split the filename into two parts
alen=${#basepath}
blen=$(($alen+2))
# base=`echo $filename | cut -c1-$alen`
rest=`echo $filename | cut -c$blen-500`

echo "Splitting and word-pair counting file >>>$rest<<<"

splitdir=$basepath/pair-split-articles
subdir=$basepath/pair-counted-articles

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
rm "$basepath/$rest"
