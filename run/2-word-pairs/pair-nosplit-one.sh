#!/bin/bash
#
# pair-nosplit-one.sh <file> <cog-host> <cog-port> <base-path>
#
# Support script for word-pair counting of pre-split plain-text.
# The <file> should contain one sentence per line, and words should be
# delimited by whitespace.
#
# <cog-host> and <cog-port> are the hostname and port number where
# the cogserver is running.
#
# Submit that one file, via perl script, to the parser. When done,
# move the file over to the `pair-counted-articles` directory in
# the <base-path>.
#
# Example usage:
#    ./pair-nosplit-one.sh file.txt localhost 17001
#    ./pair-nosplit-one.sh /home/data/dir/file.txt localhost 17001 /home/data
#

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
filename="$1"
coghost="$2"
cogport=$3
basepath="$4"

observe="observe-text"

# Punt if the cogserver has crashed. Use netcat to ping it.
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	exit 1
fi

# Split the filename into two parts
alen=${#basepath}
blen=$(($alen+2))
# base=`echo $filename | cut -c1-$alen`
rest=`echo $filename | cut -c$blen-500`

echo "Pair-count processing file >>>$rest<<<"

splitdir=$basepath/pair-articles-staging
subdir=$basepath/pair-counted-articles

# Create directories if missing
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")

# Move article to temp directory, while processing.
cp "$filename" "$splitdir/$rest"

# Submit the pre-split article
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
