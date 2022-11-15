#!/bin/bash
#
# file-block-process.sh <file> <base-path>
#
# Support script for processing whole files in on large block.
# The <file> should contain UTF-8 text.
#
# Submit that one file, via perl script, to the parser. When done,
# move the file over to the $COMPLETED_DIR directory.
#
# <file> is the file to process
# <base-path> is the directory in which the test corpora are located.
#
# Example usage:
#    ./file-block-process.sh file.txt
#    ./file-block-process.sh /home/data/dir/file.txt /home/data
#

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
filename="$1"
basepath="$2"

coghost=$HOSTNAME
cogport=$PORT
observe=$OBSERVE

# Punt if the cogserver has crashed. Use netcat to ping it.
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

# Move article to temp directory, while processing.
cp "$filename" "$splitdir/$rest"

# Submit the entire file as one big block.
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cat "$splitdir/$rest" | $cwd/submit-block.pl $coghost $cogport "$observe"

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
