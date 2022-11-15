#!/bin/bash
#
# file-xform-process.sh <file> <base-path> <xform-cmd>
#
# Support script for processing of files that need to be transformed
# in some way before they can be submitted for counting.
#
# The <xform-cmd> executable command is applied to each file, the
# output of which is submitted to the cogserver, line by line.  When
# done, the file is moved over to the $COMPLETED_DIR directory.
#
# If the files do not need any transformation or pre-processing, then
# the `file-nosplit-process.sh` script can be used.
#
# <file> is the file to process
# <base-path> is the directory in which the corpora are located.
# <xform-cmd> is an executable command that takes the <file> as
#   input, and generates white-space separated UTF-8 text as output.
#
# Example usage:
#    ./file-xform-process.sh /home/data/dir/foo.txt /home/data/ bar.sh
#
# This code is almost identical to `file-split-process.sh`

# Some versions of netcat require the -N flag, and some versions
# of netcat do not know about the -N flag. This is mega-annoying.
# Hack this to match your netcat.
netcat="nc -N"

# Set up assorted constants needed to run.
filename="$1"
basepath="$2"
xformer="$3"

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

# Create directories if missing.
mkdir -p $(dirname "$splitdir/$rest")
mkdir -p $(dirname "$subdir/$rest")

# Apply the transform to the corpus file.
$xformer "$filename" >  "$splitdir/$rest"

# Submit the transformed result, line by line.
cwd=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cat "$splitdir/$rest" | $cwd/submit-lines.pl $coghost $cogport "$observe"

# Punt if the cogserver has crashed (second test,
# before doing the mv and rm below)
haveping=`echo foo | $netcat $coghost $cogport`
if [[ $? -ne 0 ]] ; then
	echo "Error: Failed to ping cogserver after processing $rest"
	exit 1
fi

# Move the corpus object to the done-queue.
# We move the original non-xformed file to the done queue,
# and remove the xformed file. If the splitdir is non-empty,
# that means the above crashed somehow, and manual cleanup
# or recovery is needed.
mv "$basepath/$rest" "$subdir/$rest"
rm "$splitdir/$rest"
