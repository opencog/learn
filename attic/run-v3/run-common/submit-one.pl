#! /usr/bin/env perl
#
# submit-one.pl <cogserver-host> <cogserver-port> <observe-cmd>
#
# XXX FIXME This is mostly identical to `submit-plain.pl` except that
# it strips out some stray HTML and some comments. It should be
# refactored, sot that we don't have two nearly identical scripts.
#
# Read lines from `stdin` and submit them to the cogserver for
# processing. Each line is quoted and then wrapped by the <observe-cmd>
# before being sent. For example, if the line is "some text\n", and
# <observe-cmd> is "do-thing", then the CogServer will receive
#
#   (do-thing "some test")\n
#
# Note that the trailing newline is chopped off the end of the line,
# and "do-thing" is assumed to be a scheme function that the cogserver
# is able to execute.
#
# By convention, each line is assumed to be a single sentence. If
# sentence-splitting is needed, it must be done at an earlier stage.
#
# For word-pair counting, ARGV[2] is "observe-text"
# For MST disjunct counting, ARGV[2] is "observe-mst"
# For planar MST disjunct counting, ARGV[2] is "observe-mpg"
#
# Example usage:
#    cat file | ./submit-one.pl localhost 17001 observe-text
#
# This script will wait (hang) until the cogserver is idle enough
# to be able to respond. That is, it waits until the cogserver has
# (mostly) finished running the command.

die "Wrong number of args!" if ($#ARGV != 2);

my $server = $ARGV[0];
my $port = $ARGV[1];

# Verify that the host and port number are OK.
`nc -z $server $port`;
die "Netcat failed! Bad host or port?" if (0 != $?);

# This is easier than `use` or `require`, which want perl modules.
use File::Basename;
my $dirname = dirname(__FILE__);
do $dirname . "/socket-send.pl";

my $start_time = time();
my $nsent = 0;
while (<STDIN>)
{
	# Stray html markup.  Do not pass it on.
	if (/<P>/) { next; }

	# Empty line. Skip it. (Its length 1 because of newline)
	if (1 >= length $_) { next; }

	# Artificial corpora include comments starting with hash
	# that describe what kind of corpus it is. Don't process
	# the comments.
	if (/^#/) { next; }
	chop;

	send_nowait($server, $port, "($ARGV[2] \"$_\")\n");
	# print "submit-one: $_\n";
	$nsent = $nsent + 1;
}

# Wait until the cogserver is actually done.
# If we don't wait, then the time printed below is wrong.
ping_flush($server, $port);

my $elapsed = time() - $start_time;
print "Sent out file of $nsent sentences in $elapsed seconds\n";
