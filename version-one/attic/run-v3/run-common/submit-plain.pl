#! /usr/bin/env perl
#
# submit-plain.pl <cogserver-host> <cogserver-port> <observe-cmd>
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
# For pair counting, ARGV[2] is "observe-text" or "observe-window"
# For MST disjunct counting, ARGV[2] is "observe-mst"
# For planar MST disjunct counting, ARGV[2] is "observe-mpg"
#
# Example usage:
#    cat file | ./submit-plain.pl localhost 17001 "observe-window 24"
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
	# Empty line. Skip it. (Its length 1 because of newline)
	if (1 >= length $_) { next; }

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
