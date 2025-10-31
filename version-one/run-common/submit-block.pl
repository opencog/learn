#! /usr/bin/env perl
#
# submit-block.pl <cogserver-host> <cogserver-port> <observe-cmd>
#
# Read all of `stdin` and submit it as one big block to the cogserver
# for processing. The block is quoted and then wrapped by the
# <observe-cmd> before being sent. The <observe-cmd> is assumed to
# be a scheme function that takes one text-string argument.
# The entire string <observe-cmd> is sent; thus it can contain
# arguments.
#
# For pair counting, ARGV[2] is "observe-block-pairs".
# For MST/MPG counting, ARGV[2] is "observe-block-mpg".
#
# Example usage:
#    cat file | ./submit-block.pl localhost 17001 "observe-block-pairs"
#
# This script will wait (hang) until the cogserver is idle enough
# to be able to respond. That is, it waits until the cogserver has
# (mostly) finished running the command.

use utf8;

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
my $block = "";
while (<STDIN>)
{
	$block = $block . $_;
}

# Guile will choke on single back-slashes and on unescaped quotes.
# Both of these MUST be escaped.  It's all OK, though, because the
# string in guile's RAM will have the original quotes and backslashes
# as intended. i.e. they get unescaped upon ingestion. The escaping
# is only for the guile string constructor.
$block =~ s/\\/\\\\/g;
$block =~ s/\"/\\\"/g;

send_nowait($server, $port, "($ARGV[2] \"$block\")\n");
# print "submit-block: $block\n";

# Wait until the cogserver is actually done.
# If we don't wait, then the time printed below is wrong.
# XXX except this is wrong; this just stalls until some other
# socket opens up on the cogserver!
ping_flush($server, $port);

my $elapsed = time() - $start_time;
print "Sent out block of text $elapsed seconds\n";
