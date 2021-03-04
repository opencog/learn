#! /usr/bin/env perl
#
# submit-one.pl <cogserver-host> <cogserver-port> <observe-cmd>
#
# Submit a collection of sentences, one sentence at a time, to the
# cogserver located on host ARGV[0] and port ARGV[1].  The sentences
# are read from standard input, and must be arranged with one sentence
# per line. The are sent to the cogserver using ARGV[2] as the command.
#
# For word-pair counting, ARGV[2] is "observe-text"
# For MST disjunct counting, ARGV[2] is "observe-mst"
# For planar MST disjunct counting, ARGV[2] is "observe-mpg"
#
# Example usage:
#    cat file | ./submit-one.pl localhost 17001 observe-text
#

die "Wrong number of args!" if ($#ARGV != 2);

# Verify that the host and port number are OK.
`nc -z $ARGV[0] $ARGV[1]`;
die "Netcat failed! Bad host or port?" if (0 != $?);

# Use plain-old TCP sockets.
use Socket;
my $server = $ARGV[0];
my $port = $ARGV[1];

# Takes just one argument: string to send.
sub send_nowait
{
	socket(SOCKET, PF_INET, SOCK_STREAM, (getprotobyname('tcp'))[2])
		or die "Can't create a socket $!\n";

	connect(SOCKET, pack_sockaddr_in($port, inet_aton($server)))
		or die "Can't connect to port $port! \n";

	# Two dots: one to exit scheme, one to exit cogserver prompt.
	print SOCKET "$_[0]\n.\n.\n";

	# Close socket immediately.  Do NOT wait for any replies!!
	close SOCKET;
}

my $start_time = time();
while (<STDIN>)
{
	# Stray html markup.  Do not pas it on.
	if (/<P>/) { next; }

	# Artificial corpora include comments starting with hash
	# that describe what kind of corpus it is. Don't process
	# the comments.
	if (/^#/) { next; }
	chop;

	send_nowait("($ARGV[2] \"$_\")\n");
	# print "submit-one: $_\n";
}
my $elapsed = time() - $start_time;
print "Sent out article in $elapsed seconds\n";
