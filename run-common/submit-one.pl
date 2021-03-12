#! /usr/bin/env perl
#
# submit-one.pl <cogserver-host> <cogserver-port> <observe-cmd>
#
# Read lines from `stdin` and submit them to the cogserver for
# processing. Each line is quoted and then wrapped by the <observe-cmd>
# before being sent. For example, if the line is "some text\n", and
# <observe-cmd> is "do-thing", then the CogServer will receive
#
#   (do-thing "some test")\n
#
# Note that the trailing newline is chopped off the end of the line,
# and "do-thing" is assumed to be a scheme function that has been
# defined.
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

die "Wrong number of args!" if ($#ARGV != 2);

# Verify that the host and port number are OK.
`nc -z $ARGV[0] $ARGV[1]`;
die "Netcat failed! Bad host or port?" if (0 != $?);

# Use plain-old TCP sockets.
# (An earlier vrsion of this file used netcat. This was OK but...
# it bottlnecked and lead to poor performance, especially when the
# cogserver could respond quickly. Forking a new netcat each time
# was inefficient.)
use Socket;
my $server = $ARGV[0];
my $port = $ARGV[1];

# Takes just one argument: string to send.
sub send_nowait
{
	socket(SOCKET, PF_INET, SOCK_STREAM, (getprotobyname('tcp'))[2])
		or die "Can't create a socket $!\n";

	# If the cogserver is really slow (e.g. if it is being debugged)
	# the connet will fail (a max of 140 pending connects are possible
	# without changing ulimit). This is very rare. If it fails, we retry.
	my $rc = connect(SOCKET, pack_sockaddr_in($port, inet_aton($server)));
	if (not $rc) {
		print "Couldn't connect to port $port! Retrying...\n";
		my $i=0;
		sleep 1;
		while ($i<100 and not $rc) {
			$i++;
			$rc = connect(SOCKET, pack_sockaddr_in($port, inet_aton($server)));
			if (not $rc)
			{
				print "Retry $i\n";
				sleep 1;
			}
		}

		($i<100) or die "Fatal Error: Unable to connect to port $port\n";
	}

	# Two dots: one to exit scheme, one to exit cogserver prompt.
	print SOCKET "$_[0]\n.\n.\n";

	# Close socket immediately.  Do NOT wait for any replies!!
	close SOCKET;
}

my $start_time = time();
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

	send_nowait("($ARGV[2] \"$_\")\n");
	# print "submit-one: $_\n";
}
my $elapsed = time() - $start_time;
print "Sent out article in $elapsed seconds\n";
