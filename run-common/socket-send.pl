#
# socket-send.pl -- network utility to send stuff to the CogServer.
#
# Implements simple `netcat` type functionality in plain-old TCP
# sockets. Earlier versions just used netcat directly; this was OK but...
# it bottlnecked and lead to poor performance, especially when the
# cogserver could respond quickly. Forking a new netcat each time
# was remarkably inefficient, with massive slowdowns. See the mailing
# list circa 2017(?) for details.
#

use Socket;
use utf8;

# Takes three arguments: server, port and string to send.
sub send_nowait
{
	my $server = $_[0];
	my $port = $_[1];
	my $msg = $_[2];

	socket(SOCKET, PF_INET, SOCK_STREAM, (getprotobyname('tcp'))[2])
		or die "Can't create a socket $!\n";

	# We want to linger, if the message being sent is huge. But this
	# does not seem to actually work (it doesn't actually linger). WTF.
	setsockopt(SOCKET, SOL_SOCKET, SO_LINGER, pack("II",1,120))
		or die "Can't set SO_LINGER: $!\n";

	# If the CogServer is really slow (e.g. if it is being debugged)
	# the connect will fail (a max of 140 pending connects are possible
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
	print SOCKET "$msg\n.\n.\n";

	# Perl appears to truncate messages longer than 7 megabytes if
	# we close immediately. The SO_LINGER above doesn't work, and
	# flushing doesn't work. We we attempt a really ugly hack, and
	# sleep 1 second for every megabyte.
	my $msglen = length($msg);
	sleep ($msglen / 1e6);

	# Close socket immediately.  Do NOT wait for any replies!!
	close SOCKET;
}

# Hang, until the cogserver is idle enough to allow us to connect.
# Basically, the cogserver stops responding until it's work queues
# are empty enough to allow a response.
#
# Takes two arguments: server and port.
sub ping_flush
{
	my $server = $_[0];
	my $port = $_[1];

	socket(SOCKET, PF_INET, SOCK_STREAM, (getprotobyname('tcp'))[2])
		or die "Can't create a socket $!\n";

	my $rc = connect(SOCKET, pack_sockaddr_in($port, inet_aton($server)));
	if ($rc) {
		# One dot: exit cogserver prompt.
		print SOCKET ".\n";
		shutdown(SOCKET, 1);

		# Spin until EOF on the socket
		my $foo = "";
		my $rrc = read SOCKET, $foo, 500;
		while ($rrc) {
			$rrc = read SOCKET, $foo, 500;
		}

		# OK, a second shutdown returns error and sets $! to
		# `Transport endpoint is not connected` so that's really weird.
		# and a close SOCKET just flat-out crashes!
		# Whatever. The script exits, the kernel cleans up.
	}
}
