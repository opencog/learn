#! /usr/bin/env perl
#
# submit-one.pl <cogserver-host> <cogserver-port> <observe-cmd> <count-mode> <parameters>
#
# Submit a collection of sentences, one sentence at a time, to the
# cogserver located on host ARGV[0] and port ARGV[1].  The sentences
# are read from standard input, and must be arranged with one sentence
# per line. 
# For the "file" mode, where word-instance-pair weights are read from a file, 
# the input must be arranged in blocks starting with 
# one sentence, and followed by the word-pairs and their weights, one pair
# per line.  
# They are sent to the cogserver using ARGV[2] as the command.
# For word-pair counting, ARGV[2] is "observe-text-mode"
# For disjunct counting, ARGV[2] is "observe-mst-mode"
# ARGV[3] and ARGV[4] (and ARGV[5] for observe-mst-mode) are necessary
# arguments for parameter configuration (see file process-one.sh to see them)
#
# Example usage:
#    cat file | ./submit-one.pl localhost 17001 observe-text-mode "any" 24
#

die "Wrong number of args!" if ($#ARGV < 4);
my @dist_mult = @ARGV[4..$#ARGV-1];

# Use netcat only for pair-counting, not for mst-parsing (this may cause
# problems if we care about disjunct counting, but for now we don't care
# about them)
# Verify that the host and port number are OK.
`nc -z $ARGV[0] $ARGV[1]`;
die "Netcat failed! Bad host or port?" if (0 != $?);
my $netcat = "|nc -N $ARGV[0] $ARGV[1]";

# use Socket;
# my $port =  "$ARGV[1]";
# my $server = "$ARGV[0]";

# # Don't use netcat; just send directly
# # argument 1: what to send (ascii string)
# # argument 2: bool flag, wait for response, or not.
# sub send_stuff {
# 	socket(SOCKET, PF_INET, SOCK_STREAM, (getprotobyname('tcp'))[2])
# 		or die "Can't create a socket $!\n";

# 	connect(SOCKET, pack_sockaddr_in($port, inet_aton($server)))
# 		or die "Can't connect to port $port! \n";

# 	my $text = $_[0];

# 	# if subroutine has second argument, use it.
# 	my $wait_for_response = 0;
# 	if (1 < scalar @_) {
# 		$wait_for_response = $_[1];
# 	}
# 	# print "Will wait for response: $wait_for_response\n";

# 	if ($wait_for_response) {
# 		SOCKET->autoflush(1);
# 		print SOCKET "$text\n.\n.\n";
# 		my $line;
# 		while ($line = <SOCKET>) {
# 			# print "Drained: $line";
# 		}
# 		close SOCKET;
# 	} else {
# 		print SOCKET "$text\n.\n.\n";
# 		close SOCKET;
# 	}
# }

my $start_time = time();
# my $sent_nbr = 0;

if ($ARGV[3] eq "file")
{
	my $buffer="";
	while (<STDIN>)
	{
		if (/^\n$/) {
			chomp($buffer);
			open NC, $netcat || die "nc failed: $!\n";
			print NC "($ARGV[2] \"$buffer\" \"$ARGV[3]\" '(@dist_mult) \"$ARGV[5]\")\n";
			# $sent_nbr += 1;
			# send_stuff("($ARGV[2] \"$buffer\" \"$sent_nbr\" \"$ARGV[3]\" $ARGV[4] \"$ARGV[5]\")\n");
			my $elapsed = time() - $start_time;
			print "submit-one (elapsed $elapsed): $_\n";
			$buffer="";
		}
		else {
			chop;
			$buffer = $buffer . $_ . "\n";
		}
	}
}
else
{
	while (<STDIN>)
	{
		if (/^\n$/) { next; }
		chop;

 		open NC, $netcat || die "nc failed: $!\n";

		if ( $ARGV[2] eq "observe-text-mode" )
			{ print NC "($ARGV[2] \"$_\" \"$ARGV[3]\" $ARGV[4])\n"; }
			# {open NC, $netcat || die "nc failed: $!\n";
 			# print NC "($ARGV[2] \"$_\" \"$ARGV[3]\" $ARGV[4])\n"; }
			#{ send_stuff("($ARGV[2] \"$_\" \"$ARGV[3]\" $ARGV[4])\n"); }
		elsif ( $ARGV[2] eq "observe-mst-mode" )
			{ print NC "($ARGV[2] \"$_\" \"$ARGV[3]\" '(@dist_mult) \"$ARGV[5]\")\n"; }
			#{ $sent_nbr += 1;
			#send_stuff("($ARGV[2] \"$_\" \"$sent_nbr\" \"$ARGV[3]\" $ARGV[4] \"$ARGV[5]\")\n"); }
		my $elapsed = time() - $start_time;
		print "submit-one (elapsed $elapsed): $_\n";
	}
}
# sleep 5 ; # Small delay to allow socket processes to finish
print "Done with article.\n";
