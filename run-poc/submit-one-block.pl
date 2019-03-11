#! /usr/bin/env perl
#
# submit-one-block.pl <cogserver-host> <cogserver-port> <observe-cmd> <count-mode> <parameters>
#
# Submit a collection of sentences and their word-instance pair weights, 
# one sentence block at a time, to the
# cogserver located on host ARGV[0] and port ARGV[1].  The input 
# is read from standard input and must be arranged in blocks starting with 
# one sentence, and followed by the word-pairs and their weights, one pair
# per line. The are sent to the cogserver using ARGV[2] as the command.
# For mst-parsing, ARGV[2] is "observe-mst-mode"
# ARGV[3] and ARGV[4] (and ARGV[5] for observe-mst-mode) are necessary
# arguments for parameter configuration (see file process-one.sh to see them)
#
# Example usage:
#    cat file | ./submit-one-block.pl localhost 17001 observe-text-mode "file" 0
#

die "Wrong number of args!" if ($#ARGV < 4);

# Verify that the host and port number are OK.
`nc -z $ARGV[0] $ARGV[1]`;
die "Netcat failed! Bad host or port?" if (0 != $?);

my $netcat = "|nc $ARGV[0] $ARGV[1]";

my $start_time = time();
my $buffer="";
while (<STDIN>)
{
	if (/^\n$/) {
		chomp($buffer);
		open NC, $netcat || die "nc failed: $!\n";
		print NC "($ARGV[2] \"$buffer\" \"$ARGV[3]\" $ARGV[4] $ARGV[5])\n";
		my $elapsed = time() - $start_time;
		print "submit-one (elapsed $elapsed): $_\n";
		$buffer="";
	}
	else {
		chop;
		$buffer = $buffer . $_ . "\n";
	}
}
print "Done with article.\n";
