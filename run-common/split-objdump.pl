#! /usr/bin/env perl
#
# split-objump.pl -- Convert binaries into strings
#
# Example usage:
#     ./split-objump.pl /lib/x86_64-linux-gnu/libc-2.31.so
#
# Expected input is any file that objdump can disassemble. Output is
# a string of assembly, one string per text entry. The format is one
# that seems like it should be analyzable via pair-counting.
# (Just try it, you'll see...)

my $object = $ARGV[0];

open DISM, "-|", "objdump -d --no-show-raw-insn $object" or die $0;

while(<DISM>) {
	chomp;

	# Skip over blank lines, mis information
	if (/^\//) { next; }
	if (/^Disassem/) { next; }
	if (/^$/) { next; }

	# Skip the name of the routine/method
	if (/^\d+ /) { next; }

	s/^\s*\w+:\s*//;

	print "yo >>$_<<\n";
}

close DISM;
