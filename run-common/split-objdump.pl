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

my $accum = "";

while(<DISM>) {
	chomp;

	# Skip over blank lines, mis information
	if (/^\//) { next; }
	if (/^Disassem/) { next; }
	if (/^$/) { next; }

	# Skip the name of the routine/method;
	# When a new routine is reached, dump the completed line to STDOUT
	# XXX Maybe we should dump blocks!?
	if (/^\d+ /) {
		if (not $accum =~ /^$/) {
			$accum =~ s/\s*$//;
			print "line=>>$accum<<\n";
		}
		$accum = "";
		next;
	}

	# Cut off the leading address
	s/^\s*\w+:\s*//;

	# Cut off the trailing comment helping w/ the address
	s/#.*$//;

	# Cut off trailing whitespace
	s/\s*$//;

	# Some Intel x86 formatting: add assorted whitespace to
	# separate out tokens more cleanly.
	s/\*/* /;
	s/\(/ ( /;
	s/\)/ )/;
	s/\$/\$ /;
	s/,/ , /;
	s/:/ : /;

	# TODO: convert addresses to something comprehensible.
	# That is, something like
	#    jne    27ee8 <__gconv_get_alias_db@@GLIBC_PRIVATE+0x658>
	#    callq  25308 <malloc@plt>
	#    callq  25130 <*ABS*+0x8e110@plt>
	# should be convereted into some kind of ... reference.

	print "yo >>$_<<\n";

	# Accumulate the result.
	$accum .= $_ . ' NL ';
}

close DISM;
