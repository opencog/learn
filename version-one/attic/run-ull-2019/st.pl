
while (<>){
	if (/^[0-9]/) { next; }
	s/\r\n/\n/;
	if (/^$/) { next; }
	print "$_";
}
