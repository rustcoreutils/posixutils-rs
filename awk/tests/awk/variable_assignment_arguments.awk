BEGIN {
	ARGV[2] = "variable=1";
	ARGV[3] = "_variable=2";
	ARGV[4] = "v2ar4_iable=3";
	ARGC = 5;
}

END {
	print variable;
	print _variable;
	print v2ar4_iable;
}