function add_one(x) {
	x = x + 1;
	return x;
}

BEGIN {
	x = 1;
	print add_one(x);
	print x;
}