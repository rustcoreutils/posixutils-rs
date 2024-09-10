function f(a, b, array) {
	a = "a";
	b = "b";

	array[1] = a;
	array[2] = b;

	print array[1] " " array[2];
}

BEGIN {
	f();
	print a;
	print b;
	print array[1] " " array[2];
}

