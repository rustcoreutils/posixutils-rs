function a(n) {
	if (n == 0) {
		return 1;
	} else {
		return n * b(n - 1);
	}
}

function b(n) {
	if (n == 0) {
		return 1;
	} else {
		return n * a(n - 1);
	}
}

BEGIN {
	print a(5);
}