function set(array) {
	array[1] = 2;
	array[2] = 3;
}

BEGIN {
	array[1] = 1;
	set(array);
	for (i in array) {
		print i " -> " array[i];
	}
}