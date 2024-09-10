BEGIN {
	array["first"] = 1
	array["second"] = 2
	array["third"] = 3
	for (key in array) {
		print key, "->", array[key]
		delete array[key]
	}
	print "Array after deletion:"
	for (key in array) {
		print key, "->", array[key]
	}
}