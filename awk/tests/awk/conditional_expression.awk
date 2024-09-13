BEGIN {
	print 1 ? "t" : "f"
    print 0 ? "t" : "f"
	print 1 ? 1 ? "t" : "f1" : "f2"
	print 0 ? 1 ? "t" : "f1" : "f2"
	print 1 ? 0 ? "t" : "f1" : "f2"
}