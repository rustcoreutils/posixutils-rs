BEGIN {
    print OFMT
    print 123.456
    OFMT = "%.2f"
    print 123.456
    OFMT = "%.0f"
    print 123.456
    OFMT = "%e"
    print 123.456
    OFMT = "%a"
    print 123.456
}