BEGIN {
    print ORS
    print "a"
    print "b"
    ORS = "<->"
    print "a"
    print "b"
}