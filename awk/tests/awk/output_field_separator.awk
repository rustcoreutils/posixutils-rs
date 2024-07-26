BEGIN {
    print OFS
    print "a", "b", "c"
    OFS = "<->"
    print "a", "b", "c"
}