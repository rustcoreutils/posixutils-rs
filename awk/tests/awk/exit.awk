BEGIN {
    print "begin1";
}

BEGIN {
    print "begin2";
}

{ print "first record"; exit; }

END {
    print "end1";
}

END {
    print "end2";
}