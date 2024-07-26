END { print "e1" }

BEGIN { print "b1" }

1 { print 1 }

BEGIN { print "b2" }

1 { print 2 }

END { print "e2" }

1 { print 3 }

BEGIN { print "b3" }

END { print "e3" }
