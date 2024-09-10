BEGIN {
    print "test" ~ /a/
    print "matches" ~ /tch/
    print "not match" !~ /whatever/
    print "s" !~ /s/

    print "correct" " precedence" ~ /cor+/
}