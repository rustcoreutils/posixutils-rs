BEGIN {
    if (2 > 4)
        print "true branch"
    else
        print "false branch"

    if (2 < 4)
        print "true branch"
    else
        print "false branch"

    if (1)
        if (0)
            {}
        else
            print "false branch"

}