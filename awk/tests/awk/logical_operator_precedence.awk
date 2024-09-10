BEGIN {
    print 1 && 0 || 1;
    print 1 || 0 && 0;

    print !1 && 0;
    print !(1 && 0);
    print !0 || 0;
    print !(0 || 0);

    print (1 && 0) || 1;
    print 1 || (0 && 0);
    print !(1 && (0 || 1));
}