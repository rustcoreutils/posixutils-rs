BEGIN {
    print match("bcdab", "ab");
    print RSTART;
    print RLENGTH;
    n = split("a:b:c", arr, ":");
    print n;
    print arr[1];
    print arr[2];
    print arr[3];
    s = "hello world";
    gsub("o", "0", s);
    print s;
    s = "hello world";
    sub("o", "0", s);
    print s;
    print ("hello" ~ "ell");
    print ("hello" !~ "xyz");
}
