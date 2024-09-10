BEGIN {
    print index("hello", "l");
    print length("hello");
    print match("hello", /l+/);
    print RSTART;
    print RLENGTH;
    split("hello test b", a);
    print a[1];
    print a[2];
    print a[3];
    split("hello,test,b", a, /,/);
    print a[1];
    print a[2];
    print a[3];
    print sprintf("format string");
    print sprintf("format string: %s", "hello");
    print sprintf("format string: %s %d", "hello", 1);
    print tolower("HELLO");
    print toupper("hello");
    s = "hello";
    gsub(/l/, "L", s);
    print s;
    s = "hello";
    sub(/l/, "L", s);
    print s;
    print substr("hello", 2, 2);
}

$1 == 1 { print length }
$1 == 2 { print length() }
$1 == 3 { gsub(/el|ra/, "xx"); print $0 }
$1 == 4 { sub(/a/, "xx"); print $0 }
