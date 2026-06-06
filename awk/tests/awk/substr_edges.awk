BEGIN {
    print substr("hello", -1, 3)
    print substr("hello", 0, 2)
    print substr("hello", 2, -1) "|"
    print substr("hello", 2, 100)
    print substr("hello", 2)
    print substr("hello", 2.9, 1)
    print substr("hello", 10, 2) "|"
}
