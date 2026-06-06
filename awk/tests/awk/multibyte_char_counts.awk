BEGIN {
    # "AÉB" is 3 characters but 4 bytes (É is U+00C9, 2 bytes in UTF-8).
    # POSIX awk string functions count characters, not bytes.
    s = "AÉB"
    print length(s)
    print index(s, "B")
    if (match(s, /B/))
        print RSTART, RLENGTH
    print substr(s, 3, 1)
}
