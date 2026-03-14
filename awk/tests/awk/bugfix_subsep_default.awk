BEGIN {
    a[1,2] = "yes"
    # Default SUBSEP is \034 (0x1c), not space
    key = 1 SUBSEP 2
    print (key in a)
    # Verify it's not space-separated
    print ((1 " " 2) in a)
}
