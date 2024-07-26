BEGIN {
    a["a", "b", "c"] = 1
    print a["a", "b", "c"]
    print a["a" SUBSEP "b" SUBSEP "c"]
    SUBSEP = "<->"
    a["a", "b", "c"] = 2
    print a["a", "b", "c"]
    print a["a" "<->" "b" "<->" "c"]
}