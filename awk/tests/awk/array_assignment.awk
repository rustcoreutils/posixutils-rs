BEGIN {
    a[0] = 1
    print a[0]
    a[0] = "string"
    print a[0]
    a["other"] = "other string"
    print a["other"]
    print a[0]
    b["index"] = "test"
    print b["index"]
    print a[0]
    print a["other"]
}