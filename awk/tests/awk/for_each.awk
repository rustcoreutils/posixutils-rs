BEGIN {
    array["first"] = 1
    array["second"] = 2
    array["third"] = 3
    for (key in array) {
        print key, "->", array[key]
    }
}