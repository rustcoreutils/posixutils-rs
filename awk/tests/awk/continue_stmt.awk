BEGIN {
    i = 0
    while(i < 5) {
        if (i == 2) {
            i++
            continue
        }
        print i
        i++
    }
}