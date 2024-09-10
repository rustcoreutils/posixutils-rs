BEGIN {
    print array[1, 2];
    print array[1, 2, 3];
    array[1, 2, 3] = "test";
    print array[1, 2, 3];
    print array[1 SUBSEP 2 SUBSEP 3]
}