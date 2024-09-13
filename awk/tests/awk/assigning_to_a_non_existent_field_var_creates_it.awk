$1 == 1 {
    print NF
    print $0
    print $4
    $4 = 1
    print $4
    print NF
    print $0

    print $10
    $10 = "test"
    print $10
    print NF
    print $0
}