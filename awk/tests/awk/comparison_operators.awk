BEGIN {
    print(1 == 1)
    print(1 == "1")
    print("1" == 1)
    print("string" == 1)
    print(1 == "string")
    print(a == 0)
    print(0 == a)
    print("" == a)
}