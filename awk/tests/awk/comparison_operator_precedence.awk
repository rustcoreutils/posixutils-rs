BEGIN {
    print(2 + 2 == 4);
    print(5 - 2 > 2);
    print(10 / 2 <= 5);
    print(3 * 3 >= 9);
    print(2 ^ 3 != 9);

    print((2 + 2) == 4);
    print((5 - 2) > 2);
    print((10 / 2) <= 5);
    print((3 * 3) >= 9);
    print((2 ^ 3) != 9);

    print(1 < 2 && 2 > 1);
    print(3 == 3 || 4 != 4);
    print(!(5 <= 5));
    print(6 >= 6 && 7 < 8);
}