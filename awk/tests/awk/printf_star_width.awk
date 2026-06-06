BEGIN {
    printf "[%*d]\n", 5, 42
    printf "[%-*d]\n", 5, 42
    printf "[%.*f]\n", 2, 3.14159
    printf "[%*.*f]\n", 8, 2, 3.14159
    printf "[%*d]\n", -5, 42
}
