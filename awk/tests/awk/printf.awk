BEGIN {
    printf "Hello, World!\n"
    printf "Integer: %d\n", 42
    printf "Floating-point: %f\n", 3.14159
    name = "Alice"
    printf "Hello, %s!\n", name
    age = 30
    height = 5.9
    printf "Name: %s, Age: %d, Height: %.1f\n", name, age, height
    printf "Left-justified: %-10s|\n", "left"
    printf "Right-justified: %10s|\n", "right"
    printf "Zero-padded: %05d\n", 7
    printf "Hexadecimal: %x\n", 255
    printf "Octal: %o\n", 255
    printf "Scientific notation: %e\n", 12345.6789
    printf "Percentage: %%\n"
    printf "Newline: \\n, Tab: \\t\n"
    printf "Large integer: %d\n", 1234567890123456789
    printf "Negative integer: %d\n", -42
    printf "Negative floating-point: %f\n", -3.14159
    printf "Precision: %.2f\n", 3.14159
    printf "Width and precision: %10.2f\n", 3.14159
    printf "Special characters: %s\n", "Hello, \tWorld!\n"
    printf "Empty string: '%s'\n", ""
}