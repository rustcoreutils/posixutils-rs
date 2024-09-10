BEGIN {
    print CONVFMT
    print "" 123.456
    CONVFMT = "%.2f"
    print "" 123.456
    CONVFMT = "%.0f"
    print "" 123.456
    CONVFMT = "%e"
    print "" 123.456
    CONVFMT = "%a"
    print "" 123.456
}