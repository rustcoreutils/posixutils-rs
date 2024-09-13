BEGIN {
    ARGV[1] = "tests/awk/test_data.txt"
    ARGC = 2
}

{print $0}