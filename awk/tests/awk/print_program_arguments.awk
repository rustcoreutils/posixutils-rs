BEGIN {
    print ARGC
    print ARGV[0]
    print ARGV[1]
    print ARGV[2]
    print ARGV[3]

    ARGV[0] = ""
    ARGV[1] = ""
    ARGV[2] = ""
    ARGV[3] = ""
}