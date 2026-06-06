BEGIN {
    # `cmd | getline` advances NR (but not FNR); `getline < file` advances
    # neither.
    "echo a" | getline x
    print x, NR, FNR
    "echo b" | getline y
    print y, NR, FNR
}
