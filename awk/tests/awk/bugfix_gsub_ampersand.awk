BEGIN {
    # & in replacement should insert matched text
    s = "hello"
    gsub(/l/, "[&]", s)
    print s

    # \& in replacement should insert literal &
    s = "hello"
    gsub(/l/, "\\&", s)
    print s

    # Mixed & and \&
    s = "hello"
    gsub(/l/, "[&]\\&", s)
    print s
}
