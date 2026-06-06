BEGIN {
    # Success path: open a read pipe, drain it, then close() in an
    # expression context.  Before the fix this panicked ("empty stack")
    # because close() pushed no return value.
    "echo hi" | getline line
    print line
    print close("echo hi")

    # close() in an assignment context.  The pipe is already closed, so
    # it is no longer open and close() must return non-zero.
    r = close("echo hi")
    print (r != 0)

    # Failure path: closing a name that was never opened returns non-zero.
    print (close("never_opened") != 0)
}
