print_hello() {
    hello="Hello, World!"
    echo $hello
}
echo $?

print_args() {
    echo $@
    echo $*
    echo $#
    echo $1
    echo $2
    echo $3
}
echo $?

non_zero_return () {
  false
}

print_hello
echo $?
print_args a b c
echo $?
non_zero_return
echo $?
