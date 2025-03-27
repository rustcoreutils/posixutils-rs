echo 'this is a test' | cat | awk 'END {print length($0)}'

