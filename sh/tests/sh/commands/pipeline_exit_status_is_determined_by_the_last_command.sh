echo test | cat | wc -c >/dev/null | true
echo $?

echo test | cat | wc -c >/dev/null | false
echo $?