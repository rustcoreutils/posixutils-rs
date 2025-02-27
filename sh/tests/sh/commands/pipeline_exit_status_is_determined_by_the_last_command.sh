echo test | cat | wc -c | true
echo $?

echo test | cat | wc -c | false
echo $?