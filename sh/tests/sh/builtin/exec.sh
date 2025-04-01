(exec echo hello)
(exec -- echo hello)

(exec 1> $TEST_WRITE_DIR/exec.txt; echo $?; echo hello)

cat $TEST_WRITE_DIR/exec.txt
rm $TEST_WRITE_DIR/exec.txt

exec echo hello

echo wrong
