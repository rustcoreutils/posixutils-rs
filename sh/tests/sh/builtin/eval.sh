eval
echo $?

eval --
echo $?

eval echo hello
echo $?

eval -- echo hello
echo $?

eval echo hello \; echo world
echo $?

eval true '&&' echo correct
echo $?

eval false
echo $?

eval echo test > $TEST_WRITE_DIR/eval.txt
echo $?
cat $TEST_WRITE_DIR/eval.txt
rm $TEST_WRITE_DIR/eval.txt