echo 'echo hello' > $TEST_WRITE_DIR/dot.txt
. $TEST_WRITE_DIR/dot.txt
echo $?

echo 'echo hello' > $TEST_WRITE_DIR/dot.txt
. -- $TEST_WRITE_DIR/dot.txt
echo $?

echo 'echo $LINENO
echo $LINENO' > $TEST_WRITE_DIR/dot.txt
. $TEST_WRITE_DIR/dot.txt
echo $?

x=value y=other_value
echo 'echo $x $y' > $TEST_WRITE_DIR/dot.txt
. $TEST_WRITE_DIR/dot.txt
echo $?

echo 'echo hello' > $TEST_WRITE_DIR/dot.txt
. $TEST_WRITE_DIR/dot.txt > $TEST_WRITE_DIR/dot_out.txt
echo $?
cat $TEST_WRITE_DIR/dot_out.txt
rm $TEST_WRITE_DIR/dot_out.txt

echo 'false' > $TEST_WRITE_DIR/dot.txt
. $TEST_WRITE_DIR/dot.txt
echo $?

rm $TEST_WRITE_DIR/dot.txt

