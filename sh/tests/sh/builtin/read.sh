echo test > $TEST_WRITE_DIR/read.txt
read var < $TEST_WRITE_DIR/read.txt
echo $?
echo $var

echo 'val1 val2' > $TEST_WRITE_DIR/read.txt
read var1 var2 < $TEST_WRITE_DIR/read.txt
echo $?
echo $var1
echo $var2

echo 'val1 val2 val3 val4' > $TEST_WRITE_DIR/read.txt
read var1 var2 var3 < $TEST_WRITE_DIR/read.txt
echo $?
echo $var1
echo $var2
echo $var3

echo val1 > $TEST_WRITE_DIR/read.txt
read var1 var2 < $TEST_WRITE_DIR/read.txt
echo $?
echo $var1
echo $var2

rm $TEST_WRITE_DIR/read.txt