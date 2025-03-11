cd $TEST_READ_DIR

. ./file2.txt
echo $?

x=value y=other_value
. ./filea.txt
echo $?