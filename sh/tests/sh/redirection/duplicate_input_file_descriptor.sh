cd $TEST_READ_DIR

cat 5<file1.txt <&5
cat 5<file1.txt 0<&5

cat 5<file1.txt 5<&- <file1.txt