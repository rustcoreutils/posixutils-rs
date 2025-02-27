cd tests/read_dir

cat 5<file1.txt <&5
cat 5<file1.txt 0<&5

cat 5<file1.txt 5<&- <file1.txt