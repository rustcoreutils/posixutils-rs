cd $TEST_READ_DIR

cat <>file1.txt
cat 0<>file1.txt

cd ../../
mkdir -p tests/write_dir
cd $TEST_WRITE_DIR

echo test 1<>open_file_descriptor_for_read_and_write.txt
cat open_file_descriptor_for_read_and_write.txt
rm open_file_descriptor_for_read_and_write.txt