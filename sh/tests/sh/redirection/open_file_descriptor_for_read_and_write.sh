cd tests/read_dir

cat <>file1.txt
cat 0<>file1.txt

cd -
cd tests/write_dir

echo test 1<>open_file_descriptor_for_read_and_write.txt
cat open_file_descriptor_for_read_and_write.txt
rm open_file_descriptor_for_read_and_write.txt