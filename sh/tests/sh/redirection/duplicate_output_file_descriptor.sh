mkdir -p tests/write_dir
cd tests/write_dir


echo test 5>duplicate_output_file_descriptor.txt >&5
cat duplicate_output_file_descriptor.txt
echo test2 5>duplicate_output_file_descriptor.txt 1>&5 >&5
cat duplicate_output_file_descriptor.txt
echo test3 5>duplicate_output_file_descriptor.txt 5>&-
cat duplicate_output_file_descriptor.txt

rm duplicate_output_file_descriptor.txt
