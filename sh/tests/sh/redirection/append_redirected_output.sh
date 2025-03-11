mkdir -p tests/write_dir
cd $TEST_WRITE_DIR

echo test >>append_redirected_output.txt
cat append_redirected_output.txt
echo test2 >>append_redirected_output.txt
cat append_redirected_output.txt
echo test3 1>>append_redirected_output.txt
cat append_redirected_output.txt
rm append_redirected_output.txt