cd tests/write_dir

echo test >>append_redirected_output.txt
cat append_redirected_output.txt
echo test2 >>append_redirected_output.txt
cat append_redirected_output.txt
echo test3 1>>append_redirected_output.txt
cat append_redirected_output.txt
rm append_redirected_output.txt