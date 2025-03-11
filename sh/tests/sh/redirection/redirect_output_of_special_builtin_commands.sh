mkdir -p tests/write_dir
cd $TEST_WRITE_DIR

set -o > redirect_output_of_special_builtin_commands.txt
cat redirect_output_of_special_builtin_commands.txt
rm redirect_output_of_special_builtin_commands.txt