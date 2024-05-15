#!/usr/bin/env bash

# This is a tool to 

# Define the directory containing the test files
tests_dir="fixtures/integration_tests/"

# Loop through each test file in the tests directory
for test_file in "$tests_dir"*.m4; do
    # Extract the test name from the test file name
    test_name=$(basename "$test_file" .m4)

    # Define stdout and stderr file paths
    stdout_file="${tests_dir}${test_name}.stdout"
    stderr_file="${tests_dir}${test_name}.stderr"

    if [[ ! -e "$stdout_file" || ! -e "$stderr_file" ]]; then
        # Run the test using GNU m4, capturing both stdout and stderr
        m4 "$test_file" > "$stdout_file" 2> "$stderr_file"

        # Print status message
        echo "Run test $test_name using using system m4 completed. Stdout saved to $stdout_file, stderr saved to $stderr_file"
    fi
done
