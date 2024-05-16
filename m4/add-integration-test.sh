#!/usr/bin/env bash
#
# Define the directory containing the test files
testcases_dir="fixtures/integration_tests/"

# Function to generate a test case
generate_testcase() {
    local testcase_name="$1"
    local testcase_file="${testcases_dir}${testcase_name}.m4"
    local output_file="${testcases_dir}${testcase_name}.json"
    
    # Check if the test case file exists
    if [ ! -f "$testcase_file" ]; then
        echo "Error: Test case file '$testcase_name' not found."
        return
    fi
    
    # Capture stdout, stderr, and status
    local stdout=$(m4 "$testcase_file" 2> /tmp/stderr)
    local status=$?
    local stderr=$(cat /tmp/stderr)

    # Escape special characters in stdout and stderr for JSON
    stdout=$(sed -e 's/\\/\\\\/g' <<< "$stdout")
    stderr=$(sed -e 's/\\"/\\\\/g' <<< "$stderr")
    stdout=$(sed -e 's/\"/\\"/g' <<< "$stdout")
    stderr=$(sed -e 's/\"/\\"/g' <<< "$stderr")
    
    # Output JSON format to test1.json
    echo "{" > "${output_file}"
    echo "  \"stdout\": \"$stdout\"," >> "${output_file}"
    echo "  \"stderr\": \"$stderr\"," >> "${output_file}"
    echo "  \"status\": $status" >> "${output_file}"
    echo "}" >> "${output_file}"

    # Print status message
    echo "Generated test1.json for $testcase_file using system m4."

    # Print status message
    echo "Generated expected output for $testcase_file using system m4."
}

# If a test case name is provided as the first argument, generate only that test case
if [ $# -ge 1 ]; then
    generate_testcase "$1"
else
    echo "No test case name specified. Generating all test cases..."

    # Loop through each test case file in the test cases directory
    for testcase_file in "$testcases_dir"*.m4; do
        # Extract the test case name from the file name
        testcase_name=$(basename "$testcase_file" .m4)
        generate_testcase "$testcase_name"
    done
fi
