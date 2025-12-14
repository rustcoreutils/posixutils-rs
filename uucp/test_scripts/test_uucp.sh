#!/bin/bash
#
# UUCP Tests - File Transfer
#
# Tests uucp local->remote and remote->local transfers via SSH
#

set -e

# Verify environment
if [ -z "$TEST_ROOT" ]; then
    echo "ERROR: Must be run from run_tests.sh"
    exit 1
fi

echo "=== UUCP File Transfer Tests ==="

# Helper to check file content
check_file() {
    local file="$1"
    local expected="$2"
    local actual

    if [ ! -f "$file" ]; then
        echo "FAIL: File does not exist: $file"
        return 1
    fi

    actual=$(cat "$file")
    if [ "$actual" != "$expected" ]; then
        echo "FAIL: Content mismatch in $file"
        echo "  Expected: $expected"
        echo "  Actual: $actual"
        return 1
    fi
    return 0
}

# Test 1: Local to remote basic transfer
echo "Test 1: Local to remote basic transfer"
echo "Hello from local!" > "$TEST_LOCAL_DIR/file1.txt"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "$TEST_LOCAL_DIR/file1.txt" "127.0.0.1!$TEST_REMOTE_DIR/file1.txt"
check_file "$TEST_REMOTE_DIR/file1.txt" "Hello from local!"
echo "  PASSED"

# Test 2: Local to remote with directory creation
echo "Test 2: Local to remote with directory creation"
echo "Nested content" > "$TEST_LOCAL_DIR/nested.txt"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "$TEST_LOCAL_DIR/nested.txt" "127.0.0.1!$TEST_REMOTE_DIR/deep/nested/path/nested.txt"
check_file "$TEST_REMOTE_DIR/deep/nested/path/nested.txt" "Nested content"
echo "  PASSED"

# Test 3: Remote to local basic transfer
echo "Test 3: Remote to local basic transfer"
echo "Hello from remote!" > "$TEST_REMOTE_DIR/remote_file.txt"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "127.0.0.1!$TEST_REMOTE_DIR/remote_file.txt" "$TEST_LOCAL_DIR/downloaded.txt"
check_file "$TEST_LOCAL_DIR/downloaded.txt" "Hello from remote!"
echo "  PASSED"

# Test 4: Remote to local with directory creation
echo "Test 4: Remote to local with directory creation"
echo "Another remote file" > "$TEST_REMOTE_DIR/another.txt"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "127.0.0.1!$TEST_REMOTE_DIR/another.txt" "$TEST_LOCAL_DIR/new/subdir/another.txt"
check_file "$TEST_LOCAL_DIR/new/subdir/another.txt" "Another remote file"
echo "  PASSED"

# Test 5: Multiple files to remote directory
echo "Test 5: Multiple files to remote directory"
echo "A" > "$TEST_LOCAL_DIR/a.txt"
echo "B" > "$TEST_LOCAL_DIR/b.txt"
echo "C" > "$TEST_LOCAL_DIR/c.txt"
mkdir -p "$TEST_REMOTE_DIR/multi"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "$TEST_LOCAL_DIR/a.txt" "$TEST_LOCAL_DIR/b.txt" "$TEST_LOCAL_DIR/c.txt" "127.0.0.1!$TEST_REMOTE_DIR/multi/"
check_file "$TEST_REMOTE_DIR/multi/a.txt" "A"
check_file "$TEST_REMOTE_DIR/multi/b.txt" "B"
check_file "$TEST_REMOTE_DIR/multi/c.txt" "C"
echo "  PASSED"

# Test 6: Binary content preservation
echo "Test 6: Binary content preservation"
printf 'Line1\nLine2\n\tTabbed\x00Null' > "$TEST_LOCAL_DIR/binary.dat"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "$TEST_LOCAL_DIR/binary.dat" "127.0.0.1!$TEST_REMOTE_DIR/binary.dat"
if ! cmp -s "$TEST_LOCAL_DIR/binary.dat" "$TEST_REMOTE_DIR/binary.dat"; then
    echo "FAIL: Binary content not preserved"
    exit 1
fi
echo "  PASSED"

# Test 7: -j flag (print job ID)
echo "Test 7: -j flag (print job ID)"
echo "Job test" > "$TEST_LOCAL_DIR/job_test.txt"
JOB_ID=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" -j "$TEST_LOCAL_DIR/job_test.txt" "127.0.0.1!$TEST_REMOTE_DIR/job_test.txt")
if [ -z "$JOB_ID" ]; then
    echo "FAIL: -j flag did not print job ID"
    exit 1
fi
echo "  Job ID: $JOB_ID"
echo "  PASSED"

# Test 8: Path with spaces
echo "Test 8: Path with spaces"
mkdir -p "$TEST_LOCAL_DIR/path with spaces"
mkdir -p "$TEST_REMOTE_DIR/dest with spaces"
echo "Space content" > "$TEST_LOCAL_DIR/path with spaces/file with spaces.txt"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "$TEST_LOCAL_DIR/path with spaces/file with spaces.txt" "127.0.0.1!$TEST_REMOTE_DIR/dest with spaces/result.txt"
check_file "$TEST_REMOTE_DIR/dest with spaces/result.txt" "Space content"
echo "  PASSED"

# Test 9: Remote file not found (should fail)
echo "Test 9: Remote file not found (should fail)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" "127.0.0.1!$TEST_REMOTE_DIR/nonexistent_file_12345.txt" "$TEST_LOCAL_DIR/should_not_exist.txt" 2>/dev/null; then
    echo "FAIL: Should have failed for nonexistent file"
    exit 1
fi
if [ -f "$TEST_LOCAL_DIR/should_not_exist.txt" ]; then
    echo "FAIL: File should not have been created"
    exit 1
fi
echo "  PASSED (correctly failed)"

# Test 10: -f flag (don't create directories - should fail)
echo "Test 10: -f flag (don't create directories)"
echo "No mkdir test" > "$TEST_LOCAL_DIR/no_mkdir.txt"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" -f "$TEST_LOCAL_DIR/no_mkdir.txt" "127.0.0.1!$TEST_REMOTE_DIR/nonexistent_dir_xyz/file.txt" 2>/dev/null; then
    echo "FAIL: Should have failed with -f when directory doesn't exist"
    exit 1
fi
echo "  PASSED (correctly failed)"

echo ""
echo "=== All UUCP tests passed ==="
