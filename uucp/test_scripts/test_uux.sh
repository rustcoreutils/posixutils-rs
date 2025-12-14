#!/bin/bash
#
# UUX Tests - Remote Command Execution
#
# Tests uux remote command execution via SSH
#

set -e

# Verify environment
if [ -z "$TEST_ROOT" ]; then
    echo "ERROR: Must be run from run_tests.sh"
    exit 1
fi

echo "=== UUX Remote Execution Tests ==="

# Test 1: Simple remote echo
echo "Test 1: Simple remote echo"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!echo hello"
echo "  PASSED"

# Test 2: Remote command with arguments
echo "Test 2: Remote command with arguments"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!echo one two three"
echo "  PASSED"

# Test 3: Remote command that succeeds (true)
echo "Test 3: Remote command that succeeds"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!true"
echo "  PASSED"

# Test 4: Remote command that fails (false)
echo "Test 4: Remote command that fails (should return non-zero)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!false" 2>/dev/null; then
    echo "FAIL: Should have returned non-zero exit code"
    exit 1
fi
echo "  PASSED (correctly failed)"

# Test 5: -j flag (print job ID)
echo "Test 5: -j flag (print job ID)"
JOB_ID=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" -j "127.0.0.1!echo job test")
if [ -z "$JOB_ID" ]; then
    echo "FAIL: -j flag did not print job ID"
    exit 1
fi
echo "  Job ID: $JOB_ID"
echo "  PASSED"

# Test 6: -n flag (no notification - should still work)
echo "Test 6: -n flag (no notification)"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" -n "127.0.0.1!echo no notify"
echo "  PASSED"

# Test 7: Output redirection (simple >)
echo "Test 7: Output redirection"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!echo redirected > $TEST_REMOTE_DIR/redirect_output.txt"
if [ ! -f "$TEST_REMOTE_DIR/redirect_output.txt" ]; then
    echo "FAIL: Redirect output file not created"
    exit 1
fi
CONTENT=$(cat "$TEST_REMOTE_DIR/redirect_output.txt")
if [ "$CONTENT" != "redirected" ]; then
    echo "FAIL: Redirect content mismatch: $CONTENT"
    exit 1
fi
echo "  PASSED"

# Test 8: Disallowed >> (append) should fail
echo "Test 8: Disallowed >> (append)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!echo test >> /tmp/append.txt" 2>/dev/null; then
    echo "FAIL: >> should be disallowed"
    exit 1
fi
echo "  PASSED (correctly rejected)"

# Test 9: Disallowed << (heredoc) should fail
echo "Test 9: Disallowed << (heredoc)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!cat << EOF" 2>/dev/null; then
    echo "FAIL: << should be disallowed"
    exit 1
fi
echo "  PASSED (correctly rejected)"

# Test 10: Disallowed >| (clobber) should fail
echo "Test 10: Disallowed >| (clobber)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!echo test >| /tmp/clobber.txt" 2>/dev/null; then
    echo "FAIL: >| should be disallowed"
    exit 1
fi
echo "  PASSED (correctly rejected)"

# Test 11: Disallowed >& (redirect both) should fail
echo "Test 11: Disallowed >& (redirect both)"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "127.0.0.1!command >& /tmp/both.txt" 2>/dev/null; then
    echo "FAIL: >& should be disallowed"
    exit 1
fi
echo "  PASSED (correctly rejected)"

# Test 12: Local execution (no system prefix)
echo "Test 12: Local execution (no system prefix)"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "echo local execution"
echo "  PASSED"

# Test 13: Local execution with ! prefix
echo "Test 13: Local execution with ! prefix"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" "!echo local with bang"
echo "  PASSED"

# Test 14: -p flag (pipe stdin)
echo "Test 14: -p flag (pipe stdin)"
echo "piped input" | UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUX_BIN" -p "127.0.0.1!cat > $TEST_REMOTE_DIR/piped_output.txt"
CONTENT=$(cat "$TEST_REMOTE_DIR/piped_output.txt")
if [ "$CONTENT" != "piped input" ]; then
    echo "FAIL: Piped content mismatch: $CONTENT"
    exit 1
fi
echo "  PASSED"

echo ""
echo "=== All UUX tests passed ==="
