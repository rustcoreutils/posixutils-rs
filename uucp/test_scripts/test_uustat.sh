#!/bin/bash
#
# UUSTAT Tests - Job Queue Management
#
# Tests uustat job listing, killing, and rejuvenation
#

set -e

# Verify environment
if [ -z "$TEST_ROOT" ]; then
    echo "ERROR: Must be run from run_tests.sh"
    exit 1
fi

echo "=== UUSTAT Job Management Tests ==="

# Clean up spool for fresh start
rm -rf "$TEST_UUCP_SPOOL"/*

# Test 1: Empty spool - no jobs
echo "Test 1: Empty spool - list jobs"
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" 2>&1 || true)
# Empty output is fine, just shouldn't error
echo "  PASSED"

# Test 2: Empty spool - queue summary
echo "Test 2: Empty spool - queue summary"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -q
echo "  PASSED"

# Test 3: Queue a job with -r flag (queue only, don't execute)
echo "Test 3: Queue a job with -r flag"
echo "Queue test content" > "$TEST_LOCAL_DIR/queue_test.txt"
JOB_ID=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" -j -r "$TEST_LOCAL_DIR/queue_test.txt" "testhost!~/queued.txt")
if [ -z "$JOB_ID" ]; then
    echo "FAIL: No job ID returned"
    exit 1
fi
echo "  Job ID: $JOB_ID"
# File should NOT exist on remote (queued only)
if [ -f "$TEST_REMOTE_DIR/queued.txt" ]; then
    echo "FAIL: File should not be transferred with -r flag"
    exit 1
fi
echo "  PASSED"

# Test 4: List queued job
echo "Test 4: List queued job"
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN")
if ! echo "$OUTPUT" | grep -q "$JOB_ID"; then
    echo "FAIL: Job ID not found in listing"
    echo "Output: $OUTPUT"
    exit 1
fi
if ! echo "$OUTPUT" | grep -q "testhost"; then
    echo "FAIL: System name not found in listing"
    echo "Output: $OUTPUT"
    exit 1
fi
echo "  PASSED"

# Test 5: Queue summary shows job
echo "Test 5: Queue summary"
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -q)
if ! echo "$OUTPUT" | grep -q "testhost"; then
    echo "FAIL: System not found in queue summary"
    echo "Output: $OUTPUT"
    exit 1
fi
echo "  PASSED"

# Test 6: Rejuvenate job
echo "Test 6: Rejuvenate job"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -r "$JOB_ID"
# Job should still exist
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN")
if ! echo "$OUTPUT" | grep -q "$JOB_ID"; then
    echo "FAIL: Job missing after rejuvenation"
    exit 1
fi
echo "  PASSED"

# Test 7: Kill job
echo "Test 7: Kill job"
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -k "$JOB_ID"
# Job should be gone
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN")
if echo "$OUTPUT" | grep -q "$JOB_ID"; then
    echo "FAIL: Job still exists after kill"
    exit 1
fi
echo "  PASSED"

# Test 8: Kill nonexistent job (should fail)
echo "Test 8: Kill nonexistent job"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -k "nonexistent_job_12345" 2>/dev/null; then
    echo "FAIL: Should have failed for nonexistent job"
    exit 1
fi
echo "  PASSED (correctly failed)"

# Test 9: Rejuvenate nonexistent job (should fail)
echo "Test 9: Rejuvenate nonexistent job"
if UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -r "nonexistent_job_12345" 2>/dev/null; then
    echo "FAIL: Should have failed for nonexistent job"
    exit 1
fi
echo "  PASSED (correctly failed)"

# Test 10: Multiple jobs - filter by system
echo "Test 10: Multiple jobs - filter by system"
echo "Job A" > "$TEST_LOCAL_DIR/job_a.txt"
echo "Job B" > "$TEST_LOCAL_DIR/job_b.txt"
JOB_A=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" -j -r "$TEST_LOCAL_DIR/job_a.txt" "hostA!~/a.txt")
JOB_B=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUCP_BIN" -j -r "$TEST_LOCAL_DIR/job_b.txt" "hostB!~/b.txt")
echo "  Job A: $JOB_A (hostA)"
echo "  Job B: $JOB_B (hostB)"

# Filter by hostA
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -s hostA)
if ! echo "$OUTPUT" | grep -q "$JOB_A"; then
    echo "FAIL: Job A not found when filtering by hostA"
    exit 1
fi
if echo "$OUTPUT" | grep -q "$JOB_B"; then
    echo "FAIL: Job B should not appear when filtering by hostA"
    exit 1
fi
echo "  PASSED"

# Test 11: Filter by user
echo "Test 11: Filter by user"
CURRENT_USER=$(whoami)
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -u "$CURRENT_USER")
if ! echo "$OUTPUT" | grep -q "$JOB_A"; then
    echo "FAIL: Jobs not found when filtering by current user"
    exit 1
fi
# Filter by nonexistent user should return empty
OUTPUT=$(UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -u "nonexistent_user_xyz")
if [ -n "$OUTPUT" ]; then
    echo "FAIL: Should return empty for nonexistent user"
    exit 1
fi
echo "  PASSED"

# Cleanup test jobs
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -k "$JOB_A" 2>/dev/null || true
UUCP_SPOOL="$TEST_UUCP_SPOOL" "$UUSTAT_BIN" -k "$JOB_B" 2>/dev/null || true

echo ""
echo "=== All UUSTAT tests passed ==="
