#!/bin/bash
#
# compare-macos.sh - Compare our mailx implementation against macOS /usr/bin/mailx
#
# This script runs 5 key tests comparing output between the system mailx and our
# Rust implementation. Run from the mailx/ directory or repository root.
#
# Usage: ./mailx/compare-macos.sh
#
# Prerequisites:
#   - macOS with /usr/bin/mailx
#   - cargo build completed (debug or release)
#

# Don't use set -e as we expect some commands to fail (e.g., -e with no mail)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Find script directory and repo root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ -f "$SCRIPT_DIR/Cargo.toml" ]]; then
    MAILX_DIR="$SCRIPT_DIR"
elif [[ -f "$SCRIPT_DIR/../mailx/Cargo.toml" ]]; then
    MAILX_DIR="$SCRIPT_DIR/../mailx"
else
    MAILX_DIR="$(pwd)/mailx"
fi
REPO_ROOT="$(cd "$MAILX_DIR/.." && pwd)"

# Test data file
TESTDATA="$MAILX_DIR/tests/cli/testdata.mbox"

# Check prerequisites
if [[ ! -f /usr/bin/mailx ]]; then
    echo -e "${RED}Error: /usr/bin/mailx not found. This script requires macOS.${NC}"
    exit 1
fi

if [[ ! -f "$TESTDATA" ]]; then
    echo -e "${RED}Error: Test data not found at $TESTDATA${NC}"
    exit 1
fi

# Find our mailx binary
if [[ -f "$REPO_ROOT/target/release/mailx" ]]; then
    OUR_MAILX="$REPO_ROOT/target/release/mailx"
elif [[ -f "$REPO_ROOT/target/debug/mailx" ]]; then
    OUR_MAILX="$REPO_ROOT/target/debug/mailx"
else
    echo -e "${YELLOW}Building mailx...${NC}"
    (cd "$REPO_ROOT" && cargo build -p posixutils-mailx --bin mailx)
    OUR_MAILX="$REPO_ROOT/target/debug/mailx"
fi

echo "=============================================="
echo "mailx Comparison Tests: macOS vs Our Implementation"
echo "=============================================="
echo "System mailx: /usr/bin/mailx"
echo "Our mailx:    $OUR_MAILX"
echo "Test data:    $TESTDATA"
echo ""

PASSED=0
FAILED=0

# Temp files for output comparison
MACOS_OUT=$(mktemp)
OUR_OUT=$(mktemp)
trap "rm -f $MACOS_OUT $OUR_OUT" EXIT

# Helper function to run a test
run_test() {
    local test_name="$1"
    local description="$2"
    shift 2

    echo -n "Test: $test_name - $description... "
}

# Helper to show results
show_result() {
    local result="$1"
    local details="$2"

    if [[ "$result" == "pass" ]]; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        if [[ -n "$details" ]]; then
            echo "  $details"
        fi
        FAILED=$((FAILED + 1))
    fi
}

# Normalize output for comparison (remove timestamps, whitespace differences)
normalize_output() {
    # Remove leading/trailing whitespace, normalize multiple spaces
    # Remove date/time patterns that vary
    sed -E \
        -e 's/[A-Z][a-z]{2} [A-Z][a-z]{2} [0-9 ][0-9] [0-9]{2}:[0-9]{2}/DATE_TIME/g' \
        -e 's/[0-9]{4}-[0-9]{2}-[0-9]{2}/DATE/g' \
        -e 's/[0-9]+\/[0-9]+/N\/N/g' \
        -e 's/^[[:space:]]+//' \
        -e 's/[[:space:]]+$//' \
        -e 's/[[:space:]]+/ /g' \
        -e '/^$/d'
}

echo "----------------------------------------------"
echo "TEST 1: -e option (check mail presence)"
echo "----------------------------------------------"

# Test 1a: -e with mail present (should exit 0)
run_test "1a" "-e with mail present returns exit 0"
/usr/bin/mailx -n -e -f "$TESTDATA" > /dev/null 2>&1
MACOS_EXIT=$?
"$OUR_MAILX" -n -e -f "$TESTDATA" > /dev/null 2>&1
OUR_EXIT=$?

if [[ "$MACOS_EXIT" -eq 0 && "$OUR_EXIT" -eq 0 ]]; then
    show_result "pass"
else
    show_result "fail" "macOS exit=$MACOS_EXIT, ours exit=$OUR_EXIT (expected both 0)"
fi

# Test 1b: -e with empty file (should exit non-zero)
EMPTY_MBOX=$(mktemp)
run_test "1b" "-e with no mail returns non-zero exit"
/usr/bin/mailx -n -e -f "$EMPTY_MBOX" > /dev/null 2>&1
MACOS_EXIT=$?
"$OUR_MAILX" -n -e -f "$EMPTY_MBOX" > /dev/null 2>&1
OUR_EXIT=$?
rm -f "$EMPTY_MBOX"

if [[ "$MACOS_EXIT" -ne 0 && "$OUR_EXIT" -ne 0 ]]; then
    show_result "pass"
else
    show_result "fail" "macOS exit=$MACOS_EXIT, ours exit=$OUR_EXIT (expected both non-zero)"
fi

echo ""
echo "----------------------------------------------"
echo "TEST 2: -H option (headers only)"
echo "----------------------------------------------"

run_test "2" "-H shows header summary and exits"
/usr/bin/mailx -n -H -f "$TESTDATA" > "$MACOS_OUT" 2>&1
MACOS_EXIT=$?
"$OUR_MAILX" -n -H -f "$TESTDATA" > "$OUR_OUT" 2>&1
OUR_EXIT=$?

# Check both exited successfully
if [[ "$MACOS_EXIT" -ne 0 || "$OUR_EXIT" -ne 0 ]]; then
    show_result "fail" "Exit codes: macOS=$MACOS_EXIT, ours=$OUR_EXIT"
else
    # Check both contain the senders
    MACOS_HAS_ALICE=$(grep -ci "alice" "$MACOS_OUT" || true)
    MACOS_HAS_BOB=$(grep -ci "bob" "$MACOS_OUT" || true)
    MACOS_HAS_CHARLIE=$(grep -ci "charlie" "$MACOS_OUT" || true)
    OUR_HAS_ALICE=$(grep -ci "alice" "$OUR_OUT" || true)
    OUR_HAS_BOB=$(grep -ci "bob" "$OUR_OUT" || true)
    OUR_HAS_CHARLIE=$(grep -ci "charlie" "$OUR_OUT" || true)

    if [[ "$MACOS_HAS_ALICE" -gt 0 && "$OUR_HAS_ALICE" -gt 0 && \
          "$MACOS_HAS_BOB" -gt 0 && "$OUR_HAS_BOB" -gt 0 && \
          "$MACOS_HAS_CHARLIE" -gt 0 && "$OUR_HAS_CHARLIE" -gt 0 ]]; then
        show_result "pass"
    else
        show_result "fail" "Missing senders in output"
        echo "  macOS output:"
        head -5 "$MACOS_OUT" | sed 's/^/    /'
        echo "  Our output:"
        head -5 "$OUR_OUT" | sed 's/^/    /'
    fi
fi

echo ""
echo "----------------------------------------------"
echo "TEST 3: from command"
echo "----------------------------------------------"

run_test "3a" "from * shows all message headers"
echo "from *" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo "from *" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

# Count lines with message indicators (should have 3 messages)
MACOS_LINES=$(grep -ciE "alice|bob|charlie" "$MACOS_OUT" || true)
OUR_LINES=$(grep -ciE "alice|bob|charlie" "$OUR_OUT" || true)

if [[ "$MACOS_LINES" -ge 3 && "$OUR_LINES" -ge 3 ]]; then
    show_result "pass"
else
    show_result "fail" "macOS has $MACOS_LINES sender lines, ours has $OUR_LINES"
fi

run_test "3b" "from 1 shows first message header"
echo -e "from 1\nquit" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo -e "from 1\nquit" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

MACOS_HAS_ALICE=$(grep -ci "alice" "$MACOS_OUT" || true)
OUR_HAS_ALICE=$(grep -ci "alice" "$OUR_OUT" || true)

if [[ "$MACOS_HAS_ALICE" -gt 0 && "$OUR_HAS_ALICE" -gt 0 ]]; then
    show_result "pass"
else
    show_result "fail" "Both should show alice"
fi

echo ""
echo "----------------------------------------------"
echo "TEST 4: print/type command"
echo "----------------------------------------------"

run_test "4a" "print 1 displays first message"
echo -e "print 1\nquit" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo -e "print 1\nquit" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

# Both should contain the message body
MACOS_HAS_MEETING=$(grep -ci "meeting tomorrow" "$MACOS_OUT" || true)
OUR_HAS_MEETING=$(grep -ci "meeting tomorrow" "$OUR_OUT" || true)
MACOS_HAS_FROM=$(grep -c "^From:" "$MACOS_OUT" || true)
OUR_HAS_FROM=$(grep -c "^From:" "$OUR_OUT" || true)

if [[ "$MACOS_HAS_MEETING" -gt 0 && "$OUR_HAS_MEETING" -gt 0 && \
      "$MACOS_HAS_FROM" -gt 0 && "$OUR_HAS_FROM" -gt 0 ]]; then
    show_result "pass"
else
    show_result "fail" "Message content mismatch"
    echo "  macOS: meeting=$MACOS_HAS_MEETING, From=$MACOS_HAS_FROM"
    echo "  Ours:  meeting=$OUR_HAS_MEETING, From=$OUR_HAS_FROM"
fi

run_test "4b" "type command (alias for print)"
echo -e "type 2\nquit" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo -e "type 2\nquit" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

MACOS_HAS_PROJECT=$(grep -ci "project" "$MACOS_OUT" || true)
OUR_HAS_PROJECT=$(grep -ci "project" "$OUR_OUT" || true)

if [[ "$MACOS_HAS_PROJECT" -gt 0 && "$OUR_HAS_PROJECT" -gt 0 ]]; then
    show_result "pass"
else
    show_result "fail" "type command output mismatch"
fi

echo ""
echo "----------------------------------------------"
echo "TEST 5: size command"
echo "----------------------------------------------"

run_test "5a" "size 1 shows message size"
echo -e "size 1\nquit" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo -e "size 1\nquit" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

# Both should show size output with message number
MACOS_HAS_SIZE=$(grep -E "1.*[0-9]+" "$MACOS_OUT" | head -1 || true)
OUR_HAS_SIZE=$(grep -E "1.*[0-9]+" "$OUR_OUT" | head -1 || true)

if [[ -n "$MACOS_HAS_SIZE" && -n "$OUR_HAS_SIZE" ]]; then
    show_result "pass"
    echo "  macOS: $MACOS_HAS_SIZE"
    echo "  Ours:  $OUR_HAS_SIZE"
else
    show_result "fail" "Size output missing"
fi

run_test "5b" "size * shows all message sizes"
echo -e "size *\nquit" | /usr/bin/mailx -n -N -f "$TESTDATA" > "$MACOS_OUT" 2>&1
echo -e "size *\nquit" | "$OUR_MAILX" -n -N -f "$TESTDATA" > "$OUR_OUT" 2>&1

# Count size lines (should be 3)
MACOS_SIZE_LINES=$(grep -cE "^[[:space:]]*[0-9]+.*[0-9]+" "$MACOS_OUT" || true)
OUR_SIZE_LINES=$(grep -cE "^[[:space:]]*[0-9]+.*[0-9]+" "$OUR_OUT" || true)

if [[ "$MACOS_SIZE_LINES" -ge 3 && "$OUR_SIZE_LINES" -ge 3 ]]; then
    show_result "pass"
else
    show_result "fail" "macOS has $MACOS_SIZE_LINES size lines, ours has $OUR_SIZE_LINES"
fi

echo ""
echo "=============================================="
echo "SUMMARY"
echo "=============================================="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo ""

if [[ "$FAILED" -eq 0 ]]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
