#!/bin/bash
#
# UUCP Test Suite - Main Test Runner
#
# Sets up an isolated SSH environment and runs all UUCP tests.
# Does NOT modify ~/.ssh or any system files.
#
# Usage: ./run_tests.sh [--keep]
#   --keep: Don't clean up test directory after tests (for debugging)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
KEEP_TEMP=0
if [ "$1" = "--keep" ]; then
    KEEP_TEMP=1
fi

# Create unique test directory
TEST_ID="$$_$(date +%s)"
TEST_ROOT="/tmp/uucp_test_${TEST_ID}"

echo "=== UUCP Test Suite ==="
echo "Test directory: $TEST_ROOT"
echo ""

# Cleanup function
cleanup() {
    echo ""
    echo "=== Cleanup ==="

    # Stop sshd if running
    if [ -f "$TEST_ROOT/ssh/sshd.pid" ]; then
        SSHD_PID=$(cat "$TEST_ROOT/ssh/sshd.pid" 2>/dev/null || true)
        if [ -n "$SSHD_PID" ] && kill -0 "$SSHD_PID" 2>/dev/null; then
            echo "Stopping sshd (PID $SSHD_PID)..."
            kill "$SSHD_PID" 2>/dev/null || true
            sleep 1
        fi
    fi

    if [ "$KEEP_TEMP" -eq 0 ]; then
        echo "Removing test directory..."
        rm -rf "$TEST_ROOT"
    else
        echo "Keeping test directory: $TEST_ROOT"
    fi
}

# Set trap for cleanup
trap cleanup EXIT

# Create directory structure
echo "=== Setting up test environment ==="
mkdir -p "$TEST_ROOT/ssh"
mkdir -p "$TEST_ROOT/spool"
mkdir -p "$TEST_ROOT/local"
mkdir -p "$TEST_ROOT/remote"
mkdir -p "$TEST_ROOT/bin"

# Find an available port (start from 2222)
find_available_port() {
    local port=2222
    while [ $port -lt 2300 ]; do
        if ! nc -z 127.0.0.1 $port 2>/dev/null; then
            echo $port
            return
        fi
        port=$((port + 1))
    done
    echo "ERROR: No available port found" >&2
    exit 1
}

SSH_PORT=$(find_available_port)
echo "Using SSH port: $SSH_PORT"

# Generate SSH keys
echo "Generating SSH keys..."
ssh-keygen -t ed25519 -f "$TEST_ROOT/ssh/host_key" -N "" -q
ssh-keygen -t ed25519 -f "$TEST_ROOT/ssh/client_key" -N "" -q

# Create authorized_keys
cp "$TEST_ROOT/ssh/client_key.pub" "$TEST_ROOT/ssh/authorized_keys"
chmod 600 "$TEST_ROOT/ssh/authorized_keys"

# Create sshd_config
cat > "$TEST_ROOT/ssh/sshd_config" << EOF
Port $SSH_PORT
ListenAddress 127.0.0.1
HostKey $TEST_ROOT/ssh/host_key
AuthorizedKeysFile $TEST_ROOT/ssh/authorized_keys
PasswordAuthentication no
PubkeyAuthentication yes
ChallengeResponseAuthentication no
UsePAM no
PidFile $TEST_ROOT/ssh/sshd.pid
LogLevel ERROR
StrictModes no
PrintMotd no
PrintLastLog no
EOF

# Create SSH wrapper script that UUCP will use
cat > "$TEST_ROOT/bin/ssh" << EOF
#!/bin/bash
# SSH wrapper for UUCP testing - intercepts ssh calls and adds our config
exec /usr/bin/ssh \\
    -p $SSH_PORT \\
    -i "$TEST_ROOT/ssh/client_key" \\
    -o UserKnownHostsFile=/dev/null \\
    -o StrictHostKeyChecking=no \\
    -o BatchMode=yes \\
    -o LogLevel=ERROR \\
    "\$@"
EOF
chmod +x "$TEST_ROOT/bin/ssh"

# Start sshd
echo "Starting sshd..."
/usr/sbin/sshd -f "$TEST_ROOT/ssh/sshd_config" -D -e 2>"$TEST_ROOT/ssh/sshd.log" &
SSHD_PID=$!
echo $SSHD_PID > "$TEST_ROOT/ssh/sshd.pid"

# Wait for sshd to be ready
echo "Waiting for sshd to start..."
for i in {1..30}; do
    if nc -z 127.0.0.1 $SSH_PORT 2>/dev/null; then
        echo "sshd is ready!"
        break
    fi
    if ! kill -0 $SSHD_PID 2>/dev/null; then
        echo -e "${RED}ERROR: sshd failed to start${NC}"
        cat "$TEST_ROOT/ssh/sshd.log"
        exit 1
    fi
    sleep 0.1
done

# Test SSH connection
echo "Testing SSH connection..."
if ! "$TEST_ROOT/bin/ssh" 127.0.0.1 echo "SSH connection works!"; then
    echo -e "${RED}ERROR: SSH connection test failed${NC}"
    cat "$TEST_ROOT/ssh/sshd.log"
    exit 1
fi

echo ""
echo "=== SSH environment ready ==="
echo ""

# Export environment variables for test scripts
export TEST_ROOT
export SSH_PORT
export TEST_SSH_BIN="$TEST_ROOT/bin/ssh"
export TEST_UUCP_SPOOL="$TEST_ROOT/spool"
export TEST_LOCAL_DIR="$TEST_ROOT/local"
export TEST_REMOTE_DIR="$TEST_ROOT/remote"
export PATH="$TEST_ROOT/bin:$PATH"

# Build UUCP if needed
echo "=== Building UUCP utilities ==="
cd "$REPO_ROOT"
cargo build --release -p posixutils-uucp 2>&1 | tail -5

UUCP_BIN="$REPO_ROOT/target/release/uucp"
UUX_BIN="$REPO_ROOT/target/release/uux"
UUSTAT_BIN="$REPO_ROOT/target/release/uustat"

if [ ! -x "$UUCP_BIN" ]; then
    echo -e "${RED}ERROR: uucp binary not found${NC}"
    exit 1
fi

export UUCP_BIN UUX_BIN UUSTAT_BIN

echo ""
echo "=== Running tests ==="
echo ""

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local test_script="$1"
    local test_name="$(basename "$test_script" .sh)"

    echo -n "Running $test_name... "

    if bash "$test_script" > "$TEST_ROOT/${test_name}.log" 2>&1; then
        echo -e "${GREEN}PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAILED${NC}"
        echo "  Log: $TEST_ROOT/${test_name}.log"
        cat "$TEST_ROOT/${test_name}.log" | sed 's/^/  /'
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Run individual test scripts
for test_script in "$SCRIPT_DIR"/test_*.sh; do
    if [ -f "$test_script" ]; then
        run_test "$test_script"
    fi
done

echo ""
echo "=== Test Summary ==="
echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Failed: ${RED}$TESTS_FAILED${NC}"

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
fi

exit 0
