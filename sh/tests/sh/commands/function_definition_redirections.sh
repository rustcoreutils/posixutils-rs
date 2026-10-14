# A function body may be followed by redirections (XCU 2.9.5); they are
# performed each time the function runs, inside any given at the call.
hidden() { echo hidden; } >/dev/null
hidden
hidden >&2
echo after hidden

to_stderr() {
    echo to stderr
} 1>&2
to_stderr 2>/dev/null
echo after to_stderr

reads_heredoc() { read line; echo "read: $line"; } <<EOF
from heredoc
EOF
reads_heredoc
reads_heredoc

n=0
counted() { n=$((n + 1)); echo "call $n"; } >&1
counted
counted
