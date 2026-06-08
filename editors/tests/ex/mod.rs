//! Integration tests for the ex editor.
//!
//! These tests verify the ex binary works correctly in line-oriented mode,
//! testing POSIX ex commands via stdin/stdout.

use plib::testing::{get_binary_path, run_test, TestPlan};
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};
use tempfile::{NamedTempFile, TempDir};

// Helper to create a test plan for ex in silent mode
fn ex_test(stdin: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: stdin.to_string(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// Helper to test ex with a file
fn ex_test_with_file(file_content: &str, stdin: &str, expected_out: &str) {
    let temp = NamedTempFile::new().unwrap();
    fs::write(temp.path(), file_content).unwrap();

    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string(), temp.path().to_string_lossy().to_string()],
        stdin_data: stdin.to_string(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// ============================================================================
// Basic Operation Tests
// ============================================================================

#[test]
fn test_ex_quit() {
    ex_test("q\n", "");
}

#[test]
fn test_ex_append_and_print() {
    ex_test(
        "a\nhello world\nline two\n.\n1,$p\nq!\n",
        "hello world\nline two\n",
    );
}

#[test]
fn test_ex_insert_and_print() {
    ex_test(
        "a\nfirst line\n.\n1i\ninserted line\n.\n1,$p\nq!\n",
        "inserted line\nfirst line\n",
    );
}

#[test]
fn test_ex_number_command() {
    ex_test(
        "a\nline one\nline two\n.\n1,$nu\nq!\n",
        "     1\tline one\n     2\tline two\n",
    );
}

#[test]
fn test_ex_list_command() {
    // Test that list shows $ at end of lines
    ex_test("a\nhello\n.\n1l\nq!\n", "hello$\n");
}

#[test]
fn test_ex_delete() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n2d\n1,$p\nq!\n",
        "line one\nline three\n",
    );
}

#[test]
fn test_ex_substitute() {
    ex_test(
        "a\nhello world\n.\n1s/world/everyone/\n1p\nq!\n",
        "hello everyone\n",
    );
}

#[test]
fn test_ex_substitute_global() {
    ex_test(
        "a\nhello hello hello\n.\n1s/hello/hi/g\n1p\nq!\n",
        "hi hi hi\n",
    );
}

#[test]
fn test_ex_yank_and_put() {
    ex_test(
        "a\nline one\nline two\n.\n1y\n2pu\n1,$p\nq!\n",
        "line one\nline two\nline one\n",
    );
}

#[test]
fn test_ex_copy() {
    ex_test(
        "a\nline one\nline two\n.\n1co2\n1,$p\nq!\n",
        "line one\nline two\nline one\n",
    );
}

#[test]
fn test_ex_move() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n1m2\n1,$p\nq!\n",
        "line two\nline one\nline three\n",
    );
}

#[test]
fn test_ex_goto_line() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n2\np\nq!\n",
        "line two\n",
    );
}

#[test]
fn test_ex_join() {
    ex_test(
        "a\nline one\nline two\n.\n1,2j\n1p\nq!\n",
        "line one line two\n",
    );
}

#[test]
fn test_ex_undo() {
    ex_test("a\nhello\n.\n1s/hello/goodbye/\nu\n1p\nq!\n", "hello\n");
}

// ============================================================================
// File Operation Tests
// ============================================================================

#[test]
fn test_ex_read_file() {
    ex_test_with_file("content from file\n", "1,$p\nq\n", "content from file\n");
}

#[test]
fn test_ex_write_file() {
    let temp = NamedTempFile::new().unwrap();
    let path = temp.path().to_string_lossy().to_string();

    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: format!("a\ntest content\n.\nw {}\nq\n", path),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(temp.path()).unwrap();
    assert_eq!(content, "test content\n");
}

// ============================================================================
// Address Range Tests
// ============================================================================

#[test]
fn test_ex_range_all() {
    ex_test("a\na\nb\nc\nd\n.\n1,$p\nq!\n", "a\nb\nc\nd\n");
}

#[test]
fn test_ex_range_single() {
    ex_test("a\na\nb\nc\n.\n2p\nq!\n", "b\n");
}

#[test]
fn test_ex_range_explicit() {
    ex_test("a\na\nb\nc\nd\ne\n.\n2,4p\nq!\n", "b\nc\nd\n");
}

#[test]
fn test_ex_current_line_address() {
    ex_test("a\nfirst\nsecond\nthird\n.\n2\n.p\nq!\n", "second\n");
}

#[test]
fn test_ex_last_line_address() {
    ex_test("a\nfirst\nlast\n.\n$p\nq!\n", "last\n");
}

// ============================================================================
// Global Command Tests
// ============================================================================

#[test]
fn test_ex_global_delete() {
    ex_test(
        "a\nkeep this\ndelete me\nkeep this too\ndelete me also\n.\ng/delete/d\n1,$p\nq!\n",
        "keep this\nkeep this too\n",
    );
}

#[test]
fn test_ex_global_print() {
    ex_test(
        "a\napple\nbanana\napricot\ncherry\n.\ng/^a/p\nq!\n",
        "apple\napricot\n",
    );
}

// ============================================================================
// Set Option Tests
// ============================================================================

#[test]
fn test_ex_set_option() {
    // Just verify set command is accepted without error
    ex_test("set number\nq!\n", "");
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_ex_invalid_command_silent() {
    // In silent mode, errors exit with code 1
    // Note: Error message goes to stderr which TestPlan captures separately
    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: "invalidcmd\n".to_string(),
        expected_out: String::new(),
        expected_err: "Invalid command: invalidcmd\n".to_string(),
        expected_exit_code: 1,
    });
}

// ============================================================================
// Version and Help Tests
// ============================================================================

#[test]
fn test_ex_version_command() {
    // Version is an informational message, suppressed in -s (silent) mode per POSIX
    ex_test("version\nq!\n", "");
}

// ============================================================================
// Shift Command Tests
// ============================================================================

#[test]
fn test_ex_shift_right() {
    ex_test(
        "a\nline one\nline two\n.\n1,2>\n1,$p\nq!\n",
        "        line one\n        line two\n",
    );
}

#[test]
fn test_ex_shift_left() {
    ex_test("a\n        indented\n.\n1<\n1p\nq!\n", "indented\n");
}

// ============================================================================
// Line Number Command Tests
// ============================================================================

#[test]
fn test_ex_line_number() {
    ex_test("a\nline one\nline two\nline three\n.\n=\nq!\n", "3\n");
}

#[test]
fn test_ex_line_number_with_address() {
    ex_test("a\nline one\nline two\nline three\n.\n2=\nq!\n", "2\n");
}

// ============================================================================
// Print with Line Numbers (#) Tests
// ============================================================================

#[test]
fn test_ex_hash_command() {
    ex_test(
        "a\nfirst\nsecond\n.\n1,2#\nq!\n",
        "     1\tfirst\n     2\tsecond\n",
    );
}

// ============================================================================
// Z Command Tests
// ============================================================================

#[test]
fn test_ex_z_command() {
    // z should display lines from the file
    ex_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n1z3\nq!\n",
        "line 1\nline 2\nline 3\n",
    );
}

// ============================================================================
// Repeat Substitute (&) Tests
// ============================================================================

#[test]
fn test_ex_repeat_substitute() {
    ex_test(
        "a\nhello world\nhello universe\n.\n1s/hello/hi/\n2&\n1,$p\nq!\n",
        "hi world\nhi universe\n",
    );
}

// ============================================================================
// EXINIT and .exrc Tests
// ============================================================================

/// Helper to run ex with custom env and optional working directory,
/// returning (stdout, stderr, exit_code).
fn run_ex_with_env(
    stdin: &str,
    env_vars: &[(&str, &str)],
    cwd: Option<&std::path::Path>,
) -> (String, String, i32) {
    let bin = get_binary_path("ex");
    let mut cmd = Command::new(bin);
    cmd.arg("-s")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    for (k, v) in env_vars {
        if v.is_empty() {
            cmd.env_remove(k);
        } else {
            cmd.env(k, v);
        }
    }
    let mut child = cmd.spawn().expect("failed to spawn ex");
    if let Some(mut si) = child.stdin.take() {
        si.write_all(stdin.as_bytes()).unwrap();
        si.flush().unwrap();
    }
    let out = child.wait_with_output().expect("failed to wait");
    (
        String::from_utf8_lossy(&out.stdout).to_string(),
        String::from_utf8_lossy(&out.stderr).to_string(),
        out.status.code().unwrap_or(-1),
    )
}

// ============================================================================
// EXINIT / .exrc startup configuration (audit #X3, #V9)
//
// Per POSIX, EXINIT and .exrc are consulted only in interactive mode; with -s
// (or a non-terminal stdin) they are suppressed. The piped-stdin harness runs
// ex with -s, so it can only verify the suppression below; the .exrc security
// checks are unit-tested in `config.rs` (no env/cwd mutation), and EXINIT-being-
// honored interactively is verified behaviorally.
// ============================================================================

#[test]
fn test_ex_silent_suppresses_exinit() {
    // #X3: with -s (the piped harness), EXINIT is ignored.
    let home = TempDir::new().unwrap();
    let (stdout, _stderr, code) = run_ex_with_env(
        "set number?\nq!\n",
        &[
            ("EXINIT", "set number"),
            ("HOME", home.path().to_str().unwrap()),
        ],
        None,
    );
    assert_eq!(code, 0);
    assert_eq!(stdout.trim(), "nonumber"); // EXINIT suppressed under -s
}

/// Recovery round-trip: `:preserve` saves the buffer; `ex -r file` restores it.
/// Regression test for audit #X2 (preserve) and #V4/#X5 (-r recovery).
#[test]
fn test_ex_preserve_and_recover_roundtrip() {
    let rec = TempDir::new().unwrap();
    let recdir = rec.path().to_str().unwrap();
    let work = TempDir::new().unwrap();
    let file = work.path().join("doc.txt");
    fs::write(&file, "base\n").unwrap();

    let bin = get_binary_path("ex");

    // 1) Append a line and :preserve the buffer.
    let mut cmd = Command::new(&bin);
    cmd.arg("-s")
        .arg(&file)
        .env("TMPDIR", recdir)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    let mut child = cmd.spawn().unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"a\nADDED\n.\npreserve\nq!\n")
        .unwrap();
    child.wait().unwrap();

    // Recovery files live in a per-user subdir vi.recover.<uid>.
    let count: usize = fs::read_dir(rec.path())
        .unwrap()
        .flatten()
        .filter(|e| e.file_name().to_string_lossy().starts_with("vi.recover."))
        .filter_map(|e| fs::read_dir(e.path()).ok())
        .map(|d| d.count())
        .sum();
    assert!(count >= 1, "preserve should create a recovery file");

    // 2) Recover the buffer and print it.
    let mut cmd = Command::new(&bin);
    cmd.arg("-s")
        .arg("-r")
        .arg(&file)
        .env("TMPDIR", recdir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    let mut child = cmd.spawn().unwrap();
    child.stdin.take().unwrap().write_all(b"%p\nq!\n").unwrap();
    let out = child.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("ADDED"),
        "recovered buffer should contain ADDED, got: {:?}",
        stdout
    );
}

/// Tag lookup: `ex -t tag` opens the file and jumps to the definition; `:tag`
/// does the same from the command line. Regression test for #X8/#V5.
#[test]
fn test_ex_tag_lookup() {
    let dir = TempDir::new().unwrap();
    fs::write(
        dir.path().join("src.c"),
        "int helper() { }\nint main() { }\n",
    )
    .unwrap();
    fs::write(
        dir.path().join("tags"),
        "main\tsrc.c\t/^int main/\nhelper\tsrc.c\t1\n",
    )
    .unwrap();

    let bin = get_binary_path("ex");

    // -t main: jump to the pattern address and print the current line.
    let mut cmd = Command::new(&bin);
    cmd.arg("-s")
        .arg("-t")
        .arg("main")
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    let mut child = cmd.spawn().unwrap();
    child.stdin.take().unwrap().write_all(b"p\nq!\n").unwrap();
    let out = child.wait_with_output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "int main() { }"
    );

    // :tag helper: line-number address.
    let mut cmd = Command::new(&bin);
    cmd.arg("-s")
        .arg(dir.path().join("src.c"))
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    let mut child = cmd.spawn().unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"tag helper\np\nq!\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "int helper() { }"
    );
}

// ============================================================================
// Address fidelity (audit #X4 trailing delimiter, #X9 offsets)
// ============================================================================

#[test]
fn test_ex_address_search_strips_delimiter() {
    // /re/ must not include the trailing delimiter in the pattern.
    ex_test("a\napple\nbanana\ncherry\n.\n/cherry/\np\nq!\n", "cherry\n");
}

#[test]
fn test_ex_address_offset_absolute() {
    ex_test("a\nL1\nL2\nL3\nL4\n.\n2+1p\nq!\n", "L3\n");
}

#[test]
fn test_ex_address_offset_last() {
    ex_test("a\nL1\nL2\nL3\nL4\n.\n$-1p\nq!\n", "L3\n");
}

#[test]
fn test_ex_address_offset_range() {
    ex_test("a\nL1\nL2\nL3\nL4\n.\n1,2+1p\nq!\n", "L1\nL2\nL3\n");
}
