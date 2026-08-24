//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
    // Writes to a path that does not yet exist. This previously wrote to a
    // NamedTempFile, which *creates* the file -- it only passed because ex was
    // missing POSIX write rule 3 (§95507: a named target that is not the
    // current pathname and already exists must fail). Verified against vim -e,
    // which likewise refuses that write.
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("written.txt");
    let path_str = path.to_string_lossy().to_string();

    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: format!("a\ntest content\n.\nw {}\nq\n", path_str),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&path).unwrap();
    assert_eq!(content, "test content\n");
}

// ============================================================================
// POSIX write rules (ex.md §95502-95519) -- audit #X22/#X24/#X27/#X29
// ============================================================================

/// Run ex over `content` with `script`; returns (exit code, stderr, tempdir).
fn ex_run(content: &str, args: &[&str], script: &str) -> (i32, String, TempDir) {
    let dir = TempDir::new().unwrap();
    let input = dir.path().join("in.txt");
    fs::write(&input, content).unwrap();

    let mut argv: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    argv.push(input.to_string_lossy().to_string());

    let mut child = Command::new(get_binary_path("ex"))
        .args(&argv)
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(script.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stderr).to_string(),
        dir,
    )
}

#[test]
fn test_ex_write_range_writes_only_that_range() {
    // #X24: `fn write` took the range as a bool and discarded it, so a partial
    // write silently wrote the ENTIRE buffer with no diagnostic. Byte-for-byte
    // agreement with `vim -e` was confirmed for this case.
    let (code, err, dir) = ex_run(
        "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\n",
        &["-s"],
        "1,3w out.txt\nq\n",
    );
    assert_eq!(code, 0, "partial write should succeed: {}", err);
    let out =
        fs::read_to_string(dir.path().join("out.txt")).expect("partial write must create target");
    assert_eq!(
        out, "L1\nL2\nL3\n",
        "only the addressed lines may be written"
    );
}

#[test]
fn test_ex_write_range_over_existing_file_is_never_forced() {
    // Rule 6 (§95514): a partial write to an existing file must fail, and --
    // unlike rules 1/2/3/5 -- `!` does not override it (§95516-95518).
    let (code, err, dir) = ex_run(
        "L1\nL2\nL3\n",
        &["-s"],
        "w whole.txt\n1,2w! whole.txt\nq!\n",
    );
    assert_ne!(code, 0, "rule 6 must fail even with '!'");
    assert!(
        err.contains("exists"),
        "diagnostic should name the existing file: {}",
        err
    );
    let whole = fs::read_to_string(dir.path().join("whole.txt")).unwrap();
    assert_eq!(whole, "L1\nL2\nL3\n", "the earlier full write must survive");
}

#[test]
fn test_ex_write_existing_other_file_requires_force() {
    // Rule 3 (§95507). Matches vim -e, which also refuses without '!'.
    let (code, err, dir) = ex_run("L1\n", &["-s"], "w other.txt\nw other.txt\nq!\n");
    assert_ne!(code, 0, "second write to the now-existing file must fail");
    assert!(err.contains("exists"), "unexpected diagnostic: {}", err);
    assert_eq!(
        fs::read_to_string(dir.path().join("other.txt")).unwrap(),
        "L1\n"
    );

    let (code, err, _dir) = ex_run("L1\n", &["-s"], "w o.txt\nw! o.txt\nq\n");
    assert_eq!(code, 0, "'!' must override rule 3: {}", err);
}

#[test]
fn test_ex_set_readonly_blocks_write() {
    // #X27: `:set ro` was disconnected from the write check, which consulted
    // only the -R flag, so `:set ro` then `:w` silently succeeded.
    let (code, err, _dir) = ex_run("L1\n", &["-s"], "set ro\nw\nq!\n");
    assert_ne!(code, 0, "readonly must block the write");
    assert!(err.to_lowercase().contains("read-only"), "got: {}", err);

    let (code, err, _dir) = ex_run("L1\n", &["-s"], "set ro\nset wa\nw\nq\n");
    assert_eq!(code, 0, "writeany must override readonly (§95518): {}", err);

    let (code, err, _dir) = ex_run("L1\n", &["-s"], "set ro\nw!\nq\n");
    assert_eq!(code, 0, "'!' must override readonly (§95516): {}", err);
}

#[test]
fn test_ex_xit_on_unmodified_buffer_does_not_write() {
    // #X29 (§95537): xit on a buffer unmodified since the last complete write
    // is equivalent to quit; `:wq` by contrast always writes.
    let dir = TempDir::new().unwrap();
    let input = dir.path().join("in.txt");
    fs::write(&input, "L1\nL2\n").unwrap();
    let before = fs::metadata(&input).unwrap().modified().unwrap();

    // Sleep past filesystem mtime granularity.
    std::thread::sleep(std::time::Duration::from_millis(1100));

    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", input.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child.stdin.as_mut().unwrap().write_all(b"x\n").unwrap();
    let out = child.wait_with_output().unwrap();
    assert_eq!(out.status.code(), Some(0));

    let after = fs::metadata(&input).unwrap().modified().unwrap();
    assert_eq!(
        before, after,
        "xit must not rewrite a buffer that was never modified"
    );
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

// ============================================================================
// Address resolution -- audit #X10/#X15/#X16/#X17/#X25/#X30
// ============================================================================

#[test]
fn test_ex_semicolon_rebases_second_address_comma_does_not() {
    // #X10: `;` sets the current line to the first address before evaluating
    // the second; `,` leaves the second relative to the original current line.
    // Both used to behave like `;`. Verified against vim -e.
    //
    // Buffer is 6 lines, current line starts at 1.
    //   3;+1p -> 3,4   (the +1 is relative to 3)
    //   3,+1p -> 3,2   (the +1 is relative to 1) -> start > end -> error
    ex_test_with_file("L1\nL2\nL3\nL4\nL5\nL6\n", "3;+1p\nq!\n", "L3\nL4\n");

    let (code, _err, _dir) = ex_run("L1\nL2\nL3\nL4\nL5\nL6\n", &["-s"], "3,+1p\nq!\n");
    assert_ne!(
        code, 0,
        "with ',' the second address is relative to the original current line, \
         so 3,+1 is an inverted range"
    );
}

#[test]
fn test_ex_excess_leading_addresses_are_discarded() {
    // #X17: POSIX keeps only the last two addresses. `1,2,3p` previously left a
    // stray leading ',' for the command splitter and died as "Invalid command".
    // vim -e prints L2,L3 for this.
    ex_test_with_file("L1\nL2\nL3\nL4\n", "1,2,3p\nq!\n", "L2\nL3\n");
}

#[test]
fn test_ex_mark_usable_as_address() {
    // #X16: Address::Mark::resolve was a stub returning MarkNotSet because the
    // resolver had no access to the editor's mark table.
    ex_test_with_file("L1\nL2\nL3\n", "2ma a\n'ap\nq!\n", "L2\n");
}

#[test]
fn test_ex_line_zero_address_for_read() {
    // #X15: line 0 is legal for the commands that insert *after* an address,
    // where it means "before the first line". `0r` used to fail outright.
    let dir = TempDir::new().unwrap();
    let extra = dir.path().join("extra.txt");
    fs::write(&extra, "INSERTED\n").unwrap();
    let (code, err, _d) = ex_run(
        "L1\nL2\n",
        &["-s"],
        &format!("0r {}\n1,$p\nq!\n", extra.display()),
    );
    assert_eq!(code, 0, "0r must be accepted: {}", err);
}

#[test]
fn test_ex_append_honors_non_literal_addresses() {
    // #X25: the parser extracted a raw usize and could only read a literal
    // Address::Line(n), falling back to line 1 for everything else -- so `$a`
    // appended after line 1. Now matches vim -e exactly.
    ex_test_with_file(
        "L1\nL2\nL3\n",
        "$a\nAPPENDED\n.\n1,$p\nq!\n",
        "L1\nL2\nL3\nAPPENDED\n",
    );
    // `.` and a mark must work too.
    ex_test_with_file(
        "L1\nL2\nL3\n",
        "2\n.a\nAFTER2\n.\n1,$p\nq!\n",
        "L1\nL2\nAFTER2\nL3\n",
    );
}

#[test]
fn test_ex_multibyte_mark_name_does_not_panic() {
    // #X30: parse_address sliced &input[2..] after reading a char, which is not
    // a char boundary for a multibyte mark name -- that panicked.
    let (code, _err, _dir) = ex_run("L1\n", &["-s"], "'\u{e9}p\nq!\n");
    assert!(
        code == 0 || code == 1,
        "a multibyte mark name must be rejected cleanly, not panic (got {})",
        code
    );
}

// ============================================================================
// substitute -- audit #X12/#X26/#X28
// ============================================================================

#[test]
fn test_ex_substitute_case_conversion_spec_examples() {
    // The POSIX spec ships these worked examples (ex.md §95726-95732). They
    // exercise \u, \U, \e, back-references and the `p` flag at once:
    //
    //   :s/\<.at\>/\u&/gp      -> The Cat Sat on the Mat.
    //   :s/S\(.*\)M/S\U\1\eM/p  -> The Cat SAT ON THE Mat.
    //
    // The spec's first pattern uses \< and \>, which are a GNU regex
    // extension rather than POSIX BRE -- BSD libc (macOS) spells word
    // boundaries [[:<:]]/[[:>:]] and rejects these, so the substitution
    // silently did nothing there and the test failed on macOS only. A bracket
    // expression selects the same three words and keeps the expected output
    // byte-identical, while still exercising everything the example is
    // actually here to test.
    ex_test_with_file(
        "The cat sat on the mat.\n",
        "s/[csm]at/\\u&/gp\nq!\n",
        "The Cat Sat on the Mat.\n",
    );
    ex_test_with_file(
        "The cat sat on the mat.\n",
        "s/[csm]at/\\u&/g\ns/S\\(.*\\)M/S\\U\\1\\eM/p\nq!\n",
        "The Cat SAT ON THE Mat.\n",
    );
}

#[test]
fn test_ex_substitute_print_flag_emits_the_line() {
    // #X26: `p` and `c` were parsed and stored but dead -- needs_confirm() and
    // should_print() had zero call sites, so `:s/../../p` printed nothing.
    ex_test_with_file("aaa\nbbb\n", "1s/aaa/ZZZ/p\nq!\n", "ZZZ\n");
}

#[test]
fn test_ex_substitute_number_and_list_flags() {
    // `#` prefixes the line number; `l` shows the line unambiguously with a
    // trailing '$'. Neither was even parsed before.
    ex_test_with_file(
        "aaa\nbbb\naaa\n",
        "1,$s/aaa/ZZZ/#\nq!\n",
        "1\tZZZ\n3\tZZZ\n",
    );
    ex_test_with_file("aaa\n", "1s/aaa/A\tB/l\nq!\n", "A\\tB$\n");
}

#[test]
fn test_ex_substitute_empty_pattern_reuses_last_regex() {
    // #X12/#X28: an empty pattern reuses the last RE (ex.md §95700). The
    // parser used to reject `s//x/` outright as NoPreviousSubstitution, which
    // is a decision only the editor can make.
    ex_test_with_file(
        "aaa\nbbb\naaa\n",
        "/bbb/\ns//REPLACED/\n1,$p\nq!\n",
        "aaa\nREPLACED\naaa\n",
    );

    // And a substitute's own pattern becomes the last RE (#X28), so a later
    // empty pattern picks it up.
    ex_test_with_file(
        "one two\nthree two\n",
        "1s/two/2/\n2s//2/\n1,$p\nq!\n",
        "one 2\nthree 2\n",
    );
}

#[test]
fn test_ex_substitute_tilde_reuses_previous_replacement() {
    // `~` in the replacement expands to the previous substitute's replacement
    // (ex.md §95713-95714).
    ex_test_with_file(
        "aaa\nbbb\n",
        "1s/aaa/XY/\n2s/bbb/~Z/\n1,$p\nq!\n",
        "XY\nXYZ\n",
    );
}

#[test]
fn test_ex_substitute_numeric_count() {
    // A trailing count operates on that many lines starting at the last line
    // of the address range.
    ex_test_with_file(
        "a1\na2\na3\na4\n",
        "1s/a/X/ 3\n1,$p\nq!\n",
        "X1\nX2\nX3\na4\n",
    );
}

#[test]
fn test_ex_substitute_confirm_flag_reads_stdin() {
    // #X26: the `c` flag was dead. POSIX/historical ex reads the answer from
    // standard input, in batch as well as interactively, so a script can
    // answer inline. 'y' accepts, anything else declines.
    let (code, _err, dir) = ex_run(
        "aaa\nbbb\naaa\n",
        &["-s"],
        "1,$s/aaa/YES/c\ny\nn\nw out.txt\nq!\n",
    );
    assert_eq!(code, 0);
    let out = fs::read_to_string(dir.path().join("out.txt")).unwrap();
    assert_eq!(
        out, "YES\nbbb\naaa\n",
        "the confirmed match is substituted and the declined one is left alone"
    );
}

// ============================================================================
// substitute with newline in the replacement -- audit #X12 remainder
// ============================================================================

#[test]
fn test_ex_substitute_newline_splits_line() {
    // #X12: a replacement containing a newline splits one line into several.
    // replace_line would have stuffed the newline into a single Line.
    // Assert on the line *count* via `nu`, not just the printed text: a single
    // Line that merely contains a newline prints identically to two Lines, so
    // a plain `p` comparison cannot tell the split apart from buffer
    // corruption.
    ex_test_with_file(
        "a-b\nc-d\n",
        "1s/-/\\n/\n1,$nu\nq!\n",
        "     1\ta\n     2\tb\n     3\tc-d\n",
    );
}

#[test]
fn test_ex_substitute_newline_is_undoable() {
    // The reason this was deferred: ChangeKind::Replace is a within-line
    // delete_char loop and cannot express a change in line count, so `u`
    // would have corrupted the buffer. ChangeKind::ReplaceLines now does.
    // Assert the intermediate state too: the buffer must actually reach 3
    // lines before the undo, otherwise this passes trivially against a build
    // where no split ever happened.
    ex_test_with_file(
        "a-b\nc-d\n",
        "1s/-/\\n/\n$=\nu\n$=\n1,$nu\nq!\n",
        "3\n2\n     1\ta-b\n     2\tc-d\n",
    );
}

#[test]
fn test_ex_substitute_newline_over_a_range_visits_every_line() {
    // Splitting shifts every later line down, so the loop bound has to grow
    // with it or the tail of the range gets skipped.
    ex_test_with_file(
        "a-b\nc-d\n",
        "1,$s/-/\\n/\n1,$nu\nq!\n",
        "     1\ta\n     2\tb\n     3\tc\n     4\td\n",
    );
}

#[test]
fn test_ex_backslash_newline_continues_the_command() {
    // The POSIX way to put a newline in a replacement is to escape a real
    // newline, so the command spans two input lines:
    //     s/-/\
    //     /
    // The reader used to cut the command at the newline, leaving the trailing
    // '/' to parse as a bare search ("No previous search pattern").
    ex_test_with_file(
        "a-b\nc-d\n",
        "1s/-/\\\n/\n1,$nu\nq!\n",
        "     1\ta\n     2\tb\n     3\tc-d\n",
    );
}

// ============================================================================
// Remaining ex deferrals -- audit #X18/#X23
// ============================================================================

#[test]
fn test_ex_tilde_uses_last_regex_with_previous_replacement() {
    // #X18: `~` pairs the previous substitute's REPLACEMENT with the last RE,
    // which may have come from a search. That is what distinguishes it from
    // `&`, which reuses the previous pattern.
    //
    // s/foo/XX/ then /bar/ then 2~  ->  line 2 becomes "XX two"
    ex_test_with_file(
        "foo one\nbar two\nfoo three\n",
        "1s/foo/XX/\n/bar/\n2~\n1,$p\nq!\n",
        "XX one\nXX two\nfoo three\n",
    );

    // `&` by contrast reuses the previous pattern, so it hits line 3.
    ex_test_with_file(
        "foo one\nbar two\nfoo three\n",
        "1s/foo/XX/\n3&\n1,$p\nq!\n",
        "XX one\nbar two\nXX three\n",
    );
}

#[test]
fn test_ex_read_shell_command_inherits_stdin() {
    // #X23: `:r !cmd` handed the child Stdio::null(), so a command that reads
    // stdin got nothing. POSIX (ex.md §95278-95280) says the program's stdin
    // "shall be set to the standard input of the ex program when it was
    // invoked". Here `cat` must therefore see the remaining script text.
    let (code, _err, dir) = ex_run("L1\n", &["-s"], "r !echo INSERTED\nw out.txt\nq!\n");
    assert_eq!(code, 0);
    let out = fs::read_to_string(dir.path().join("out.txt")).unwrap();
    assert!(
        out.contains("INSERTED"),
        "`:r !cmd` must insert the command's output, got {:?}",
        out
    );
}

// ============================================================================
// Command modifier/arg gaps (audit line 345) -- part 1
// ============================================================================

#[test]
fn test_ex_join_follows_posix_rules() {
    // ex.md §95060-95070. The old implementation collapsed all of this into
    // "trim leading spaces, add one space".
    //
    // Rule 4: current line ends in '.' -> two spaces.
    ex_test_with_file(
        "End of sentence.\n   next line\n",
        "1,2j\n1l\nq!\n",
        "End of sentence.  next line$\n",
    );
    // Rule 3: joined line starts with ')' -> no separator at all.
    ex_test_with_file("foo\n  ) paren\n", "1,2j\n1l\nq!\n", "foo) paren$\n");
    // Rule 2: a line that is empty after trimming is dropped.
    ex_test_with_file("a\n\nb\n", "1,3j\n1l\nq!\n", "a b$\n");
    // Rule 5: otherwise a single space.
    ex_test_with_file("a\n   b\n", "1,2j\n1l\nq!\n", "a b$\n");
}

#[test]
fn test_ex_join_bang_does_not_modify_lines() {
    // §95060-95061: `j!` joins with no modification of any line, so the
    // leading whitespace survives verbatim.
    ex_test_with_file(
        "End of sentence.\n   next line\n",
        "1,2j!\n1l\nq!\n",
        "End of sentence.   next line$\n",
    );
}

#[test]
fn test_ex_quit_warns_while_more_files_remain() {
    // POSIX: `:q` warns while files remain in the argument list; `:q!` does
    // not. Previously `:q` ignored the argument list entirely.
    let dir = TempDir::new().unwrap();
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    fs::write(&a, "A\n").unwrap();
    fs::write(&b, "B\n").unwrap();

    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", a.to_str().unwrap(), b.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child.stdin.as_mut().unwrap().write_all(b"q\nq!\n").unwrap();
    let out = child.wait_with_output().unwrap();
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(
        err.to_lowercase().contains("more files"),
        "plain :q should warn that more files remain, got {:?}",
        err
    );
}

// ============================================================================
// Command modifier/arg gaps -- part 2
// ============================================================================

#[test]
fn test_ex_list_uses_posix_escapes() {
    // ex.md §95237-95244: XBD Table 5-1 escapes, three-digit octal for other
    // non-printables, '$' at end of line and '\$' for a literal '$'.
    // `:l` previously used ^I-style caret notation and escaped neither
    // backslash nor '$', so its output was not unambiguous.
    ex_test_with_file("a\tb\n", "1l\nq!\n", "a\\tb$\n");
    ex_test_with_file("a\\b\n", "1l\nq!\n", "a\\\\b$\n");
    ex_test_with_file("cost $5\n", "1l\nq!\n", "cost \\$5$\n");
    // SOH (0x01) is not in Table 5-1, so it becomes a three-digit octal.
    ex_test_with_file("a\u{01}b\n", "1l\nq!\n", "a\\001b$\n");
}

#[test]
fn test_ex_at_at_repeats_last_buffer() {
    // `@@` means "repeat the last executed buffer". The parser accepted '@'
    // as a buffer *name*, so it failed with `Buffer "@" is empty`.
    ex_test_with_file("s/o/O/\nooo\n", "1y a\n2\n@a\n@@\n2p\nq!\n", "OOo\n");
}

// ============================================================================
// Command modifier/arg gaps -- part 3 (the `!` modifier and z/o/f/! arguments)
// ============================================================================

/// Run ex and return (stdout, stderr) without asserting on either.
fn ex_output(args: &[&str], stdin: &str) -> (String, String) {
    let mut child = Command::new(get_binary_path("ex"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    (
        String::from_utf8_lossy(&out.stdout).to_string(),
        String::from_utf8_lossy(&out.stderr).to_string(),
    )
}

#[test]
fn test_ex_bang_is_rejected_on_commands_that_do_not_take_one() {
    // ex.md §94854-94857 defines '!' as a modifier only for the commands whose
    // synopsis spells it. The parser used to fold '!' into the command *name*,
    // so a bang on any other command was silently swallowed.
    let (_, err) = ex_output(&["-s"], "pwd!\nq!\n");
    assert!(
        !err.is_empty(),
        "`:pwd!` should be rejected, got no diagnostic"
    );

    // ...while a command that does take one still works.
    let (out, _) = ex_output(&["-s"], "a\nx\n.\nq!\n");
    assert_eq!(out, "");
}

#[test]
fn test_ex_bang_must_be_adjacent_to_the_command_name() {
    // "the '!' character shall only act as a modifier if there is no <blank>
    // between it and the command name" (§94854-94857). With a blank, `:q !`
    // is not the forced quit, so a modified buffer must still refuse.
    let (_, err) = ex_output(&["-s"], "a\nx\n.\nq !\n");
    assert!(
        !err.is_empty(),
        "`:q !` must not be read as the forced quit"
    );
}

#[test]
fn test_ex_read_bang_runs_a_command_rather_than_forcing() {
    // `read` is one of the three commands whose '!' is not a modifier
    // (§94854-94857, §95268): `:r!cmd` reads the command's output.
    ex_test("r!echo hello\n1,$p\nq!\n", "hello\n");
    // ...and a <backslash> suppresses that meaning (§95285-95286), so this
    // names a file rather than a command.
    let (_, err) = ex_output(&["-s"], "r\\!nosuchfile\nq!\n");
    assert!(
        !err.is_empty(),
        "`:r \\!nosuchfile` should try to read a file and fail"
    );
}

#[test]
fn test_ex_append_bang_toggles_autoindent() {
    // §94894-94896: '!' toggles the autoindent edit option for the duration of
    // the command only, and §94705-94707 has ex supply the autoindent
    // characters as the input prompt. With autoindent off, `:a!` turns it on,
    // so both appended lines pick up the indent of the line they follow.
    ex_test_with_file(
        "    base\n",
        "1a!\none\ntwo\n.\n1,$p\nq!\n",
        "    base\n    one\n    two\n",
    );
    // Without the bang and with autoindent off, nothing is supplied.
    ex_test_with_file(
        "    base\n",
        "1a\none\ntwo\n.\n1,$p\nq!\n",
        "    base\none\ntwo\n",
    );
    // And with autoindent set, the bang turns it back off.
    ex_test_with_file(
        "    base\n",
        "set autoindent\n1a!\none\ntwo\n.\n1,$p\nq!\n",
        "    base\none\ntwo\n",
    );
    // The `.` terminator is still recognised while indent is being supplied
    // (the autoindent characters are a prompt, not input), and a line holding
    // nothing but autoindent is discarded (§94742-94743).
    ex_test_with_file(
        "    base\n",
        "1a!\none\n\ntwo\n.\n1,$p\nq!\n",
        "    base\n    one\n\n    two\n",
    );
}

#[test]
fn test_ex_change_count_extends_the_range() {
    // `c[hange][!][count]` -- count is "equivalent to specifying an additional
    // address ... equal to the last address specified plus count-1"
    // (§94785-94789), so `1c2` replaces lines 1 and 2.
    ex_test_with_file(
        "one\ntwo\nthree\n",
        "1c2\nNEW\n.\n1,$p\nq!\n",
        "NEW\nthree\n",
    );
}

#[test]
fn test_ex_z_repeated_type_characters_scroll_further() {
    // §95562-95592 defines the displacement in terms of the *number* of type
    // characters, so `z--` is not `z-`. Only the first character used to be
    // read, making every repeat a no-op.
    let file = "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\n";
    // `6z-2` -> decrement by (1 x 2) - 1 = 1, so start at line 5, 2 lines.
    ex_test_with_file(file, "6z-2\nq!\n", "l5\nl6\n");
    // `6z--2` -> decrement by (2 x 2) - 1 = 3, so start at line 3, 2 lines.
    ex_test_with_file(file, "6z--2\nq!\n", "l3\nl4\n");
    // `.` may not be repeated (§95572).
    let (_, err) = ex_output(&["-s"], "a\nx\n.\n1z..\nq!\n");
    assert!(!err.is_empty(), "`z..` should be an error");
}

#[test]
fn test_ex_z_past_the_end_is_an_error() {
    // "If incrementing the current line would cause it to be greater than the
    // last line in the edit buffer, it shall be an error." (§95552-95553)
    let (_, err) = ex_output(&["-s"], "a\nonly\n.\n$\nz\nq!\n");
    assert!(!err.is_empty(), "`z` on the last line should be an error");
    // Likewise running off the front (§95564).
    let (_, err) = ex_output(&["-s"], "a\na\nb\nc\n.\n1z-5\nq!\n");
    assert!(!err.is_empty(), "`1z-5` should be an error");
}

#[test]
fn test_ex_shell_escape_warns_about_unsaved_changes() {
    // §95607-95608: "a warning message shall be written if the edit buffer has
    // been modified since the last complete write, and the warn edit option is
    // set". The `warn` option was read nowhere in the crate.
    let (out, _) = ex_output(&["-s"], "a\nx\n.\n!true\nq!\n");
    assert!(
        out.contains("No write since last change"),
        "expected the warn message in {out:?}"
    );

    // `set nowarn` suppresses it...
    let (out, _) = ex_output(&["-s"], "set nowarn\na\nx\n.\n!true\nq!\n");
    assert!(
        !out.contains("No write since last change"),
        "`set nowarn` should suppress the warning: {out:?}"
    );
    // ...and an unmodified buffer never triggers it.
    let (out, _) = ex_output(&["-s"], "!true\nq!\n");
    assert!(
        !out.contains("No write since last change"),
        "an unmodified buffer must not warn: {out:?}"
    );
}

// ============================================================================
// Command modifier/arg gaps -- part 4 (argument lists and +command)
// ============================================================================

#[test]
fn test_ex_edit_accepts_a_plus_command() {
    // `e[dit][!][+command][file]` (§94946). The whole argument string used to
    // be taken as the filename, so this opened a file literally named
    // "+2 <path>".
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("three.txt");
    fs::write(&path, "one\ntwo\nthree\n").unwrap();
    let path = path.to_string_lossy().to_string();

    // `+2` runs `:2`, so the current line is 2 and `.p` prints "two".
    ex_test(&format!("e +2 {path}\n.p\nq!\n"), "two\n");
    // A bare `+` starts at the last line.
    ex_test(&format!("e + {path}\n.p\nq!\n"), "three\n");
    // Without a +command the file still opens normally.
    ex_test(&format!("e {path}\n1,$p\nq!\n"), "one\ntwo\nthree\n");
}

#[test]
fn test_ex_edit_plus_command_blanks_can_be_escaped() {
    // "<blank> characters within the +command can be escaped by preceding them
    // with a <backslash> character" (§94954-94955).
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("f.txt");
    fs::write(&path, "alpha\nbeta\n").unwrap();
    let path = path.to_string_lossy().to_string();

    // The replacement text holds a <blank>, escaped so the +command is not
    // split there and the rest taken as the filename.
    ex_test(
        &format!("e +2s/beta/B\\ E/ {path}\n1,$p\nq!\n"),
        "alpha\nB E\n",
    );
    // Unescaped, the same blank ends the +command, leaving an unterminated
    // substitute and taking `E/` as the start of the filename.
    let (_, err) = ex_output(&["-s"], &format!("e +2s/beta/B E/ {path}\n1,$p\nq!\n"));
    assert!(
        !err.is_empty(),
        "an unescaped blank should split the +command"
    );
}

#[test]
fn test_ex_next_replaces_the_argument_list() {
    // "Set the argument list to the specified filenames ... set the current
    // pathname to the first filename specified" (§95181-95184). The file list
    // was discarded outright, so `:n a b` behaved as a plain `:n`.
    let dir = TempDir::new().unwrap();
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    fs::write(&a, "AAA\n").unwrap();
    fs::write(&b, "BBB\n").unwrap();
    let (a, b) = (
        a.to_string_lossy().to_string(),
        b.to_string_lossy().to_string(),
    );

    // Start with no argument list at all; `:n a b` must set one and open `a`.
    ex_test(&format!("n {a} {b}\n1,$p\nq!\n"), "AAA\n");
    // ...and a following `:n` walks to the second entry of that new list.
    ex_test(&format!("n {a} {b}\nn\n1,$p\nq!\n"), "BBB\n");
    // `:n` also takes a +command.
    ex_test(&format!("n +$ {a} {b}\n.p\nq!\n"), "AAA\n");
}

#[test]
fn test_ex_next_honours_autowrite() {
    // "it shall be an error, unless the file is successfully written as
    // specified by the autowrite option" (§95178-95180).
    let dir = TempDir::new().unwrap();
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    fs::write(&a, "AAA\n").unwrap();
    fs::write(&b, "BBB\n").unwrap();
    let (a_path, b_path) = (
        a.to_string_lossy().to_string(),
        b.to_string_lossy().to_string(),
    );

    // Without autowrite a modified buffer refuses.
    let (_, err) = ex_output(&["-s", &a_path, &b_path], "a\nextra\n.\nn\nq!\n");
    assert!(
        !err.is_empty(),
        "`:n` on a modified buffer should be an error without autowrite"
    );

    // With autowrite it writes and moves on.
    let (out, _) = ex_output(
        &["-s", &a_path, &b_path],
        "set autowrite\na\nextra\n.\nn\n1,$p\nq!\n",
    );
    assert_eq!(out, "BBB\n", "expected to advance to the second file");
    assert_eq!(
        fs::read_to_string(&a).unwrap(),
        "AAA\nextra\n",
        "autowrite should have written the first file back"
    );
}

// ============================================================================
// `:z` regressions found by review
// ============================================================================

#[test]
fn test_ex_z_zero_count_does_not_panic() {
    // `repeats * count - 1` and `(repeats + 1) * count - 1` underflow `usize`
    // when count is 0, aborting the editor and losing the buffer in any build
    // with overflow checks. A literal `z-0` reaches it, and so does `z-` after
    // `:set scroll=0`, since the default count is `2 * scroll`.
    //
    // POSIX 95574 step 1: "If count is zero, nothing shall be written."
    // In a release build the subtraction wraps rather than panicking, and the
    // huge displacement then fails the bounds check -- so the observable
    // symptom is a spurious *error*. Asserting the specified outcome (nothing
    // written, nothing diagnosed) catches both builds.
    for cmd in ["1z-0", "1z^0", "1z+0", "1z.0"] {
        let (out, err) = ex_output(&["-s"], &format!("a\na\nb\nc\n.\n{cmd}\nq!\n"));
        assert_eq!(out, "", "a zero count must write nothing, got {out:?}");
        assert!(
            err.is_empty(),
            "`{cmd}` must not be an error: a zero count writes nothing, and the \
             displacement stays within the buffer. Got {err:?}"
        );
    }

    // The default count is `2 * scroll`, so `scroll=0` reaches the same path
    // without a literal zero anywhere in the command.
    let (out, err) = ex_output(&["-s"], "a\na\nb\nc\n.\nset scroll=0\n2z-\nq!\n");
    assert_eq!(out, "", "scroll=0 must write nothing, got {out:?}");
    assert!(err.is_empty(), "`z-` with scroll=0 must not error: {err:?}");
}

#[test]
fn test_ex_z_plus_starts_the_following_screenful() {
    // ex.md §95567-95571: for `+` the specified line is incremented by
    // `(((number of '+') - 1) x count) + 1` and lines are written "starting at
    // the new value of line". The single-`+` arm computed that value, used it
    // for the bounds check, then discarded it -- so `z+` re-displayed the line
    // the user was already on and never reached the end of the screenful.
    let file = "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8\nl9\n";
    ex_test_with_file(file, "1z+3\nq!\n", "l2\nl3\nl4\n");
    // Two `+` characters displace by ((2-1) x count) + 1, unchanged.
    ex_test_with_file(file, "1z++3\nq!\n", "l5\nl6\nl7\n");
    // No type at all still starts at the specified line.
    ex_test_with_file(file, "1z3\nq!\n", "l1\nl2\nl3\n");
}

#[test]
fn test_ex_z_blank_before_the_type_is_an_error() {
    // ex.md §95554-95555: "If there are <blank> characters between the type
    // argument and the preceding z command name or optional '!' character, it
    // shall be an error." The blank made `parse_z_args` miss the type entirely
    // and fall through to its count branch, so `z -` silently scrolled forward.
    for cmd in ["z -", "z .", "z  +", "z!  -"] {
        let (_, err) = ex_output(&["-s"], &format!("a\na\nb\nc\n.\n{cmd}\nq!\n"));
        assert!(
            !err.is_empty(),
            "`{cmd}` must be an error: a <blank> may not precede the type"
        );
    }
    // A blank before a *count* is still legal -- the rule is scoped to the type.
    ex_test_with_file("l1\nl2\nl3\nl4\n", "1z 2\nq!\n", "l1\nl2\n");
}

// ============================================================================
// Argument splitting and text input, from review
// ============================================================================

#[test]
fn test_ex_next_honours_escaped_blanks_in_filenames() {
    // `split_plus_command` honours `\ ` per §94954-94955 and then the operands
    // were split with `split_whitespace`, which tore `my\ file.txt` into two
    // and left the backslash on the first, so `:n` opened a file named `my\`.
    let dir = TempDir::new().unwrap();
    let spaced = dir.path().join("my file.txt");
    let plain = dir.path().join("plain.txt");
    fs::write(&spaced, "SPACED\n").unwrap();
    fs::write(&plain, "PLAIN\n").unwrap();

    let escaped = spaced.to_string_lossy().replace(' ', "\\ ");
    ex_test(
        &format!("n {escaped} {}\n1,$p\nq!\n", plain.to_string_lossy()),
        "SPACED\n",
    );
    // ...and the second entry of that argument list is still reachable.
    ex_test(
        &format!("n {escaped} {}\nn\n1,$p\nq!\n", plain.to_string_lossy()),
        "PLAIN\n",
    );
}

#[test]
fn test_ex_read_backslash_only_escapes_a_bang() {
    // §95285-95286 gives the <backslash> one job: suppressing the
    // `!`-means-command reading. Stripping it unconditionally renamed every
    // path, so `:r \tmp/x` looked for `tmp/x`.
    // Unescaped, `!` marks the rest as a command to run.
    ex_test("r !echo hello\n1,$p\nq!\n", "hello\n");

    // Escaped, it names a file instead -- so this must *not* run echo, and must
    // fail looking for a file whose name begins with '!'.
    let (out, err) = ex_output(&["-s"], "r \\!echo hello\n1,$p\nq!\n");
    assert!(
        !out.contains("hello"),
        "`r \\!echo` must not run the command, got {out:?}"
    );
    assert!(
        !err.is_empty(),
        "reading a file named `!echo ...` must fail"
    );

    // A backslash before anything else is part of the pathname, not an escape.
    // The file below exists; naming it with a leading backslash must not find
    // it, which is what stripping the backslash unconditionally used to do.
    let dir = TempDir::new().unwrap();
    let real = dir.path().join("x");
    fs::write(&real, "CONTENT\n").unwrap();

    let (out, err) = ex_output(
        &["-s"],
        &format!("r \\{}\n1,$p\nq!\n", real.to_string_lossy()),
    );
    assert!(
        !out.contains("CONTENT"),
        "`r \\{}` names a path starting with a backslash, which does not \
         exist; it must not read the unescaped file. Got {out:?}",
        real.to_string_lossy()
    );
    assert!(!err.is_empty(), "the nonexistent path must be diagnosed");

    // ...and without the backslash the same file reads fine, so the test above
    // is not passing merely because the path was unreadable.
    ex_test(
        &format!("r {}\n1,$p\nq!\n", real.to_string_lossy()),
        "CONTENT\n",
    );
}

#[test]
fn test_ex_autoindent_keeps_a_line_typed_as_blanks() {
    // §94742-94743 discards a line with "no characters other than autoindent
    // characters". The autoindent prefix is held apart from what the user
    // types, so that condition is "the user entered nothing" -- testing the
    // trimmed input also discarded a line deliberately typed as spaces.
    ex_test_with_file(
        "    base\n",
        "1a!\none\n  \ntwo\n.\n1,$p\nq!\n",
        "    base\n    one\n      \n      two\n",
    );
}

// ============================================================================
// The initial current line
// ============================================================================
//
// POSIX: ex begins on the *last* line of the edit buffer; vi begins on the
// first. Starting ex on line 1 made every command with a defaulted or relative
// address act on the wrong end of the file -- `:t.` appended after line 1
// instead of after the last line. Cross-checked against /usr/bin/ex.

#[test]
fn test_ex_starts_on_the_last_line() {
    ex_test_with_file("one\ntwo\nthree\n", ".=\nq!\n", "3\n");
}

#[test]
fn test_ex_default_address_is_the_last_line() {
    ex_test_with_file("one\ntwo\nthree\n", "p\nq!\n", "three\n");
}

#[test]
fn test_ex_copy_to_current_appends_at_the_end() {
    ex_test_with_file("one\ntwo\n", "1t.\n%p\nq!\n", "one\ntwo\none\n");
}

#[test]
fn test_ex_starts_on_the_last_line_of_a_single_line_file() {
    ex_test_with_file("only\n", ".=\nq!\n", "1\n");
}
