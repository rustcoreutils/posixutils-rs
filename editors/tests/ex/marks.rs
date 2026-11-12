//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Marks follow their lines (POSIX 95138-95141), come back with a line an
//! undo restores, and the previous-context mark `'` (95142-95166).

use super::addresses::{ex, ok};

const ABCD: &str = "a\nb\nc\nd\n";

#[test]
fn test_mark_follows_its_line() {
    assert_eq!(ok(ABCD, "3k a\n1d\n'ap\nq!\n"), "c\n");
    assert_eq!(ok(ABCD, "2k a\n1t0\n'ap\nq!\n"), "b\n");
    assert_eq!(ok(ABCD, "1k a\n1m$\n'a=\nq!\n"), "4\n");
}

#[test]
fn test_mark_on_deleted_line_is_an_error() {
    let (_, _, code, file) = ex(ABCD, "2k a\n2d\n'ad\nw\nq\n");
    assert_ne!(code, 0, "'a named a deleted line");
    assert_eq!(file, ABCD);
}

#[test]
fn test_mark_survives_substitute_on_its_line() {
    assert_eq!(ok(ABCD, "2k a\n2s/b/X/\n1\n'ap\nq!\n"), "a\nX\n");
}

#[test]
fn test_undo_restores_mark_of_restored_line() {
    // POSIX 95139-95140.
    assert_eq!(ok(ABCD, "2k a\n2d\nu\n'ap\nq!\n"), "b\n");
}

#[test]
fn test_previous_context_mark() {
    // `2` sets `'` to where we were ($), so `''` goes back there.
    assert_eq!(ok(ABCD, "2\n''p\nq!\n"), "b\nd\n");
    // A search address sets it too.
    assert_eq!(ok(ABCD, "1\n/c/\n''p\nq!\n"), "a\nc\na\n");
}

/// A file `f` with `content` and a tags file naming tag `t` as `addr` in
/// `file`; run `script` in ex.
fn ex_with_tag(content: &str, file: &str, addr: &str, script: &str) -> (String, i32) {
    ex_in_tag_dir(&["-s", "f"], content, file, addr, script)
}

/// Run ex with `args` in a directory holding file `f` (`content`) and a
/// `tags` file with tag `t` at `addr` in `file`.
fn ex_in_tag_dir(
    args: &[&str],
    content: &str,
    file: &str,
    addr: &str,
    script: &str,
) -> (String, i32) {
    use plib::testing::get_binary_path;
    use std::io::Write;
    use std::process::{Command, Stdio};
    let dir = plib::tmp::TempDir::new().unwrap();
    std::fs::write(dir.path().join("f"), content).unwrap();
    std::fs::write(dir.path().join("tags"), format!("t\t{file}\t{addr}\n")).unwrap();
    let mut child = Command::new(get_binary_path("ex"))
        .args(args)
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env("TMPDIR", dir.path())
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(script.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    (
        String::from_utf8(out.stdout).unwrap(),
        out.status.code().unwrap_or(-1),
    )
}

#[test]
fn test_tag_in_current_file_ignores_modified_buffer() {
    // POSIX 95419-95423: only another file's tag refuses a modified buffer.
    let (out, code) = ex_with_tag(
        "a\nchar *p;\n",
        "f",
        "/^char \\*p;$/",
        "1d\ntag t\n.p\nq!\n",
    );
    assert_eq!((out.as_str(), code), ("char *p;\n", 0));
}

#[test]
fn test_tag_pattern_is_the_last_re() {
    let (out, _) = ex_with_tag("a\nxyz\nxyz\n", "f", "/xyz/", "tag t\ns//Q/\n.p\nq!\n");
    assert_eq!(out, "Q\n");
}

#[test]
fn test_tag_not_found_is_an_error() {
    let (_, code) = ex_with_tag("a\n", "f", "1", "tag nosuch\nq!\n");
    assert_ne!(code, 0);
}

#[test]
fn test_mark_through_undo_redo_undo() {
    // `u` twice redoes the delete, which drops the mark again; a third `u`
    // brings line and mark back once more.
    assert_eq!(ok(ABCD, "3k a\n3d\nu\nu\nu\n'ap\nq!\n"), "c\n");
    let (_, _, code, _) = ex(ABCD, "3k a\n3d\nu\nu\n'ap\nq!\n");
    assert_ne!(code, 0, "'a named a line the redo deleted");
}

#[test]
fn test_mark_survives_undo_of_move() {
    // Undoing `:m` must not delete and re-insert lines that did not change.
    assert_eq!(ok(ABCD, "3k a\n1m$\nu\n'ap\nq!\n"), "c\n");
}

#[test]
fn test_mark_restored_by_undo_of_global_delete() {
    // Each deletion's marks come back in the numbering it left behind.
    assert_eq!(ok("x1\nx2\nc\nd\n", "2k a\ng/^x/d\nu\n'ap\nq!\n"), "x2\n");
}

#[test]
fn test_mark_on_added_line_comes_back_with_redo() {
    // `u` removes the copy and its mark; the second `u` (a redo) puts both
    // back.
    assert_eq!(ok(ABCD, "1t0\n1k a\nu\nu\n'ap\nq!\n"), "a\n");
}

#[test]
fn test_tag_pattern_last_re_is_literal() {
    // The tag search is nomagic: as the last RE, `[1]` stays literal.
    let (out, _) = ex_with_tag("a\na[1]\n", "f", "/^a[1]$/", "tag t\ns//Q/\n.p\nq!\n");
    assert_eq!(out, "Q\n");
}

#[test]
fn test_failed_command_drops_do_not_leak_into_next_undo() {
    // A mark dropped by a later delete is that delete's, not an earlier
    // command's.
    assert_eq!(ok(ABCD, "2k a\n2d\n1k b\nu\n'ap\n'bp\nq!\n"), "b\na\n");
}

#[test]
fn test_moved_line_keeps_mark_through_undo_and_redo() {
    assert_eq!(
        ok(ABCD, "1k a\n1m$\n'ap\nu\n'ap\nu\n'a=\nq!\n"),
        "a\na\n4\n"
    );
}

#[test]
fn test_undo_of_in_place_change_keeps_mark() {
    // `>` rewrites its line; undoing it must not delete and re-insert it.
    assert_eq!(ok(ABCD, "2k a\n2>\nu\n'ap\nq!\n"), "b\n");
}

#[test]
fn test_undo_of_multi_line_join_restores_marks_in_place() {
    // The joins renumber as they go; the marks come back on their own
    // lines.
    assert_eq!(ok(ABCD, "3k x\n2k y\n1,3j\nu\n'xp\n'yp\nq!\n"), "c\nb\n");
}

#[test]
fn test_undo_of_move_among_repeated_lines() {
    // The rotation search must cope with lines that repeat.
    let text = "x\n\n\n\nz\n";
    assert_eq!(ok(text, "3k a\n1m4\nu\n'a=\n%p\nq!\n"), "3\nx\n\n\n\nz\n");
}

#[test]
fn test_mark_command_accepts_quote_marks() {
    // POSIX 95130-95131: `'` and `` ` `` are marks too.
    assert_eq!(ok(ABCD, "3k '\n1\n2k `\n''p\nq!\n"), "a\nb\n");
}

#[test]
fn test_mark_command_sets_first_non_blank() {
    // 95135: `:k` marks the first non-blank; `` `a `` goes to it -- in ex,
    // seen as the line; the column shows up in vi.
    assert_eq!(ok("  a\nb\n", "1k a\n'ap\nq!\n"), "  a\n");
}

#[test]
fn test_destination_address_sets_context_mark() {
    // 95144: `$` as the destination of `:m` is an ex address.
    assert_eq!(ok(ABCD, "2p\n.m$\n''p\nq!\n"), "b\nb\n");
}

#[test]
fn test_startup_tag_not_found_still_edits() {
    let (out, _) = ex_in_tag_dir(
        &["-s", "-t", "nosuch"],
        "a\n",
        "f",
        "1",
        "a\nx\n.\n%p\nq!\n",
    );
    assert_eq!(out, "x\n");
}
