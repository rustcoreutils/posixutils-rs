//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The ex command line (POSIX 94542-94646): `|` between commands,
//! abbreviated names, `"` comments, flags and counts after commands, and
//! escaped delimiters in `s` and `g`.

use super::addresses::{ex, ok};

const ABCD: &str = "a\nb\nc\nd\n";

#[test]
fn test_vertical_line_separates_commands() {
    assert_eq!(ok(ABCD, "1p|2p\nq!\n"), "a\nb\n");
    assert_eq!(ok(ABCD, "1p|\nq!\n"), "a\n");
    let (_, _, code, file) = ex("a\n", "%s/a/X/|w\nq\n");
    assert_eq!((code, file.as_str()), (0, "X\n"));
}

#[test]
fn test_global_takes_the_rest_of_the_line() {
    assert_eq!(ok("xa\nb\nxc\n", "g/x/s/x/y/|p\nq!\n"), "ya\nyc\n");
}

#[test]
fn test_shell_command_takes_the_rest_of_the_line() {
    assert!(ok(ABCD, "!echo a|tr a b\nq!\n").contains("b\n"));
}

#[test]
fn test_escaped_vertical_line_is_literal() {
    // `\|` is a `|` in the file name, not the end of the command: what is
    // written to `a|b` reads back from `a|b`.
    assert_eq!(ok("x\n", "w a\\|b\n$r a\\|b\n%p\nq!\n"), "x\nx\n");
}

#[test]
fn test_append_text_after_vertical_line() {
    // POSIX 94640-94642: a, c and i ended by `|` take the rest of the line as
    // their text.
    assert_eq!(ok("a\nb\n", "1a|new\n%p\nq!\n"), "a\nnew\nb\n");
}

#[test]
fn test_map_stops_only_at_unquoted_vertical_line() {
    let out = ok(ABCD, "map Q 1p\x16|2p|map\nq!\n");
    assert!(out.contains("1p|2p"), "map listing: {out:?}");
}

#[test]
fn test_abbreviated_command_names() {
    assert_eq!(ok(ABCD, "1del\n%p\nq!\n"), "b\nc\nd\n");
    assert_eq!(ok(ABCD, "1dele\n%p\nq!\n"), "b\nc\nd\n");
    assert_eq!(ok(ABCD, "2pri\nq!\n"), "b\n");
    assert_eq!(ok(ABCD, "1subst/a/X/\n1p\nq!\n"), "X\n");
    assert_eq!(ok(ABCD, "1,2co$\n%p\nq!\n"), "a\nb\nc\nd\na\nb\n");
}

#[test]
fn test_mark_name_attached_to_k() {
    assert_eq!(ok(ABCD, "2ka\n'ap\nq!\n"), "b\n");
}

#[test]
fn test_comments() {
    assert_eq!(ok(ABCD, "\" a comment\n1p\nq!\n"), "a\n");
    assert_eq!(ok(ABCD, "1p \" trailing comment\nq!\n"), "a\n");
}

#[test]
fn test_leading_colons_are_skipped() {
    assert_eq!(ok(ABCD, ":::2p\nq!\n"), "b\n");
}

#[test]
fn test_counts_and_flags_after_commands() {
    assert_eq!(ok(ABCD, "1p 2\nq!\n"), "a\nb\n");
    assert_eq!(ok(ABCD, "1d2\n%p\nq!\n"), "c\nd\n");
    assert_eq!(ok(ABCD, "1dp\nq!\n"), "b\n");
    assert_eq!(ok(ABCD, "1d#\nq!\n"), "     1  b\n");
    assert_eq!(ok(ABCD, "1s/a/X/#\nq!\n"), "     1  X\n");
    assert_eq!(ok(ABCD, "2d+\nq!\n"), "d\n");
}

#[test]
fn test_number_format() {
    // POSIX 95236: "%6d" and two blanks.
    assert_eq!(ok(ABCD, "1,2nu\nq!\n"), "     1  a\n     2  b\n");
}

#[test]
fn test_implied_print_remembers_flags() {
    // POSIX 94561-94564: the last #, p and l flags apply to the implied print.
    assert_eq!(ok(ABCD, "1nu\n2\nq!\n"), "     1  a\n     2  b\n");
}

#[test]
fn test_escaped_delimiter_in_substitute_and_global() {
    assert_eq!(ok("x/y\n", "s/x\\/y/Z/\np\nq!\n"), "Z\n");
    assert_eq!(ok("x/y\n", "s/x/a\\/b/\np\nq!\n"), "a/b/y\n");
    assert_eq!(ok("x/y\n", "s/[/]/Z/\np\nq!\n"), "xZy\n");
    assert_eq!(ok("x/y\nz\n", "g/x\\/y/d\n%p\nq!\n"), "z\n");
}

#[test]
fn test_initial_command_is_one_command_line() {
    // `|` separates the commands of a -c command line (and of EXINIT, which
    // runs through the same path but is skipped when stdin is not a
    // terminal, 94234) by the usual rules: a `map` keeps its ^V-quoted `|`.
    use plib::testing::get_binary_path;
    use std::io::Write;
    use std::process::{Command, Stdio};
    let dir = plib::tmp::TempDir::new().unwrap();
    std::fs::write(dir.path().join("f"), "a\n").unwrap();
    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", "-c", "set sw=3|map Q 1p\x16|2p", "f"])
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"set sw?\nmap\nq!\n")
        .unwrap();
    let out = String::from_utf8(child.wait_with_output().unwrap().stdout).unwrap();
    assert!(out.contains("shiftwidth=3"), "{out:?}");
    assert!(out.contains("1p|2p"), "{out:?}");
}

#[test]
fn test_huge_count_is_an_error_not_a_panic() {
    for cmd in [
        "1d 99999999999999999999",
        "1>99999999999999999999",
        "1j 99999999999999999999",
    ] {
        let (_, _, code, _) = ex(ABCD, &format!("{cmd}\nq!\n"));
        assert!(code == 1, "{cmd}: exit {code}");
    }
    // A count that does fit but runs past the end stops at the last line.
    assert_eq!(ok(ABCD, "2d 18446744073709551615\n%p\nq!\n"), "a\n");
}

#[test]
fn test_shift_count_is_lines_and_repeats_are_width() {
    // POSIX 95628-95649: `>>` is two shiftwidths; the count is lines.
    assert_eq!(ok("a\nb\nc\n", "set sw=4\n1>>\n1p\nq!\n"), "        a\n");
    assert_eq!(
        ok("a\nb\nc\n", "set sw=4\n1> 2\n%p\nq!\n"),
        "    a\n    b\nc\n"
    );
    // The shifted lines go to the unnamed buffer.
    assert_eq!(ok("a\nb\n", "set sw=2\n1>\n$put\n%p\nq!\n"), "  a\nb\na\n");
}

#[test]
fn test_count_starts_at_the_last_address() {
    // POSIX 94785-94788: `1,2d 2` is lines 2-3.
    assert_eq!(ok(ABCD, "1,2d 2\n%p\nq!\n"), "a\nd\n");
    assert_eq!(ok(ABCD, "1,2p 2\nq!\n"), "b\nc\n");
}

#[test]
fn test_substitute_leaves_current_line_on_last_change() {
    assert_eq!(ok("ab\nc\n", "1s/a/X/|p\nq!\n"), "Xb\n");
    assert_eq!(ok("ab\nc\n", "1s/a/X/|s/b/Y/|p\nq!\n"), "XY\n");
}

#[test]
fn test_copy_and_move_take_no_count_and_yank_no_flags() {
    for cmd in ["2,3m0 9", "1t$ 2", "1y 2p"] {
        let (_, _, code, _) = ex(ABCD, &format!("{cmd}\nq!\n"));
        assert_ne!(code, 0, "{cmd} accepted");
    }
}

#[test]
fn test_print_offset_flag() {
    assert_eq!(ok(ABCD, "1p+\nq!\n"), "a\nb\n");
}

#[test]
fn test_command_names_ignore_case() {
    assert_eq!(ok(ABCD, "2P\nq!\n"), "b\n");
}
