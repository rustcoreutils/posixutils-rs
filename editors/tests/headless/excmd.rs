//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ex commands typed in vi: `:` lines against vi's own `/` prompt, and
//! failures that stay on the status line.

use vi_rs::Editor;

fn editor(text: &str) -> Editor {
    let mut e = Editor::new_headless();
    e.set_buffer_text(text);
    e
}

#[test]
fn test_colon_line_number_past_either_end_clamps() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys(":999\r").unwrap();
    assert_eq!(e.get_cursor().line, 3);
    e.execute_keys(":0\r").unwrap();
    assert_eq!(e.get_cursor().line, 1);
}

#[test]
fn test_colon_search_address_runs_its_command() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys(":/b/d\r").unwrap();
    assert_eq!(e.get_buffer_text(), "a\nc\n");
}

#[test]
fn test_slash_prompt_is_a_search() {
    let mut e = editor("a\nb/d\nc\n");
    // At the `/` prompt `b/d` is the pattern `b` and an offset: still a
    // search, never an ex command.
    e.execute_keys("/b\\/d\r").unwrap();
    assert_eq!(e.get_cursor().line, 2);
    assert_eq!(e.get_buffer_text(), "a\nb/d\nc\n");
}

#[test]
fn test_failed_command_does_not_change_the_search_pattern() {
    let mut e = editor("b\nc\nb\nc\n");
    e.execute_keys("/b\r").unwrap();
    // A line that fails to parse runs nothing, its address included.
    e.execute_keys(":/c/zzz\r").unwrap();
    e.execute_keys("1Gn").unwrap();
    assert_eq!(e.get_cursor().line, 3, "n searched for something else");
}

#[test]
fn test_failed_ampersand_stays_on_the_status_line() {
    let mut e = editor("foo\nbar\n");
    e.execute_keys(":s/foo/x/\r").unwrap();
    // No `foo` on line 2: an error message, not the end of the session.
    assert!(e.execute_keys("j&").is_ok());
    assert_eq!(e.get_buffer_text(), "x\nbar\n");
}

#[test]
fn test_global_keeps_the_search_direction() {
    let mut e = editor("x\nfoo\nx\nfoo\nx\n");
    e.execute_keys("G?foo\r").unwrap();
    // The global's RE becomes the last RE; the direction stays backward.
    e.execute_keys(":g/x/p\r").unwrap();
    assert_eq!(e.get_cursor().line, 5);
    e.execute_keys("n").unwrap();
    // Backward from line 5 is line 3; forward would have wrapped to 1.
    assert_eq!(e.get_cursor().line, 3);
}

#[test]
fn test_vi_mark_follows_its_line() {
    let mut e = editor("a\nb\nc\nd\n");
    // Mark `b`, delete the line above it, and go back to the mark.
    e.execute_keys("jmakdd'a").unwrap();
    assert_eq!(e.get_cursor().line, 1);
    assert_eq!(e.get_buffer_text(), "b\nc\nd\n");
}

#[test]
fn test_vi_previous_context_mark() {
    let mut e = editor("a\nb\nc\nd\n");
    e.execute_keys("3G").unwrap();
    e.execute_keys("1G''").unwrap();
    assert_eq!(e.get_cursor().line, 3);
    e.execute_keys("``").unwrap();
    assert_eq!(e.get_cursor().line, 1);
}

#[test]
fn test_vi_multi_line_join_undo_restores_mark() {
    let mut e = editor("a\nb\nc\nd\n");
    e.execute_keys("3Gmx1G3Ju'x").unwrap();
    assert_eq!(e.get_cursor().line, 3);
}

#[test]
fn test_vi_undo_of_shift_keeps_mark() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys("2Gma>>u1G'a").unwrap();
    assert_eq!(e.get_cursor().line, 2);
}

#[test]
fn test_vi_g_without_line_change_keeps_context_mark() {
    // Rule 6: `G` sets `''` only when the current line changes.
    let mut e = editor("a\nb\n  c\n");
    e.execute_keys("2G3G$G''").unwrap();
    assert_eq!(e.get_cursor().line, 2);
}

#[test]
fn test_vi_m_accepts_quote_mark() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys("3Gm'1G`'").unwrap();
    assert_eq!(e.get_cursor().line, 3);
}

#[test]
fn test_vi_address_with_bar_prints_and_sets_context_mark() {
    // 94553-94561: `:3|` is not 6.a -- it is an implied print, which jumps.
    let mut e = editor("a\nb\nc\n");
    e.execute_keys(":3|\r''").unwrap();
    assert_eq!(e.get_cursor().line, 1);
}

#[test]
fn test_quit_twice_with_files_left() {
    // 95262-95264: with files left, `:q` is an error -- unless the command
    // before it was a quit too.
    let dir = plib::tmp::TempDir::new().unwrap();
    let (f, g) = (dir.path().join("f"), dir.path().join("g"));
    std::fs::write(&f, "f\n").unwrap();
    std::fs::write(&g, "g\n").unwrap();
    let names = vec![
        f.to_str().unwrap().to_string(),
        g.to_str().unwrap().to_string(),
    ];
    let mut e = Editor::new_headless();
    e.open_files(names.clone()).unwrap();
    e.execute_keys(":q\r").unwrap();
    assert!(!e.should_quit(), "files left: the first :q refuses");
    e.execute_keys(":q\r").unwrap();
    assert!(e.should_quit(), "a second :q in a row quits");

    // A command in between starts the count again.
    let mut e = Editor::new_headless();
    e.open_files(names).unwrap();
    e.execute_keys(":q\r0:q\r").unwrap();
    assert!(!e.should_quit());
}

/// An editor on two files, `f` and `g`, in `dir`.
fn on_two_files(dir: &plib::tmp::TempDir) -> Editor {
    let names: Vec<String> = ["f", "g"]
        .iter()
        .map(|n| {
            let p = dir.path().join(n);
            std::fs::write(&p, format!("{n}\n")).unwrap();
            p.to_str().unwrap().to_string()
        })
        .collect();
    let mut e = Editor::new_headless();
    e.open_files(names).unwrap();
    e
}

#[test]
fn test_wq_and_zz_follow_the_quit_rules() {
    // 95497, 95537-95540: write, then quit as `:q` would.
    let dir = plib::tmp::TempDir::new().unwrap();
    let mut e = on_two_files(&dir);
    e.execute_keys(":wq\r").unwrap();
    assert!(!e.should_quit(), "files left");
    e.execute_keys(":wq\r").unwrap();
    assert!(e.should_quit());
    let mut e = on_two_files(&dir);
    e.execute_keys("ZZ").unwrap();
    assert!(!e.should_quit(), "files left");
    e.execute_keys("ZZ").unwrap();
    assert!(e.should_quit());
}

#[test]
fn test_a_search_between_quits_counts() {
    let dir = plib::tmp::TempDir::new().unwrap();
    let mut e = on_two_files(&dir);
    e.execute_keys(":q\r/f\r:q\r").unwrap();
    assert!(!e.should_quit(), "the search came between");
}
