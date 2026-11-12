//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! vi commands over bytes that are not UTF-8: each such byte is one
//! character to every motion and operator, and is written back as itself.

use plib::tmp::TempDir;
use std::fs;
use vi_rs::Editor;

/// Open a file holding `content`, run `keys` in vi command mode, write it,
/// and return what the file then holds.
fn edit(content: &[u8], keys: &str) -> Vec<u8> {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("f");
    fs::write(&path, content).unwrap();
    let mut editor = Editor::new_headless();
    editor.open(path.to_str().unwrap()).unwrap();
    editor.execute_keys(keys).unwrap();
    editor.execute_initial_command("w").unwrap();
    fs::read(&path).unwrap()
}

#[test]
fn test_x_deletes_a_byte_as_one_character() {
    assert_eq!(edit(b"a\xffb\n", "lx"), b"ab\n");
    assert_eq!(edit(b"a\xffb\n", "llx"), b"a\xff\n");
}

#[test]
fn test_motions_step_over_a_byte() {
    assert_eq!(edit(b"\xfe\xfeb\n", "$x"), b"\xfe\xfe\n");
    // A byte that is not a character is no letter: to `w` it is
    // punctuation, a word of its own, as `-` would be.
    assert_eq!(edit(b"x\xff y\n", "wx"), b"x y\n");
    assert_eq!(edit(b"x \xffy\n", "wx"), b"x y\n");
}

#[test]
fn test_yank_put_and_delete_lines_keep_bytes() {
    assert_eq!(edit(b"a\xff\nz\n", "yyjp"), b"a\xff\nz\na\xff\n");
    assert_eq!(edit(b"a\xff\nz\n", "ddp"), b"z\na\xff\n");
}

#[test]
fn test_search_past_a_byte() {
    assert_eq!(edit(b"q\xffb\n", "/b\rx"), b"q\xff\n");
}

#[test]
fn test_undo_restores_bytes() {
    assert_eq!(edit(b"a\xffb\n", "lxu"), b"a\xffb\n");
}

#[test]
fn test_replace_and_insert_next_to_a_byte() {
    assert_eq!(edit(b"a\xffb\n", "lrZ"), b"aZb\n");
    assert_eq!(edit(b"a\xffb\n", "lix\x1b"), b"ax\xffb\n");
}
