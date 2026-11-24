//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! File names and arguments are bytes. A pathname is any bytes but NUL and
//! slash, and what a comment or a login is in some character set is not the
//! utilities' business: none of it has to be UTF-8.

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::OsStringExt;
use std::path::Path;
use std::process::Output;

use plib::tmp::TempDir;

use super::common::run_bytes;

/// `prefix` and then `name`, as a file name.
fn named(prefix: &str, name: &[u8]) -> OsString {
    OsString::from_vec([prefix.as_bytes(), name].concat())
}

fn run(cmd: &str, args: &[&OsStr], dir: &Path) -> Output {
    run_bytes(cmd, args, dir, b"")
}

fn ok(cmd: &str, args: &[&OsStr], dir: &Path) -> Output {
    let out = run(cmd, args, dir);
    assert!(out.status.success(), "{cmd} {args:?}: {out:?}");
    out
}

/// An s-file whose name is not UTF-8 is an s-file like any other: every
/// utility finds it and the files that go with it (g-, p-, l-, x-, q-, z-),
/// and the keywords made from its name carry the name's bytes. They used to
/// take the name for text, and a name that was not text for no s-file.
#[test]
fn an_s_file_name_need_not_be_utf8() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let name = b"caf\xe9.c";
    let s = named("s.", name);
    let g = dir.join(named("", name));
    let o = OsStr::new;

    // Made under a plain name and renamed: this test is about the name.
    std::fs::write(dir.join("init"), b"%M% %F%\n").unwrap();
    ok("admin", &[o("-iinit"), o("s.x")], dir);
    std::fs::rename(dir.join("s.x"), dir.join(&s)).unwrap();

    ok("val", &[&s], dir);
    let out = ok("get", &[o("-p"), o("-s"), &s], dir);
    assert_eq!(out.stdout, [&name[..], b" s.", name, b"\n"].concat());
    let out = ok("prs", &[o("-d:M: :F: :PN:"), &s], dir);
    assert_eq!(
        out.stdout,
        [&name[..], b" s.", name, b" s.", name, b"\n"].concat()
    );

    ok("get", &[o("-e"), o("-s"), &s], dir);
    assert!(g.exists(), "the g-file has the name's bytes");
    assert!(dir.join(named("p.", name)).exists());
    let out = ok("sact", &[&s], dir);
    assert!(out.stdout.starts_with(b"1.1 1.2 "), "{out:?}");
    ok("unget", &[o("-s"), &s], dir);
    assert!(!g.exists() && !dir.join(named("p.", name)).exists());

    ok("get", &[o("-e"), o("-s"), &s], dir);
    std::fs::write(&g, b"second\n").unwrap();
    ok("delta", &[o("-s"), o("-ytwo"), &s], dir);
    let out = ok("get", &[o("-p"), o("-s"), o("-l"), &s], dir);
    assert_eq!(out.stdout, b"second\n");
    assert!(dir.join(named("l.", name)).exists());
    ok("rmdel", &[o("-r1.2"), &s], dir);
    ok("val", &[&s], dir);

    // Nothing but the s-file and what the test made is left behind.
    let mut left: Vec<Vec<u8>> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().into_vec())
        .collect();
    left.sort();
    assert_eq!(
        left,
        [b"init".to_vec(), named("l.", name).into_vec(), s.into_vec()]
    );
}

/// A name that only looks like an s-file's, or is no name at all, is
/// refused by every utility: nothing is derived from it.
#[test]
fn a_name_without_a_stem_is_not_an_s_file() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    for name in ["s.", "s", "x.c", "d/s./"] {
        for cmd in ["get", "delta", "rmdel", "unget", "sact", "prs"] {
            let args: Vec<&OsStr> = match cmd {
                "rmdel" => vec![OsStr::new("-r1.1"), OsStr::new(name)],
                _ => vec![OsStr::new(name)],
            };
            let out = run(cmd, &args, dir);
            assert_eq!(out.status.code(), Some(1), "{cmd} {name:?}: {out:?}");
            assert!(!out.stderr.is_empty(), "{cmd} {name:?} says why");
        }
        let out = run("admin", &[OsStr::new("-n"), OsStr::new(name)], dir);
        assert_eq!(out.status.code(), Some(1), "admin {name:?}: {out:?}");
    }
    assert_eq!(
        std::fs::read_dir(dir).unwrap().count(),
        0,
        "nothing was made"
    );
}

/// `bytes` as one argument word.
fn word(bytes: &[u8]) -> OsString {
    OsString::from_vec(bytes.to_vec())
}

/// What a command line gives to be stored -- a comment, a login, a flag's
/// text, an MR number -- is stored as given, and file names on it are the
/// files': none of them has to be UTF-8. An argument that was not UTF-8
/// used to be refused outright, by admin for the whole command line.
#[test]
fn arguments_need_not_be_utf8() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    let s = word(b"s.na\xefve");
    std::fs::write(dir.join(word(b"in\xff")), b"text\n").unwrap();
    std::fs::write(dir.join(word(b"desc\xff")), b"about \xe9\n").unwrap();

    ok(
        "admin",
        &[
            &word(b"-iin\xff"),
            &word(b"-tdesc\xff"),
            &word(b"-ycr\xe9\xe9"),
            &word(b"-am\xfcller"),
            &word(b"-fqqu\xe9"),
            &word(b"-fmmod\xe9"),
            o("-fv"),
            &word(b"-mmr\xe9 mr2"),
            &s,
        ],
        dir,
    );
    // (A keyword whose value is lines ends each with a newline.)
    let show = |spec: &[u8]| ok("prs", &[&word(spec), &s], dir).stdout;
    assert_eq!(show(b"-d:C:."), b"cr\xe9\xe9\n.\n");
    assert_eq!(show(b"-d:UN:."), b"m\xfcller\n.\n");
    assert_eq!(show(b"-d:Q:|:M:"), b"qu\xe9|mod\xe9\n");
    assert_eq!(show(b"-d:MR:."), b"mr\xe9\nmr2\n.\n");
    assert_eq!(show(b"-d:FD:."), b"about \xe9\n.\n");
    // The data specification's own text is bytes too.
    assert_eq!(show(b"-d\xe9:I:\xff"), b"\xe91.1\xff\n");

    // val matches the module name and type as bytes.
    ok("val", &[&word(b"-mmod\xe9"), &s], dir);
    let out = run("val", &[&word(b"-mmod\xe8"), &s], dir);
    assert_eq!(out.status.code(), Some(0x01), "{out:?}");

    // delta: the comment and the MRs. The user list holds only m\xfcller,
    // so it is emptied first.
    ok("admin", &[&word(b"-em\xfcller"), &s], dir);
    ok("get", &[o("-e"), o("-s"), &s], dir);
    std::fs::write(dir.join(word(b"na\xefve")), b"more\n").unwrap();
    ok(
        "delta",
        &[o("-s"), &word(b"-ych\xe0nge"), &word(b"-mm\xe93"), &s],
        dir,
    );
    assert_eq!(show(b"-d:C:|:MR:."), b"ch\xe0nge\n|m\xe93\n.\n");
}

/// The argument of delta's `-y` and `-m` and of prs's `-r` is optional and
/// attached: the word after the bare option is an operand, not its value.
#[test]
fn a_bare_option_leaves_the_next_word_an_operand() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    std::fs::write(dir.join("init"), b"one\n").unwrap();
    ok("admin", &[o("-iinit"), o("s.f")], dir);
    ok("get", &[o("-e"), o("-s"), o("s.f")], dir);
    std::fs::write(dir.join("f"), b"two\n").unwrap();

    // `-y s.f`: an empty comment, and s.f the file. It used to make a
    // comment of "s.f" and fail for want of a file.
    ok("delta", &[o("-s"), o("-y"), o("s.f")], dir);
    let out = ok("prs", &[o("-d:I: [:C:]"), o("-r"), o("s.f")], dir);
    assert_eq!(out.stdout, b"1.2 []\n");
    let out = ok("prs", &[o("-d:I:"), o("-r1.1"), o("s.f")], dir);
    assert_eq!(out.stdout, b"1.1\n");
}
