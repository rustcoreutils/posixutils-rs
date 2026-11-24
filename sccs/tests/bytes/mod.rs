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
