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

/// A file name for these tests: `stem`, a byte that is not ASCII, `suffix`.
///
/// Where the file system takes any bytes for a name, the name is not UTF-8.
/// Some do not: APFS refuses such a name outright ("Illegal byte sequence").
/// There the name is UTF-8 with a character outside ASCII, which is as far
/// from text-as-ASCII as that file system lets a name be, and the utilities
/// are still held to carrying it through byte for byte.
fn odd_name(dir: &Path, stem: &str, suffix: &str) -> Vec<u8> {
    let probe = dir.join(named("probe", b"\xff"));
    let any_bytes = std::fs::write(&probe, b"").is_ok();
    let _ = std::fs::remove_file(&probe);
    let odd: &[u8] = if any_bytes {
        b"\xe9"
    } else {
        "\u{e9}".as_bytes()
    };
    [stem.as_bytes(), odd, suffix.as_bytes()].concat()
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
    let name = &odd_name(dir, "caf", ".c")[..];
    let s = named("s.", name);
    let g = dir.join(named("", name));
    let o = OsStr::new;

    // Made under a plain name and renamed: this test is about the name.
    std::fs::write(dir.join("init"), b"%M% %F%\n").unwrap();
    ok("admin", &[o("-iinit"), o("s.x")], dir);
    std::fs::rename(dir.join("s.x"), dir.join(&s)).unwrap();

    ok("val", &[&s], dir);
    let out = ok("get", &[o("-p"), o("-s"), &s], dir);
    assert_eq!(out.stdout, [name, b" s.", name, b"\n"].concat());
    let out = ok("prs", &[o("-d:M: :F: :PN:"), &s], dir);
    assert_eq!(
        out.stdout,
        [name, b" s.", name, b" s.", name, b"\n"].concat()
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
    // The files named on the command line: the s-file and its g-file, the
    // -i file and the -t file.
    let gfile = odd_name(dir, "na", "ve");
    let s = named("s.", &gfile);
    let (input, desc) = (odd_name(dir, "in", ""), odd_name(dir, "desc", ""));
    std::fs::write(dir.join(word(&input)), b"text\n").unwrap();
    std::fs::write(dir.join(word(&desc)), b"about \xe9\n").unwrap();

    ok(
        "admin",
        &[
            &named("-i", &input),
            &named("-t", &desc),
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
    std::fs::write(dir.join(word(&gfile)), b"more\n").unwrap();
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

/// The `sccs` front end sorts the words of its command line into options
/// and files and hands them on; it does not need them to be text. It used
/// to refuse a command line holding any word that was not UTF-8.
#[test]
fn the_front_end_passes_bytes_through() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    let name = &odd_name(dir, "na", "ve.c")[..];
    let file = word(name);
    std::fs::write(dir.join(&file), b"one\n").unwrap();

    ok("sccs", &[o("create"), &file], dir);
    assert!(dir.join("SCCS").join(named("s.", name)).exists());
    assert!(dir.join(named(",", name)).exists(), "the original is kept");

    ok("sccs", &[o("edit"), &file], dir);
    let out = ok("sccs", &[o("tell")], dir);
    assert_eq!(out.stdout, [name, b"\n"].concat());
    let out = ok("sccs", &[o("info")], dir);
    assert!(out.stdout.starts_with(&[name, b": "].concat()), "{out:?}");

    std::fs::write(dir.join(&file), b"one\ntwo\n").unwrap();
    ok("sccs", &[o("delget"), &word(b"-yfix\xe9"), &file], dir);
    assert_eq!(std::fs::read(dir.join(&file)).unwrap(), b"one\ntwo\n");
    let out = ok(
        "prs",
        &[
            o("-d:C:."),
            o("-r1.2"),
            &dir.join("SCCS").join(named("s.", name)).into_os_string(),
        ],
        dir,
    );
    assert_eq!(out.stdout, b"fix\xe9\n.\n");

    // A command that is not text is no command; it is named in the report.
    let out = run("sccs", &[&word(b"g\xe9t"), &file], dir);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert!(String::from_utf8_lossy(&out.stderr).contains("unknown command"));
}

/// A pathname written to the output is the pathname: its bytes, not a
/// rendering of them as text.
#[test]
fn a_pathname_is_written_as_its_bytes() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    let mut files = Vec::new();
    for name in [odd_name(dir, "a", ""), odd_name(dir, "b", "")] {
        let name = &name[..];
        std::fs::write(dir.join("init"), b"@(#)id\n").unwrap();
        ok("admin", &[o("-iinit"), o("s.x")], dir);
        let s = named("s.", name);
        std::fs::rename(dir.join("s.x"), dir.join(&s)).unwrap();
        ok("get", &[o("-e"), o("-s"), o("-g"), &s], dir);
        files.push(s);
    }
    let banner = |name: &OsString| [b"\n", name.as_encoded_bytes(), b":\n"].concat();
    let has = |out: &[u8], what: &[u8]| out.windows(what.len()).any(|w| w == what);

    let out = ok("sact", &[&files[0], &files[1]], dir);
    assert!(out.stdout.starts_with(&banner(&files[0])), "{out:?}");
    assert!(has(&out.stdout, &banner(&files[1])), "{out:?}");

    let out = ok("get", &[o("-k"), o("-g"), &files[0], &files[1]], dir);
    assert!(out.stdout.starts_with(&banner(&files[0])), "{out:?}");

    let out = ok("unget", &[&files[0], &files[1]], dir);
    assert!(has(&out.stdout, &banner(&files[1])), "{out:?}");

    let out = ok("what", &[&files[0]], dir);
    assert!(
        out.stdout
            .starts_with(&[files[0].as_encoded_bytes(), b":\n"].concat()),
        "{out:?}"
    );
}

/// delta's `-m mrlist` takes its argument like any option that requires one
/// (POSIX delta SYNOPSIS: `[-m mrlist]`, beside `[-y[comment]]`): attached,
/// or as the next word. Only `-y`'s is optional.
#[test]
fn delta_m_takes_a_separate_argument() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    std::fs::write(dir.join("init"), b"one\n").unwrap();
    ok(
        "admin",
        &[o("-iinit"), o("-fv"), o("-mfirst"), o("s.f")],
        dir,
    );
    for (n, args) in [
        vec![o("-m"), o("mr1 mr2")],
        vec![o("-mmr3")],
        vec![o("-pm"), o("mr4")],
    ]
    .into_iter()
    .enumerate()
    {
        ok("get", &[o("-e"), o("-s"), o("s.f")], dir);
        std::fs::write(dir.join("f"), format!("text {n}\n")).unwrap();
        let args: Vec<&OsStr> = [o("-s"), o("-yc")]
            .into_iter()
            .chain(args)
            .chain([o("s.f")])
            .collect();
        ok("delta", &args, dir);
    }
    let out = ok("prs", &[o("-e"), o("-d:I: :MR:."), o("s.f")], dir);
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "1.4 mr4\n.\n1.3 mr3\n.\n1.2 mr1\nmr2\n.\n1.1 first\n.\n"
    );
}

/// The pathnames read from standard input for a `-` operand are bytes, one
/// to a line. The list used to be read as text, and ended in silence at the
/// first name that was not UTF-8: that file and every one after it were
/// skipped, with exit status 0.
#[test]
fn a_list_of_pathnames_on_standard_input_is_bytes() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();
    let o = OsStr::new;
    let two = odd_name(dir, "tw", "");
    for (name, text) in [(&b"one"[..], "1\n"), (&two, "2\n"), (b"three", "3\n")] {
        std::fs::write(dir.join("init"), text).unwrap();
        ok("admin", &[o("-iinit"), o("s.x")], dir);
        std::fs::rename(dir.join("s.x"), dir.join(named("s.", name))).unwrap();
    }
    let out = run_bytes(
        "get",
        &[o("-s"), o("-p"), o("-")],
        dir,
        &[b"s.one\ns.", &two[..], b"\n\ns.three\n"].concat(),
    );
    assert!(out.status.success(), "{out:?}");
    assert_eq!(out.stdout, b"1\n2\n3\n");
}
