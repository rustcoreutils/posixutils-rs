//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-mindepth`, `-maxdepth` and `-printf`: GNU extensions that debhelper
//! runs for every Debian package (dh_update_autotools_config, dh_autoreconf,
//! dh_installdeb, dh_md5sums, dh_installgsettings, dh_movelibkdeinit).

use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::{symlink, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, UNIX_EPOCH};

use plib::testing::get_binary_path;

use super::{run_test_find, run_test_find_sorted, scratch_dir};

/// A tree with files at depths 1-2, a symlink, a hidden directory and a
/// regular file with a known size and nanosecond mtime:
///
/// ```text
/// a.txt (6 bytes, mtime 1577934245.123456789)   link -> a.txt
/// sub/b (2 bytes)   sub/deep/   .hid/b (1 byte)
/// ```
fn make_tree(tag: &str) -> PathBuf {
    let dir = scratch_dir(tag);
    let mut a = File::create(dir.join("a.txt")).unwrap();
    a.write_all(b"hello\n").unwrap();
    a.set_modified(UNIX_EPOCH + Duration::new(1_577_934_245, 123_456_789))
        .unwrap();
    fs::create_dir_all(dir.join("sub/deep")).unwrap();
    fs::write(dir.join("sub/b"), "x\n").unwrap();
    fs::create_dir(dir.join(".hid")).unwrap();
    fs::write(dir.join(".hid/b"), "y").unwrap();
    symlink("a.txt", dir.join("link")).unwrap();
    dir
}

/// Run find in `cwd` and return (stdout bytes, stderr, exit code).
fn find_in(cwd: &Path, args: &[&str]) -> (Vec<u8>, String, i32) {
    let output = Command::new(get_binary_path("find"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");
    (
        output.stdout,
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().unwrap_or(-1),
    )
}

fn sorted_lines(bytes: &[u8]) -> Vec<String> {
    let mut lines: Vec<String> = String::from_utf8_lossy(bytes)
        .lines()
        .map(String::from)
        .collect();
    lines.sort();
    lines
}

#[test]
fn find_mindepth_maxdepth() {
    let dir = make_tree("depth");
    let d = dir.to_str().unwrap();
    let at = |p: &str| format!("{d}/{p}");
    let (a, link, sub, hid) = (at("a.txt"), at("link"), at("sub"), at(".hid"));
    run_test_find_sorted(
        &[d, "-mindepth", "1", "-maxdepth", "1"],
        &[&a, &link, &sub, &hid],
        "",
        0,
    );
    run_test_find_sorted(&[d, "-maxdepth", "0"], &[d], "", 0);
    let (sub_b, deep, hid_b) = (at("sub/b"), at("sub/deep"), at(".hid/b"));
    run_test_find_sorted(&[d, "-mindepth", "2"], &[&sub_b, &deep, &hid_b], "", 0);
    // Global options: placed after a test they still apply to the whole walk.
    run_test_find_sorted(&[d, "-type", "f", "-maxdepth", "1"], &[&a], "", 0);
    // With -depth, the limits select the same entries.
    run_test_find_sorted(
        &[d, "-depth", "-mindepth", "2", "-maxdepth", "2"],
        &[&sub_b, &deep, &hid_b],
        "",
        0,
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// dh_update_autotools_config: without `-mindepth 1` the starting point `.`
/// would match `-name '.*'` and be pruned.
#[test]
fn find_mindepth_dh_update_autotools_config() {
    let dir = make_tree("dh_autotools");
    let (out, err, code) = find_in(
        &dir,
        &[
            "-mindepth",
            "1",
            "(",
            "-type",
            "d",
            "-name",
            ".*",
            "-prune",
            ")",
            "-o",
            "-type",
            "f",
            "-name",
            "b",
            "-print",
        ],
    );
    assert_eq!(
        (sorted_lines(&out), err.as_str(), code),
        (vec!["./sub/b".to_string()], "", 0)
    );
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_maxdepth_bad_argument() {
    run_test_find(
        &[".", "-maxdepth", "x"],
        "",
        "find: invalid argument to -maxdepth: x\n",
        1,
    );
    run_test_find(
        &[".", "-mindepth"],
        "",
        "find: -mindepth requires an argument\n",
        1,
    );
}

/// dh_autoreconf's `timesize` and `md5` modes.
#[test]
fn find_printf_dh_autoreconf() {
    let dir = make_tree("dh_autoreconf");
    let (out, err, code) = find_in(
        &dir,
        &[
            "!",
            "-path",
            "./.hid/*",
            "-type",
            "f",
            "-printf",
            "%s|%T@  %p\\n",
            "-o",
            "-type",
            "l",
            "-printf",
            "symlink  %p\\n",
        ],
    );
    assert_eq!(
        (sorted_lines(&out), err.as_str(), code),
        (
            vec![
                "2|".to_string() + &mtime_at(&dir.join("sub/b")) + "  ./sub/b",
                "6|1577934245.1234567890  ./a.txt".to_string(),
                "symlink  ./link".to_string(),
            ],
            "",
            0
        )
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// `%T@` as GNU find 4.9 prints it: seconds, a point, and ten digits.
fn mtime_at(path: &Path) -> String {
    let t = fs::metadata(path).unwrap().modified().unwrap();
    let d = t.duration_since(UNIX_EPOCH).unwrap();
    format!("{}.{:09}0", d.as_secs(), d.subsec_nanos())
}

/// dh_installdeb (`/etc/%P\n`), dh_md5sums (`%P\0`), dh_installgsettings
/// (`%P`): `%P` drops the starting point, and is empty for it.
#[test]
fn find_printf_relative_path_and_escapes() {
    let dir = make_tree("printf_rel");
    let d = dir.to_str().unwrap();
    let (out, _, code) = find_in(&dir, &[d, "-type", "f", "-printf", "/etc/%P\\n"]);
    assert_eq!(code, 0);
    assert_eq!(
        sorted_lines(&out),
        ["/etc/.hid/b", "/etc/a.txt", "/etc/sub/b"]
    );

    let (out, _, code) = find_in(&dir, &["-name", "a.txt", "-printf", "%P\\0"]);
    assert_eq!((out, code), (b"a.txt\0".to_vec(), 0));

    let (out, _, code) = find_in(&dir, &[".", "-maxdepth", "0", "-printf", "[%P]%%\\\\\\n"]);
    assert_eq!((out, code), (b"[]%\\\n".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

/// `-printf` is an action: no implicit `-print` is added.
#[test]
fn find_printf_suppresses_default_print() {
    let dir = make_tree("printf_action");
    let (out, _, code) = find_in(&dir, &["-name", "a.txt", "-printf", "x"]);
    assert_eq!((out, code), (b"x".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

/// Only the directives and escapes debhelper uses are implemented; any other
/// is refused before the walk rather than printed wrongly.
#[test]
fn find_printf_unsupported_is_an_error() {
    run_test_find(
        &[".", "-printf", "%u\\n"],
        "",
        "find: -printf: unsupported directive %u\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "%-10p"],
        "",
        "find: -printf: unsupported directive %-\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "a\\tb"],
        "",
        "find: -printf: unsupported escape \\t\n",
        1,
    );
    run_test_find(
        &[".", "-printf", "%"],
        "",
        "find: -printf: unsupported directive %\n",
        1,
    );
}

/// Output written before an `-exec` runs reaches stdout before the child's.
#[test]
fn find_printf_flushed_before_exec() {
    let dir = make_tree("printf_exec");
    let (out, _, code) = find_in(
        &dir,
        &[
            ".",
            "-maxdepth",
            "1",
            "-name",
            "a.txt",
            "-printf",
            "X",
            "-exec",
            "echo",
            "Y",
            ";",
        ],
    );
    assert_eq!((out, code), (b"XY\n".to_vec(), 0));
    let (out, _, code) = find_in(
        &dir,
        &[
            "sub", "-name", "b", "-printf", "X", "-exec", "echo", "Y", "{}", "+",
        ],
    );
    assert_eq!((out, code), (b"XY sub/b\n".to_vec(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

/// A package-like staging tree for the GNU tests and operators debhelper
/// passes (dh_fixperms, dh_compress, dh_md5sums, dh_shlibdeps, dh_install):
///
/// ```text
/// exe (0755, 10 B)  ux (0100, 1 B)  plain (0644, empty)
/// four (4096 B)  fourplus (4097 B)  big (5000 B)
/// emptydir/  DEBIAN/control
/// doc/pkg/README  doc/pkg/Notes.HTML  doc/pkg/examples/ex.txt
/// link -> exe   dangling -> nowhere
/// ```
fn make_pkg_tree(tag: &str) -> PathBuf {
    let dir = scratch_dir(tag);
    let file = |name: &str, len: usize, mode: u32| {
        let p = dir.join(name);
        fs::write(&p, vec![b'x'; len]).unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(mode)).unwrap();
    };
    fs::create_dir_all(dir.join("emptydir")).unwrap();
    fs::create_dir_all(dir.join("DEBIAN")).unwrap();
    fs::create_dir_all(dir.join("doc/pkg/examples")).unwrap();
    file("exe", 10, 0o755);
    file("ux", 1, 0o100);
    file("plain", 0, 0o644);
    file("four", 4096, 0o644);
    file("fourplus", 4097, 0o644);
    file("big", 5000, 0o644);
    file("DEBIAN/control", 1, 0o644);
    file("doc/pkg/README", 1, 0o644);
    file("doc/pkg/Notes.HTML", 1, 0o644);
    file("doc/pkg/examples/ex.txt", 1, 0o644);
    symlink("exe", dir.join("link")).unwrap();
    symlink("nowhere", dir.join("dangling")).unwrap();
    dir
}

/// Run find in `dir` and expect the sorted output lines, no diagnostics and a
/// zero exit status.
fn expect_lines(dir: &Path, args: &[&str], expected: &[&str]) {
    let (out, err, code) = find_in(dir, args);
    let mut want: Vec<String> = expected.iter().map(|s| s.to_string()).collect();
    want.sort();
    assert_eq!(
        (sorted_lines(&out), err.as_str(), code),
        (want, "", 0),
        "find {args:?}"
    );
}

/// `-true` and `-false`. dh_fixperms passes `-a -true` around every chmod
/// walk (with find's stderr sent to /dev/null, so a rejection silently left
/// permissions unfixed); dh_compress prunes with `-prune -false`.
#[test]
fn find_true_false() {
    let dir = make_pkg_tree("true_false");
    expect_lines(&dir, &["-name", "plain", "-true"], &["./plain"]);
    expect_lines(&dir, &["-false"], &[]);
    expect_lines(
        &dir,
        &["-name", "plain", "-false", "-o", "-name", "exe"],
        &["./exe"],
    );
    // dh_fixperms: `find DIR EXPR -a -true -a -true -print0`
    let (out, err, code) = find_in(
        &dir,
        &[".", "-name", "exe", "-a", "-true", "-a", "-true", "-print0"],
    );
    assert_eq!((out, err.as_str(), code), (b"./exe\0".to_vec(), "", 0));
    // dh_compress: the pruned examples directory yields nothing.
    expect_lines(
        &dir,
        &[
            "doc",
            "(",
            "-type",
            "d",
            "(",
            "-name",
            "_sources",
            "-o",
            "-path",
            "doc/pkg/examples",
            ")",
            "-prune",
            "-false",
            ")",
            "-o",
            "-type",
            "f",
            "-print",
        ],
        &["doc/pkg/README", "doc/pkg/Notes.HTML"],
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// [`expect_lines`] with the arguments given as whitespace-separated words.
fn expect_words(dir: &Path, words: &str, expected: &[&str]) {
    let args: Vec<&str> = words.split_whitespace().collect();
    expect_lines(dir, &args, expected);
}

/// `-size +4k` (dh_compress): GNU's `k` counts KiB, rounding up like the
/// POSIX 512-byte blocks. Only the `k` unit is accepted.
#[test]
fn find_size_kilobytes() {
    let dir = make_pkg_tree("size_k");
    expect_words(&dir, "-type f -size +4k", &["./fourplus", "./big"]);
    expect_words(&dir, "-type f -size 4k", &["./four"]);
    expect_words(&dir, "-type f -size -1k", &["./plain"]);
    expect_words(
        &dir,
        "-type f -size 1k",
        &[
            "./exe",
            "./ux",
            "./DEBIAN/control",
            "./doc/pkg/README",
            "./doc/pkg/Notes.HTML",
            "./doc/pkg/examples/ex.txt",
        ],
    );
    run_test_find(&[".", "-size", "1M"], "", "find: invalid number: 1M\n", 1);
    fs::remove_dir_all(&dir).unwrap();
}

/// `-regex` as debhelper passes it: dh_md5sums, dh_fixperms, and the `-X`
/// exclusions every dh_* tool turns into `-regex .*X.* -or ...`. The pattern
/// matches the WHOLE pathname.
#[test]
fn find_regex_debhelper() {
    let dir = make_pkg_tree("regex_dh");
    // dh_md5sums: `find -type f ! -regex './DEBIAN/.*' -printf '%P\0'`
    let (out, err, code) = find_in(
        &dir,
        &[
            "-type",
            "f",
            "!",
            "-regex",
            "./DEBIAN/.*",
            "-printf",
            "%P\\0",
        ],
    );
    let mut names: Vec<&[u8]> = out.split(|b| *b == 0).filter(|s| !s.is_empty()).collect();
    names.sort();
    let want: Vec<&[u8]> = vec![
        b"big",
        b"doc/pkg/Notes.HTML",
        b"doc/pkg/README",
        b"doc/pkg/examples/ex.txt",
        b"exe",
        b"four",
        b"fourplus",
        b"plain",
        b"ux",
    ];
    assert_eq!((names, err.as_str(), code), (want, "", 0));
    // Anchored at both ends.
    expect_words(&dir, "-regex exe", &[]);
    expect_words(&dir, "-regex ./ex", &[]);
    expect_words(&dir, "-regex .*/exe", &["./exe"]);
    // dh_fixperms, on an absolute staging path.
    let d = dir.to_str().unwrap();
    let doc = format!("{d}/doc");
    let examples = format!("{d}/doc/[^/]*/examples/.*");
    let (readme, notes) = (
        format!("{d}/doc/pkg/README"),
        format!("{d}/doc/pkg/Notes.HTML"),
    );
    expect_lines(
        &dir,
        &[&doc, "-type", "f", "!", "-regex", &examples],
        &[&readme, &notes],
    );
    // dh_* -X.HTML -Xxampl (debhelper joins them with `-or`)
    expect_words(
        &dir,
        r"( -regex .*\.HTML.* -o -regex .*xampl.* )",
        &[
            "./doc/pkg/Notes.HTML",
            "./doc/pkg/examples",
            "./doc/pkg/examples/ex.txt",
        ],
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// The Emacs syntax that is GNU find's default: `+` and `?` are operators,
/// `\(`, `\)` and `\|` group and alternate, a bare `(`, `|` or `{` is a
/// literal, and so is an operator with nothing to repeat.
#[test]
fn find_regex_emacs_syntax() {
    let dir = scratch_dir("regex_emacs");
    for name in ["ee", "exe", "exxe", "c++", "paren(1)", "{1}", "+", "a|b"] {
        fs::write(dir.join(name), "").unwrap();
    }
    expect_words(&dir, "-regex .*/ex+e", &["./exe", "./exxe"]);
    expect_words(&dir, "-regex .*/ex?e", &["./ee", "./exe"]);
    expect_words(&dir, r"-regex .*/\(ee\|exe\)", &["./ee", "./exe"]);
    expect_words(&dir, r"-regex .*c\+\+", &["./c++"]);
    expect_words(&dir, "-regex .*/c++", &[]);
    expect_words(&dir, "-regex .*/paren(1)", &["./paren(1)"]);
    expect_words(&dir, "-regex .*{1}", &["./{1}"]);
    expect_words(&dir, "-regex .*/a|b", &["./a|b"]);
    expect_words(&dir, r"-regex .*/\(+\)", &["./+"]);
    expect_words(&dir, r"-regex ^\./exe$", &["./exe"]);
    expect_words(&dir, "-regex .*/[^e]*", &["./c++", "./{1}", "./+", "./a|b"]);
    fs::remove_dir_all(&dir).unwrap();
}

/// Emacs constructs with no POSIX counterpart are refused, not misread.
#[test]
fn find_regex_unsupported_is_an_error() {
    run_test_find(
        &[".", "-regex", r".*\w"],
        "",
        "find: -regex: unsupported escape \\w\n",
        1,
    );
    run_test_find(
        &[".", "-regex", "[[:alpha:]]"],
        "",
        "find: -regex: unsupported bracket expression in [[:alpha:]]\n",
        1,
    );
    run_test_find(
        &[".", "-regex", "a["],
        "",
        "find: -regex: unterminated bracket expression in a[\n",
        1,
    );
    run_test_find(
        &[".", "-regex", "a\\"],
        "",
        "find: -regex: trailing backslash in a\\\n",
        1,
    );
}

/// `-or` and `-and`, GNU spellings of `-o` and `-a` with the same
/// precedence: dh_install, dh_installdocs, dh_shlibdeps and the `-X`
/// exclusions (`-regex .*X.* -or -regex .*Y.*`).
#[test]
fn find_or_and_spellings() {
    let dir = make_pkg_tree("or_and");
    expect_words(
        &dir,
        "( -type f -or -type l ) -and -name *e*",
        &["./exe", "./doc/pkg/examples/ex.txt", "./doc/pkg/Notes.HTML"],
    );
    // `-and` binds tighter than `-or`.
    expect_words(
        &dir,
        "-name e* -and -type d -or -name plain",
        &["./emptydir", "./doc/pkg/examples", "./plain"],
    );
    expect_words(
        &dir,
        r"! ( -regex .*\.HTML.* -or -regex .*xampl.* ) -and -path ./doc*",
        &["./doc", "./doc/pkg", "./doc/pkg/README"],
    );
    run_test_find(
        &[".", "-name", "x", "-or"],
        "",
        "find: unexpected end of expression\n",
        1,
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// `-perm /mode` (dh_shlibdeps `-perm /111`): any of the bits is set; with
/// no bits at all it matches every file, as in GNU find.
#[test]
fn find_perm_any_bits() {
    let dir = make_pkg_tree("perm_any");
    expect_words(&dir, "-type f -perm /111", &["./exe", "./ux"]);
    expect_words(&dir, "-type f -perm /011", &["./exe"]);
    expect_words(&dir, "-type f -perm /u+x", &["./exe", "./ux"]);
    expect_words(&dir, "-name plain -perm /000", &["./plain"]);
    // dh_shlibdeps
    expect_words(
        &dir,
        "-type f ( -perm /111 -or -name *.so* -or -name *.node )",
        &["./exe", "./ux"],
    );
    run_test_find(&[".", "-perm", "/9"], "", "find: invalid mode: 9\n", 1);
    fs::remove_dir_all(&dir).unwrap();
}

/// `-empty`: an empty regular file or a directory with no entries; never a
/// symlink. dh_install copies `-type d -and -empty`, dh_installdocs skips
/// `! -empty`.
#[test]
fn find_empty() {
    let dir = make_pkg_tree("empty");
    expect_words(&dir, "-empty", &["./plain", "./emptydir"]);
    expect_words(&dir, "( -type d -and -empty )", &["./emptydir"]);
    expect_words(
        &dir,
        "doc ( -type f -or -type l ) -and ! -empty",
        &[
            "doc/pkg/README",
            "doc/pkg/Notes.HTML",
            "doc/pkg/examples/ex.txt",
        ],
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// Every entry under `dir`, relative to it, sorted.
fn tree_listing(dir: &Path) -> Vec<String> {
    fn walk(base: &Path, d: &Path, out: &mut Vec<String>) {
        for e in fs::read_dir(d).unwrap() {
            let p = e.unwrap().path();
            out.push(p.strip_prefix(base).unwrap().to_string_lossy().into_owned());
            if fs::symlink_metadata(&p).unwrap().is_dir() {
                walk(base, &p, out);
            }
        }
    }
    let mut out = Vec::new();
    walk(dir, dir, &mut out);
    out.sort();
    out
}

/// `-delete`: dh_autotools-dev_restoreconfig and dh_doxygen remove files
/// with it. It is an action, implies `-depth` so a whole tree goes, never
/// removes the starting point `.`, and reports what it cannot remove.
#[test]
fn find_delete() {
    let dir = scratch_dir("delete");
    for d in ["a/b", "keep", "rmme/x/y", "full"] {
        fs::create_dir_all(dir.join(d)).unwrap();
    }
    for f in [
        "config.sub.dh-orig",
        "a/b/config.guess.dh-orig",
        "a/b/config.guess",
        "rmme/x/y/z",
        "full/f",
        "keep/k.md5",
        "keep/k.map",
        "keep/k.html",
    ] {
        fs::write(dir.join(f), "").unwrap();
    }
    // dh_autotools-dev_restoreconfig
    expect_words(
        &dir,
        ". -type f ( -name config.guess.dh-orig -o -name config.sub.dh-orig ) -delete",
        &[],
    );
    // dh_doxygen
    expect_words(
        &dir,
        "keep -type f -a ( -name *.md5 -o -name *.map ) -delete",
        &[],
    );
    // A whole tree, children first.
    expect_words(&dir, "rmme -delete", &[]);
    assert_eq!(
        tree_listing(&dir),
        [
            "a",
            "a/b",
            "a/b/config.guess",
            "full",
            "full/f",
            "keep",
            "keep/k.html"
        ]
    );

    let (out, err, code) = find_in(&dir, &["-name", "full", "-delete"]);
    assert_eq!(
        (out, err.as_str(), code),
        (
            vec![],
            "find: cannot delete './full': Directory not empty\n",
            1
        )
    );

    expect_words(&dir, "-delete", &[]);
    assert!(dir.is_dir());
    assert_eq!(tree_listing(&dir), Vec::<String>::new());

    run_test_find(
        &[".", "-prune", "-delete"],
        "",
        "find: -delete implies -depth, which makes -prune do nothing; give -depth explicitly to go ahead\n",
        1,
    );
    fs::remove_dir_all(&dir).unwrap();
}

/// `-executable` (dh_movelibkdeinit `-type f -executable`): the invoking
/// user may execute the file (`access(X_OK)`), or search the directory.
#[test]
fn find_executable() {
    let dir = make_pkg_tree("executable");
    expect_words(&dir, "-type f -executable", &["./exe", "./ux"]);
    expect_words(&dir, "-type d -name emptydir -executable", &["./emptydir"]);
    // A symlink is tested through to its target; a dangling one is not.
    expect_words(&dir, "-name link -executable", &["./link"]);
    expect_words(&dir, "-name dangling -executable", &[]);
    fs::remove_dir_all(&dir).unwrap();
}
