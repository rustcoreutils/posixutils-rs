//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::Command;

fn run_ln(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_ln"))
        .args(args)
        .output()
        .unwrap()
}

fn dir(name: &str) -> String {
    let d = format!("{}/{name}", env!("CARGO_TARGET_TMPDIR"));
    let _ = fs::remove_dir_all(&d);
    fs::create_dir_all(&d).unwrap();
    d
}

// Hard link (default) and symbolic link (-s).
#[test]
fn test_ln_hard_and_symbolic() {
    let d = dir("test_ln_hard_and_symbolic");
    let src = format!("{d}/src");
    fs::write(&src, b"hello").unwrap();

    let hard = format!("{d}/hard");
    assert!(run_ln(&[&src, &hard]).status.success());
    assert_eq!(
        fs::metadata(&hard).unwrap().ino(),
        fs::metadata(&src).unwrap().ino()
    );

    let sym = format!("{d}/sym");
    assert!(run_ln(&["-s", &src, &sym]).status.success());
    assert!(fs::symlink_metadata(&sym).unwrap().file_type().is_symlink());

    fs::remove_dir_all(&d).unwrap();
}

// `-f` removes an existing destination before linking.
#[test]
fn test_ln_force_over_existing() {
    let d = dir("test_ln_force_over_existing");
    let src = format!("{d}/src");
    let dst = format!("{d}/dst");
    fs::write(&src, b"src").unwrap();
    fs::write(&dst, b"dst").unwrap();

    // Without -f the existing destination is an error.
    assert!(!run_ln(&[&src, &dst]).status.success());
    // With -f it is replaced (now a hard link to src).
    assert!(run_ln(&["-f", &src, &dst]).status.success());
    assert_eq!(
        fs::metadata(&dst).unwrap().ino(),
        fs::metadata(&src).unwrap().ino()
    );

    fs::remove_dir_all(&d).unwrap();
}

// `ln -f a a` (same file) is refused rather than destroying the only copy.
#[test]
fn test_ln_force_same_file_refused() {
    let d = dir("test_ln_force_same_file_refused");
    let src = format!("{d}/src");
    fs::write(&src, b"keep").unwrap();

    let out = run_ln(&["-f", &src, &src]);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("same file"));
    assert!(Path::new(&src).exists());

    fs::remove_dir_all(&d).unwrap();
}

// `ln -f -L <symlink-to-X> X` must compare the *referent* (which -L hard-links)
// against the destination, so the same-file guard fires and X is not unlinked.
#[test]
fn test_ln_force_logical_same_file_refused() {
    let d = dir("test_ln_force_logical_same_file_refused");
    let target = format!("{d}/target");
    let sym = format!("{d}/sym");
    fs::write(&target, b"keep").unwrap();
    std::os::unix::fs::symlink("target", &sym).unwrap();

    // Source is the symlink, dest is its referent; with -L the referent would be hard-linked onto
    // itself. The same-file check must refuse this and leave `target` intact.
    let out = run_ln(&["-f", "-L", &sym, &target]);
    assert!(
        !out.status.success(),
        "ln -f -L onto the referent must fail"
    );
    assert!(String::from_utf8_lossy(&out.stderr).contains("same file"));
    assert!(
        Path::new(&target).exists(),
        "the referent must be preserved"
    );
    assert_eq!(fs::read(&target).unwrap(), b"keep");

    fs::remove_dir_all(&d).unwrap();
}

// With an existing-directory final operand, sources are linked into it.
#[test]
fn test_ln_into_directory() {
    let d = dir("test_ln_into_directory");
    let src = format!("{d}/src");
    let into = format!("{d}/into");
    fs::write(&src, b"x").unwrap();
    fs::create_dir(&into).unwrap();

    assert!(run_ln(&[&src, &into]).status.success());
    assert!(Path::new(&format!("{into}/src")).exists());

    fs::remove_dir_all(&d).unwrap();
}

// More than two operands with a non-directory final operand is an error (no partial).
#[test]
fn test_ln_multi_nondir_target_errors() {
    let d = dir("test_ln_multi_nondir_target_errors");
    let a = format!("{d}/a");
    let b = format!("{d}/b");
    let c = format!("{d}/c"); // does not exist, not a directory
    fs::write(&a, b"a").unwrap();
    fs::write(&b, b"b").unwrap();

    let out = run_ln(&[&a, &b, &c]);
    assert_eq!(out.status.code(), Some(1));
    assert!(!Path::new(&c).exists(), "no link created");

    fs::remove_dir_all(&d).unwrap();
}

// -L hard-links a symlink's referent; -P hard-links the symlink itself.
#[test]
fn test_ln_logical_physical() {
    let d = dir("test_ln_logical_physical");
    let target = format!("{d}/target");
    let symsrc = format!("{d}/symsrc");
    fs::write(&target, b"t").unwrap();
    std::os::unix::fs::symlink("target", &symsrc).unwrap();

    let hp = format!("{d}/hp");
    assert!(run_ln(&["-P", &symsrc, &hp]).status.success());
    assert!(fs::symlink_metadata(&hp).unwrap().file_type().is_symlink());

    let hl = format!("{d}/hl");
    assert!(run_ln(&["-L", &symsrc, &hl]).status.success());
    let md = fs::symlink_metadata(&hl).unwrap();
    assert!(!md.file_type().is_symlink());
    assert_eq!(md.ino(), fs::metadata(&target).unwrap().ino());

    fs::remove_dir_all(&d).unwrap();
}

fn run_ln_in(cwd: &str, args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_ln"))
        .args(args)
        .current_dir(cwd)
        .output()
        .unwrap()
}

fn link_text(p: &str) -> String {
    fs::read_link(p).unwrap().to_str().unwrap().to_string()
}

// -s -r: the link text is the source's path relative to the link's directory.
// libselinux runs `ln -sf --relative /abs/dir/lib.so.1 /abs/dir/lib.so`.
#[test]
fn test_ln_relative_symlink() {
    let d = dir("test_ln_relative_symlink");
    fs::create_dir_all(format!("{d}/sub/deeper")).unwrap();
    fs::create_dir_all(format!("{d}/x/y")).unwrap();
    fs::write(format!("{d}/lib.so.1"), b"l").unwrap();
    fs::write(format!("{d}/lib.so"), b"old").unwrap();

    let out = run_ln(&[
        "-sf",
        "--relative",
        &format!("{d}/lib.so.1"),
        &format!("{d}/lib.so"),
    ]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(link_text(&format!("{d}/lib.so")), "lib.so.1");

    // Down, up, and a source that does not exist (resolved lexically).
    let link = format!("{d}/down");
    assert!(run_ln(&["-sr", &format!("{d}/sub/deeper/t"), &link])
        .status
        .success());
    assert_eq!(link_text(&link), "sub/deeper/t");

    let link = format!("{d}/x/y/up");
    assert!(run_ln(&["-s", "-r", &format!("{d}/lib.so.1"), &link])
        .status
        .success());
    assert_eq!(link_text(&link), "../../lib.so.1");

    // Target-directory form: the link lands in sub/ and points back up.
    assert!(
        run_ln(&["-sr", &format!("{d}/lib.so.1"), &format!("{d}/sub")])
            .status
            .success()
    );
    assert_eq!(link_text(&format!("{d}/sub/lib.so.1")), "../lib.so.1");

    // A relative source names a path from the current directory, not from
    // the link's directory.
    let out = run_ln_in(&d, &["-sr", "lib.so.1", "x/y/rel"]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(link_text(&format!("{d}/x/y/rel")), "../../lib.so.1");

    fs::remove_dir_all(&d).unwrap();
}

// -r resolves symbolic links in the link's directory before computing the path.
#[test]
fn test_ln_relative_through_symlinked_dir() {
    let d = dir("test_ln_relative_through_symlinked_dir");
    fs::create_dir_all(format!("{d}/real/inner")).unwrap();
    std::os::unix::fs::symlink("real/inner", format!("{d}/alias")).unwrap();
    fs::write(format!("{d}/t"), b"t").unwrap();

    assert!(run_ln(&["-sr", &format!("{d}/t"), &format!("{d}/alias/l")])
        .status
        .success());
    assert_eq!(link_text(&format!("{d}/real/inner/l")), "../../t");
    assert_eq!(fs::read(format!("{d}/alias/l")).unwrap(), b"t");

    fs::remove_dir_all(&d).unwrap();
}

// -r needs -s.
#[test]
fn test_ln_relative_requires_symbolic() {
    let d = dir("test_ln_relative_requires_symbolic");
    fs::write(format!("{d}/a"), b"a").unwrap();
    let out = run_ln(&["-r", &format!("{d}/a"), &format!("{d}/b")]);
    assert_eq!(out.status.code(), Some(1));
    assert!(!Path::new(&format!("{d}/b")).exists());
    fs::remove_dir_all(&d).unwrap();
}

// One operand links into the current directory under its last component.
// perl's build runs `ln -s regen-configure/U`.
#[test]
fn test_ln_single_operand() {
    let d = dir("test_ln_single_operand");
    fs::create_dir_all(format!("{d}/regen-configure/U")).unwrap();

    let out = run_ln_in(&d, &["-s", "regen-configure/U"]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(link_text(&format!("{d}/U")), "regen-configure/U");

    // A hard link of a file in another directory.
    fs::write(format!("{d}/regen-configure/f"), b"f").unwrap();
    assert!(run_ln_in(&d, &["regen-configure/f"]).status.success());
    assert_eq!(
        fs::metadata(format!("{d}/f")).unwrap().ino(),
        fs::metadata(format!("{d}/regen-configure/f"))
            .unwrap()
            .ino()
    );

    // The name already exists in the current directory.
    let out = run_ln_in(&d, &["f"]);
    assert_eq!(out.status.code(), Some(1));

    fs::remove_dir_all(&d).unwrap();
}
