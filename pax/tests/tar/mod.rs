//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for the `tar` compatibility front-end.

use crate::common::{assert_failure, assert_success, have_tool, run_tar, stderr_str, stdout_str};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Build the tree every test archives: a file, a subdirectory with two files
/// (one of which the exclusion tests target), and a symlink.
fn setup(root: &Path) -> PathBuf {
    let src = root.join("src");
    fs::create_dir_all(src.join("sub")).unwrap();
    fs::write(src.join("a.txt"), "alpha\n").unwrap();
    fs::write(src.join("sub/b.txt"), "beta\n").unwrap();
    fs::write(src.join("sub/c.o"), "object\n").unwrap();
    #[cfg(unix)]
    std::os::unix::fs::symlink("a.txt", src.join("link.txt")).unwrap();
    src
}

/// Member names in an archive, sorted, with any "./" prefix and trailing slash
/// removed so the assertions do not depend on how the tree was named on the
/// command line. The archive root itself is dropped.
///
/// This splits the listing on newlines, so it cannot describe a member whose
/// own name contains one; those tests check the extracted tree instead.
fn members(root: &Path, archive: &str) -> Vec<String> {
    let out = run_tar(&["-tf", archive], root);
    assert_success(&out, "tar -tf");
    let mut names: Vec<String> = stdout_str(&out)
        .lines()
        .map(|l| l.trim_end_matches('/').trim_start_matches("./").to_string())
        .filter(|l| !l.is_empty() && l != ".")
        .collect();
    names.sort();
    names
}

#[test]
fn test_tar_old_style_bundle_create_list_extract() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    // The historic calling convention: option letters in the first operand,
    // with no leading dash, and their arguments taken from what follows.
    let out = run_tar(&["cvf", "../out.tar", "."], &src);
    assert_success(&out, "tar cvf");
    // -v in write mode reports pathnames on stderr, as pax does.
    assert!(
        stderr_str(&out).contains("a.txt"),
        "tar cvf should list members on stderr, got: {}",
        stderr_str(&out)
    );

    assert_eq!(
        members(temp.path(), "out.tar"),
        vec!["a.txt", "link.txt", "sub", "sub/b.txt", "sub/c.o"]
    );

    let out = run_tar(&["xf", "../out.tar"], &dest);
    assert_success(&out, "tar xf");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
    assert_eq!(
        fs::read_to_string(dest.join("sub/b.txt")).unwrap(),
        "beta\n"
    );
    #[cfg(unix)]
    assert_eq!(
        fs::read_link(dest.join("link.txt")).unwrap(),
        Path::new("a.txt")
    );
}

#[test]
fn test_tar_dash_options_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    assert_success(
        &run_tar(&["-c", "-f", "../out.tar", "."], &src),
        "tar -c -f",
    );
    assert_success(&run_tar(&["-x", "-f", "../out.tar"], &dest), "tar -x -f");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
}

#[test]
fn test_tar_clustered_and_glued_option_argument() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    // -f's argument glued to the letter that ends the cluster.
    assert_success(&run_tar(&["-cvf../glued.tar", "."], &src), "tar -cvf<arg>");
    assert!(members(temp.path(), "glued.tar").contains(&"a.txt".to_string()));
}

#[test]
fn test_tar_directory_option_on_create_and_extract() {
    let temp = TempDir::new().unwrap();
    setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    // -f is resolved before -C takes effect, so a relative archive name still
    // refers to the directory the command was invoked from.
    let out = run_tar(&["-cf", "out.tar", "-C", "src", "."], temp.path());
    assert_success(&out, "tar -cf -C");
    assert!(temp.path().join("out.tar").exists());
    assert!(members(temp.path(), "out.tar").contains(&"a.txt".to_string()));

    let out = run_tar(&["-xf", "out.tar", "-C", "dest"], temp.path());
    assert_success(&out, "tar -xf -C");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
}

#[test]
fn test_tar_rejects_a_second_directory_option() {
    let temp = TempDir::new().unwrap();
    setup(temp.path());

    // Honoring only one of two -C options would archive the wrong tree.
    let out = run_tar(
        &["-cf", "out.tar", "-C", "src", "-C", "sub", "."],
        temp.path(),
    );
    assert_failure(&out, "tar with two -C options");
    assert!(stderr_str(&out).contains("only one -C"));
}

#[test]
fn test_tar_files_from() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    fs::write(src.join("list"), "a.txt\nsub/b.txt\n").unwrap();

    assert_success(
        &run_tar(&["-cf", "../out.tar", "-T", "list"], &src),
        "tar -T",
    );
    assert_eq!(
        members(temp.path(), "out.tar"),
        vec!["a.txt".to_string(), "sub/b.txt".to_string()]
    );
}

#[test]
fn test_tar_files_from_null_separated() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    // A name containing a newline only survives a NUL-separated list.
    fs::write(src.join("odd\nname"), "odd\n").unwrap();
    fs::write(src.join("list"), "a.txt\0odd\nname\0").unwrap();

    assert_success(
        &run_tar(&["-cf", "../out.tar", "--null", "-T", "list"], &src),
        "tar --null -T",
    );

    // The listing splits on newlines, so the round trip is checked on disk.
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    assert_success(&run_tar(&["-xf", "../out.tar"], &dest), "tar -xf");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
    assert_eq!(
        fs::read_to_string(dest.join("odd\nname")).unwrap(),
        "odd\n",
        "the embedded newline should have survived"
    );
}

#[test]
fn test_tar_exclude_pattern_and_file() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    // --exclude is unanchored, so *.o matches sub/c.o without a directory part.
    assert_success(
        &run_tar(&["-cf", "../ex.tar", "--exclude=*.o", "."], &src),
        "tar --exclude",
    );
    let names = members(temp.path(), "ex.tar");
    assert!(!names.contains(&"sub/c.o".to_string()), "got {:?}", names);
    assert!(names.contains(&"sub/b.txt".to_string()), "got {:?}", names);

    fs::write(src.join("skip"), "sub\n").unwrap();
    assert_success(
        &run_tar(&["-cf", "../ex2.tar", "-X", "skip", "."], &src),
        "tar -X",
    );
    let names = members(temp.path(), "ex2.tar");
    assert!(
        !names.iter().any(|n| n.starts_with("sub")),
        "an excluded directory takes its subtree with it: {:?}",
        names
    );
}

#[test]
fn test_tar_exclude_on_extract() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    assert_success(&run_tar(&["-cf", "../out.tar", "."], &src), "tar -cf");
    assert_success(
        &run_tar(&["-xf", "../out.tar", "--exclude=*.o"], &dest),
        "tar -xf --exclude",
    );
    assert!(dest.join("sub/b.txt").exists());
    assert!(!dest.join("sub/c.o").exists());
}

#[test]
fn test_tar_strip_components() {
    let temp = TempDir::new().unwrap();
    setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    assert_success(&run_tar(&["-cf", "out.tar", "src"], temp.path()), "tar -cf");
    assert_success(
        &run_tar(&["-xf", "../out.tar", "--strip-components=1"], &dest),
        "tar --strip-components=1",
    );
    // "src/a.txt" lands as "a.txt"; "src" itself has one component and so
    // names nothing once stripped.
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
    assert!(dest.join("sub/b.txt").exists());
    assert!(!dest.join("src").exists());
}

#[test]
fn test_tar_strip_components_shows_stripped_names_in_listing() {
    let temp = TempDir::new().unwrap();
    setup(temp.path());

    assert_success(&run_tar(&["-cf", "out.tar", "src"], temp.path()), "tar -cf");
    let out = run_tar(&["-tf", "out.tar", "--strip-components=1"], temp.path());
    assert_success(&out, "tar -tf --strip-components");
    let listing = stdout_str(&out);
    assert!(
        listing.contains("a.txt") && !listing.contains("src/a.txt"),
        "listing should report the names extraction would create: {}",
        listing
    );
}

#[test]
fn test_tar_to_stdout() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    assert_success(&run_tar(&["-cf", "../out.tar", "."], &src), "tar -cf");
    let out = run_tar(&["-xOf", "../out.tar", "./a.txt"], &src);
    assert_success(&out, "tar -xOf");
    assert_eq!(stdout_str(&out), "alpha\n");
    // Nothing was written to the working directory.
    assert!(!src.join("a.txt.new").exists());
}

#[test]
fn test_tar_to_stdout_requires_extract() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let out = run_tar(&["-cOf", "../out.tar", "."], &src);
    assert_failure(&out, "tar -c -O");
    assert!(stderr_str(&out).contains("-O applies only to extraction"));
}

#[test]
fn test_tar_keep_old_files() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    fs::write(dest.join("a.txt"), "untouched\n").unwrap();

    assert_success(&run_tar(&["-cf", "../out.tar", "."], &src), "tar -cf");
    run_tar(&["-xkf", "../out.tar"], &dest);
    assert_eq!(
        fs::read_to_string(dest.join("a.txt")).unwrap(),
        "untouched\n",
        "-k must not overwrite an existing file"
    );
}

#[test]
fn test_tar_gzip_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();

    assert_success(&run_tar(&["-czf", "../out.tgz", "."], &src), "tar -czf");
    let raw = fs::read(temp.path().join("out.tgz")).unwrap();
    assert_eq!(&raw[..2], &[0x1f, 0x8b], "output should be a gzip stream");

    assert_success(&run_tar(&["-xzf", "../out.tgz"], &dest), "tar -xzf");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
}

#[test]
fn test_tar_blocking_factor() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    // tar counts 512-byte blocks, so -b 4 is a 2048-byte record.
    assert_success(
        &run_tar(&["-cf", "../out.tar", "-b", "4", "."], &src),
        "tar -b 4",
    );
    let size = fs::metadata(temp.path().join("out.tar")).unwrap().len();
    assert_eq!(
        size % 2048,
        0,
        "archive should be a whole number of records"
    );
}

#[test]
fn test_tar_strict_ustar_rejects_unsplittable_name() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    // A single component well past the 100-byte ustar name field, with no
    // slash to split on, cannot be represented in ustar at all.
    let long = "x".repeat(150);
    fs::write(src.join(&long), "long\n").unwrap();

    let out = run_tar(&["-cf", "../out.tar", "."], &src);
    assert_failure(&out, "tar -cf with an unsplittable long name");

    // --format=pax is the documented way through.
    let out = run_tar(&["-cf", "../pax.tar", "--format=pax", "."], &src);
    assert_success(&out, "tar --format=pax");
    assert!(members(temp.path(), "pax.tar")
        .iter()
        .any(|n| n.len() == 150));
}

#[test]
fn test_tar_rejects_unsupported_and_unknown_options() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    let out = run_tar(&["-cjf", "../out.tar", "."], &src);
    assert_failure(&out, "tar -j");
    assert!(stderr_str(&out).contains("only gzip"));

    let out = run_tar(&["--frobnicate"], &src);
    assert_failure(&out, "tar --frobnicate");
    assert!(stderr_str(&out).contains("unrecognized option"));

    let out = run_tar(&["-cf", "../out.tar"], &src);
    assert_success(&out, "tar -cf with no operands");
    let out = run_tar(&["-f", "../out.tar"], &src);
    assert_failure(&out, "tar with no operation");
    assert!(stderr_str(&out).contains("is required"));
}

#[test]
fn test_tar_absolute_names_refused_on_extract() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    assert_success(&run_tar(&["-cf", "../out.tar", "."], &src), "tar -cf");

    let out = run_tar(&["-xPf", "../out.tar"], &src);
    assert_failure(&out, "tar -xP");
    assert!(stderr_str(&out).contains("below the current directory"));
}

#[test]
fn test_tar_strips_leading_slash_when_storing() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let absolute = src.join("a.txt");

    let out = run_tar(&["-cf", "out.tar", absolute.to_str().unwrap()], temp.path());
    assert_success(&out, "tar -cf with an absolute operand");
    let names = members(temp.path(), "out.tar");
    assert!(
        names.iter().all(|n| !n.starts_with('/')),
        "stored names must be relative: {:?}",
        names
    );
}

#[test]
fn test_tar_append_and_update() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    assert_success(&run_tar(&["-cf", "../ap.tar", "a.txt"], &src), "tar -cf");
    assert_success(
        &run_tar(&["-rf", "../ap.tar", "sub/b.txt"], &src),
        "tar -rf",
    );
    assert_eq!(
        members(temp.path(), "ap.tar"),
        vec!["a.txt".to_string(), "sub/b.txt".to_string()]
    );

    // -u on an unchanged file appends nothing...
    assert_success(&run_tar(&["-uf", "../ap.tar", "a.txt"], &src), "tar -uf");
    assert_eq!(
        members(temp.path(), "ap.tar"),
        vec!["a.txt".to_string(), "sub/b.txt".to_string()]
    );

    // ...but a newer copy is appended.
    let future = filetime::FileTime::from_unix_time(
        filetime::FileTime::from_last_modification_time(&fs::metadata(src.join("a.txt")).unwrap())
            .unix_seconds()
            + 120,
        0,
    );
    filetime::set_file_mtime(src.join("a.txt"), future).unwrap();
    assert_success(&run_tar(&["-uf", "../ap.tar", "a.txt"], &src), "tar -uf");
    assert_eq!(
        members(temp.path(), "ap.tar"),
        vec![
            "a.txt".to_string(),
            "a.txt".to_string(),
            "sub/b.txt".to_string()
        ]
    );
}

#[test]
fn test_tar_no_recursion() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());

    assert_success(
        &run_tar(&["-cf", "../out.tar", "--no-recursion", "sub"], &src),
        "tar --no-recursion",
    );
    assert_eq!(members(temp.path(), "out.tar"), vec!["sub".to_string()]);
}

#[test]
fn test_tar_help_and_version_exit_zero() {
    let temp = TempDir::new().unwrap();
    let out = run_tar(&["--help"], temp.path());
    assert_success(&out, "tar --help");
    assert!(stdout_str(&out).contains("Usage: tar"));

    let out = run_tar(&["--version"], temp.path());
    assert_success(&out, "tar --version");
    assert!(stdout_str(&out).starts_with("tar "));
}

#[test]
fn test_tar_cross_tool_system_tar_reads_ours() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    if !have_tool("tar") {
        eprintln!("skipping cross-tool test: no system tar");
        return;
    }

    assert_success(&run_tar(&["-cf", "../out.tar", "."], &src), "tar -cf");
    let out = Command::new("tar")
        .args(["-tf", "out.tar"])
        .current_dir(temp.path())
        .output();
    let Ok(out) = out else {
        eprintln!("skipping cross-tool test: system tar would not run");
        return;
    };
    assert!(
        out.status.success(),
        "system tar could not read our archive: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("a.txt"));
}

#[test]
fn test_tar_cross_tool_we_read_system_tar() {
    let temp = TempDir::new().unwrap();
    let src = setup(temp.path());
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    if !have_tool("tar") {
        eprintln!("skipping cross-tool test: no system tar");
        return;
    }

    let made = Command::new("tar")
        .args(["-cf", "../sys.tar", "."])
        .current_dir(&src)
        .output();
    let Ok(made) = made else {
        eprintln!("skipping cross-tool test: system tar would not run");
        return;
    };
    if !made.status.success() {
        eprintln!("skipping cross-tool test: system tar failed to create");
        return;
    }

    assert_success(&run_tar(&["-xf", "../sys.tar"], &dest), "tar -xf sys.tar");
    assert_eq!(fs::read_to_string(dest.join("a.txt")).unwrap(), "alpha\n");
}
