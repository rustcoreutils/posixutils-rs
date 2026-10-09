//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A directory member whose name is already a directory. POSIX lets pax
//! extract into it. Like libarchive, pax changes such a directory's mode only
//! under `-p p` (or `-p e`), and its owner only under `-p o`; and then only
//! where nobody else could have created the name first -- in a parent others
//! can create entries in, the directory there may be anyone's renamed to the
//! member's name, a private one of the user's own included, and giving it the
//! archive's mode would open it up.

use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

/// A source tree with a directory `d`, mode 0755, holding a file.
fn source_tree(temp: &TempDir) -> PathBuf {
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "data\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    src
}

/// An archive of `source_tree`'s `d` and `d/f`.
fn archive_with_open_directory(temp: &TempDir) -> PathBuf {
    let src = source_tree(temp);
    let mut write = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-f", "../a.tar"])
        .current_dir(&src)
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    write
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"d\nd/f\n")
        .unwrap();
    drop(write.stdin.take());
    assert!(write.wait().unwrap().success());
    temp.path().join("a.tar")
}

/// A destination of mode `mode` holding an existing directory `d` of mode
/// 0700 with a file in it -- in the attack, the user's own private
/// `secrets`, renamed to the member's name before pax runs.
fn dest_with_private_d(temp: &TempDir, mode: u32) -> PathBuf {
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(mode)).unwrap();
    fs::create_dir(dest.join("secrets")).unwrap();
    fs::write(dest.join("secrets/key"), "secret\n").unwrap();
    fs::set_permissions(dest.join("secrets"), fs::Permissions::from_mode(0o700)).unwrap();
    fs::rename(dest.join("secrets"), dest.join("d")).unwrap();
    dest
}

/// Run pax with `args` in `dir`.
fn pax(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::null())
        .output()
        .unwrap()
}

fn mode_of(path: &Path) -> u32 {
    fs::metadata(path).unwrap().permissions().mode() & 0o7777
}

const DIAGNOSTIC: &str = "not applying owner, mode or times";

/// Without -p p, an existing directory keeps its mode: nothing is wrong, and
/// nothing is said.
#[test]
fn test_extract_without_p_leaves_an_existing_directory_mode_alone() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o777);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(!stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
    assert!(dest.join("d/f").exists());
}

/// With -p e, in a destination others can create entries in, the directory
/// found there may be anyone's renamed to the member's name: it keeps its
/// own attributes, and that is diagnosed.
#[test]
fn test_extract_pe_leaves_a_renamed_in_private_directory_closed() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o777);

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        mode_of(&dest.join("d")),
        0o700,
        "the private directory was opened up"
    );
    // Extracted into, as POSIX allows.
    assert!(dest.join("d/f").exists());
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
}

/// Re-extracting into a group-writable destination (a umask of 002): without
/// -p the directories found there are left as they are, with no error.
#[test]
fn test_reextract_into_a_group_writable_destination_without_p() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o775);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// The same with -p e, the group being one others are in: they could have created the name
/// first.
#[test]
fn test_reextract_into_a_group_writable_destination_with_pe() {
    let Some(shared) = plib::testing::shared_group() else {
        eprintln!("note: the user belongs to no group shared with others; test skipped");
        return;
    };
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o775);
    std::os::unix::fs::chown(&dest, None, Some(shared)).unwrap();

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// Under a umask of 002 the destination is group-writable; when its group is
/// the user's private group -- nobody else in it, nobody else's primary
/// group -- that write permission is the user's own, and -p e gives the
/// existing directory the member's mode, in both modes.
#[test]
fn test_pe_stamps_an_existing_directory_in_a_destination_of_the_users_private_group() {
    if plib::testing::user_private_group().is_none() {
        eprintln!("note: this host gives the user no private group; test skipped");
        return;
    }
    for copy in [false, true] {
        let temp = TempDir::new().unwrap();
        let archive = archive_with_open_directory(&temp);
        let src = temp.path().join("src");
        let dest = dest_with_private_d(&temp, 0o775);
        let out = if copy {
            pax(&src, &["-rw", "-p", "e", "d", dest.to_str().unwrap()])
        } else {
            pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()])
        };
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(out.status.success(), "copy={copy}: stderr: {stderr}");
        assert_eq!(mode_of(&dest.join("d")), 0o755, "copy={copy}");
    }
}

/// An ACL entry naming another user widens the group bits of the mode to the
/// ACL's mask: a destination of the user's private group, 0755 but for an
/// ACL granting someone else write, shows 0775 -- and others can create
/// entries in it. Under -p e the existing directory keeps its mode, in both
/// modes.
#[test]
fn test_pe_leaves_an_existing_directory_alone_where_an_acl_lets_others_write() {
    if plib::testing::user_private_group().is_none() {
        eprintln!("note: this host gives the user no private group; test skipped");
        return;
    }
    for copy in [false, true] {
        let temp = TempDir::new().unwrap();
        let archive = archive_with_open_directory(&temp);
        let src = temp.path().join("src");
        let dest = dest_with_private_d(&temp, 0o755);
        if !plib::testing::grant_named_acl(&dest) {
            return;
        }
        assert_eq!(mode_of(&dest), 0o775, "the mask shows in the group bits");
        let out = if copy {
            pax(&src, &["-rw", "-p", "e", "d", dest.to_str().unwrap()])
        } else {
            pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()])
        };
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(mode_of(&dest.join("d")), 0o700, "copy={copy}: opened up");
        assert_eq!(out.status.code(), Some(1), "copy={copy}: stderr: {stderr}");
        assert!(stderr.contains(DIAGNOSTIC), "copy={copy}: stderr: {stderr}");
    }
}

/// In a destination only the user can create entries in, -p e gives an
/// existing directory the member's mode, as POSIX describes.
#[test]
fn test_extract_pe_stamps_an_existing_directory_in_a_private_destination() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o755);

    let out = pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o755);
}

/// And without -p p, even there, it keeps its own.
#[test]
fn test_extract_without_p_keeps_an_existing_directory_mode_in_a_private_destination() {
    let temp = TempDir::new().unwrap();
    let archive = archive_with_open_directory(&temp);
    let dest = dest_with_private_d(&temp, 0o755);

    let out = pax(&dest, &["-r", "-f", archive.to_str().unwrap()]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stderr: {stderr}");
    assert_eq!(mode_of(&dest.join("d")), 0o700);
}

/// Copy mode treats an existing destination directory the same way: left
/// alone without -p, and under -p e only where nobody else can create
/// entries beside it.
#[test]
fn test_copy_onto_an_existing_directory_follows_p() {
    for (privs, parent, code, mode) in [
        (None, 0o777, 0, 0o700),
        (Some("e"), 0o777, 1, 0o700),
        (Some("e"), 0o755, 0, 0o755),
    ] {
        let temp = TempDir::new().unwrap();
        let src = source_tree(&temp);
        let dest = dest_with_private_d(&temp, parent);
        let mut args = vec!["-rw"];
        if let Some(p) = privs {
            args.extend(["-p", p]);
        }
        args.extend(["d", dest.to_str().unwrap()]);
        let out = pax(&src, &args);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(code), "{args:?}: stderr: {stderr}");
        assert_eq!(mode_of(&dest.join("d")), mode, "{args:?}");
        assert!(dest.join("d/f").exists(), "{args:?}");
    }
}

/// A source tree `p/secret/f`, with `p/secret` mode 0777, and an archive of
/// `p/secret` and its file alone: `p` is made, or walked through, only to
/// reach it.
fn deep_source(temp: &TempDir) -> (PathBuf, PathBuf) {
    let src = temp.path().join("deep-src");
    fs::create_dir_all(src.join("p/secret")).unwrap();
    fs::write(src.join("p/secret/f"), "data\n").unwrap();
    fs::set_permissions(src.join("p/secret"), fs::Permissions::from_mode(0o777)).unwrap();
    let mut write = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-f", "../deep.tar"])
        .current_dir(&src)
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let list = b"p/secret\np/secret/f\n";
    write.stdin.as_mut().unwrap().write_all(list).unwrap();
    drop(write.stdin.take());
    assert!(write.wait().unwrap().success());
    (src, temp.path().join("deep.tar"))
}

/// A destination `G` of mode `mode` holding the user's own `X` (0755) with a
/// private `secret` (0700) in it -- and, as someone who can write `G` would
/// arrange, `X` renamed to `p`, so that the member `p/secret` names the
/// private directory. `p` itself is the user's, and nobody else can create
/// entries in it: the one level that is not safe is the one above.
fn dest_with_renamed_chain(temp: &TempDir, mode: u32) -> PathBuf {
    let dest = temp.path().join("G");
    fs::create_dir(&dest).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(mode)).unwrap();
    fs::create_dir_all(dest.join("X/secret")).unwrap();
    fs::write(dest.join("X/secret/key"), "secret\n").unwrap();
    fs::set_permissions(dest.join("X"), fs::Permissions::from_mode(0o755)).unwrap();
    fs::set_permissions(dest.join("X/secret"), fs::Permissions::from_mode(0o700)).unwrap();
    fs::rename(dest.join("X"), dest.join("p")).unwrap();
    dest
}

/// Trust holds along the whole chain: a directory found below one that
/// someone else could have put there -- `p`, renamed in -- is no safer for
/// sitting in a parent only the user can write. Under -p e it keeps its own
/// mode, and that is diagnosed; without -p it takes its times and nothing
/// else, with no error.
#[test]
fn test_extract_trust_holds_along_the_chain() {
    for (privs, code) in [(Some("e"), 1), (None, 0)] {
        let temp = TempDir::new().unwrap();
        let (_, archive) = deep_source(&temp);
        // World-writable: others can write it whatever its group.
        let dest = dest_with_renamed_chain(&temp, 0o777);
        let mut args = vec!["-r"];
        if let Some(p) = privs {
            args.extend(["-p", p]);
        }
        args.extend(["-f", archive.to_str().unwrap()]);
        let out = pax(&dest, &args);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            mode_of(&dest.join("p/secret")),
            0o700,
            "{args:?}: the private directory was opened up"
        );
        assert_eq!(out.status.code(), Some(code), "{args:?}: stderr: {stderr}");
        assert_eq!(stderr.contains(DIAGNOSTIC), code == 1, "{args:?}: {stderr}");
        assert!(dest.join("p/secret/f").exists(), "{args:?}");
    }
}

/// The same in copy mode.
#[test]
fn test_copy_trust_holds_along_the_chain() {
    for (privs, code) in [(Some("e"), 1), (None, 0)] {
        let temp = TempDir::new().unwrap();
        let (src, _) = deep_source(&temp);
        // World-writable: others can write it whatever its group.
        let dest = dest_with_renamed_chain(&temp, 0o777);
        let mut args = vec!["-rw"];
        if let Some(p) = privs {
            args.extend(["-p", p]);
        }
        args.extend(["p/secret", dest.to_str().unwrap()]);
        let out = pax(&src, &args);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            mode_of(&dest.join("p/secret")),
            0o700,
            "{args:?}: the private directory was opened up"
        );
        assert_eq!(out.status.code(), Some(code), "{args:?}: stderr: {stderr}");
        assert!(dest.join("p/secret/f").exists(), "{args:?}");
    }
}

/// Where every level of the chain is the user's alone, a directory found deep
/// in it still takes the member's mode under -p e, in both modes.
#[test]
fn test_a_private_chain_still_stamps_under_pe() {
    for copy in [false, true] {
        let temp = TempDir::new().unwrap();
        let (src, archive) = deep_source(&temp);
        let dest = dest_with_renamed_chain(&temp, 0o755);
        let out = if copy {
            pax(
                &src,
                &["-rw", "-p", "e", "p/secret", dest.to_str().unwrap()],
            )
        } else {
            pax(&dest, &["-r", "-p", "e", "-f", archive.to_str().unwrap()])
        };
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(out.status.success(), "copy={copy}: stderr: {stderr}");
        assert_eq!(mode_of(&dest.join("p/secret")), 0o777, "copy={copy}");
    }
}

/// A copy-mode destination named through a symbolic link that sits in a
/// directory others can write: whoever planted the link chose the directory
/// it leads to, so nothing found there is trusted, and under -p e the
/// existing directory keeps its mode. In a directory only the user can write,
/// the link is the user's own, and the directory is stamped.
#[test]
fn test_copy_trusts_no_destination_reached_through_a_link_others_could_plant() {
    // The link as the last component, or in the middle (`m -> ..`, then `dest`).
    for dest in ["../open/l/", "../open/m/dest"] {
        for (open_mode, code, mode) in [(0o777, 1, 0o700), (0o755, 0, 0o755)] {
            let temp = TempDir::new().unwrap();
            let src = source_tree(&temp);
            let home = dest_with_private_d(&temp, 0o755);
            let open = temp.path().join("open");
            fs::create_dir(&open).unwrap();
            std::os::unix::fs::symlink(&home, open.join("l")).unwrap();
            std::os::unix::fs::symlink("..", open.join("m")).unwrap();
            fs::set_permissions(&open, fs::Permissions::from_mode(open_mode)).unwrap();
            let out = pax(&src, &["-rw", "-p", "e", "d", dest]);
            let stderr = String::from_utf8_lossy(&out.stderr);
            fs::set_permissions(&open, fs::Permissions::from_mode(0o755)).unwrap();
            let case = format!("{dest} in {open_mode:o}");
            assert_eq!(out.status.code(), Some(code), "{case}: {stderr}");
            assert_eq!(mode_of(&home.join("d")), mode, "{case}");
            assert!(home.join("d/f").exists(), "{case}");
        }
    }
}

/// Run pax with `args` in `dir` under umask 002.
fn pax_umask_002(dir: &Path, args: &[&str]) -> Output {
    use std::os::unix::process::CommandExt;
    let mut command = Command::new(env!("CARGO_BIN_EXE_pax"));
    command.args(args).current_dir(dir).stdin(Stdio::null());
    // SAFETY: umask is async-signal-safe.
    unsafe {
        command.pre_exec(|| {
            libc::umask(0o002);
            Ok(())
        });
    }
    command.output().unwrap()
}

/// The modification time of `path`, in seconds.
fn mtime_of(path: &Path) -> i64 {
    use std::os::unix::fs::MetadataExt;
    fs::metadata(path).unwrap().mtime()
}

/// Set the modification time of the directory or file `path`.
fn set_mtime(path: &Path, time: std::time::SystemTime) {
    fs::File::open(path).unwrap().set_modified(time).unwrap();
}

/// A tree made under a umask of 002 -- every directory group-writable, of the
/// user's private group, as Debian-style user private groups intend -- and
/// extracted again with -p e: every directory found there is the user's alone,
/// so each takes the member's times and mode, with no diagnostic and exit 0.
/// Any group-writable directory on the way used to leave every directory
/// found below it untouched, with "not applying owner, mode or times" and
/// exit 1.
#[test]
fn test_pe_reextracts_a_umask_002_tree_of_the_users_private_group() {
    if plib::testing::user_private_group().is_none() {
        eprintln!("note: this host gives the user no private group; test skipped");
        return;
    }
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dirs = ["t", "t/a", "t/a/b"];
    fs::create_dir_all(src.join("t/a/b")).unwrap();
    fs::write(src.join("t/a/b/f"), "data\n").unwrap();
    let then = std::time::UNIX_EPOCH + std::time::Duration::from_secs(978_307_200);
    for dir in dirs.iter().rev() {
        fs::set_permissions(src.join(dir), fs::Permissions::from_mode(0o775)).unwrap();
        set_mtime(&src.join(dir), then);
    }
    let out = pax(&src, &["-w", "-f", "../a.tar", "t"]);
    assert!(out.status.success(), "pax -w");
    let archive = temp.path().join("a.tar");
    let archive = archive.to_str().unwrap();

    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    fs::set_permissions(&dest, fs::Permissions::from_mode(0o775)).unwrap();
    let out = pax_umask_002(&dest, &["-r", "-f", archive]);
    assert!(out.status.success(), "first extraction");
    let now = std::time::SystemTime::now();
    for dir in dirs {
        assert_eq!(mode_of(&dest.join(dir)), 0o775, "{dir}");
        set_mtime(&dest.join(dir), now);
    }

    let out = pax_umask_002(&dest, &["-r", "-p", "e", "-f", archive]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(!stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    for dir in dirs {
        assert_eq!(mtime_of(&dest.join(dir)), 978_307_200, "{dir}: times");
        assert_eq!(mode_of(&dest.join(dir)), 0o775, "{dir}: mode");
    }
}
