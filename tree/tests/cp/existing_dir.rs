//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A source directory whose destination name is already a directory. cp copies into it, and
//! under -p gives it the source's owner, mode and times -- but, as pax does, only where nobody
//! else could have created that name first: in a destination others can create entries in,
//! the directory there may be anyone's renamed to the name, a private one of the user's own
//! included, and giving it the source's mode would open it up. There it keeps all its own, and
//! that is diagnosed. The trust is carried down from the destination directory the user named,
//! one directory at a time, and a directory cp made itself starts it afresh.

use plib::testing::get_binary_path;
use plib::tmp::{tempdir, TempDir};
use std::fs::{self, File, FileTimes};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, SystemTime};

/// The modification time every source directory is given, far from any time a copy runs at.
const OLD_MTIME: i64 = 1_000_000_000;

const DIAGNOSTIC: &str = "the directory was already there, and others can create entries";

fn set_mode(path: &Path, mode: u32) {
    fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
}

fn mode_of(path: &Path) -> u32 {
    fs::symlink_metadata(path).unwrap().mode() & 0o7777
}

fn mtime_of(path: &Path) -> i64 {
    fs::symlink_metadata(path).unwrap().mtime()
}

/// Give the directory `path` the modification time `OLD_MTIME`.
fn set_old_mtime(path: &Path) {
    let old = SystemTime::UNIX_EPOCH + Duration::from_secs(OLD_MTIME as u64);
    File::open(path)
        .unwrap()
        .set_times(FileTimes::new().set_modified(old).set_accessed(old))
        .unwrap();
}

/// A source directory `dir` (mode `mode`, modified at `OLD_MTIME`) holding a file `f`.
fn source_dir(dir: &Path, mode: u32) {
    fs::create_dir_all(dir).unwrap();
    fs::write(dir.join("f"), "data\n").unwrap();
    set_mode(dir, mode);
    set_old_mtime(dir);
}

/// A directory `dir` of mode `mode` holding an existing directory `name` of mode 0700 with a
/// file in it -- in the attack, the user's own private `secrets`, renamed to the name the
/// source directory maps to before cp runs.
fn with_private_dir(dir: &Path, mode: u32, name: &str) {
    fs::create_dir_all(dir).unwrap();
    let secrets = dir.join("secrets");
    fs::create_dir(&secrets).unwrap();
    fs::write(secrets.join("key"), "secret\n").unwrap();
    set_mode(&secrets, 0o700);
    fs::rename(&secrets, dir.join(name)).unwrap();
    set_mode(dir, mode);
}

/// Run cp with `args` in `cwd`.
fn cp(cwd: &Path, args: &[&str]) -> Output {
    Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp")
}

/// `src/d` (0755) and a destination `dest` of mode `dest_mode` with a private `d` in it.
fn secrets_scenario(dest_mode: u32) -> TempDir {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("src/d"), 0o755);
    with_private_dir(&temp.path().join("dest"), dest_mode, "d");
    temp
}

/// Give `dir` a group the user shares with others (`plib::testing::shared_group`), so that
/// its group write permission is theirs too; `false`, with a note, when the user has none.
fn share_group(dir: &Path) -> bool {
    let Some(gid) = plib::testing::shared_group() else {
        eprintln!("note: the user belongs to no group shared with others; case skipped");
        return false;
    };
    std::os::unix::fs::chown(dir, None, Some(gid)).unwrap();
    true
}

/// The private directory renamed to the source's name keeps its mode and its own times under
/// -p wherever others can create entries in the destination: group (a group others are in) or
/// other writable, or sticky and world writable. That is diagnosed, naming it, and the exit
/// status is 1; its contents are still copied.
#[test]
fn cp_pr_leaves_a_found_directory_alone_below_an_open_destination() {
    for dest_mode in [0o777, 0o1777, 0o775] {
        let temp = secrets_scenario(dest_mode);
        if dest_mode == 0o775 && !share_group(&temp.path().join("dest")) {
            continue;
        }
        let out = cp(temp.path(), &["-pR", "src/d", "dest"]);
        let stderr = String::from_utf8_lossy(&out.stderr);
        let d = temp.path().join("dest/d");
        assert_eq!(
            mode_of(&d),
            0o700,
            "{dest_mode:o}: the private directory was opened up"
        );
        assert_ne!(
            mtime_of(&d),
            OLD_MTIME,
            "{dest_mode:o}: its times were changed"
        );
        assert_eq!(
            out.status.code(),
            Some(1),
            "{dest_mode:o}: stderr: {stderr}"
        );
        assert!(
            stderr.contains(DIAGNOSTIC),
            "{dest_mode:o}: stderr: {stderr}"
        );
        assert!(
            stderr.contains("'dest/d'"),
            "{dest_mode:o}: stderr: {stderr}"
        );
        assert_eq!(fs::read_to_string(d.join("f")).unwrap(), "data\n");
        assert_eq!(fs::read_to_string(d.join("key")).unwrap(), "secret\n");
        set_mode(&temp.path().join("dest"), 0o755);
    }
}

/// Under a umask of 002 the destination is group-writable; when its group is the user's
/// private group -- nobody else in it, nobody else's primary group -- that write permission is
/// the user's own, and -p stamps the found directory as in a destination of mode 0755.
#[test]
fn cp_pr_stamps_a_found_directory_in_a_destination_of_the_users_private_group() {
    if plib::testing::user_private_group().is_none() {
        eprintln!("note: this host gives the user no private group; test skipped");
        return;
    }
    let temp = secrets_scenario(0o775);
    let out = cp(temp.path(), &["-pR", "src/d", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let d = temp.path().join("dest/d");
    assert_eq!(mode_of(&d), 0o755);
    assert_eq!(mtime_of(&d), OLD_MTIME);
}

/// An ACL entry naming another user widens the group bits of the mode to the ACL's mask: a
/// destination of the user's private group, 0755 but for an ACL granting someone else write,
/// shows 0775 -- and others can create entries in it. The found directory is left alone.
#[test]
fn cp_pr_leaves_a_found_directory_alone_where_an_acl_lets_others_write() {
    if plib::testing::user_private_group().is_none() {
        eprintln!("note: this host gives the user no private group; test skipped");
        return;
    }
    let temp = secrets_scenario(0o755);
    let dest = temp.path().join("dest");
    if !plib::testing::grant_named_acl(&dest) {
        return;
    }
    assert_eq!(mode_of(&dest), 0o775, "the mask shows in the group bits");
    let out = cp(temp.path(), &["-pR", "src/d", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        mode_of(&dest.join("d")),
        0o700,
        "the private directory was opened up"
    );
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
}

/// Without -p nothing is asked for: the found directory keeps its mode, as always, and nothing
/// is said.
#[test]
fn cp_r_without_p_leaves_a_found_directory_mode_alone_silently() {
    let temp = secrets_scenario(0o777);
    let out = cp(temp.path(), &["-R", "src/d", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(stderr, "");
    assert_eq!(mode_of(&temp.path().join("dest/d")), 0o700);
    assert!(temp.path().join("dest/d/f").exists());
    set_mode(&temp.path().join("dest"), 0o755);
}

/// Where nobody but the user can create entries in the destination, -p gives a found
/// directory the source's mode and times, as GNU cp does.
#[test]
fn cp_pr_stamps_a_found_directory_in_a_private_destination() {
    let temp = secrets_scenario(0o755);
    let out = cp(temp.path(), &["-pR", "src/d", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let d = temp.path().join("dest/d");
    assert_eq!(mode_of(&d), 0o755);
    assert_eq!(mtime_of(&d), OLD_MTIME);
}

/// The destination directory the user named is the anchor, and is never judged itself:
/// copying a directory's contents into it (`src/.`) gives it the source's attributes under -p
/// wherever it is, as GNU cp does.
#[test]
fn cp_pr_stamps_the_named_destination_itself() {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("src/d"), 0o755);
    with_private_dir(&temp.path().join("open"), 0o777, "dest");
    let out = cp(temp.path(), &["-pR", "src/d/.", "open/dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let dest = temp.path().join("open/dest");
    assert_eq!(mode_of(&dest), 0o755);
    assert_eq!(mtime_of(&dest), OLD_MTIME);
    set_mode(&temp.path().join("open"), 0o755);
}

/// A directory this run made, found again by a later operand, is still the run's own, wherever
/// it is: merging two sources into one directory of an open destination gives it the second
/// one's attributes under -p, as GNU cp does -- whether it was made by the first operand's
/// copy or on the way by `--parents`.
#[test]
fn cp_pr_trusts_a_directory_it_made_when_found_again() {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("s1/x"), 0o700);
    source_dir(&temp.path().join("s2/x"), 0o750);
    // A file of its own: cp refuses to overwrite one it has just made.
    fs::rename(temp.path().join("s2/x/f"), temp.path().join("s2/x/g")).unwrap();
    set_old_mtime(&temp.path().join("s2/x"));
    fs::create_dir(temp.path().join("dest")).unwrap();
    set_mode(&temp.path().join("dest"), 0o777);
    let out = cp(temp.path(), &["-pR", "s1/x", "s2/x", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let x = temp.path().join("dest/x");
    assert_eq!(mode_of(&x), 0o750);
    assert_eq!(mtime_of(&x), OLD_MTIME);
    set_mode(&temp.path().join("dest"), 0o755);

    // `--parents a/b/c` makes `dest/a` and `dest/a/b` under the open `dest`; `--parents a/b`
    // then finds both, and `dest/a/b` -- inside a directory the run made -- takes `a/b`'s
    // attributes. (`c` is empty: cp refuses to overwrite a file it has just made.)
    let temp = tempdir().unwrap();
    fs::create_dir_all(temp.path().join("a/b/c")).unwrap();
    source_dir(&temp.path().join("a/b"), 0o750);
    set_mode(&temp.path().join("a"), 0o755);
    fs::create_dir(temp.path().join("dest")).unwrap();
    set_mode(&temp.path().join("dest"), 0o777);
    let out = cp(temp.path(), &["-pR", "--parents", "a/b/c", "a/b", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let b = temp.path().join("dest/a/b");
    assert_eq!(mode_of(&b), 0o750);
    assert_eq!(mtime_of(&b), OLD_MTIME);
    assert!(b.join("f").exists() && b.join("c").is_dir());
    set_mode(&temp.path().join("dest"), 0o755);
}

/// Trust holds along the whole chain. A found directory others can write breaks it for every
/// directory found below it, though the destination above is the user's alone; and one found
/// in an open destination -- its own mode private, but possibly renamed in -- breaks it too.
#[test]
fn cp_pr_trust_holds_along_the_chain() {
    // (destination mode, `p`'s mode): `p` is found in the destination, `p/secret` in `p`.
    for (dest_mode, p_mode) in [(0o755, 0o777), (0o777, 0o755)] {
        let temp = tempdir().unwrap();
        source_dir(&temp.path().join("src/p/secret"), 0o755);
        set_mode(&temp.path().join("src/p"), 0o755);
        let dest = temp.path().join("dest");
        fs::create_dir(&dest).unwrap();
        with_private_dir(&dest.join("p"), p_mode, "secret");
        set_mode(&dest, dest_mode);
        let case = format!("dest {dest_mode:o}, p {p_mode:o}");

        let out = cp(temp.path(), &["-pR", "src/p", "dest"]);
        let stderr = String::from_utf8_lossy(&out.stderr);
        let secret = dest.join("p/secret");
        assert_eq!(
            mode_of(&secret),
            0o700,
            "{case}: the private directory was opened up"
        );
        assert_ne!(mtime_of(&secret), OLD_MTIME, "{case}");
        assert_eq!(out.status.code(), Some(1), "{case}: stderr: {stderr}");
        assert!(
            stderr.contains("'dest/p/secret'"),
            "{case}: stderr: {stderr}"
        );
        assert!(secret.join("f").exists(), "{case}");
        set_mode(&dest, 0o755);
    }
}

/// `--parents` carries the trust from the destination it names through every directory it
/// walks: `p`, found in an open destination, hands none to `p/secret` found in it.
#[test]
fn cp_parents_pr_trust_holds_along_the_chain() {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("p/secret"), 0o755);
    set_mode(&temp.path().join("p"), 0o755);
    let dest = temp.path().join("dest");
    fs::create_dir(&dest).unwrap();
    with_private_dir(&dest.join("p"), 0o755, "secret");
    set_mode(&dest, 0o777);

    let out = cp(temp.path(), &["-pR", "--parents", "p/secret", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let secret = dest.join("p/secret");
    assert_eq!(
        mode_of(&secret),
        0o700,
        "the private directory was opened up"
    );
    assert_ne!(mtime_of(&secret), OLD_MTIME);
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "stderr: {stderr}");
    assert!(secret.join("f").exists());

    // In a destination only the user can write, the same chain is trusted.
    set_mode(&dest, 0o755);
    let out = cp(temp.path(), &["-pR", "--parents", "p/secret", "dest"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(mode_of(&secret), 0o755);
    assert_eq!(mtime_of(&secret), OLD_MTIME);
}

/// Judging the directory a found one is in needs only search permission on it, as copying
/// into the found one does: under a destination the user may search and write but not read
/// (0300), the copy is made and the found directory stamped.
#[test]
fn cp_pr_judges_a_found_directory_in_an_unreadable_destination() {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("s2"), 0o750);
    let r = temp.path().join("r");
    fs::create_dir_all(r.join("s2")).unwrap();
    set_mode(&r.join("s2"), 0o700);
    set_mode(&r, 0o300);

    let out = cp(temp.path(), &["-pR", "s2", "r"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    set_mode(&r, 0o755);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(fs::read_to_string(r.join("s2/f")).unwrap(), "data\n");
    assert_eq!(mode_of(&r.join("s2")), 0o750);
    assert_eq!(mtime_of(&r.join("s2")), OLD_MTIME);
}

/// `--parents .` copies the working directory's contents into the target itself, as GNU cp
/// does -- not into a directory of the target's own name inside it.
#[test]
fn cp_parents_dot_copies_into_the_target_itself() {
    let temp = tempdir().unwrap();
    let sub = temp.path().join("sub");
    fs::create_dir_all(sub.join("d")).unwrap();
    fs::write(sub.join("d/f"), "f\n").unwrap();
    fs::create_dir(temp.path().join("t")).unwrap();

    let out = cp(&sub, &["-R", "--parents", ".", "../t"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let t = temp.path().join("t");
    assert_eq!(fs::read_to_string(t.join("d/f")).unwrap(), "f\n");
    assert!(
        !t.join("t").exists(),
        "copied into a directory named like the target"
    );
}

/// A source ending in `..` has a destination ending in `..` too: not a name inside the target,
/// where `--parents` copies, but whatever directory that `..` reaches, which no chain of trust
/// from the target describes. It is refused, and nothing there is touched.
#[test]
fn cp_parents_refuses_a_source_ending_in_dot_dot() {
    let temp = tempdir().unwrap();
    let a = temp.path().join("a");
    source_dir(&a.join("g"), 0o777);
    let c = a.join("c");
    fs::create_dir_all(c.join("g")).unwrap();
    set_mode(&c.join("g"), 0o755);
    fs::create_dir(c.join("t")).unwrap();
    set_mode(&c.join("t"), 0o777);

    let out = cp(&c, &["-pR", "--parents", "..", "t"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    set_mode(&c.join("t"), 0o755);
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    assert!(stderr.contains("'..'"), "stderr: {stderr}");
    assert_eq!(mode_of(&c.join("g")), 0o755);
    assert!(!c.join("g/f").exists());
}

/// `open` (mode `open_mode`) holding `d -> ../home`, and `home` (mode `home_mode`) holding the
/// user's private `src` (0700); and a source `src` (0755, modified at `OLD_MTIME`).
fn link_scenario(open_mode: u32, home_mode: u32) -> TempDir {
    let temp = tempdir().unwrap();
    source_dir(&temp.path().join("src"), 0o755);
    with_private_dir(&temp.path().join("home"), home_mode, "src");
    let open = temp.path().join("open");
    fs::create_dir(&open).unwrap();
    std::os::unix::fs::symlink("../home", open.join("d")).unwrap();
    set_mode(&open, open_mode);
    temp
}

/// A destination named through a symbolic link that sits in a directory others can write:
/// whoever planted the link chose the directory it leads to, so nothing found there is
/// trusted. Under -p the found directory keeps everything, and that is reported; the copy
/// itself still follows the link, as GNU cp's does. In a directory only the user can write,
/// the link is the user's own, and the found directory is stamped.
#[test]
fn cp_pr_trusts_no_directory_reached_through_a_link_others_could_plant() {
    for (open_mode, code, mode) in [(0o777, 1, 0o700), (0o755, 0, 0o755)] {
        let temp = link_scenario(open_mode, 0o755);
        let out = cp(temp.path(), &["-pR", "src", "open/d/"]);
        let stderr = String::from_utf8_lossy(&out.stderr);
        set_mode(&temp.path().join("open"), 0o755);
        let found = temp.path().join("home/src");
        assert_eq!(
            out.status.code(),
            Some(code),
            "open {open_mode:o}: {stderr}"
        );
        assert_eq!(mode_of(&found), mode, "open {open_mode:o}");
        assert_eq!(
            mtime_of(&found) == OLD_MTIME,
            code == 0,
            "open {open_mode:o}"
        );
        assert_eq!(stderr.contains("'open/d/src'"), code == 1, "{stderr}");
        assert_eq!(fs::read_to_string(found.join("f")).unwrap(), "data\n");
    }
}

/// The link need not be the last component: one in the middle of the destination's path, or
/// one a `..` climbs back out of, leads just as far from where the user pointed.
#[test]
fn cp_pr_trusts_no_directory_reached_through_a_link_anywhere_in_the_destination() {
    let temp = link_scenario(0o777, 0o755);
    let home = temp.path().join("home");
    with_private_dir(&home.join("sub"), 0o755, "src");
    std::os::unix::fs::symlink("../home/sub", temp.path().join("open/e")).unwrap();
    for (destination, found) in [("open/d/sub", "home/sub/src"), ("open/e/..", "home/src")] {
        let out = cp(temp.path(), &["-pR", "src", destination]);
        let stderr = String::from_utf8_lossy(&out.stderr);
        let found = temp.path().join(found);
        assert_eq!(out.status.code(), Some(1), "{destination}: {stderr}");
        assert!(stderr.contains(DIAGNOSTIC), "{destination}: {stderr}");
        assert_eq!(mode_of(&found), 0o700, "{destination}: opened up");
        assert_eq!(fs::read_to_string(found.join("f")).unwrap(), "data\n");
    }
    set_mode(&temp.path().join("open"), 0o755);
}

/// The same for the destination itself, copied into (`src/.`): named through a link in a
/// directory others can write, it is the directory found, and keeps everything under -p.
#[test]
fn cp_pr_leaves_alone_a_named_destination_reached_through_a_link_others_could_plant() {
    let temp = link_scenario(0o777, 0o700);
    let out = cp(temp.path(), &["-pR", "src/.", "open/d/"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    set_mode(&temp.path().join("open"), 0o755);
    let home = temp.path().join("home");
    assert_eq!(out.status.code(), Some(1), "{stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "{stderr}");
    assert_eq!(mode_of(&home), 0o700, "the private directory was opened up");
    assert_eq!(fs::read_to_string(home.join("f")).unwrap(), "data\n");
}

/// `--parents ../d/x`: the `..` climbs above the target, into a directory nothing vouched for
/// -- whatever its own mode -- and `d/x` found there keeps everything under -p.
#[test]
fn cp_parents_dot_dot_above_the_target_trusts_nothing() {
    let temp = tempdir().unwrap();
    // Run in `c`: the source `../s/d/x` is `s/d/x`, its copy `w/t/../s/d/x`, `w/s/d/x`,
    // where the user's own `w` already holds `s/d` and a private `x`.
    fs::create_dir(temp.path().join("c")).unwrap();
    source_dir(&temp.path().join("s/d/x"), 0o755);
    fs::create_dir_all(temp.path().join("w/t")).unwrap();
    fs::create_dir_all(temp.path().join("w/s/d")).unwrap();
    with_private_dir(&temp.path().join("w/s/d"), 0o755, "x");
    let out = cp(
        &temp.path().join("c"),
        &["-pR", "--parents", "../s/d/x", "../w/t"],
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    let found = temp.path().join("w/s/d/x");
    assert_eq!(mode_of(&found), 0o700, "{stderr}");
    assert_eq!(out.status.code(), Some(1), "{stderr}");
    assert!(stderr.contains(DIAGNOSTIC), "{stderr}");
    assert_eq!(fs::read_to_string(found.join("f")).unwrap(), "data\n");
}

/// `--parents a/../d/x`: the `..` returns from `a`, which the run makes, to the directory the
/// walk was in before -- which hands what it handed then, not what a directory made below it
/// hands. Under a target reached through a link planted in a directory others can write,
/// `d/x` keeps everything, with the `..` as without it; under a target the user's own, it is
/// stamped either way.
#[test]
fn cp_parents_dot_dot_returns_to_the_trust_it_left() {
    for (target, code, mode) in [("../open/tl", 1, 0o700), ("../home/H", 0, 0o755)] {
        for source in ["a/../d/x", "d/x"] {
            let temp = tempdir().unwrap();
            let src = temp.path().join("srcroot");
            fs::create_dir_all(src.join("a")).unwrap();
            source_dir(&src.join("d/x"), 0o755);
            set_mode(&src.join("d"), 0o755);
            let home = temp.path().join("home/H");
            fs::create_dir_all(home.join("d")).unwrap();
            set_mode(&home.join("d"), 0o755);
            with_private_dir(&home.join("d"), 0o755, "x");
            let open = temp.path().join("open");
            fs::create_dir(&open).unwrap();
            std::os::unix::fs::symlink("../home/H", open.join("tl")).unwrap();
            set_mode(&open, 0o777);

            let out = cp(&src, &["-pR", "--parents", source, target]);
            let stderr = String::from_utf8_lossy(&out.stderr);
            set_mode(&open, 0o755);
            let case = format!("{source} into {target}");
            let x = home.join("d/x");
            assert_eq!(mode_of(&x), mode, "{case}: {stderr}");
            assert_eq!(out.status.code(), Some(code), "{case}: {stderr}");
            assert_eq!(fs::read_to_string(x.join("f")).unwrap(), "data\n", "{case}");
        }
    }
}
