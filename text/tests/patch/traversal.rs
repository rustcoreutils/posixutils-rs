//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A patch must never write outside the directory it patches.
//!
//! dpkg-source applies the patches of a downloaded source package, so the
//! names in a patch are untrusted.  Every way a patch can name a file is
//! tried here -- `..` components, an absolute name, a directory that is a
//! symbolic link out of the tree, a file that is such a link -- for each kind
//! of change (modify, create, delete, empty under -E, a failing hunk and its
//! rejects, git renames and copies), under -p0, under -p1 with backups, and
//! under the exact options dpkg-source passes.  After each run the directory
//! beside the tree, and the one above it, must be as they were.
//!
//! GNU patch 2.7.6 refuses all of these but an absolute name under -p0:
//! told to delete one, it "assumes -R" and writes the file's backup and
//! rejects beside it, and told to create one that exists, it changes it.

use super::symlinks::Fixture;
use std::fs;

const P0: &[&str] = &["-p0", "-t"];
const P1_BACKUP: &[&str] = &["-p1", "-t", "-b"];
/// What dpkg-source passes for each patch of a "3.0 (quilt)" package.
const DPKG_QUILT: &[&str] = &[
    "-t",
    "-F",
    "0",
    "-N",
    "-p1",
    "-u",
    "-V",
    "never",
    "-E",
    "-b",
    "-B",
    ".pc/p/",
    "--reject-file=-",
];
/// What dpkg-source passes for a "1.0" package's .diff.gz (with the -E it
/// adds for a "2.0" package).
const DPKG_DIFF: &[&str] = &[
    "-t",
    "-F",
    "0",
    "-N",
    "-p1",
    "-u",
    "-V",
    "never",
    "-b",
    "-z",
    ".dpkg-orig",
    "-E",
];
const OPTION_SETS: [&[&str]; 4] = [P0, P1_BACKUP, DPKG_QUILT, DPKG_DIFF];

/// What the tree holds before the patch: `f`, plus a link out of it.
#[derive(Clone, Copy, Debug)]
enum Setup {
    Plain,
    /// `link` -> `../outside`, a directory outside the tree.
    DirLink,
    /// `g` -> `../outside/target`, a file outside the tree.
    LeafLink,
    /// `g` -> `../outside/new`, which does not exist.
    DanglingLink,
}

fn fixture(setup: Setup) -> Fixture {
    let fx = Fixture::new("traversal");
    fs::write(fx.in_tree("f"), "target\n").unwrap();
    match setup {
        Setup::Plain => {}
        Setup::DirLink => fx.link_to_outside("link", "../outside"),
        Setup::LeafLink => fx.link_to_outside("g", "../outside/target"),
        Setup::DanglingLink => fx.link_to_outside("g", "../outside/new"),
    }
    fx
}

/// The old and new header names for `name` under `args`: as they are under
/// -p0, behind `a/` and `b/` under -p1. An absolute name is never prefixed.
fn header_names(args: &[&str], name: &str) -> (String, String) {
    if args.contains(&"-p1") && !name.starts_with('/') {
        (format!("a/{name}"), format!("b/{name}"))
    } else {
        (name.to_string(), name.to_string())
    }
}

/// The kinds of change a patch can make to the file `name`.
#[derive(Clone, Copy, Debug)]
enum Change {
    Modify,
    FailingHunk,
    Empty,
    Context,
    Create,
    Delete,
    GitCreate,
}

fn patch_text(change: Change, args: &[&str], name: &str) -> String {
    let (old, new) = header_names(args, name);
    match change {
        Change::Modify => format!("--- {old}\n+++ {new}\n@@ -1 +1 @@\n-target\n+pwned\n"),
        Change::FailingHunk => {
            format!("--- {old}\n+++ {new}\n@@ -1 +1 @@\n-nomatch\n+pwned\n")
        }
        // Under -E the emptied file is removed.
        Change::Empty => format!("--- {old}\n+++ {new}\n@@ -1 +0,0 @@\n-target\n"),
        Change::Context => format!(
            "*** {old}\n--- {new}\n***************\n*** 1 ****\n! target\n--- 1 ----\n! pwned\n"
        ),
        Change::Create => format!("--- /dev/null\n+++ {new}\n@@ -0,0 +1 @@\n+pwned\n"),
        Change::Delete => format!("--- {old}\n+++ /dev/null\n@@ -1 +0,0 @@\n-target\n"),
        Change::GitCreate => format!(
            "diff --git {old} {new}\nnew file mode 100644\n--- /dev/null\n+++ {new}\n\
             @@ -0,0 +1 @@\n+pwned\n"
        ),
    }
}

/// Run `patch` under `args` and check that nothing outside the tree
/// changed, and, when `refused`, that patch reported failure.
fn check(fx: &Fixture, args: &[&str], patch: &str, refused: bool) {
    let (code, err) = fx.run(args, patch);
    fx.assert_outside_untouched();
    let mut beside: Vec<String> = fs::read_dir(&fx.root)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    beside.sort();
    assert_eq!(beside, ["outside", "p.diff", "tree"], "{args:?}\n{patch}");
    if refused {
        assert_ne!(code, 0, "{args:?}\n{patch}\nstderr: {err}");
    }
}

/// Names that leave the tree by `..`, for a file that exists outside it
/// (`target`) and for one that does not (`new`).
const DOTDOT_NAMES: [&str; 4] = [
    "../outside/target",
    "a/../../outside/target",
    "./../outside/target",
    "../outside/new",
];

#[test]
fn dotdot_names_are_refused() {
    let changes = [
        Change::Modify,
        Change::FailingHunk,
        Change::Empty,
        Change::Context,
        Change::Create,
        Change::Delete,
        Change::GitCreate,
    ];
    for args in OPTION_SETS {
        for name in DOTDOT_NAMES {
            for change in changes {
                let fx = fixture(Setup::Plain);
                check(&fx, args, &patch_text(change, args, name), true);
                assert_eq!(fs::read_to_string(fx.in_tree("f")).unwrap(), "target\n");
            }
        }
    }
}

// An absolute name: refused under -p0; under -p1 its first component is
// stripped and what is left is a name inside the tree.
#[test]
fn absolute_names_stay_out() {
    let changes = [
        Change::Modify,
        Change::FailingHunk,
        Change::Empty,
        Change::Context,
        Change::Create,
        Change::Delete,
    ];
    for args in OPTION_SETS {
        for leaf in ["target", "new"] {
            for change in changes {
                let fx = fixture(Setup::Plain);
                let name = fx.outside.join(leaf).display().to_string();
                let creates_inside = args.contains(&"-p1") && matches!(change, Change::Create);
                check(&fx, args, &patch_text(change, args, &name), !creates_inside);
            }
        }
    }
}

// A directory in the name that is a link out of the tree.
#[test]
fn names_through_a_linked_directory_are_refused() {
    let changes = [
        Change::Modify,
        Change::FailingHunk,
        Change::Empty,
        Change::Context,
        Change::Create,
        Change::Delete,
        Change::GitCreate,
    ];
    for args in OPTION_SETS {
        for name in ["link/target", "link/new"] {
            for change in changes {
                let fx = fixture(Setup::DirLink);
                check(&fx, args, &patch_text(change, args, name), true);
            }
        }
    }
}

// The file itself is a link out of the tree, to a file or dangling.
#[test]
fn a_linked_file_is_refused() {
    let changes = [
        Change::Modify,
        Change::FailingHunk,
        Change::Empty,
        Change::Context,
        Change::Create,
        Change::Delete,
        Change::GitCreate,
    ];
    for args in OPTION_SETS {
        for setup in [Setup::LeafLink, Setup::DanglingLink] {
            for change in changes {
                let fx = fixture(setup);
                check(&fx, args, &patch_text(change, args, "g"), true);
            }
        }
    }
}

// A git rename or copy to a name outside the tree moves nothing out of it.
#[test]
fn git_rename_and_copy_stay_in() {
    for args in OPTION_SETS {
        for to in ["../outside/r", "a/../../outside/r", "link/r"] {
            for verb in ["rename", "copy"] {
                let fx = fixture(Setup::DirLink);
                let (old, _) = header_names(args, "f");
                let (_, new) = header_names(args, to);
                let patch = format!(
                    "diff --git {old} {new}\nsimilarity index 100%\n{verb} from f\n{verb} to {to}\n"
                );
                check(&fx, args, &patch, false);
                assert_eq!(fs::read_to_string(fx.in_tree("f")).unwrap(), "target\n");
            }
        }
    }
}

// The backup's name is a link out of the tree, under each way dpkg-source
// names backups: `-z .dpkg-orig`, and `-B .pc/p/` with the prefix's
// directory or the backup itself a link. The backup replaces a linked name
// or is refused; it is never written through it.
#[test]
fn backups_never_go_through_a_link() {
    let modify = |args: &[&str]| patch_text(Change::Modify, args, "f");

    let fx = fixture(Setup::Plain);
    fx.link_to_outside("f.dpkg-orig", "../outside/target");
    check(&fx, DPKG_DIFF, &modify(DPKG_DIFF), false);

    let fx = fixture(Setup::Plain);
    fx.link_to_outside("f.dpkg-orig", "../outside/new");
    check(&fx, DPKG_DIFF, &modify(DPKG_DIFF), false);

    let fx = fixture(Setup::Plain);
    fs::create_dir(fx.in_tree(".pc")).unwrap();
    fx.link_to_outside(".pc/p", "../outside");
    check(&fx, DPKG_QUILT, &modify(DPKG_QUILT), true);

    let fx = fixture(Setup::Plain);
    fs::create_dir_all(fx.in_tree(".pc/p")).unwrap();
    fx.link_to_outside(".pc/p/f", "../../../outside/target");
    check(&fx, DPKG_QUILT, &modify(DPKG_QUILT), false);

    for args in [P1_BACKUP, &["-p1", "-t", "-b", "-V", "simple"][..]] {
        let fx = fixture(Setup::Plain);
        fx.link_to_outside("f.orig", "../outside/target");
        check(&fx, args, &modify(args), false);
    }
}

// Rejects go to `f.rej` beside the file, which replaces a link there, or
// nowhere under `--reject-file=-`.
#[test]
fn rejects_never_go_through_a_link() {
    let failing = |args: &[&str]| patch_text(Change::FailingHunk, args, "f");
    for args in [P0, P1_BACKUP, DPKG_DIFF] {
        let fx = fixture(Setup::Plain);
        fx.link_to_outside("f.rej", "../outside/target");
        check(&fx, args, &failing(args), true);
    }
    let fx = fixture(Setup::Plain);
    fx.link_to_outside("f.rej", "../outside/target");
    check(&fx, DPKG_QUILT, &failing(DPKG_QUILT), true);
    assert!(fs::symlink_metadata(fx.in_tree("f.rej"))
        .unwrap()
        .file_type()
        .is_symlink());
}
