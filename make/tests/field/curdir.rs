//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX 105884-105893: the `CURDIR` macro.

use crate::common::Dir;

const SHOW: &str = "all:\n\t@echo \"$(CURDIR)\"\n\t@echo \"[$$CURDIR]\"\n";

fn canonical(dir: &Dir) -> String {
    std::fs::canonicalize(dir.path(""))
        .unwrap()
        .to_string_lossy()
        .into_owned()
}

#[test]
fn curdir_is_the_working_directory() {
    let dir = Dir::new("curdir_plain");
    dir.write("Makefile", SHOW);
    // Not put in the environment of a recipe that did not inherit it.
    dir.make(&[]).ok(&format!("{}\n[]\n", canonical(&dir)));
}

#[test]
fn curdir_follows_dash_c() {
    let dir = Dir::new("curdir_dash_c");
    dir.write("sub/Makefile", SHOW);
    let sub = canonical(&dir) + "/sub";
    dir.make(&["-C", "sub"]).ok(&format!("{sub}\n[]\n"));
}

// "The CURDIR environment variable shall not affect the value of the CURDIR
// macro unless the -e option is specified" -- and the environment variable
// is then set to the macro's value.
#[test]
fn the_environment_does_not_set_curdir() {
    let dir = Dir::new("curdir_env");
    dir.write("Makefile", SHOW);
    let here = canonical(&dir);
    dir.make_env(&[], &[("CURDIR", "/elsewhere")])
        .ok(&format!("{here}\n[{here}]\n"));
}

#[test]
fn dash_e_lets_the_environment_set_curdir() {
    let dir = Dir::new("curdir_env_e");
    dir.write("Makefile", SHOW);
    dir.make_env(&["-e"], &[("CURDIR", "/elsewhere")])
        .ok("/elsewhere\n[/elsewhere]\n");
}

#[test]
fn the_makefile_and_command_line_replace_curdir() {
    let dir = Dir::new("curdir_override");
    dir.write("Makefile", &format!("CURDIR = /mine\n{SHOW}"));
    dir.make(&[]).ok("/mine\n[]\n");
    dir.make(&["CURDIR=/cmd"]).ok("/cmd\n[/cmd]\n");
}
