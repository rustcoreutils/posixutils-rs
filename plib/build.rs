//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build script for plib.
//!
//! Windows has no POSIX regex in its C runtime, so for a Windows target this
//! compiles musl's (vendor/musl-regex) into a static library that
//! `plib::regex` links against. Unix targets use the C library's own regex
//! and compile nothing.
//!
//! The script runs on the host, so it asks Cargo about the target
//! (`CARGO_CFG_TARGET_OS`) rather than testing `cfg!(windows)`.
//!
//! When there is no C compiler for the target the C is not compiled, and no
//! diagnostic is printed. That is what lets `cargo check` and `cargo clippy
//! --target x86_64-pc-windows-msvc` run on a Linux host, which has no MSVC
//! compiler: checking produces no object code, so nothing needs the library.
//! (Cargo does not tell a build script whether it is checking or building.)
//! A real build on such a host fails at the link, on the unresolved
//! `plib_regcomp`. A compiler that is found and then fails is a hard error.

use std::env;
use std::path::Path;

const VENDOR: &str = "vendor/musl-regex";

const SOURCES: [&str; 5] = [
    "regcomp.c",
    "regexec.c",
    "regerror.c",
    "tre-mem.c",
    "iswctype.c",
];

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={VENDOR}");

    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        build_musl_regex();
    }
}

fn build_musl_regex() {
    let mut build = cc::Build::new();
    build
        .include(format!("{VENDOR}/include"))
        .files(SOURCES.iter().map(|f| format!("{VENDOR}/src/{f}")))
        // musl is C99 with `restrict`; MSVC accepts `restrict` only in its
        // C11 mode (/std:c11), and gcc takes the same spelling.
        .std("c11")
        // Vendored code, kept as close to upstream as possible: no extra
        // warnings (-Wall, /W4) on top of the compiler's defaults, since cc
        // relays each as a cargo warning and they are not ours to fix.
        .warnings(false);

    // The probe is quiet: finding no compiler is not worth a warning here.
    let msvc = env::var("CARGO_CFG_TARGET_ENV").as_deref() == Ok("msvc");
    let probe = build.clone().cargo_warnings(false).try_get_compiler();
    let available = probe.is_ok_and(|tool| {
        // With no MSVC compiler to be found, as on a Linux host, cc falls back
        // to the host's own `cc`, which exists but cannot build for MSVC.
        (!msvc || tool.is_like_msvc()) && compiler_exists(tool.path())
    });
    if available {
        build.compile("plib_musl_regex");
    }
}

/// Whether `path` names an existing file, directly or found on `PATH`.
fn compiler_exists(path: &Path) -> bool {
    if path.components().count() > 1 {
        return path.is_file();
    }
    let Some(dirs) = env::var_os("PATH") else {
        return false;
    };
    env::split_paths(&dirs).any(|dir| {
        let candidate = dir.join(path);
        candidate.is_file() || candidate.with_extension("exe").is_file()
    })
}
