//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! strip on linked ELF files.
//!
//! ELF-specific: cc produces Mach-O on macOS, which strip does not support.

#![cfg(target_os = "linux")]

use crate::c_compiler;
use object::read::elf::ElfFile64;
use object::{Endianness, Object, ObjectSection};
use plib::tmp::TempDir;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

const LIB_C: &str = "int lib_f(int x){return x+1;}\n\
int lib_g(int x){return lib_f(x)*2;}\n\
static int hid(int x){return x*3;}\n\
int lib_h(int x){return hid(x);}\n";

const MAIN_C: &str = "#include <stdio.h>\n\
int lib_g(int);\n\
int main(void){printf(\"%d\\n\", lib_g(4));return 0;}\n";

fn cc(dir: &Path, args: &[&str]) {
    let out = c_compiler()
        .current_dir(dir)
        .args(args)
        .output()
        .expect("failed to run cc");
    assert!(
        out.status.success(),
        "cc {:?} failed: {}",
        args,
        String::from_utf8_lossy(&out.stderr)
    );
}

fn strip(args: &[&str], file: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_strip"))
        .env("LC_ALL", "C")
        .args(args)
        .arg(file)
        .output()
        .expect("failed to run strip")
}

fn strip_ok(args: &[&str], file: &Path) {
    let out = strip(args, file);
    assert!(
        out.status.success(),
        "strip {:?} failed: {}",
        args,
        String::from_utf8_lossy(&out.stderr)
    );
}

fn run_prints(prog: &Path, expected: &str) {
    let out = Command::new(prog).output().expect("run stripped program");
    assert!(
        out.status.success(),
        "{} failed after strip: {}",
        prog.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), expected);
}

fn section_names(bytes: &[u8]) -> Vec<String> {
    let elf = ElfFile64::<Endianness>::parse(bytes).expect("stripped file must be ELF");
    elf.sections()
        .map(|s| s.name().unwrap_or("").to_string())
        .collect()
}

/// Write `lib.c` and `main.c` in `dir`.
fn write_sources(dir: &Path) {
    fs::write(dir.join("lib.c"), LIB_C).unwrap();
    fs::write(dir.join("main.c"), MAIN_C).unwrap();
}

#[test]
fn test_strip_pie_executable_still_runs() {
    // Deleting every REL/RELA section took .rela.dyn out of a PIE, and the
    // dynamic loader then aborted on the first relocation it read.
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    let exe = dir.path().join("pie");
    cc(
        dir.path(),
        &["-g", "-fPIE", "-pie", "-o", "pie", "main.c", "lib.c"],
    );
    strip_ok(&[], &exe);
    run_prints(&exe, "10\n");
    let names = section_names(&fs::read(&exe).unwrap());
    assert!(names.iter().any(|n| n == ".rela.dyn"), "{names:?}");
    assert!(!names.iter().any(|n| n == ".symtab" || n == ".strtab"));
    assert!(!names.iter().any(|n| n.starts_with(".debug")));
}

#[test]
fn test_strip_shared_library_still_loads() {
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    cc(
        dir.path(),
        &["-g", "-fPIC", "-shared", "-o", "libx.so", "lib.c"],
    );
    let rpath = format!("-Wl,-rpath,{}", dir.path().display());
    cc(dir.path(), &["-o", "main", "main.c", "libx.so", &rpath]);
    strip_ok(&[], &dir.path().join("libx.so"));
    run_prints(&dir.path().join("main"), "10\n");
}
