//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Every thread-local access model, assembled, linked and run.
//

//! Thread-local storage, end to end.
//!
//! The other TLS tests inspect assembly text, which is how three defects
//! survived: aarch64 Initial Exec was printed with x86-64's relocation names
//! (`:gottpoff:`), which no aarch64 assembler accepts; its sequence borrowed
//! X16 for the thread pointer while the global-store path passed X16 as the
//! destination, so every store to an `extern` thread-local wrote to twice the
//! thread pointer; and a floating-point thread-local read, on both targets,
//! went through a path that built its own global operand and read the
//! variable's initialization image instead of this thread's copy (an
//! `extern` one did not even link). Only running the code shows any of that.
//!
//! Each program covers every access width (1, 2, 4 and 8 bytes, `float`,
//! `double`, `long double`, an array element and a 40-byte struct), reads,
//! writes, address-of and struct copies in both directions. [`USER`] is Local
//! Exec for its own thread-locals and Initial Exec for [`DEF`]'s; [`LIB`] is
//! built position-independent into a shared library, which is the descriptor
//! model, and also reads a thread-local its executable defines.

use crate::common::{
    aarch64_cross_available, compile_and_run_two_units, create_c_file, cross_link_and_run, run_c17,
};
use plib::tmp::NamedTempFile;
use std::process::Command;

const USER: &str = r#"
struct S { long a, b, c, d, e; };
/* Defined here: Local Exec in an executable. */
_Thread_local char tc = 1;
_Thread_local short ts = 2;
_Thread_local int ti = 3;
_Thread_local long tl = 4;
_Thread_local double td = 5.5;
_Thread_local long tarr[4] = {6, 7, 8, 9};
_Thread_local struct S tstruct = {10, 11, 12, 13, 14};
_Thread_local float tf = 1.5f;
_Thread_local long double tld = 2.5L;
/* Defined in def.c: Initial Exec. */
extern _Thread_local char ec;
extern _Thread_local short es;
extern _Thread_local int ei;
extern _Thread_local long el;
extern _Thread_local double ed;
extern _Thread_local long earr[4];
extern _Thread_local struct S estruct;
extern _Thread_local float ef;
extern _Thread_local long double eld;

#define NI __attribute__((noinline))
NI long sum_local(void) { return tc + ts + ti + tl + (long)td + tarr[3] + tstruct.e; }
NI long sum_extern(void) { return ec + es + ei + el + (long)ed + earr[3] + estruct.e; }
NI void bump_all(void) {
    tc++; ts++; ti++; tl++; td += 1.0; tarr[1]++; tstruct.b++; tf += 1.0f; tld += 1.0L;
    ec++; es++; ei++; el++; ed += 1.0; earr[1]++; estruct.b++; ef += 1.0f; eld += 1.0L;
}
NI long *addr_local(void) { return &tarr[2]; }
NI long *addr_extern(void) { return &earr[2]; }
NI struct S copy_out_local(void) { return tstruct; }
NI struct S copy_out_extern(void) { return estruct; }
NI void copy_in(struct S v) { tstruct = v; estruct = v; }
NI void copy_across(void) { estruct = tstruct; }
NI void store_local(int v) { ti = v; tc = (char)v; }
NI void store_extern(int v) { ei = v; ec = (char)v; }

int main(void)
{
    if (sum_local() != 1 + 2 + 3 + 4 + 5 + 9 + 14) return 1;
    if (sum_extern() != 21 + 22 + 23 + 24 + 25 + 29 + 34) return 2;
    bump_all();
    if (tc != 2 || ts != 3 || ti != 4 || tl != 5 || td != 6.5 || tarr[1] != 8 || tstruct.b != 12) return 3;
    if (ec != 22 || es != 23 || ei != 24 || el != 25 || ed != 26.5 || earr[1] != 28 || estruct.b != 32) return 4;
    if (tf != 2.5f || tld != 3.5L || ef != 4.5f || eld != 5.5L) return 10;
    *addr_local() = 100; *addr_extern() = 200;
    if (tarr[2] != 100 || earr[2] != 200) return 5;
    struct S a = copy_out_local(), b = copy_out_extern();
    if (a.e != 14 || b.e != 34 || a.b != 12 || b.b != 32) return 6;
    struct S v = {41, 42, 43, 44, 45};
    copy_in(v);
    if (tstruct.a != 41 || estruct.e != 45) return 7;
    tstruct.c = 99;
    copy_across();
    if (estruct.c != 99) return 8;
    store_local(77); store_extern(88);
    if (ti != 77 || tc != 77 || ei != 88 || ec != 88) return 9;
    return 0;
}
"#;

const DEF: &str = r#"
struct S { long a, b, c, d, e; };
_Thread_local char ec = 21;
_Thread_local short es = 22;
_Thread_local int ei = 23;
_Thread_local long el = 24;
_Thread_local double ed = 25.5;
_Thread_local long earr[4] = {26, 27, 28, 29};
_Thread_local struct S estruct = {30, 31, 32, 33, 34};
_Thread_local float ef = 3.5f;
_Thread_local long double eld = 4.5L;
"#;

const LIB: &str = r#"
struct S { long a, b, c, d, e; };
_Thread_local char lc = 1;
_Thread_local int li = 2;
_Thread_local long larr[4] = {3, 4, 5, 6};
_Thread_local struct S ls = {7, 8, 9, 10, 11};
_Thread_local double ldb = 0.5;
extern _Thread_local long exe_tl;
long lib_sum(void) { return lc + li + larr[3] + ls.e + exe_tl + (long)(ldb * 2); }
void lib_bump(void) { lc++; li++; larr[1]++; ls.b++; exe_tl++; ldb += 1.0; }
long *lib_addr(void) { return &larr[2]; }
struct S lib_copy(void) { return ls; }
void lib_store(struct S v) { ls = v; }
"#;

const LIB_MAIN: &str = r#"
struct S { long a, b, c, d, e; };
_Thread_local long exe_tl = 100;
long lib_sum(void); void lib_bump(void); long *lib_addr(void);
struct S lib_copy(void); void lib_store(struct S);
int main(void)
{
    if (lib_sum() != 1 + 2 + 6 + 11 + 100 + 1) return 1;
    lib_bump();
    if (lib_sum() != 2 + 3 + 6 + 11 + 101 + 3 || exe_tl != 101) return 2;
    *lib_addr() = 50;
    if (*lib_addr() != 50) return 3;
    if (lib_copy().b != 9) return 4;
    struct S v = {1, 2, 3, 4, 5}; lib_store(v);
    if (lib_copy().e != 5) return 5;
    return 0;
}
"#;

/// c17's aarch64 assembly for a source, removed when dropped along with the
/// source it was compiled from.
struct Aarch64Asm {
    _src: NamedTempFile,
    path: String,
}

impl Aarch64Asm {
    fn new(name: &str, src: &str, opts: &[&str]) -> Self {
        let c = create_c_file(name, src);
        let path = c.path().with_extension("s").to_string_lossy().to_string();
        let c_path = c.path().to_string_lossy().to_string();
        let mut args = vec!["--target", "aarch64-unknown-linux-gnu", "-S", "-o", &path];
        args.extend_from_slice(opts);
        args.push(&c_path);
        let run = run_c17(&args);
        assert!(
            run.success,
            "c17 failed on {name} {opts:?}:\n{}",
            run.stderr
        );
        Self { _src: c, path }
    }
}

impl Drop for Aarch64Asm {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

#[test]
fn tls_models_x86_64_executable() {
    for opts in [&["-O0"][..], &["-O2"], &["-O2", "-fPIE"]] {
        let opts: Vec<String> = opts.iter().map(|s| s.to_string()).collect();
        assert_eq!(
            compile_and_run_two_units("tls_models", USER, DEF, &opts),
            0,
            "at {opts:?}"
        );
    }
}

#[test]
fn tls_models_aarch64_executable() {
    if !aarch64_cross_available() {
        eprintln!("SKIP tls_models_aarch64_executable: no aarch64 cross toolchain");
        return;
    }
    for opts in [&["-O0"][..], &["-O2"], &["-O2", "-fPIE"]] {
        let user = Aarch64Asm::new("tls_user", USER, opts);
        let def = create_c_file("tls_def", DEF);
        let code = cross_link_and_run("tls_models", &[&user.path, &def.path().to_string_lossy()]);
        assert_eq!(code, 0, "at {opts:?}");
    }
}

/// The descriptor model: c17 builds the library, gcc links it and the
/// executable dynamically, and qemu runs the result through the dynamic
/// loader.
#[test]
fn tls_models_aarch64_shared_library() {
    if !aarch64_cross_available() {
        eprintln!("SKIP tls_models_aarch64_shared_library: no aarch64 cross toolchain");
        return;
    }
    let dir = plib::tmp::Builder::new()
        .prefix("c17_tls_so_")
        .tempdir()
        .expect("tempdir");
    let d = dir.path();
    for opt in ["-O0", "-O2"] {
        let lib = Aarch64Asm::new("tls_lib", LIB, &[opt, "-fPIC"]);
        let so = d.join("libtlsc17.so");
        let built = Command::new("aarch64-linux-gnu-gcc")
            .args(["-shared", "-o"])
            .arg(&so)
            .arg(&lib.path)
            .output()
            .expect("cross gcc");
        assert!(
            built.status.success(),
            "linking the library failed at {opt}:\n{}",
            String::from_utf8_lossy(&built.stderr)
        );
        let main = create_c_file("tls_lib_main", LIB_MAIN);
        let exe = d.join("tls_main");
        let linked = Command::new("aarch64-linux-gnu-gcc")
            .arg(main.path())
            .arg("-L")
            .arg(d)
            .arg("-ltlsc17")
            .arg("-o")
            .arg(&exe)
            .output()
            .expect("cross gcc");
        assert!(
            linked.status.success(),
            "linking the executable failed at {opt}:\n{}",
            String::from_utf8_lossy(&linked.stderr)
        );
        let run = Command::new("qemu-aarch64-static")
            .env("QEMU_LD_PREFIX", "/usr/aarch64-linux-gnu")
            .arg("-E")
            .arg(format!("LD_LIBRARY_PATH={}", d.display()))
            .arg(&exe)
            .output()
            .expect("qemu");
        assert_eq!(
            run.status.code(),
            Some(0),
            "at {opt}: {}",
            String::from_utf8_lossy(&run.stderr)
        );
    }
}
