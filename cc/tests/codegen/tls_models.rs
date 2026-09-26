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

use crate::codegen::asm_probe::{asm_for_with, body_of};
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
/* A thread-local named by an inline-asm memory operand, while another's
   address is live. The operand used to reach the backend as a global, whose
   descriptor call -- invisible to the allocator -- overwrote the live
   address, so `li` was read back through a garbage pointer. */
long lib_asm(long x)
{
    long out;
    li = (int)x;
    __asm__("ldr %0, %1" : "=r"(out) : "m"(larr[2]));
    return out + li;
}
"#;

const LIB_MAIN: &str = r#"
struct S { long a, b, c, d, e; };
_Thread_local long exe_tl = 100;
long lib_sum(void); void lib_bump(void); long *lib_addr(void);
struct S lib_copy(void); void lib_store(struct S); long lib_asm(long);
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
    if (lib_asm(7) != 50 + 7) return 6;
    return 0;
}
"#;

/// Parameters live across a thread-local access that *calls* something: the
/// descriptor resolver on ELF shared objects, the TLV getter on Darwin. Both
/// take their argument and return their result in the first argument
/// register (x0 on aarch64), and the Darwin x86-64 getter also destroys %rdi.
/// aarch64 never moved an incoming argument out of a register such a sequence
/// overwrites, so `ti = a` stored the argument and then every later use of `a`
/// read the thread-local's *address*: `store_params(77, ...)` left `ti` at
/// 77 + (address), and a macOS thread worker's `k` became a pointer.
const PARAM_LIB: &str = r#"
_Thread_local int ti = 3;
_Thread_local char tc = 1;
_Thread_local long tl = 5;
void store_params(int a, int b, long c) { ti = a; tc = (char)b; tl = c; ti += a; }
int get_ti(void) { return ti; }
int get_tc(void) { return tc; }
long get_tl(void) { return tl; }
"#;

const PARAM_MAIN: &str = r#"
void store_params(int, int, long);
int get_ti(void); int get_tc(void); long get_tl(void);
int main(void)
{
    store_params(77, 66, 55);
    if (get_ti() != 154) return 1;
    if (get_tc() != 66) return 2;
    if (get_tl() != 55) return 3;
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

/// A host executable: Local Exec for its own thread-locals and Initial Exec for
/// another unit's on ELF hosts, the TLV getter on a macOS host.
#[test]
fn tls_models_host_executable() {
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
    for (lib_src, main_src) in [(LIB, LIB_MAIN), (PARAM_LIB, PARAM_MAIN)] {
        run_aarch64_shared_library(lib_src, main_src);
    }
}

fn run_aarch64_shared_library(lib_src: &str, main_src: &str) {
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
        let lib = Aarch64Asm::new("tls_lib", lib_src, &[opt, "-fPIC"]);
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
        let main = create_c_file("tls_lib_main", main_src);
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

/// The x86-64 descriptor model, native: c17 builds the library `-fPIC`, the
/// host compiler links it and the executable, and the result runs here.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn tls_models_x86_64_shared_library() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_tls_so_x86_")
        .tempdir()
        .expect("tempdir");
    let d = dir.path();
    // `LIB` carries aarch64 inline assembly, so only the parameter program.
    for (lib_src, main_src) in [(PARAM_LIB, PARAM_MAIN)] {
        for opt in ["-O0", "-O2"] {
            let c = create_c_file("tls_lib_x86", lib_src);
            let s = d.join("lib.s");
            let run = run_c17(&[
                opt,
                "-fPIC",
                "-S",
                "-o",
                &s.to_string_lossy(),
                &c.path().to_string_lossy(),
            ]);
            assert!(run.success, "c17 failed at {opt}:\n{}", run.stderr);
            let so = d.join("libtlsc17x.so");
            let built = Command::new("cc")
                .args(["-shared", "-o"])
                .arg(&so)
                .arg(&s)
                .output()
                .expect("cc");
            assert!(
                built.status.success(),
                "linking the library failed at {opt}:\n{}",
                String::from_utf8_lossy(&built.stderr)
            );
            let main = create_c_file("tls_lib_main_x86", main_src);
            let exe = d.join("tls_main_x86");
            let linked = Command::new("cc")
                .arg(main.path())
                .arg("-L")
                .arg(d)
                .arg("-ltlsc17x")
                .arg("-o")
                .arg(&exe)
                .output()
                .expect("cc");
            assert!(
                linked.status.success(),
                "linking the executable failed at {opt}:\n{}",
                String::from_utf8_lossy(&linked.stderr)
            );
            let ran = Command::new(&exe)
                .env("LD_LIBRARY_PATH", d)
                .output()
                .expect("run");
            assert_eq!(ran.status.code(), Some(0), "at {opt}");
        }
    }
}

/// An inline-asm memory operand naming an Initial Exec thread-local, when
/// every other scratch register is already spent.
///
/// The operand's address is computed before the template into a scratch
/// register, and X15 is the last one offered. The Initial Exec sequence uses
/// X15 as its thread-pointer temporary, so with the operand also in X15 it
/// became `mrs x15, tpidr_el0; add x15, x15, x15` -- twice the thread pointer
/// -- and the template loaded through a wild address. Such an operand is never
/// given X15 now; the statement offers X15 first to an operand that may take
/// it (here the constant `1`).
#[test]
fn tls_ie_asm_operand_with_the_scratch_registers_spent() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let user = r#"
extern __thread int t;
__attribute__((noinline)) int f(void)
{
    int out;
    __asm__ volatile("ldr %w0, %6\n\tadd %w0, %w0, %w1\n\tadd %w0, %w0, %w2\n\t"
                     "add %w0, %w0, %w3\n\tadd %w0, %w0, %w4\n\tadd %w0, %w0, %w5"
                     : "=&r"(out)
                     : "r"(1), "r"(2), "r"(3), "r"(4), "r"(5), "m"(t));
    return out;
}
int main(void) { return f() == 42 + 15 ? 0 : 1; }
"#;
    let def = create_c_file("tls_ie_asm_def", "__thread int t = 42;\n");
    for opt in ["-O0", "-O2"] {
        let asm = Aarch64Asm::new("tls_ie_asm", user, &[opt]);
        let code = cross_link_and_run("tls_ie_asm", &[&asm.path, &def.path().to_string_lossy()]);
        assert_eq!(code, 0, "at {opt}");
    }
}

// ---------------------------------------------------------------------------
// Mach-O thread-local variables
// ---------------------------------------------------------------------------
//
// Darwin has no static TLS model. The symbol a program names is a
// three-word descriptor in `__DATA,__thread_vars` -- `{ __tlv_bootstrap, 0,
// image }` -- and every access calls the getter it holds. c17 emitted
// thread-locals as plain data and reached them as plain globals (`@PAGE`,
// `@GOTPAGE`, `(%rip)`), so every thread shared one copy, and an `extern`
// one did not link. Nothing here can assemble or run Mach-O, so the shape is
// pinned against clang's layout below, and
// [`tls_threads_have_their_own_copies`] is the run-time proof on a macOS host.

const DARWIN_AARCH64: &str = "aarch64-apple-darwin";
const DARWIN_X86_64: &str = "x86_64-apple-darwin";

/// Every definition is an image plus a descriptor, with linkage on the
/// descriptor: initialized data, zero-fill, a static, a weak one, an
/// over-aligned one.
#[test]
fn tls_macho_definitions_are_descriptors() {
    let src = r#"
_Thread_local int tv = 5;
_Thread_local long tz;
static _Thread_local int ts = 3;
__attribute__((weak)) _Thread_local int tw = 9;
_Alignas(64) _Thread_local char ta[64];
int use(void) { return tv + (int)tz + ts + tw + ta[0]; }
"#;
    for triple in [DARWIN_AARCH64, DARWIN_X86_64] {
        let asm = asm_for_with("tls_macho_defs", triple, src, &["-O2"]);
        let descriptor = |name: &str| {
            format!(
                ".p2align 3\n_{name}:\n    .quad __tlv_bootstrap\n    .quad 0\n    .quad _{name}$tlv$init\n"
            )
        };
        for name in ["tv", "tz", "ts", "tw", "ta"] {
            assert!(
                asm.contains(&descriptor(name)),
                "{triple}: no TLV descriptor for {name}:\n{asm}"
            );
        }
        assert!(
            asm.contains(
                ".section __DATA,__thread_data,thread_local_regular\n.p2align 2\n_tv$tlv$init:\n    .long 5\n"
            ),
            "{triple}: initialized image:\n{asm}"
        );
        assert!(
            asm.contains(".tbss _tz$tlv$init, 8, 3\n"),
            "{triple}: zero-fill image:\n{asm}"
        );
        assert!(
            asm.contains(".tbss _ta$tlv$init, 64, 6\n"),
            "{triple}: over-aligned image:\n{asm}"
        );
        assert!(
            asm.contains(".section __DATA,__thread_vars,thread_local_variables\n.globl _tv\n"),
            "{triple}: an external descriptor is global:\n{asm}"
        );
        assert!(
            asm.contains(".globl _tw\n.weak_definition _tw\n"),
            "{triple}: a weak descriptor:\n{asm}"
        );
        assert!(
            !asm.contains(".globl _ts\n") && !asm.contains("$tlv$init\n.globl"),
            "{triple}: a static descriptor and every image stay local:\n{asm}"
        );
    }
}

/// Every access form -- each width, `long double`, a struct copied both
/// ways, an array element, address-of, an `extern` and a weak thread-local,
/// and an inline-asm memory operand -- goes through the TLV getter, and no
/// thread-local is ever reached as plain data.
const MACHO_FORMS: &str = r#"
struct big { long a[5]; };
_Thread_local char tc = 1;
_Thread_local short tsh = 2;
_Thread_local long double tld = 3.5L;
_Thread_local float tf;
_Thread_local struct big tb;
_Thread_local int tarr[16];
__attribute__((weak)) _Thread_local int tw = 9;
static __thread int tstat;
extern _Thread_local double tex;
struct big copy_out(void) { return tb; }
void copy_in(struct big *p) { tb = *p; }
int elem(int i) { return tarr[i] + tarr[3]; }
void elem_set(int i, int v) { tarr[i] = v; tarr[7] = v; }
long double getld(void) { return tld; }
void setld(long double v) { tld = v; }
float getf(void) { return tf; }
int narrow(void) { return tc + tsh + tstat + tw; }
double getex(void) { return tex; }
int *arr_addr(void) { return &tarr[5]; }
double keep(double a, double b) { double s = a * b; tc = 1; return s + a; }
long asm_mem(void) { long out; __asm__("" : "=r"(out) : "m"(tarr[2])); return out; }
"#;

#[test]
fn tls_macho_every_access_calls_the_getter() {
    let names = ["tc", "tsh", "tld", "tf", "tb", "tarr", "tw", "tstat", "tex"];
    for triple in [DARWIN_AARCH64, DARWIN_X86_64] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_macho_forms", triple, MACHO_FORMS, &[opt]);
            for name in names {
                let sequence = if triple == DARWIN_AARCH64 {
                    format!(
                        "    adrp x0, _{name}@TLVPPAGE\n    ldr x0, [x0, _{name}@TLVPPAGEOFF]\n    ldr x16, [x0]\n    blr x16\n"
                    )
                } else {
                    format!("    movq _{name}@TLVP(%rip), %rdi\n    call *(%rdi)\n")
                };
                assert!(
                    asm.contains(&sequence),
                    "{triple} {opt}: {name} is not reached through its descriptor:\n{asm}"
                );
                for plain in [
                    format!("_{name}@PAGE"),
                    format!("_{name}@GOTPAGE"),
                    format!("_{name}(%rip)"),
                    format!("_{name}@GOTPCREL"),
                    format!("_{name}+"),
                ] {
                    assert!(
                        !asm.contains(&plain),
                        "{triple} {opt}: {name} reached as plain data ({plain}):\n{asm}"
                    );
                }
            }
        }
    }
}

/// The x86-64 getter keeps no XMM register, so no floating-point value may
/// be live in one across `call *(%rdi)`: each must be reloaded or rewritten
/// after the call before it is read.
#[test]
fn tls_macho_x86_64_getter_destroys_xmm_registers() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("tls_macho_fp", DARWIN_X86_64, MACHO_FORMS, &[opt]);
        let body = body_of(&asm, "keep");
        let after = body
            .split("    call *(%rdi)\n")
            .nth(1)
            .unwrap_or_else(|| panic!("{opt}: no getter call in keep:\n{body}"));
        let mut written = std::collections::HashSet::new();
        for line in after.lines() {
            let line = line.trim();
            let Some((mnemonic, operands)) = line.split_once(' ') else {
                continue;
            };
            let dest = operands.rsplit(", ").next().unwrap_or("");
            let pure_move = mnemonic.starts_with("mov");
            for reg in operands.split(", ").filter(|op| op.starts_with("%xmm")) {
                let is_dest_only = pure_move && reg == dest;
                assert!(
                    is_dest_only || written.contains(reg),
                    "{opt}: {reg} read after the getter destroyed it:\n{body}"
                );
            }
            if dest.starts_with("%xmm") {
                written.insert(dest.to_string());
            }
        }
    }
}

/// No incoming argument may be read from a register the TLV getter destroys.
///
/// The getter takes its descriptor in, and returns the address in, x0 on
/// aarch64; on x86-64 it takes %rdi and returns %rax. So after the call an
/// `int` parameter can no longer be in w0, or in %edi/%rdi: reading one there
/// before rewriting it reads the thread-local's address or garbage. This is
/// exactly what the macOS CI runner hit (`store_local(77)` stored an address,
/// a thread worker's `k` became a pointer and faulted).
#[test]
fn tls_macho_arguments_survive_the_getter() {
    let src = r#"
_Thread_local int ti;
_Thread_local char tc;
_Thread_local long tl;
void st(int a, int b, long c) { ti = a; tc = (char)b; tl = c; ti += a; }
"#;
    // `clobbered` is what an int or long argument would be read as; `names`
    // is every spelling of the register, any of which rewrites it.
    for (triple, call, clobbered, names) in [
        (
            DARWIN_AARCH64,
            "    blr x16\n",
            &["w0"][..],
            &["w0", "x0"][..],
        ),
        (
            DARWIN_X86_64,
            "    call *(%rdi)\n",
            &["%edi", "%rdi"][..],
            &["%dil", "%di", "%edi", "%rdi"][..],
        ),
    ] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_macho_args", triple, src, &[opt]);
            let body = body_of(&asm, "st");
            for after in body.split(call).skip(1) {
                for line in after.lines() {
                    let line = line.trim();
                    let Some((mnemonic, operands)) = line.split_once(' ') else {
                        continue;
                    };
                    let ops: Vec<&str> = operands.split(", ").collect();
                    // x86-64 writes its last operand, and every instruction
                    // but a move also reads it. aarch64 writes its first --
                    // except a store, where every register operand is read.
                    let (dest, srcs): (&str, Vec<&str>) = if triple == DARWIN_X86_64 {
                        let dest = ops[ops.len() - 1];
                        let mut srcs = ops[..ops.len() - 1].to_vec();
                        if !mnemonic.starts_with("mov") {
                            srcs.push(dest);
                        }
                        (dest, srcs)
                    } else if mnemonic.starts_with("st") {
                        ("", ops.clone())
                    } else {
                        (ops[0], ops[1..].to_vec())
                    };
                    let reads_clobbered = srcs.iter().any(|op| clobbered.contains(op));
                    assert!(
                        !reads_clobbered,
                        "{triple} {opt}: an argument read from a register the getter \
                         destroyed ({line}):\n{body}"
                    );
                    if names.contains(&dest) {
                        break;
                    }
                }
            }
        }
    }
}

/// Each thread starts from the initial image, keeps its own copy of every
/// thread-local -- scalar, `double`, `long double`, struct, zero-filled,
/// static and `extern` -- while the others write theirs, and has its own
/// address for it. On Linux this passes already; on a macOS host it is what
/// proves the descriptors are right, since one shared copy fails it.
#[test]
fn tls_threads_have_their_own_copies() {
    let a = r#"
#include <pthread.h>

struct triple { long a, b, c; };
_Thread_local int tv = 5;
_Thread_local double td = 1.5;
_Thread_local struct triple tt = {1, 2, 3};
_Thread_local long double tld = 2.5L;
_Thread_local long tz;
static _Thread_local char tc = 7;
extern _Thread_local int te;
int *addr_te(void);

#define N 4
static int arrived;
static void *seen[N + 1];

/* Each thread must start from the initial image, write its own values, and
   still see exactly those after every other thread has written theirs: one
   shared copy fails the second check. */
static long check(long k)
{
    if (tv != 5 || td != 1.5 || tt.b != 2 || tld != 2.5L || tz != 0 || tc != 7 || te != 11)
        return 1;
    tv = (int)k; td = k * 0.5; tt.b = k; tld = k; tz = k * 3; tc = (char)k; te = (int)k * 2;
    seen[k] = &tv;
    __atomic_add_fetch(&arrived, 1, __ATOMIC_SEQ_CST);
    while (__atomic_load_n(&arrived, __ATOMIC_SEQ_CST) < N)
        ;
    if (tv != k || td != k * 0.5 || tt.b != k || tld != k || tz != k * 3 || tc != (char)k)
        return 2;
    if (te != k * 2 || addr_te() != &te)
        return 3;
    return 0;
}

static void *worker(void *arg) { return (void *)check((long)arg); }

int main(void)
{
    pthread_t t[N];
    for (long k = 0; k < N; k++)
        if (pthread_create(&t[k], 0, worker, (void *)(k + 1)))
            return 10;
    for (int k = 0; k < N; k++) {
        void *r;
        pthread_join(t[k], &r);
        if (r) return 20 + (int)(long)r;
    }
    /* The main thread's copies are untouched. */
    if (tv != 5 || tt.b != 2 || tc != 7 || te != 11) return 30;
    for (int i = 1; i <= N; i++)
        for (int j = i + 1; j <= N; j++)
            if (seen[i] == seen[j]) return 40;
    return 0;
}
"#;
    let b = r#"
_Thread_local int te = 11;
int *addr_te(void) { return &te; }
"#;
    for opt in ["-O0", "-O2"] {
        let opts = vec![opt.to_string()];
        assert_eq!(
            compile_and_run_two_units("tls_threads", a, b, &opts),
            0,
            "at {opt}"
        );
    }
}

/// FreeBSD is ELF and uses the same thread-pointer models as Linux. The
/// backends asked for Linux alone before treating a symbol as thread-local,
/// so on FreeBSD every thread-local was plain data -- one copy for all
/// threads.
#[test]
fn tls_freebsd_uses_the_elf_models() {
    let src = r#"
_Thread_local int tv = 5;
extern _Thread_local int te;
int get(void) { return tv + te; }
void set(int x) { tv = x; te = x; }
int *addr(void) { return &tv; }
"#;
    for (triple, local, ie) in [
        (
            "x86_64-unknown-freebsd",
            "%fs:tv@TPOFF",
            "te@GOTTPOFF(%rip)",
        ),
        ("aarch64-unknown-freebsd", ":tprel_hi12:tv", ":gottprel:te"),
    ] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_freebsd", triple, src, &[opt]);
            assert!(
                asm.contains(local) && asm.contains(ie),
                "{triple} {opt}: expected Local Exec and Initial Exec:\n{asm}"
            );
            assert!(
                !asm.contains("tv(%rip)")
                    && !asm.contains(":lo12:tv")
                    && !asm.contains("te@GOTPCREL"),
                "{triple} {opt}: a thread-local reached as plain data:\n{asm}"
            );
        }
    }
}

/// Code for a FreeBSD shared object never uses Local Exec.
///
/// Local Exec bakes in an offset from the main executable's thread pointer,
/// which is only right inside the executable. FreeBSD keeps the static models
/// for shared code (Linux takes the descriptor model there), and the choice of
/// Initial Exec ignored `-fPIC`, so a thread-local defined in the same file got
/// `%fs:t@TPOFF`, which `ld -shared` refuses outright on x86-64 and aarch64's
/// linker accepts and then resolves against the wrong block.
#[test]
fn tls_freebsd_shared_objects_use_initial_exec() {
    let src = r#"
_Thread_local int t;
extern _Thread_local int e;
int get(void) { return t + e; }
int *addr(void) { return &t; }
"#;
    for (triple, ie, le) in [
        ("x86_64-unknown-freebsd", "t@GOTTPOFF(%rip)", "@TPOFF"),
        ("aarch64-unknown-freebsd", ":gottprel:t", ":tprel_"),
    ] {
        for flags in [
            &["-O2", "-fPIC"][..],
            &["-O0", "-fPIC"],
            &["-O2", "--shared"],
        ] {
            let asm = asm_for_with("tls_freebsd_so", triple, src, flags);
            assert!(
                asm.contains(ie) && !asm.contains(le),
                "{triple} {flags:?}: expected Initial Exec, no Local Exec:\n{asm}"
            );
        }
    }
}

/// The x86-64 half of the above, linked: `ld -shared` accepts it. FreeBSD
/// and Linux share the ELF relocations, so the host linker is the check.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn tls_freebsd_shared_object_links() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_tls_freebsd_so_")
        .tempdir()
        .expect("tempdir");
    let d = dir.path();
    let c = create_c_file(
        "tls_freebsd_so",
        "_Thread_local int t;\nint get(void) { return t; }\n",
    );
    let s = d.join("t.s");
    let o = d.join("t.o");
    let so = d.join("libt.so");
    let run = run_c17(&[
        "--target",
        "x86_64-unknown-freebsd",
        "-O2",
        "-fPIC",
        "-S",
        "-o",
        &s.to_string_lossy(),
        &c.path().to_string_lossy(),
    ]);
    assert!(run.success, "{}", run.stderr);
    let assembled = Command::new("as")
        .arg(&s)
        .arg("-o")
        .arg(&o)
        .output()
        .expect("as");
    assert!(assembled.status.success());
    let linked = Command::new("ld")
        .arg("-shared")
        .arg(&o)
        .arg("-o")
        .arg(&so)
        .output()
        .expect("ld");
    assert!(
        linked.status.success(),
        "{}",
        String::from_utf8_lossy(&linked.stderr)
    );
}
