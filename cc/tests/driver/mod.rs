//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// POSIX conformance tests for the c17 driver's operand handling.
//
// Every other suite compiles exactly one source through `compile_and_run`,
// which is why #U1 (multi-translation-unit compilation) went unnoticed. These
// drive the binary with a raw argument vector instead.
//

use crate::common::run_c17;
use std::path::{Path, PathBuf};
use std::process::Command;

/// A scratch directory that removes itself.
struct WorkDir(tempfile::TempDir);

impl WorkDir {
    fn new(name: &str) -> Self {
        WorkDir(
            tempfile::Builder::new()
                .prefix(&format!("c17_driver_{}_", name))
                .tempdir()
                .expect("failed to create work dir"),
        )
    }

    fn path(&self) -> &Path {
        self.0.path()
    }

    /// Write `content` to `name` inside the work dir and return its path.
    fn write(&self, name: &str, content: &str) -> PathBuf {
        let p = self.path().join(name);
        std::fs::write(&p, content).expect("failed to write source");
        p
    }

    fn join(&self, name: &str) -> PathBuf {
        self.path().join(name)
    }
}

fn s(p: &Path) -> String {
    p.to_string_lossy().into_owned()
}

/// Run a built executable and return its exit code.
fn run_exe(p: &Path) -> i32 {
    Command::new(p)
        .output()
        .expect("failed to run built executable")
        .status
        .code()
        .unwrap_or(-1)
}

const CALLER: &str = "int helper(void);\nint main(void){return helper();}\n";
const HELPER_7: &str = "int helper(void){return 7;}\n";

// ============================================================================
// #U1 — operands are combined into a single link
// ============================================================================

/// Two `.c` operands must produce one executable, not two isolated links.
#[test]
fn driver_links_multiple_sources() {
    let w = WorkDir::new("multisrc");
    let a = w.write("m1.c", CALLER);
    let b = w.write("m2.c", HELPER_7);
    let exe = w.join("mA");

    let r = run_c17(&[&s(&a), &s(&b), "-o", &s(&exe)]);
    assert!(
        r.success,
        "c17 m1.c m2.c -o mA failed: {}{}",
        r.stdout, r.stderr
    );
    assert!(exe.exists(), "no executable produced");
    assert_eq!(run_exe(&exe), 7, "linked program returned the wrong value");
}

/// Spec EXAMPLE 1: `c17 foo.c bar.o`.
#[test]
fn driver_links_source_with_object_operand() {
    let w = WorkDir::new("srcobj");
    let a = w.write("m1.c", CALLER);
    let b = w.write("m2.c", HELPER_7);
    let obj = w.join("m2.o");
    let exe = w.join("mB");

    let r = run_c17(&["-c", &s(&b), "-o", &s(&obj)]);
    assert!(r.success, "failed to build m2.o: {}", r.stderr);

    let r = run_c17(&[&s(&a), &s(&obj), "-o", &s(&exe)]);
    assert!(r.success, "c17 m1.c m2.o -o mB failed: {}", r.stderr);
    assert_eq!(run_exe(&exe), 7);
}

/// An object operand may precede the source operand.
#[test]
fn driver_object_operand_may_come_first() {
    let w = WorkDir::new("objfirst");
    let a = w.write("m1.c", CALLER);
    let b = w.write("m2.c", HELPER_7);
    let obj = w.join("m2.o");
    let exe = w.join("mC");

    assert!(run_c17(&["-c", &s(&b), "-o", &s(&obj)]).success);

    let r = run_c17(&[&s(&obj), &s(&a), "-o", &s(&exe)]);
    assert!(r.success, "c17 m2.o m1.c -o mC failed: {}", r.stderr);
    assert_eq!(run_exe(&exe), 7);
}

/// Objects alone, with no `-o`, link to `a.out` like any other invocation.
/// This previously required `-o` and silently did nothing without it.
#[test]
fn driver_links_objects_without_dash_o() {
    let w = WorkDir::new("aout");
    let src = w.write("solo.c", "int main(void){return 3;}\n");
    let obj = w.join("solo.o");
    assert!(run_c17(&["-c", &s(&src), "-o", &s(&obj)]).success);

    // `a.out` lands in the current directory, so run from the work dir.
    let out = Command::new(env!("CARGO_BIN_EXE_c17"))
        .arg(&obj)
        .current_dir(w.path())
        .output()
        .expect("failed to run c17");
    assert!(
        out.status.success(),
        "c17 solo.o failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let aout = w.join("a.out");
    assert!(aout.exists(), "no a.out produced");
    assert_eq!(run_exe(&aout), 3);
}

// ============================================================================
// #U2 — a failing operand must not abort the remaining ones
// ============================================================================

/// CONSEQUENCES OF ERRORS (88185-88187): diagnose, keep compiling the other
/// operands, do not link, exit non-zero.
#[test]
fn driver_continues_past_a_failing_operand() {
    let w = WorkDir::new("continue");
    let bad = w.write("bad.c", "int bad(void){ return undefined_thing; }\n");
    let good = w.write("good.c", "int good(void){ return 1; }\n");

    let out = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-c"])
        .arg(&bad)
        .arg(&good)
        .current_dir(w.path())
        .output()
        .expect("failed to run c17");

    assert!(
        !out.status.success(),
        "a compile error must yield a non-zero exit status"
    );
    assert!(
        w.join("good.o").exists(),
        "good.c must still be compiled after bad.c fails; stderr was:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !w.join("bad.o").exists(),
        "bad.c must not produce an object file"
    );
}

/// The error state is global; make sure it does not leak from one operand to
/// the next and fail a file that is actually fine.
#[test]
fn driver_error_state_does_not_leak_between_operands() {
    let w = WorkDir::new("noleak");
    let a = w.write("ok1.c", "int a(void){return 1;}\n");
    let b = w.write("ok2.c", "int b(void){return 2;}\n");

    let out = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-c"])
        .arg(&a)
        .arg(&b)
        .current_dir(w.path())
        .output()
        .expect("failed to run c17");

    assert!(
        out.status.success(),
        "two clean sources must compile: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(w.join("ok1.o").exists() && w.join("ok2.o").exists());
}

// ============================================================================
// #U10 — `-c -o` with several sources
// ============================================================================

/// The spec leaves this unspecified (88338-88343), so the behavior is not a
/// defect — but it must not be silent.
#[test]
fn driver_warns_on_dash_c_dash_o_with_multiple_sources() {
    let w = WorkDir::new("cowarn");
    let a = w.write("w1.c", "int a(void){return 1;}\n");
    let b = w.write("w2.c", "int b(void){return 2;}\n");
    let obj = w.join("combined.o");

    let r = run_c17(&["-c", &s(&a), &s(&b), "-o", &s(&obj)]);
    assert!(r.success, "compilation should still succeed: {}", r.stderr);
    assert!(
        r.stderr.contains("warning") && r.stderr.contains("-o"),
        "expected a warning about -o with multiple sources, got:\n{}",
        r.stderr
    );
}

// ============================================================================
// Regressions guarding the restructure
// ============================================================================

/// `-c` without `-o` still writes $(basename operand .c).o per source.
#[test]
fn driver_dash_c_names_one_object_per_source() {
    let w = WorkDir::new("percomp");
    let a = w.write("p1.c", "int a(void){return 1;}\n");
    let b = w.write("p2.c", "int b(void){return 2;}\n");

    let out = Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(["-c"])
        .arg(&a)
        .arg(&b)
        .current_dir(w.path())
        .output()
        .expect("failed to run c17");
    assert!(out.status.success());
    assert!(w.join("p1.o").exists(), "p1.o missing");
    assert!(w.join("p2.o").exists(), "p2.o missing");
}

/// Early-exit modes produce no object and attempt no link.
#[test]
fn driver_early_exit_modes_do_not_link() {
    let w = WorkDir::new("earlyexit");
    // No main, so a link would fail — proving none is attempted.
    let src = w.write("frag.c", "int frag(void){return 1;}\n");

    let r = run_c17(&["-E", &s(&src)]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert!(r.stdout.contains("frag"), "-E produced no output");

    let asm = w.join("frag.s");
    let r = run_c17(&["-S", &s(&src), "-o", &s(&asm)]);
    assert!(r.success, "-S failed: {}", r.stderr);
    assert!(asm.exists(), "-S produced no assembly");

    assert!(
        !w.join("a.out").exists(),
        "an early-exit mode must not link"
    );
}

/// `-E` used to accept `-o` and then discard it: the preprocessed text went to
/// stdout, no file appeared, and the exit status was 0. POSIX leaves the
/// combination unspecified (88941-88942), but silently ignoring an option is
/// not one of the available readings, and every build system running
/// `cc -E -o foo.i foo.c` assumes the gcc/clang behavior.
#[test]
fn driver_preprocess_honors_dash_o() {
    let w = WorkDir::new("eout");
    let src = w.write("pp.c", "#define N 7\nint frag(void){return N;}\n");
    let out = w.join("pp.i");

    let r = run_c17(&["-E", &s(&src), "-o", &s(&out)]);
    assert!(r.success, "-E -o failed: {}{}", r.stdout, r.stderr);
    assert!(out.exists(), "-E -o produced no file");
    assert!(
        r.stdout.is_empty(),
        "-E -o must not also write to stdout:\n{}",
        r.stdout
    );

    let body = std::fs::read_to_string(&out).unwrap();
    assert!(body.contains("frag"), "no preprocessed text in the file");
    assert!(body.contains("7"), "the macro was not expanded");
    // STDOUT 88032-88038: the mandated line marker must survive the rerouting.
    assert!(
        body.contains("# 1 \""),
        "no line marker in the file:\n{}",
        body
    );
}

/// `-o -` is stdout, matching the spelling `-S` already used.
#[test]
fn driver_preprocess_dash_o_dash_is_stdout() {
    let w = WorkDir::new("edash");
    let src = w.write("pp.c", "int frag(void){return 1;}\n");

    let r = run_c17(&["-E", &s(&src), "-o", "-"]);
    assert!(r.success, "-E -o - failed: {}", r.stderr);
    assert!(r.stdout.contains("frag"), "-E -o - produced no output");
    assert!(
        !w.join("-").exists(),
        "-o - must not create a file named '-'"
    );
}

/// Several sources share one `-E` sink, so they concatenate rather than each
/// truncating the last — the same thing they already did on stdout.
#[test]
fn driver_preprocess_dash_o_collects_every_source() {
    let w = WorkDir::new("emulti");
    let a = w.write("p1.c", "int one(void){return 1;}\n");
    let b = w.write("p2.c", "int two(void){return 2;}\n");
    let out = w.join("both.i");

    let r = run_c17(&["-E", &s(&a), &s(&b), "-o", &s(&out)]);
    assert!(r.success, "-E of two sources failed: {}", r.stderr);
    assert!(
        r.stderr.contains("warning") && r.stderr.contains("-o"),
        "expected a warning about -o with several sources, got:\n{}",
        r.stderr
    );

    let body = std::fs::read_to_string(&out).unwrap();
    assert!(
        body.contains("one") && body.contains("two"),
        "one source overwrote the other:\n{}",
        body
    );
}

/// 87883-87885: with `-E`, "no compilation shall be performed". An assembler
/// operand was dispatched to `as` regardless of the mode, so `c17 -E foo.s`
/// assembled it and left a foo.o behind.
#[test]
fn driver_preprocess_does_not_assemble() {
    let w = WorkDir::new("easm");
    let asm = w.write("bare.s", ".text\n.globl bare\nbare: ret\n");

    let r = run_c17(&["-E", &s(&asm)]);
    assert!(r.success, "-E of an .s operand failed: {}", r.stderr);
    assert!(
        !w.join("bare.o").exists(),
        "-E must not assemble an .s operand"
    );
}

/// An `__asm__` label names a symbol the assembler and linker will see, so the
/// bytes have to be the source's. The label is a string literal, whose payload
/// holds one `char` per source byte; reading it as if it were already Rust
/// text turned `caf\u{e9}` into `caf\u{c3}\u{a9}`, and the reference no longer
/// matched a symbol of that name.
///
/// Proven by resolving against a symbol defined in assembly, so a mangled
/// label fails the link rather than agreeing with itself.
///
/// Linux-only: the fixture spells ELF directives, and Mach-O's symbol naming
/// for asm labels is not something this can check from here.
#[cfg(target_os = "linux")]
#[test]
fn driver_asm_label_keeps_source_bytes() {
    let w = WorkDir::new("asmlabel");
    let asm = w.write(
        "sym.S",
        "\t.data\n\
         \t.globl café\n\
         café:\n\
         \t.long 7\n",
    );
    let src = w.write(
        "use.c",
        "extern int v __asm__(\"café\");\nint main(void){ return v - 7; }\n",
    );
    let exe = w.join("prog");

    let r = run_c17(&[&s(&asm), &s(&src), "-o", &s(&exe)]);
    assert!(
        r.success,
        "link with a UTF-8 asm label failed:\n{}",
        r.stderr
    );
    assert_eq!(run_exe(&exe), 0, "the label did not resolve to the symbol");
}

/// `-fno-inline` turns general inlining off without turning optimization off,
/// and `__attribute__((always_inline))` still fires -- GCC's behaviour, and
/// what glibc's `__fortify_function` depends on.
///
/// The flag used to be swallowed by the `-f*` catch-all and do nothing at all.
#[test]
fn driver_fno_inline_stops_general_inlining_only() {
    let w = WorkDir::new("noinline");
    let src = w.write(
        "t.c",
        "static int helper(int x) { return x + 1; }\n\
         __attribute__((always_inline)) static inline int forced(int x) { return x + 2; }\n\
         int main(void) { return helper(1) + forced(2); }\n",
    );

    // (flags, helper is called, forced is called)
    for (flags, helper_called) in [
        (vec!["-O2"], false),
        (vec!["-O2", "-fno-inline"], true),
        // Last one wins, as in GCC.
        (vec!["-O2", "-fno-inline", "-finline"], false),
        // A *different* flag: it governs functions not declared `inline`,
        // and must not be mistaken for -fno-inline.
        (vec!["-O2", "-fno-inline-functions"], false),
        (vec!["-O0"], true),
    ] {
        let mut argv = flags.clone();
        argv.push("-S");
        let src_s = s(&src);
        let out = w.join("t.s");
        let out_s = s(&out);
        argv.extend_from_slice(&[&src_s, "-o", &out_s]);
        let r = run_c17(&argv);
        assert!(r.success, "{flags:?} failed:\n{}", r.stderr);

        let asm = std::fs::read_to_string(&out).unwrap();
        assert_eq!(
            asm.contains("helper"),
            helper_called,
            "{flags:?}: helper should {} be called\n{asm}",
            if helper_called { "" } else { "not" }
        );
        // always_inline is honoured whatever the flags say.
        assert!(
            !asm.contains("call\tforced") && !asm.contains("bl\tforced"),
            "{flags:?}: always_inline must still fire\n{asm}"
        );
    }
}

/// Every `-O` spelling GCC and Clang accept must be answered, not crashed on.
///
/// `is_valid_opt_level` claimed `s`, `z`, `fast` and `g` and forwarded them to
/// a `u32` argument, so `-Os` died with clap's "invalid value 's' for '-O
/// [<level>]': invalid digit found in string" -- a message about an
/// implementation detail, for a flag every distro build system passes.
#[test]
fn driver_accepts_the_optimization_levels_it_claims() {
    let w = WorkDir::new("optlevel");
    let src = w.write("t.c", "int main(void){ return 0; }\n");

    for flag in ["-O0", "-O", "-O1", "-O2", "-O3", "-Og", "-Os"] {
        let r = run_c17(&[flag, "-c", &s(&src), "-o", &s(&w.join("t.o"))]);
        assert!(r.success, "{flag} should be accepted:\n{}", r.stderr);
    }

    // Turned down by name, so the message says which flag and why.
    for (flag, needle) in [("-Ofast", "-Ofast"), ("-Oz", "-Oz")] {
        let r = run_c17(&[flag, "-c", &s(&src), "-o", &s(&w.join("t.o"))]);
        assert!(!r.success, "{flag} should be refused");
        assert!(
            r.stderr.contains(needle),
            "{flag}: the message should name the flag, got:\n{}",
            r.stderr
        );
    }

    // A level that is not a level at all.
    assert!(!run_c17(&["-O9", "-c", &s(&src), "-o", &s(&w.join("t.o"))]).success);
}

/// POSIX allows the level as a separate operand, and GCC lets the last `-O`
/// win. Neither may be disturbed by the spellings above.
#[test]
fn driver_optimization_level_separate_and_last_wins() {
    let w = WorkDir::new("optlast");
    let src = w.write("t.c", "int main(void){ return 0; }\n");
    let obj = w.join("t.o");

    assert!(run_c17(&["-O", "2", "-c", &s(&src), "-o", &s(&obj)]).success);
    assert!(run_c17(&["-O", "s", "-c", &s(&src), "-o", &s(&obj)]).success);
    assert!(run_c17(&["-O2", "-O0", "-c", &s(&src), "-o", &s(&obj)]).success);
    assert!(run_c17(&["-O0", "-Os", "-c", &s(&src), "-o", &s(&obj)]).success);

    // `-O` with a following operand that is not a level leaves the operand
    // alone -- it is the source file.
    let r = run_c17(&["-O", "-c", &s(&src), "-o", &s(&obj)]);
    assert!(r.success, "bare -O before other flags:\n{}", r.stderr);
}

/// An apostrophe in an assembly comment is prose, not the start of a literal.
/// Lexing it as C did swallowed the rest of the line, so a `.S` file whose
/// comment said "don't" either assembled to the wrong bytes or made `as` fail
/// with "junk at end of line". `#` there introduces a comment too, and warning
/// about each one as an unknown directive buried real diagnostics.
///
/// Only directives, so the fixture is neither arch- nor object-format-specific.
#[test]
fn driver_asm_apostrophe_in_comment() {
    let w = WorkDir::new("asmquote");
    let asm = w.write(
        "quoted.S",
        "# don't panic, this is a comment\n\
         /* it's a block comment */\n\
         \t.data\n\
         \t.byte 7 // that's seven\n",
    );
    let obj = w.join("quoted.o");

    let r = run_c17(&["-c", &s(&asm), "-o", &s(&obj)]);
    assert!(r.success, "assembling quoted.S failed:\n{}", r.stderr);
    assert!(obj.exists(), "no object produced for quoted.S");
    assert!(
        !r.stderr.contains("missing terminating"),
        "an apostrophe in a comment opened a literal:\n{}",
        r.stderr
    );
    assert!(
        !r.stderr.contains("unknown preprocessor directive"),
        "a `#` comment was reported as a directive:\n{}",
        r.stderr
    );
}

/// `-o` belongs to the compile or link output in every other mode. `-E` is the
/// only one that may open it, and opening it eagerly would truncate the
/// executable a normal compile is about to write.
#[test]
fn driver_dash_o_is_untouched_without_dash_e() {
    let w = WorkDir::new("noeout");
    let a = w.write("m1.c", CALLER);
    let b = w.write("m2.c", HELPER_7);
    let exe = w.join("prog");

    let r = run_c17(&[&s(&a), &s(&b), "-o", &s(&exe)]);
    assert!(r.success, "link failed: {}{}", r.stdout, r.stderr);
    assert_eq!(run_exe(&exe), 7, "the executable was clobbered");
}

// ============================================================================
// #U3 — -L/-l order relative to the operands is significant
// ============================================================================

/// Build two archives that define the same symbol with different values, so
/// the link order is observable in the program's exit status.
fn build_rival_archives(w: &WorkDir) -> PathBuf {
    let libdir = w.join("libdir");
    std::fs::create_dir_all(&libdir).unwrap();

    for (name, value) in [("Q", 1), ("P", 2)] {
        let src = w.write(
            &format!("{}.c", name),
            &format!("int which(void){{return {};}}\n", value),
        );
        let obj = w.join(&format!("{}.o", name));
        assert!(run_c17(&["-c", &s(&src), "-o", &s(&obj)]).success);
        let ar = Command::new("ar")
            .arg("rcs")
            .arg(libdir.join(format!("lib{}.a", name)))
            .arg(&obj)
            .status()
            .expect("failed to run ar");
        assert!(ar.success(), "ar failed for lib{}", name);
    }
    libdir
}

/// "A library shall be searched when its name is encountered" — so the first
/// `-l` naming a definition wins, and swapping the two changes the result.
#[test]
fn driver_library_order_is_significant() {
    let w = WorkDir::new("liborder");
    let libdir = build_rival_archives(&w);
    let user = w.write(
        "usr.c",
        "int which(void);\nint main(void){return which();}\n",
    );
    let ldir = format!("-L{}", libdir.to_string_lossy());

    let exe1 = w.join("ord1");
    let r = run_c17(&[&s(&user), &ldir, "-lQ", "-lP", "-o", &s(&exe1)]);
    assert!(r.success, "-lQ -lP link failed: {}", r.stderr);
    assert_eq!(run_exe(&exe1), 1, "-lQ came first, so libQ must win");

    let exe2 = w.join("ord2");
    let r = run_c17(&[&s(&user), &ldir, "-lP", "-lQ", "-o", &s(&exe2)]);
    assert!(r.success, "-lP -lQ link failed: {}", r.stderr);
    assert_eq!(run_exe(&exe2), 2, "-lP came first, so libP must win");
}

/// An archive named *before* the object that references it is searched too
/// early to satisfy that reference. This is the observable consequence of
/// honoring position, and it fails only if ordering is really preserved.
///
/// Linux only. The negative half of this pair depends on the *linker's*
/// one-pass archive semantics, which is a GNU ld property, not a POSIX
/// guarantee: Apple's linker resolves across every archive it is given
/// regardless of order, so naming a library early still satisfies a later
/// reference. `driver_library_order_is_significant` covers the positive
/// direction — that c17 preserves the order it was given — everywhere.
#[cfg(target_os = "linux")]
#[test]
fn driver_library_named_before_its_user_does_not_resolve() {
    let w = WorkDir::new("libearly");
    let libdir = build_rival_archives(&w);
    let user = w.write(
        "usr.c",
        "int which(void);\nint main(void){return which();}\n",
    );
    let ldir = format!("-L{}", libdir.to_string_lossy());
    let exe = w.join("early");

    let r = run_c17(&[&ldir, "-lQ", &s(&user), "-o", &s(&exe)]);
    assert!(
        !r.success,
        "a library searched before its user must leave the reference unresolved"
    );
}

// ============================================================================
// #C50 — the seven standard libraries shall be found
// ============================================================================

/// 88089-88093: `c`, `l`, `m`, `pthread`, `rt`, `xnet` and `y` *shall be
/// found* when named as a `-l` option-argument, and — except for the shared C
/// library — "need not exist as regular files".
///
/// `xnet` and `y` exist on no glibc system and no macOS; their interfaces are
/// in libc. c17 used to forward the name to the host linker, which answered
/// `cannot find -lxnet` and failed the link.
#[test]
fn driver_finds_every_standard_library() {
    let w = WorkDir::new("stdlibs");
    let src = w.write("t.c", "int main(void){return 0;}\n");

    for lib in ["c", "l", "m", "pthread", "rt", "xnet", "y"] {
        let exe = w.join(&format!("std_{}", lib));
        let r = run_c17(&[&s(&src), "-l", lib, "-o", &s(&exe)]);
        assert!(
            r.success,
            "-l {} failed to link: {}{}",
            lib, r.stdout, r.stderr
        );
        assert!(exe.exists(), "-l {} produced no executable", lib);
        assert_eq!(run_exe(&exe), 0);
    }
}

/// Dropping is confined to the seven names. A library the user named and the
/// host does not have must still be a link error, or a typo becomes silence.
#[test]
fn driver_still_fails_on_a_missing_ordinary_library() {
    let w = WorkDir::new("nolib");
    let src = w.write("t.c", "int main(void){return 0;}\n");
    let exe = w.join("t.out");

    let r = run_c17(&[&s(&src), "-l", "c17nosuchlibrary", "-o", &s(&exe)]);
    assert!(!r.success, "a missing ordinary library must fail the link");
}

/// A standard name the host *does* provide is forwarded, not dropped — and one
/// the user supplies under `-L` counts as providing it. Here `libm.a` in the
/// scratch directory defines the symbol `main` calls, so the program can only
/// link, and only return 42, if the `-l m` really reached the linker.
#[test]
fn driver_prefers_a_supplied_standard_library() {
    let w = WorkDir::new("ownlibm");
    let libdir = w.join("libdir");
    std::fs::create_dir_all(&libdir).unwrap();

    let src = w.write("m_impl.c", "int c17_probe(void){return 42;}\n");
    let obj = w.join("m_impl.o");
    assert!(run_c17(&["-c", &s(&src), "-o", &s(&obj)]).success);
    let ar = Command::new("ar")
        .arg("rcs")
        .arg(libdir.join("libm.a"))
        .arg(&obj)
        .status()
        .expect("failed to run ar");
    assert!(ar.success(), "ar failed for libm.a");

    let user = w.write(
        "u.c",
        "int c17_probe(void);\nint main(void){return c17_probe();}\n",
    );
    let exe = w.join("ownm");
    let ldir = format!("-L{}", libdir.to_string_lossy());
    let r = run_c17(&[&s(&user), &ldir, "-l", "m", "-o", &s(&exe)]);
    assert!(r.success, "-L dir -l m failed: {}{}", r.stdout, r.stderr);
    assert_eq!(
        run_exe(&exe),
        42,
        "the -L directory's libm.a was not searched"
    );
}

// ============================================================================
// #U4 — the four previously missing mandated options
// ============================================================================

#[test]
fn driver_accepts_mandated_options() {
    let w = WorkDir::new("u4opts");
    let src = w.write("t.c", "int main(void){return 0;}\n");

    for opts in [
        vec!["-B", "dynamic"],
        vec!["-B", "static"],
        vec!["-R", "/tmp"],
        vec!["-s"],
    ] {
        let exe = w.join(&format!("t_{}", opts.join("_").replace('/', "_")));
        let mut argv: Vec<&str> = opts.clone();
        let src_s = s(&src);
        let exe_s = s(&exe);
        argv.extend(["-o", &exe_s, &src_s]);
        let r = run_c17(&argv);
        assert!(r.success, "{:?} rejected: {}", opts, r.stderr);
        assert!(exe.exists(), "{:?} produced no executable", opts);
        assert_eq!(run_exe(&exe), 0);
    }
}

/// `-B` takes only the two modes the spec names.
#[test]
fn driver_rejects_unknown_binding_mode() {
    let w = WorkDir::new("badbind");
    let src = w.write("t.c", "int main(void){return 0;}\n");
    let exe = w.join("t.out");

    let r = run_c17(&["-B", "bogus", &s(&src), "-o", &s(&exe)]);
    assert!(!r.success, "-B bogus must be rejected");
    assert!(
        r.stderr.contains("-B"),
        "expected a diagnostic naming -B, got:\n{}",
        r.stderr
    );
}

/// `-s` strips the symbol table.
#[test]
fn driver_dash_s_strips_symbols() {
    let w = WorkDir::new("strip");
    let src = w.write(
        "t.c",
        "int helper_symbol(void){return 1;}\nint main(void){return helper_symbol()-1;}\n",
    );
    let plain = w.join("plain");
    let stripped = w.join("stripped");

    assert!(run_c17(&[&s(&src), "-o", &s(&plain)]).success);
    assert!(run_c17(&["-s", &s(&src), "-o", &s(&stripped)]).success);

    let plain_len = std::fs::metadata(&plain).unwrap().len();
    let stripped_len = std::fs::metadata(&stripped).unwrap().len();
    assert!(
        stripped_len < plain_len,
        "-s should shrink the binary: {} vs {}",
        stripped_len,
        plain_len
    );
    assert_eq!(run_exe(&stripped), 0, "stripped binary must still run");
}

// ============================================================================
// #U6 — TMPDIR and scratch-file hygiene
// ============================================================================

/// Intermediates go under `TMPDIR` (XSI, 88020-88022) and leave nothing behind.
#[test]
fn driver_honors_tmpdir_and_cleans_up() {
    let w = WorkDir::new("tmpdir");
    let tmp = w.join("mytmp");
    std::fs::create_dir_all(&tmp).unwrap();
    let src = w.write("t.c", "int main(void){return 0;}\n");
    let exe = w.join("t.out");

    let out = Command::new(env!("CARGO_BIN_EXE_c17"))
        .env("TMPDIR", &tmp)
        .arg(&src)
        .args(["-o"])
        .arg(&exe)
        .output()
        .expect("failed to run c17");
    assert!(
        out.status.success(),
        "compile under TMPDIR failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(exe.exists());

    let leftovers: Vec<_> = std::fs::read_dir(&tmp).unwrap().collect();
    assert!(
        leftovers.is_empty(),
        "c17 left {} entries in TMPDIR",
        leftovers.len()
    );
}

/// Compiling several sources in one process must not let them collide over a
/// shared scratch filename.
#[test]
fn driver_multiple_sources_do_not_share_temp_files() {
    let w = WorkDir::new("tempcollide");
    let a = w.write("t1.c", "int one(void){return 11;}\n");
    let b = w.write("t2.c", "int two(void){return 22;}\n");
    let c = w.write(
        "t3.c",
        "int one(void); int two(void);\nint main(void){return one()+two()-33;}\n",
    );
    let exe = w.join("tri");

    let r = run_c17(&[&s(&a), &s(&b), &s(&c), "-o", &s(&exe)]);
    assert!(r.success, "three-source link failed: {}", r.stderr);
    assert_eq!(run_exe(&exe), 0);
}

/// Operands are compiled into one shared scratch directory now that the link
/// happens once. Naming each object after the source's *stem* means
/// `a/util.c` and `b/util.c` both become `util.o`: the second overwrites the
/// first, and the link line then names that one object twice.
#[test]
fn driver_distinguishes_operands_with_the_same_basename() {
    let w = WorkDir::new("samestem");
    std::fs::create_dir_all(w.join("a")).unwrap();
    std::fs::create_dir_all(w.join("b")).unwrap();
    let main = w.write(
        "m.c",
        "int fa(void);int fb(void);int main(void){return fa()+fb();}\n",
    );
    let a = w.write("a/util.c", "int fa(void){return 3;}\n");
    let b = w.write("b/util.c", "int fb(void){return 4;}\n");
    let exe = w.join("same");

    let r = run_c17(&[&s(&main), &s(&a), &s(&b), "-o", &s(&exe)]);
    assert!(
        r.success,
        "two operands sharing a basename must not collide: {}{}",
        r.stdout, r.stderr
    );
    assert_eq!(run_exe(&exe), 7, "wrong object linked");
}

/// The same collision, but between a source and an object operand that share
/// a stem — the object must not be overwritten by the compile of the source.
#[test]
fn driver_source_does_not_clobber_a_like_named_object_operand() {
    let w = WorkDir::new("stemobj");
    std::fs::create_dir_all(w.join("sub")).unwrap();
    let helper = w.write("sub/util.c", "int fb(void){return 4;}\n");
    let obj = w.join("util.o");
    let r = run_c17(&["-c", &s(&helper), "-o", &s(&obj)]);
    assert!(
        r.success,
        "compiling the helper failed: {}{}",
        r.stdout, r.stderr
    );

    let main = w.write("util.c", "int fb(void);int main(void){return fb()+3;}\n");
    let exe = w.join("mixed");
    let r = run_c17(&[&s(&main), &s(&obj), "-o", &s(&exe)]);
    assert!(
        r.success,
        "source and object sharing a stem must not collide: {}{}",
        r.stdout, r.stderr
    );
    assert_eq!(run_exe(&exe), 7);
}

// ============================================================================
// #C140 — a `.i` operand is not preprocessed a second time
//
// POSIX 87981-87983: a `.i` is "a text file containing the output of c17 -E",
// and "the processing already performed by c17 -E when the file was produced
// shall not be repeated when the file is compiled."
//
// That is not a wholesale skip of translation phase 4. GCC keeps a five-entry
// allowlist -- `#define`, `#undef`, `#pragma`, `#ident`, `#sccs` -- plus the
// null `#` and linemarkers; everything else leaves a stray `#`. These pin both
// halves, because dropping the allowlist would silently lose `#pragma pack`.
// ============================================================================

/// Compile `body` as a `.i` and return (success, combined output).
fn compile_dot_i(name: &str, body: &str, extra: &[&str]) -> (bool, String) {
    let w = WorkDir::new(name);
    let src = w.write("t.i", body);
    let obj = w.join("t.o");
    let mut args: Vec<String> = extra.iter().map(|s| s.to_string()).collect();
    args.extend(["-c".to_string(), s(&src), "-o".to_string(), s(&obj)]);
    let argv: Vec<&str> = args.iter().map(String::as_str).collect();
    let r = run_c17(&argv);
    (r.success, format!("{}{}", r.stdout, r.stderr))
}

/// A macro defined in a `.i` is recorded but never substituted.
///
/// This is the headline case: c17 re-ran phase 4 and expanded `X`, so the
/// program returned 1 where gcc rejects the source outright.
#[test]
fn driver_dot_i_does_not_expand_macros() {
    let (ok, out) = compile_dot_i("i_nomacro", "#define X 1\nint main(void){return X;}\n", &[]);
    assert!(
        !ok,
        "a `.i` was preprocessed again -- `X` must stay undeclared:\n{out}"
    );
    assert!(out.contains("X"), "expected a diagnostic naming X:\n{out}");
}

/// `-D` on the command line is phase-4 processing and does not reach a `.i`.
#[test]
fn driver_dot_i_ignores_command_line_defines() {
    let (ok, out) = compile_dot_i("i_nodef", "int main(void){return X;}\n", &["-D", "X=0"]);
    assert!(!ok, "-D reached a `.i` operand:\n{out}");
}

/// Predefined macros are spent too -- `c17 -E` already replaced them.
#[test]
fn driver_dot_i_does_not_expand_predefined_macros() {
    for m in ["__LINE__", "__STDC__", "__FILE__"] {
        let (ok, out) = compile_dot_i(
            "i_nopredef",
            &format!("int f(void){{return sizeof {m};}}\n"),
            &[],
        );
        assert!(!ok, "{m} was expanded inside a `.i`:\n{out}");
    }
}

/// The allowlist: these directives still run in a `.i`.
#[test]
fn driver_dot_i_keeps_the_allowlisted_directives() {
    for (what, body) in [
        ("#define", "#define X 1\nint main(void){return 0;}\n"),
        ("#undef", "#undef NOPE\nint main(void){return 0;}\n"),
        (
            "#pragma",
            "#pragma GCC unknown_to_everyone\nint main(void){return 0;}\n",
        ),
        ("#ident", "#ident \"x\"\nint main(void){return 0;}\n"),
        ("null #", "#\nint main(void){return 0;}\n"),
    ] {
        let (ok, out) = compile_dot_i("i_allow", body, &[]);
        assert!(ok, "{what} must survive in a `.i`:\n{out}");
    }
}

/// Everything outside the allowlist leaves a stray `#`.
///
/// `#if 0` is the one that matters most: its body must be *kept*, because the
/// conditional was already resolved when the `.i` was produced.
#[test]
fn driver_dot_i_rejects_directives_outside_the_allowlist() {
    for (what, body) in [
        (
            "#include",
            "#include <stdio.h>\nint main(void){return 0;}\n",
        ),
        ("#if", "#if 0\n#endif\nint main(void){return 0;}\n"),
        ("#ifdef", "#ifdef NOPE\n#endif\nint main(void){return 0;}\n"),
        ("#error", "#error boom\nint main(void){return 0;}\n"),
        ("#line", "#line 100 \"fake.c\"\nint main(void){return 0;}\n"),
    ] {
        let (ok, out) = compile_dot_i("i_reject", body, &[]);
        assert!(!ok, "{what} was processed inside a `.i`:\n{out}");
        assert!(
            out.contains("stray '#'"),
            "{what} should leave a stray `#`:\n{out}"
        );
    }
}

/// `#pragma pack` must still take effect, or a preprocessed file silently
/// loses its layout -- the reason the allowlist exists rather than a blanket
/// skip of every `#` line.
#[test]
fn driver_dot_i_still_honors_pragma_pack() {
    let w = WorkDir::new("i_pack");
    let src = w.write(
        "t.i",
        "#pragma pack(1)\nstruct S{char c;int i;};\n\
         int main(void){return sizeof(struct S)==5?0:1;}\n",
    );
    let exe = w.join("t");
    let r = run_c17(&[&s(&src), "-o", &s(&exe)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);
    assert_eq!(run_exe(&exe), 0, "#pragma pack was dropped inside a `.i`");
}

/// `_Pragma` is a phase-4 operator, and `c17 -E` has already lowered it.
#[test]
fn driver_dot_i_does_not_process_the_pragma_operator() {
    let w = WorkDir::new("i_pragma_op");
    let src = w.write(
        "t.i",
        "_Pragma(\"pack(1)\")\nstruct S{char c;int i;};\n\
         int main(void){return sizeof(struct S)==5?0:1;}\n",
    );
    let r = run_c17(&[&s(&src), "-o", &s(&w.join("t"))]);
    assert!(!r.success, "_Pragma was executed inside a `.i`");
}

/// Phases 1 and 2 do not run again either.
#[test]
fn driver_dot_i_skips_trigraphs_and_line_splicing() {
    // Phase 1: a trigraph stays three characters even with --trigraphs.
    let (ok, out) = compile_dot_i(
        "i_trigraph",
        "int main(void){return sizeof(\"??=\")==4?0:1;}\n",
        &["--trigraphs"],
    );
    assert!(ok, "{out}");
    let w = WorkDir::new("i_trigraph_run");
    let src = w.write("t.i", "int main(void){return sizeof(\"??=\")==4?0:1;}\n");
    let exe = w.join("t");
    let r = run_c17(&["--trigraphs", &s(&src), "-o", &s(&exe)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);
    assert_eq!(run_exe(&exe), 0, "a trigraph was folded inside a `.i`");

    // Phase 2: a backslash-newline is text, not a splice.
    let (ok, out) = compile_dot_i(
        "i_splice",
        "int a\\\nb = 0;\nint main(void){return 0;}\n",
        &[],
    );
    assert!(!ok, "a `.i` was line-spliced again:\n{out}");
}

/// Phases 5 and 6 are the compiler's, not the preprocessor's, and still run.
#[test]
fn driver_dot_i_still_runs_the_later_phases() {
    let w = WorkDir::new("i_late");
    let src = w.write(
        "t.i",
        "int main(void){return sizeof(\"a\" \"b\")==3 && sizeof(\"\\x41\\101\")==3 ? 0 : 1;}\n",
    );
    let exe = w.join("t");
    let r = run_c17(&[&s(&src), "-o", &s(&exe)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);
    assert_eq!(
        run_exe(&exe),
        0,
        "string concatenation or escapes stopped working"
    );
}

/// A `.c` holding the same text is still fully preprocessed -- the gate is the
/// suffix, and nothing about the normal path changed.
#[test]
fn driver_dot_c_is_still_preprocessed() {
    let w = WorkDir::new("i_control");
    let src = w.write("t.c", "#define X 0\nint main(void){return X;}\n");
    let exe = w.join("t");
    let r = run_c17(&[&s(&src), "-o", &s(&exe)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);
    assert_eq!(run_exe(&exe), 0);
}

// ============================================================================
// #C141 — `-E` output must be C, not the internal pragma marker
// ============================================================================

/// A `#pragma pack` must survive `-E` as a directive.
///
/// It reaches the parser as a marker token carrying an internal payload, and
/// `show_token` spells that payload `<PRAGMA pack:set:1>` -- a debug form, not
/// C. The `-E` writer filtered the other internal markers but not this one, so
/// `c17 -E` on any source using the pragma produced a file that neither c17
/// nor gcc would compile. POSIX 87981 makes that output a `.i` operand, so
/// this made the whole `.i` form unreachable for such a source.
#[test]
fn driver_preprocess_writes_pragma_pack_as_a_directive() {
    let w = WorkDir::new("pp_pragma");
    let src = w.write(
        "q.c",
        "#pragma pack(1)\nstruct S{char c;int i;};\n\
         int main(void){return sizeof(struct S)==5?0:1;}\n",
    );
    let i = w.join("q.i");
    let r = run_c17(&["-E", &s(&src), "-o", &s(&i)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);

    let body = std::fs::read_to_string(&i).unwrap();
    assert!(
        !body.contains("<PRAGMA"),
        "the internal marker reached the output:\n{body}"
    );
    assert!(
        body.contains("#pragma pack(1)"),
        "the pragma was dropped instead of written back:\n{body}"
    );

    // And the round trip has to keep the packing, not merely parse.
    let exe = w.join("q");
    let r = run_c17(&[&s(&i), "-o", &s(&exe)]);
    assert!(
        r.success,
        "-E output did not compile: {}{}",
        r.stdout, r.stderr
    );
    assert_eq!(run_exe(&exe), 0, "the packing was lost across -E");
}

/// Every `#pragma pack` form round-trips, not just the one with a number.
#[test]
fn driver_preprocess_writes_every_pragma_pack_form() {
    let w = WorkDir::new("pp_pragma_forms");
    let src = w.write(
        "f.c",
        "#pragma pack(push, 1)\nstruct A{char c;int i;};\n\
         #pragma pack(pop)\nstruct B{char c;int i;};\n\
         #pragma pack()\nstruct C{char c;int i;};\n\
         int main(void){return sizeof(struct A)==5 && sizeof(struct B)==8 ? 0 : 1;}\n",
    );
    let i = w.join("f.i");
    let r = run_c17(&["-E", &s(&src), "-o", &s(&i)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);

    let body = std::fs::read_to_string(&i).unwrap();
    assert!(!body.contains("<PRAGMA"), "internal marker leaked:\n{body}");
    for want in [
        "#pragma pack(push, 1)",
        "#pragma pack(pop)",
        "#pragma pack()",
    ] {
        assert!(body.contains(want), "missing {want}:\n{body}");
    }

    let exe = w.join("f");
    let r = run_c17(&[&s(&i), "-o", &s(&exe)]);
    assert!(r.success, "{}{}", r.stdout, r.stderr);
    assert_eq!(run_exe(&exe), 0, "push/pop did not survive -E");
}

/// Preprocessing an already-preprocessed file stays stable and correct.
///
/// The second pass must attribute the text to the file the linemarkers name,
/// not to the `.i` it is reading, and the result must still compile and run.
#[test]
fn driver_preprocess_of_a_dot_i_is_stable() {
    let w = WorkDir::new("pp_idem");
    let src = w.write(
        "q.c",
        "#pragma pack(1)\nstruct S{char c;int i;};\n\
         int main(void){return sizeof(struct S)==5?0:1;}\n",
    );
    let one = w.join("q1.i");
    let two = w.join("q2.i");
    let three = w.join("q3.i");
    assert!(run_c17(&["-E", &s(&src), "-o", &s(&one)]).success);
    assert!(run_c17(&["-E", &s(&one), "-o", &s(&two)]).success);
    assert!(run_c17(&["-E", &s(&two), "-o", &s(&three)]).success);

    // The first line names the file being read, so it differs by construction;
    // everything after it has converged.
    let skip_first = |p: &Path| {
        std::fs::read_to_string(p)
            .unwrap()
            .lines()
            .skip(1)
            .collect::<Vec<_>>()
            .join("\n")
    };
    assert_eq!(
        skip_first(&two),
        skip_first(&three),
        "re-preprocessing keeps changing the output"
    );

    // The second-generation file names the original source, not the `.i`.
    let body = std::fs::read_to_string(&two).unwrap();
    assert!(
        body.contains("q.c\""),
        "lost the original file name:\n{body}"
    );

    for f in [&one, &two, &three] {
        let exe = w.join("out");
        let r = run_c17(&[&s(f), "-o", &s(&exe)]);
        assert!(
            r.success,
            "{} did not compile: {}{}",
            s(f),
            r.stdout,
            r.stderr
        );
        assert_eq!(run_exe(&exe), 0, "{} lost the packing", s(f));
    }
}

/// A search-path option must not be mistaken for a pathname operand.
///
/// `VALUE_OPTIONS` tells the link-order scan which options swallow the word
/// after them. `-isystem` and `-idirafter` were listed under their single-dash
/// gcc spellings, but `preprocess_args_from` rewrites those to double-dash
/// before clap sees them and the scan runs on the rewritten vector -- so the
/// entries never matched, the directory was read as a pathname operand, and
/// `ordering_recovered` went false. The link line then silently fell back to
/// its unordered shape.
///
/// The observable is which of two rival archives wins, as in
/// `driver_library_order_is_significant`: that is a property of the order c17
/// hands the linker, so it holds everywhere. The sharper negative -- a library
/// named before its user failing to resolve -- is GNU ld's one-pass archive
/// semantics rather than a POSIX guarantee, and lives in the Linux-only test
/// below.
#[test]
fn driver_isystem_does_not_disturb_library_ordering() {
    let w = WorkDir::new("isystem_order");
    let libdir = build_rival_archives(&w);
    std::fs::create_dir_all(w.join("inc")).unwrap();
    let user = w.write(
        "usr.c",
        "int which(void);\nint main(void){return which();}\n",
    );
    let ldir = format!("-L{}", libdir.to_string_lossy());
    let incdir = s(&w.join("inc"));

    for opt in ["-isystem", "-idirafter"] {
        let exe1 = w.join("ord1");
        let r = run_c17(&[
            opt,
            &incdir,
            &s(&user),
            &ldir,
            "-lQ",
            "-lP",
            "-o",
            &s(&exe1),
        ]);
        assert!(r.success, "{opt} broke the link: {}{}", r.stdout, r.stderr);
        assert_eq!(
            run_exe(&exe1),
            1,
            "{opt} disturbed ordering: -lQ came first, so libQ must win"
        );

        let exe2 = w.join("ord2");
        let r = run_c17(&[
            opt,
            &incdir,
            &s(&user),
            &ldir,
            "-lP",
            "-lQ",
            "-o",
            &s(&exe2),
        ]);
        assert!(r.success, "{opt} broke the link: {}{}", r.stdout, r.stderr);
        assert_eq!(
            run_exe(&exe2),
            2,
            "{opt} disturbed ordering: -lP came first, so libP must win"
        );
    }
}

/// The negative half of the pair above: with a search-path option present, a
/// library named before its user must still fail to resolve.
///
/// Linux only, for the reason given on
/// `driver_library_named_before_its_user_does_not_resolve`: Apple's linker
/// resolves across every archive regardless of order, so the failure this
/// looks for cannot be produced there.
#[cfg(target_os = "linux")]
#[test]
fn driver_isystem_does_not_recover_a_library_named_too_early() {
    let w = WorkDir::new("isystem_early");
    let libdir = build_rival_archives(&w);
    std::fs::create_dir_all(w.join("inc")).unwrap();
    let user = w.write(
        "usr.c",
        "int which(void);\nint main(void){return which();}\n",
    );
    let ldir = format!("-L{}", libdir.to_string_lossy());
    let incdir = s(&w.join("inc"));

    // The baseline, without the option.
    let before = run_c17(&[&ldir, "-lQ", &s(&user), "-o", &s(&w.join("a"))]);
    assert!(
        !before.success,
        "an archive named before its user must not resolve: {}{}",
        before.stdout, before.stderr
    );

    // Adding a search-path option must not change that.
    for opt in ["-isystem", "-idirafter"] {
        let r = run_c17(&[
            opt,
            &incdir,
            &ldir,
            "-lQ",
            &s(&user),
            "-o",
            &s(&w.join("b")),
        ]);
        assert!(
            !r.success,
            "{opt} made link ordering fall back to its unordered shape: {}{}",
            r.stdout, r.stderr
        );
    }
}

/// A diagnostic from inside a directive is attributed like every other.
///
/// A directive handler pulls its own tokens, so they never pass the
/// linemarker remap in the main loop. `#pragma pack` captured its position
/// before remapping and handed it to `parse_pack_body`, which reports five
/// diagnostics from it -- so a malformed pragma cited the `.i` while the error
/// on the very next line cited the original file. Two diagnostics about one
/// file disagreeing on which file it is.
#[test]
fn driver_dot_i_attributes_pragma_diagnostics_to_the_original_file() {
    let w = WorkDir::new("i_pragma_diag");
    let src = w.write(
        "pp.i",
        "# 50 \"orig.c\"\n#pragma pack(bogus)\nint main(void){ return nope; }\n",
    );
    let r = run_c17(&["-c", &s(&src), "-o", &s(&w.join("pp.o"))]);
    assert!(
        !r.success,
        "the undeclared identifier should fail the compile"
    );

    assert!(
        r.stderr.contains("orig.c:50:"),
        "the pragma diagnostic was not attributed to orig.c:50:\n{}",
        r.stderr
    );
    assert!(
        r.stderr.contains("orig.c:51:"),
        "the following error was not attributed to orig.c:51:\n{}",
        r.stderr
    );
    // The physical positions in the preprocessed text must not survive.
    assert!(
        !r.stderr.contains("pp.i:2:") && !r.stderr.contains("pp.i:3:"),
        "a diagnostic still cites the position in the preprocessed text:\n{}",
        r.stderr
    );
}

/// `#ident` and `#sccs` are directives c17 knows, in both modes.
///
/// Both were named in the `.i` allowlist while having no dispatch arm, so
/// they fell through to the unknown-directive arm and warned -- in a `.c` as
/// well as a `.i`. gcc is silent about both. An allowlist that names a
/// directive the dispatcher does not handle claims support that is not there.
#[test]
fn driver_ident_and_sccs_are_accepted_quietly() {
    for (name, body) in [
        (
            "ident",
            "#ident \"version string\"\nint main(void){return 0;}\n",
        ),
        (
            "sccs",
            "#sccs \"version string\"\nint main(void){return 0;}\n",
        ),
    ] {
        for suffix in ["c", "i"] {
            let w = WorkDir::new(&format!("{name}_{suffix}"));
            let src = w.write(&format!("t.{suffix}"), body);
            let r = run_c17(&["-c", &s(&src), "-o", &s(&w.join("t.o"))]);
            assert!(
                r.success,
                "#{name} in a .{suffix} was rejected: {}{}",
                r.stdout, r.stderr
            );
            assert!(
                r.stderr.is_empty(),
                "#{name} in a .{suffix} produced a diagnostic:\n{}",
                r.stderr
            );
        }
    }
}

/// `-P` asks for the preprocessed text with no `# <line> "<file>"` markers.
/// It is what things that are not C compilers want — linker scripts, generated
/// headers, assembler input.
#[test]
fn driver_dash_p_suppresses_line_markers() {
    let w = WorkDir::new("dash_p");
    let src = w.write("p.c", "#define X 1\n\n\nint v = X;\n");
    let r = run_c17(&["-E", "-P", &s(&src)]);
    assert!(r.success, "-E -P failed: {}", r.stderr);
    assert!(
        !r.stdout.contains('#'),
        "-P output must carry no markers:\n{}",
        r.stdout
    );
    assert!(
        r.stdout.contains("int v = 1;"),
        "the text survives:\n{}",
        r.stdout
    );
    // Byte-for-byte what gcc produces for this input.
    assert_eq!(
        r.stdout, "int v = 1;\n",
        "unexpected layout: {:?}",
        r.stdout
    );
}

/// Without `-P` the markers are still mandated, so the two must not have been
/// conflated.
#[test]
fn driver_without_dash_p_line_markers_remain() {
    let w = WorkDir::new("no_dash_p");
    let src = w.write("q.c", "int v = 1;\n");
    let r = run_c17(&["-E", &s(&src)]);
    assert!(r.success, "-E failed: {}", r.stderr);
    assert!(
        r.stdout.contains("# 1 \""),
        "expected a marker:\n{}",
        r.stdout
    );
}

/// `-include <file>` processes the file as if `#include "<file>"` were the
/// first line of the source: after `-D`/`-U`, before the source, in order.
#[test]
fn driver_dash_include_injects_a_header() {
    let w = WorkDir::new("dash_include");
    w.write("one.h", "int one_thing = 1;\n");
    w.write("two.h", "int two_thing = 2;\n");
    let src = w.write("m.c", "int main(void) { return one_thing + two_thing; }\n");

    let r = run_c17(&[
        "-E",
        "-P",
        "-include",
        &s(&w.join("one.h")),
        "-include",
        &s(&w.join("two.h")),
        &s(&src),
    ]);
    assert!(r.success, "-E failed: {}", r.stderr);
    let one = r
        .stdout
        .find("one_thing = 1")
        .expect("first -include missing");
    let two = r
        .stdout
        .find("two_thing = 2")
        .expect("second -include missing");
    let main = r
        .stdout
        .find("int main")
        .expect("the source itself missing");
    assert!(one < two && two < main, "wrong order:\n{}", r.stdout);
}

/// The injected header has to reach the compiler, not just `-E` — the two
/// paths share a token vector, and getting the stream markers wrong breaks
/// only the one that parses.
#[test]
fn driver_dash_include_reaches_the_compiler() {
    let w = WorkDir::new("dash_include_compile");
    w.write("pre.h", "int from_header = 7;\n");
    let src = w.write("m.c", "int main(void) { return from_header; }\n");
    let exe = w.join("m.bin");

    let r = run_c17(&["-include", &s(&w.join("pre.h")), &s(&src), "-o", &s(&exe)]);
    assert!(r.success, "compile failed: {}", r.stderr);
    assert_eq!(run_exe(&exe), 7, "the injected header did not take effect");
}

/// A `-include` naming a file that is not there is an error, not silence.
#[test]
fn driver_dash_include_missing_file_is_an_error() {
    let w = WorkDir::new("dash_include_missing");
    let src = w.write("m.c", "int main(void) { return 0; }\n");
    let r = run_c17(&["-E", "-include", "no_such_header.h", &s(&src)]);
    assert!(!r.success, "a missing -include must fail:\n{}", r.stdout);
    assert!(
        r.stderr.contains("file not found"),
        "expected a not-found diagnostic, got:\n{}",
        r.stderr
    );
}

/// `-dM` dumps every macro in force as the `#define` that would make it,
/// instead of the preprocessed text.
#[test]
fn driver_dash_dm_dumps_macro_definitions() {
    let w = WorkDir::new("dash_dm");
    let src = w.write(
        "m.c",
        "#define OBJ 1\n#define FN(a,b) ((a)+(b))\n#define EMPTY\n\
         #define VAR(...) f(__VA_ARGS__)\n#define STR(x) #x\n#define CAT(a,b) a##b\n\
         #define GNU(a, rest...) g(rest)\nint v;\n",
    );
    let r = run_c17(&["-dM", "-E", &s(&src)]);
    assert!(r.success, "-dM failed: {}", r.stderr);

    for want in [
        "#define OBJ 1",
        "#define FN(a,b) ((a)+(b))",
        "#define VAR(...) f(__VA_ARGS__)",
        // The `#` and `##` operators are put back, and parameters are named
        // rather than shown as the indices the body actually stores.
        "#define STR(x) #x",
        "#define CAT(a,b) a##b",
        // The GNU named-variadic spelling round-trips as itself.
        "#define GNU(a,rest...) g(rest)",
    ] {
        assert!(
            r.stdout.contains(want),
            "expected {:?} in:\n{}",
            want,
            r.stdout
        );
    }
    assert!(
        r.stdout.lines().any(|l| l.trim_end() == "#define EMPTY"),
        "an empty replacement list still gets a line:\n{}",
        r.stdout
    );

    // The source's own text is not the output.
    assert!(
        !r.stdout.contains("int v;"),
        "-dM replaces the text:\n{}",
        r.stdout
    );

    // Predefines are in there too, which is what `-dM` is usually asked for.
    assert!(
        r.stdout.contains("#define __STDC__ 1"),
        "missing predefines"
    );

    // The dynamic builtins have no replacement list to print; gcc omits them.
    for absent in [
        "#define __FILE__",
        "#define __LINE__",
        "#define __COUNTER__",
    ] {
        assert!(!r.stdout.contains(absent), "{} should be omitted", absent);
    }
}

/// The macro table is a `HashMap`, whose iteration order differs per process.
/// `-dM` sorts, or its output would differ run to run.
#[test]
fn driver_dash_dm_output_is_deterministic() {
    let w = WorkDir::new("dash_dm_det");
    let src = w.write("m.c", "#define B 2\n#define A 1\n#define C 3\nint v;\n");
    let first = run_c17(&["-dM", "-E", &s(&src)]);
    let second = run_c17(&["-dM", "-E", &s(&src)]);
    assert!(first.success && second.success);
    assert_eq!(first.stdout, second.stdout, "-dM output must not vary");

    let names: Vec<&str> = first
        .stdout
        .lines()
        .filter_map(|l| l.strip_prefix("#define "))
        .map(|l| l.split(['(', ' ']).next().unwrap_or(""))
        .collect();
    let mut sorted = names.clone();
    sorted.sort_unstable();
    assert_eq!(names, sorted, "-dM output must be sorted by name");
}

/// `-M` and `-MM` write a make rule naming what the source depends on, instead
/// of compiling it. This is how a make-based build tracks header changes, and
/// c17 had no way to produce one.
#[test]
fn driver_dash_mm_writes_a_dependency_rule() {
    let w = WorkDir::new("dash_mm");
    w.write("local.h", "int local_thing;\n");
    let src = w.write(
        "d.c",
        "#include \"local.h\"\n#include <stdio.h>\nint main(void){return 0;}\n",
    );

    let r = run_c17(&["-MM", &s(&src)]);
    assert!(r.success, "-MM failed: {}", r.stderr);
    let rule = r.stdout.trim_end();
    assert!(
        rule.starts_with("d.o:"),
        "target should be the source renamed .o: {:?}",
        rule
    );
    assert!(
        rule.contains("local.h"),
        "the user header is a prerequisite: {:?}",
        rule
    );
    // `-MM` leaves out headers found on a system path. The `<>` vs `""`
    // spelling is not the test — where it resolved is.
    assert!(
        !rule.contains("stdio.h"),
        "-MM omits system headers: {:?}",
        rule
    );

    // Nothing was compiled.
    assert!(!w.join("d.o").exists(), "-MM must not produce an object");
}

/// `-M` keeps the system headers `-MM` drops.
#[test]
fn driver_dash_m_includes_system_headers() {
    let w = WorkDir::new("dash_m");
    let src = w.write("d.c", "#include <stdio.h>\nint main(void){return 0;}\n");
    let r = run_c17(&["-M", &s(&src)]);
    assert!(r.success, "-M failed: {}", r.stderr);
    assert!(
        r.stdout.contains("stdio.h"),
        "-M lists system headers:\n{}",
        r.stdout
    );
}

/// `-MT` names the target; `-MP` adds a bare rule per header so that deleting
/// one does not break the build before the makefile is regenerated.
#[test]
fn driver_dependency_rule_target_and_phony() {
    let w = WorkDir::new("dash_mt");
    w.write("local.h", "int local_thing;\n");
    let src = w.write("d.c", "#include \"local.h\"\nint main(void){return 0;}\n");

    let r = run_c17(&["-MM", "-MT", "obj/d.o", &s(&src)]);
    assert!(r.success, "-MT failed: {}", r.stderr);
    assert!(
        r.stdout.starts_with("obj/d.o:"),
        "-MT names the target:\n{}",
        r.stdout
    );

    let r = run_c17(&["-MM", "-MP", &s(&src)]);
    assert!(r.success, "-MP failed: {}", r.stderr);
    let phony: Vec<&str> = r.stdout.lines().filter(|l| l.ends_with(':')).collect();
    assert_eq!(
        phony.len(),
        1,
        "one bare rule, for the header only:\n{}",
        r.stdout
    );
    assert!(
        phony[0].contains("local.h"),
        "the bare rule names the header:\n{}",
        r.stdout
    );
}

/// `-MD` and `-MMD` are side outputs: the rule goes to a file *and* the source
/// is still compiled. Putting them in the `-E` branch would have lost that.
#[test]
fn driver_dash_mmd_writes_the_rule_and_still_compiles() {
    let w = WorkDir::new("dash_mmd");
    w.write("local.h", "int local_thing;\n");
    let src = w.write("d.c", "#include \"local.h\"\nint main(void){return 0;}\n");
    let obj = w.join("d.o");

    let r = run_c17(&["-MMD", "-c", &s(&src), "-o", &s(&obj)]);
    assert!(r.success, "-MMD failed: {}", r.stderr);
    assert!(obj.exists(), "-MMD must still compile");

    // The rule defaults to the source renamed `.d`.
    let dep = w.join("d.d");
    assert!(dep.exists(), "-MMD writes a .d beside the source");
    let rule = std::fs::read_to_string(&dep).unwrap();
    assert!(rule.contains("local.h"), "unexpected rule: {:?}", rule);
}

/// `-MF` says where the rule goes, and then nothing goes to stdout.
#[test]
fn driver_dash_mf_redirects_the_rule() {
    let w = WorkDir::new("dash_mf");
    w.write("local.h", "int local_thing;\n");
    let src = w.write("d.c", "#include \"local.h\"\nint main(void){return 0;}\n");
    let out = w.join("custom.d");

    let r = run_c17(&["-MM", "-MF", &s(&out), &s(&src)]);
    assert!(r.success, "-MF failed: {}", r.stderr);
    assert!(
        r.stdout.is_empty(),
        "-MF means nothing on stdout: {:?}",
        r.stdout
    );
    let rule = std::fs::read_to_string(&out).unwrap();
    assert!(rule.contains("local.h"), "unexpected rule: {:?}", rule);
}

/// A header reached twice is one prerequisite, not two — and an include guard
/// skipping the second read must not lose it either.
#[test]
fn driver_dependency_rule_lists_each_header_once() {
    let w = WorkDir::new("dash_mm_once");
    w.write("g.h", "#ifndef G_H\n#define G_H\nint g;\n#endif\n");
    let src = w.write(
        "d.c",
        "#include \"g.h\"\n#include \"g.h\"\nint main(void){return 0;}\n",
    );
    let r = run_c17(&["-MM", &s(&src)]);
    assert!(r.success, "-MM failed: {}", r.stderr);
    assert_eq!(
        r.stdout.matches("g.h").count(),
        1,
        "a header included twice is one prerequisite:\n{}",
        r.stdout
    );
}

/// `-include` of a bundled header has to name the header and pass its text,
/// not the other way round. Reversed, the filename was tokenized as the source
/// and the header body became the stream's name.
#[test]
fn driver_dash_include_finds_a_bundled_header() {
    let w = WorkDir::new("dash_include_builtin");
    let src = w.write("m.c", "int main(void) { return (int)true - 1; }\n");
    let exe = w.join("m.bin");
    let r = run_c17(&["-include", "stdbool.h", &s(&src), "-o", &s(&exe)]);
    assert!(r.success, "compile failed: {}", r.stderr);
    assert_eq!(run_exe(&exe), 0, "`true` should have expanded to 1");
}

/// A dependency rule built from a translation unit that did not preprocess is
/// incomplete, and exiting 0 would have a makefile record it as authoritative.
#[test]
fn driver_dependency_rule_fails_when_preprocessing_did() {
    let w = WorkDir::new("dep_exit");
    let src = w.write("d.c", "#include \"no_such_header.h\"\nint v;\n");
    let r = run_c17(&["-MM", &s(&src)]);
    assert!(
        !r.success,
        "a missing header must fail the rule:\n{}",
        r.stdout
    );
    assert!(
        r.stderr.contains("file not found"),
        "expected the not-found diagnostic, got:\n{}",
        r.stderr
    );
}
