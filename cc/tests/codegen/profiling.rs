//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-pg` programs: the profiling call keeps every argument where the
// function expects it, and a `-pg` link writes `gmon.out` for gprof.
//

use crate::common::{compile_and_run_aarch64_with, run_c17, work_dir};
use std::process::Command;

/// Arguments in every register class, a variadic call, a struct by value
/// and recursion, each read after the entry's profiling call.
const PROGRAM: &str = r#"
#include <stdarg.h>
struct pair { long a; double b; };
__attribute__((noinline)) long ints(long a, long b, long c, long d, long e, long f, long g)
{ return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g; }
__attribute__((noinline)) double fps(double a, float b, double c, double d, double e,
                                     double f, double g, double h, double i)
{ return a + b + c + d + e + f + g + h + i; }
__attribute__((noinline)) long sum(int n, ...)
{
    va_list ap; long s = 0;
    va_start(ap, n);
    for (int i = 0; i < n; i++) s += va_arg(ap, long);
    va_end(ap);
    return s;
}
__attribute__((noinline)) double byval(struct pair p, int k) { return p.a * k + p.b; }
__attribute__((noinline)) long fib(long n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
__attribute__((noinline)) long tick(long x) { return x + 1; }
static long leaf(long x) { return x * 3; }
long (*volatile indirect)(long) = leaf;
int main(void)
{
    if (ints(1, 2, 3, 4, 5, 6, 7) != 140) return 1;
    if (fps(1, 2, 3, 4, 5, 6, 7, 8, 9) != 45.0) return 2;
    if (sum(4, 10L, 20L, 30L, 40L) != 100) return 3;
    struct pair p = { 7, 0.5 };
    if (byval(p, 3) != 21.5) return 4;
    if (fib(20) != 6765) return 5;
    if (indirect(14) != 42) return 6;
    long t = 0;
    for (int i = 0; i < 1000; i++) t = tick(t);
    if (t != 1000) return 7;
    return 0;
}
"#;

/// The program built with `-pg` runs correctly at -O0 and -O2, on aarch64
/// under qemu too, where `_mcount` is an ordinary call the arguments must
/// survive.
#[test]
fn pg_program_keeps_its_arguments() {
    let dir = work_dir("pg_args");
    let src = dir.path().join("pg.c");
    std::fs::write(&src, PROGRAM).unwrap();
    for opt in ["-O0", "-O1", "-O2"] {
        let exe = dir.path().join(format!("pg{opt}"));
        let run = run_c17(&[
            opt,
            "-pg",
            "-o",
            exe.to_str().unwrap(),
            src.to_str().unwrap(),
        ]);
        assert!(run.success, "{opt}: {}", run.stderr);
        let status = Command::new(&exe)
            .current_dir(dir.path())
            .status()
            .expect("run the -pg program");
        assert_eq!(status.code(), Some(0), "-pg {opt}");
        if let Some(code) = compile_and_run_aarch64_with("pg_args", PROGRAM, &[opt, "-pg"], &[]) {
            assert_eq!(code, 0, "aarch64 -pg {opt}");
        }
    }
}

/// A `-pg` link takes the profiling startup, so the program writes
/// `gmon.out` on exit, and gprof reads from it how often each function was
/// called. A program linked from `-pg` objects without `-pg` writes none,
/// as with gcc.
#[test]
fn pg_link_writes_gmon_out() {
    let dir = work_dir("pg_gmon");
    let src = dir.path().join("pg.c");
    std::fs::write(&src, PROGRAM).unwrap();
    let exe = dir.path().join("pg");
    let run = run_c17(&[
        "-O2",
        "-pg",
        "-o",
        exe.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(run.success, "{}", run.stderr);
    let status = Command::new(&exe)
        .current_dir(dir.path())
        .status()
        .expect("run the -pg program");
    assert_eq!(status.code(), Some(0));
    let gmon = dir.path().join("gmon.out");
    assert!(gmon.exists(), "no gmon.out after a -pg run");

    // gprof's flat profile counts the calls `mcount` saw.
    if let Ok(out) = Command::new("gprof")
        .args(["-b", "-p", exe.to_str().unwrap(), gmon.to_str().unwrap()])
        .output()
    {
        let report = String::from_utf8_lossy(&out.stdout);
        assert!(out.status.success(), "gprof:\n{report}");
        let tick = report
            .lines()
            .find(|l| l.split_whitespace().last() == Some("tick"))
            .unwrap_or_else(|| panic!("no tick in the profile:\n{report}"));
        assert_eq!(
            tick.split_whitespace().nth(3),
            Some("1000"),
            "tick's call count:\n{report}"
        );
    }

    // Compiled with -pg, linked without: no profiling startup, no file.
    std::fs::remove_file(&gmon).unwrap();
    let obj = dir.path().join("pg.o");
    let run = run_c17(&[
        "-O2",
        "-pg",
        "-c",
        "-o",
        obj.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(run.success, "{}", run.stderr);
    let plain = dir.path().join("plain");
    let run = run_c17(&["-o", plain.to_str().unwrap(), obj.to_str().unwrap()]);
    assert!(run.success, "{}", run.stderr);
    let status = Command::new(&plain)
        .current_dir(dir.path())
        .status()
        .expect("run the program");
    assert_eq!(status.code(), Some(0));
    assert!(!gmon.exists(), "gmon.out without a -pg link");
}
