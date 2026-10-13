//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtins that are one machine instruction: glibc's `__builtin_ia32_rdtsc`
// and `__builtin_ia32_rdtscp` (hp-timing.h), `__builtin_ia32_cvtpd2ps`
// (sincosf_poly.h), `__builtin_thread_pointer` (thread_pointer.h) and
// aarch64's `__builtin_aarch64_{get,set}_{fpcr,fpsr}` (fpu_control.h), each
// as gcc 13 has it on its targets.
//

use crate::common::{
    asm_for_at, compile_and_run_aarch64_with, compile_and_run_everywhere, compile_expect_error,
    compile_rejected_with, create_c_file, run_c17,
};

/// The time-stamp counter, alone and with the processor's signature: both
/// count up, and `rdtscp` stores the signature the instruction gives.
#[cfg(target_arch = "x86_64")]
#[test]
fn builtins_ia32_rdtsc_rdtscp() {
    let src = r#"
#include <x86intrin.h>
int main(void) {
    unsigned long long a = __builtin_ia32_rdtsc();
    unsigned aux = 0xdeadbeef;
    unsigned long long b = __builtin_ia32_rdtscp(&aux);
    unsigned long long c = __rdtsc();
    unsigned aux2 = 0;
    unsigned long long d = __rdtscp(&aux2);
    if (a == 0 || b < a || c < b || d < c)
        return 1;
    /* The signature is IA32_TSC_AUX, the same on every read of one CPU
       only; it is at least written. */
    if (aux == 0xdeadbeef)
        return 2;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        let flags = vec![opt.to_string()];
        assert_eq!(
            crate::common::compile_and_run("ia32_rdtsc", src, &flags),
            0,
            "{opt}"
        );
    }
    let asm = asm_for_at(
        "c17_ia32_rdtsc_",
        "unsigned long long a(void) { return __builtin_ia32_rdtsc(); }\n\
         unsigned long long b(unsigned *p) { return __builtin_ia32_rdtscp(p); }\n",
        &["--target=x86_64-unknown-linux-gnu", "-O2"],
    );
    assert!(
        asm.contains("rdtsc\n") || asm.contains("rdtsc\t") || asm.contains("rdtsc "),
        "{asm}"
    );
    assert!(asm.contains("rdtscp"), "{asm}");
    assert!(!asm.contains("call"), "{asm}");
}

/// `cvtpd2ps`: two `double` lanes rounded to `float` in the current
/// rounding direction, the upper two lanes zero, as `_mm_cvtpd_ps`.
#[cfg(target_arch = "x86_64")]
#[test]
fn builtins_ia32_cvtpd2ps() {
    let src = r#"
#include <emmintrin.h>
#include <fenv.h>
typedef double v2df __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
int main(void) {
    volatile double x = -0.1, y = -1e300;
    v2df v = {x, y};
    v4sf r = __builtin_ia32_cvtpd2ps(v);
    if (r[0] != (float)-0.1 || r[1] != -__builtin_inff() || r[2] != 0 || r[3] != 0)
        return 1;
    fesetround(FE_UPWARD);
    v4sf up = __builtin_ia32_cvtpd2ps(v);
    fesetround(FE_TONEAREST);
    if (!(up[0] > r[0]) || up[1] != -__FLT_MAX__)
        return 2;
    __m128 m = _mm_cvtpd_ps(_mm_set_pd(2.5, 1.25));
    if (_mm_cvtss_f32(m) != 1.25f || m[1] != 2.5f || m[2] != 0 || m[3] != 0)
        return 3;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        let flags = vec![opt.to_string(), "-lm".to_string()];
        assert_eq!(
            crate::common::compile_and_run("ia32_cvtpd2ps", src, &flags),
            0,
            "{opt}"
        );
    }
}

/// The `__builtin_ia32_*` names are x86-64's: gcc's aarch64 compiler has
/// never heard of them, nor does `__has_builtin` there.
#[test]
fn builtins_ia32_are_x86_64_only() {
    let src = "#if __has_builtin(__builtin_ia32_rdtsc) != defined(__x86_64__)\n\
               #error rdtsc\n#endif\n\
               #if __has_builtin(__builtin_ia32_cvtpd2ps) != defined(__x86_64__)\n\
               #error cvtpd2ps\n#endif\n\
               #if !__has_builtin(__builtin_thread_pointer)\n#error tp\n#endif\n\
               int x;\n";
    for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        asm_for_at("c17_ia32_has_", src, &[&format!("--target={target}")]);
    }
    compile_expect_error(
        "ia32_cvtpd2ps_arg",
        "float f(double d) { return __builtin_ia32_cvtpd2ps(d)[0]; }\n",
        "__builtin_ia32_cvtpd2ps",
    );
}

/// The thread pointer is the one the TLS ABI defines: `%fs:0` on x86-64,
/// `tpidr_el0` on aarch64, read without a call.
#[test]
fn builtins_thread_pointer() {
    let src = r#"
static _Thread_local int t;
int main(void) {
    void *tp = __builtin_thread_pointer();
    void *want;
#if defined(__x86_64__)
    __asm__("movq %%fs:0, %0" : "=r"(want));
#else
    __asm__("mrs %0, tpidr_el0" : "=r"(want));
#endif
    if (tp != want || tp == 0)
        return 1;
    /* Local-exec TLS sits at a fixed offset from it. */
    t = 7;
    return __builtin_thread_pointer() == tp ? 0 : 2;
}
"#;
    if cfg!(target_os = "linux") {
        compile_and_run_everywhere("thread_pointer", src);
    }
    let fn_src = "void *d(void) { return __builtin_thread_pointer(); }\n";
    let x86 = asm_for_at(
        "c17_tp_x86_",
        fn_src,
        &["--target=x86_64-unknown-linux-gnu", "-O2"],
    );
    assert!(x86.contains("%fs:0"), "{x86}");
    assert!(!x86.contains("call"), "{x86}");
    let a64 = asm_for_at(
        "c17_tp_a64_",
        fn_src,
        &["--target=aarch64-unknown-linux-gnu", "-O2"],
    );
    assert!(a64.contains("tpidr_el0"), "{a64}");
    assert!(
        !a64.lines().any(|l| l.trim_start().starts_with("bl ")),
        "{a64}"
    );
}

/// The instructions of function `name` in `asm`, trimmed: from its label to
/// its `.size`, directives and labels left out.
fn instructions_of(asm: &str, name: &str) -> Vec<String> {
    asm.lines()
        .skip_while(|l| *l != format!("{name}:"))
        .skip(1)
        .take_while(|l| !l.starts_with(".size"))
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('.') && !l.ends_with(':'))
        .map(str::to_string)
        .collect()
}

/// aarch64's floating-point control and status registers: each builtin is
/// one `mrs` or `msr` of an X register, as gcc 13 emits it, and stays where
/// the program put it among the floating-point operations it governs --
/// the multiply after a change of rounding mode is done in that mode, and
/// the status read after a divide sees the divide's flags.
#[test]
fn builtins_aarch64_fpcr_fpsr_instructions() {
    let src = r#"
unsigned get(void) { return __builtin_aarch64_get_fpcr(); }
void set(unsigned v) { __builtin_aarch64_set_fpcr(v); }
unsigned long long get64(void) { return __builtin_aarch64_get_fpcr64(); }
void set64(unsigned long long v) { __builtin_aarch64_set_fpcr64(v); }
unsigned gsr(void) { return __builtin_aarch64_get_fpsr(); }
void ssr(unsigned v) { __builtin_aarch64_set_fpsr(v); }
unsigned long long gsr64(void) { return __builtin_aarch64_get_fpsr64(); }
void ssr64(unsigned long long v) { __builtin_aarch64_set_fpsr64(v); }
double around(double a, double b) {
    __builtin_aarch64_set_fpcr(0x400000);
    double x = a * b;
    __builtin_aarch64_set_fpcr(0);
    return x;
}
unsigned flags(double a, double b, double *out) {
    __builtin_aarch64_set_fpsr(0);
    *out = a / b;
    return __builtin_aarch64_get_fpsr();
}
"#;
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_at(
            "c17_a64_fpcr_",
            src,
            &["--target=aarch64-unknown-linux-gnu", opt],
        );
        for (func, insn) in [
            ("get", "mrs x"),
            ("get64", "mrs x"),
            ("gsr", "mrs x"),
            ("gsr64", "mrs x"),
            ("set", "msr fpcr, x"),
            ("set64", "msr fpcr, x"),
            ("ssr", "msr fpsr, x"),
            ("ssr64", "msr fpsr, x"),
        ] {
            let body = instructions_of(&asm, func);
            assert!(
                body.iter().any(|l| l.starts_with(insn)),
                "{opt}: {func} has no `{insn}`:\n{body:#?}"
            );
            assert!(
                !body.iter().any(|l| l.starts_with("bl ")),
                "{opt}: {func} calls:\n{body:#?}"
            );
        }
        for (func, reg) in [("get", "fpcr"), ("get64", "fpcr"), ("gsr", "fpsr")] {
            let body = instructions_of(&asm, func);
            assert!(
                body.iter()
                    .any(|l| l.starts_with("mrs x") && l.ends_with(&format!(", {reg}"))),
                "{opt}: {func} reads no {reg}:\n{body:#?}"
            );
        }
        let order = |func: &str, wanted: &[&str]| {
            let body = instructions_of(&asm, func);
            let seen: Vec<&str> = body
                .iter()
                .filter_map(|l| wanted.iter().find(|w| l.starts_with(**w)).copied())
                .collect();
            assert_eq!(seen, wanted, "{opt}: {func}:\n{body:#?}");
        };
        order("around", &["msr fpcr", "fmul", "msr fpcr"]);
        order("flags", &["msr fpsr", "fdiv", "mrs"]);
    }
}

/// The names are aarch64's: gcc's x86-64 compiler has never heard of them,
/// so a call is an implicit declaration there, and `__has_builtin` answers
/// accordingly on each target.
#[test]
fn builtins_aarch64_fpcr_fpsr_are_aarch64_only() {
    let names = [
        "__builtin_aarch64_get_fpcr",
        "__builtin_aarch64_set_fpcr",
        "__builtin_aarch64_get_fpsr",
        "__builtin_aarch64_set_fpsr",
        "__builtin_aarch64_get_fpcr64",
        "__builtin_aarch64_set_fpcr64",
        "__builtin_aarch64_get_fpsr64",
        "__builtin_aarch64_set_fpsr64",
    ];
    let mut src = String::new();
    for name in names {
        src.push_str(&format!(
            "#if __has_builtin({name}) != defined(__aarch64__)\n#error {name}\n#endif\n"
        ));
    }
    src.push_str("int x;\n");
    for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        asm_for_at("c17_a64_fp_has_", &src, &[&format!("--target={target}")]);
    }
    // On x86-64 a call is any undeclared function's: an error, and gcc 13's
    // implicit declaration under -fpermissive.
    let call = "unsigned f(void) { return __builtin_aarch64_get_fpcr(); }\n";
    let x86 = ["--target=x86_64-unknown-linux-gnu"];
    let stderr = compile_rejected_with("a64_fpcr_on_x86", call, &x86);
    assert!(stderr.contains("'__builtin_aarch64_get_fpcr'"), "{stderr}");
    let c_file = create_c_file("a64_fpcr_permissive", call);
    let dir = crate::common::work_dir("a64_fpcr_permissive");
    let out = dir.path().join("out.s");
    let run = run_c17(&[
        x86[0],
        "-fpermissive",
        "-S",
        "-o",
        out.to_str().unwrap(),
        c_file.path().to_str().unwrap(),
    ]);
    assert!(run.success, "{}", run.stderr);
    assert!(
        run.stderr
            .contains("implicit declaration of function '__builtin_aarch64_get_fpcr'"),
        "{}",
        run.stderr
    );
    let asm = std::fs::read_to_string(&out).unwrap();
    assert!(
        asm.contains("call") && asm.contains("__builtin_aarch64_get_fpcr"),
        "{asm}"
    );
    // gcc's argument checks: the plain forms take an `unsigned int`, and
    // a read takes nothing.
    let stderr = compile_rejected_with(
        "a64_fpcr_args",
        "void f(void) { __builtin_aarch64_set_fpcr(); }\n\
         unsigned g(void) { return __builtin_aarch64_get_fpsr(1); }\n",
        &["--target=aarch64-unknown-linux-gnu"],
    );
    assert!(
        stderr.contains("too few arguments to function '__builtin_aarch64_set_fpcr'"),
        "{stderr}"
    );
    assert!(
        stderr.contains("too many arguments to function '__builtin_aarch64_get_fpsr'"),
        "{stderr}"
    );
}

/// glibc's <fpu_control.h> on aarch64 under qemu: `_FPU_GETCW` and
/// `_FPU_SETCW` round-trip the control register and change the rounding
/// direction `fegetround` reports and a division is done in; `_FPU_GETFPSR`
/// and `_FPU_SETFPSR` read and clear the flag a division by zero raises;
/// and the 64-bit forms see the same registers. Exit codes 1..=12.
#[test]
fn builtins_aarch64_fpu_control_h() {
    let src = r#"
#include <fpu_control.h>
#include <fenv.h>
volatile double one = 1.0, three = 3.0, zero = 0.0, sink;
int main(void) {
    fpu_control_t orig, cw, back;
    _FPU_GETCW(orig);
    if (orig != _FPU_DEFAULT) return 1;
    double nearest = one / three;
    cw = (orig & ~_FPU_FPCR_RM_MASK) | 0x400000;
    _FPU_SETCW(cw);
    _FPU_GETCW(back);
    if (back != cw) return 2;
    if (fegetround() != FE_UPWARD) return 3;
    double up = one / three;
    _FPU_SETCW((orig & ~_FPU_FPCR_RM_MASK) | 0x800000);
    double down = one / three;
    _FPU_SETCW(orig);
    if (!(up > down) || (nearest != up && nearest != down)) return 4;
    if (fegetround() != FE_TONEAREST || one / three != nearest) return 5;

    fpu_fpsr_t sr;
    _FPU_SETFPSR(0);
    sink = one / zero;
    _FPU_GETFPSR(sr);
    if (!(sr & FE_DIVBYZERO)) return 6;
    if (!fetestexcept(FE_DIVBYZERO)) return 7;
    _FPU_SETFPSR(0);
    _FPU_GETFPSR(sr);
    if (sr != 0 || fetestexcept(FE_ALL_EXCEPT)) return 8;

    unsigned long long c64 = __builtin_aarch64_get_fpcr64();
    if (c64 != orig) return 9;
    __builtin_aarch64_set_fpcr64(c64 | _FPU_FPCR_RM_MASK);
    if (fegetround() != FE_TOWARDZERO) return 10;
    __builtin_aarch64_set_fpcr64(c64);
    __builtin_aarch64_set_fpsr64(FE_INEXACT);
    if (__builtin_aarch64_get_fpsr64() != FE_INEXACT || !fetestexcept(FE_INEXACT)) return 11;
    feclearexcept(FE_ALL_EXCEPT);
    if (__builtin_aarch64_get_fpsr() != 0 || fegetround() != FE_TONEAREST) return 12;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64_with("a64_fpu_control", src, &[opt], &["-lm"]) {
            assert_eq!(code, 0, "{opt}");
        }
    }
}
