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
// (sincosf_poly.h) and `__builtin_thread_pointer` (thread_pointer.h), each
// as gcc 13 has it on its targets.
//

use crate::common::{asm_for_at, compile_and_run_everywhere, compile_expect_error};

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
