//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc builtins real code uses that c17 lacked: the `_FloatN` and `q`
// spellings of fabs, copysign, inf, nan and friends (glibc's <math.h>
// reaches for them), `sqrtf128`/`fmaf128`, the position builtins
// `__builtin_FILE`/`LINE`/`FUNCTION`, `__builtin_expect_with_probability`,
// `__builtin_dynamic_object_size`, and x86's CPU detection.
//

use crate::common::{compile_and_run, compile_expect_error};
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
use crate::common::{compile_with_host_cc, create_c_file};
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
use std::process::Command;

#[test]
fn builtins_float_n_spellings() {
    let src = r#"
#include <string.h>
int main(void) {
    if (__builtin_fabsf32(-1.5f) != 1.5f) return 1;
    if (__builtin_copysignf32(2.0f, -1.0f) != -2.0f) return 2;
    if (__builtin_fabsf64(-1.25) != 1.25) return 3;
    if (__builtin_copysignf64(3.0, -0.0) != -3.0) return 4;
    if (_Generic(__builtin_fabsf32(1.0f), float: 0, default: 1)) return 5;
    if (_Generic(__builtin_fabsf64(1.0), double: 0, default: 1)) return 6;
#ifdef __FLT128_MANT_DIG__
    _Float128 a = -2.5;
    if (__builtin_fabsq(a) != 2.5 || __builtin_fabsf128(a) != 2.5) return 10;
    if (__builtin_copysignq(1.0, a) != -1.0 || __builtin_copysignf128(1, 1) != 1) return 11;
    if (__builtin_sqrtf128(16) != 4 || __builtin_fmaf128(2, 3, 4) != 10) return 12;
    _Float128 inf = __builtin_infq();
    if (!(inf > 1e300) || __builtin_huge_valq() != inf) return 13;
    _Float128 n = __builtin_nanq("5");
    if (n == n) return 14;
    unsigned long long w[2];
    memcpy(w, &n, sizeof w);
    /* The payload lands in the low bits, little-endian or not. */
    if ((w[0] & 0xff) != 5 && (w[1] & 0xff) != 5) return 15;
#endif
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_float_n", src, &["-lm".to_string()]),
        0
    );
}

/// Where the call is written: the presumed file and line, after `#line`,
/// and the enclosing function's name, or "" at file scope.
#[test]
fn builtins_position_of_the_call() {
    let src = r#"
#include <string.h>
static const char *file_scope_fn = __builtin_FUNCTION();
static int file_scope_line = __builtin_LINE();
int main(void) {
    if (strcmp(file_scope_fn, "") != 0) return 1;
    if (file_scope_line != 4) return 2;
#line 77 "renamed.c"
    const char *fl = __builtin_FILE(); int ln = __builtin_LINE();
    if (strcmp(fl, "renamed.c") != 0 || ln != 77) return 3;
    if (strcmp(__builtin_FUNCTION(), "main") != 0) return 4;
    if (__LINE__ != 80 || strcmp(__FILE__, "renamed.c") != 0) return 5;
    char buf[16];
    if (__builtin_dynamic_object_size(buf, 0) != 16) return 6;
    if (__builtin_dynamic_object_size(buf + 4, 1) != 12) return 7;
    int x = 3;
    if (!__builtin_expect_with_probability(x == 3, 1, 0.9)) return 8;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_position", src, &[]), 0);

    compile_expect_error(
        "builtins_probability_range",
        "int f(int x) { return __builtin_expect_with_probability(x, 1, 2.0); }\n",
        "probability must be a constant floating-point expression between 0 and 1",
    );
}

/// `#line` moves diagnostics, as gcc's does, not only `__LINE__`.
#[test]
fn builtins_line_directive_moves_diagnostics() {
    compile_expect_error(
        "line_moves_diagnostics",
        "int a;\n#line 77 \"renamed.c\"\nint b = undeclared_x;\n",
        "renamed.c:77:",
    );
}

/// x86-64 CPU detection reads libgcc's `__cpu_model` as gcc's code does.
#[cfg(target_arch = "x86_64")]
#[test]
fn builtins_x86_cpu_detection() {
    let src = r#"
int main(void) {
    __builtin_cpu_init();
    /* Every x86-64 CPU has SSE2, long mode and is x86-64. */
    if (!__builtin_cpu_supports("sse2")) return 1;
    if (!__builtin_cpu_supports("lm")) return 2;
    if (!__builtin_cpu_supports("cmov")) return 3;
    /* Exactly one vendor, or neither (a VM may hide it), never both. */
    if (__builtin_cpu_is("intel") && __builtin_cpu_is("amd")) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_cpu", src, &[]), 0);
    compile_expect_error(
        "builtins_cpu_bad_name",
        "int f(void) { return __builtin_cpu_supports(\"nosuch\"); }\n",
        "parameter to builtin not valid: nosuch",
    );
}

/// The CPU names c17 once lacked (and one name per `__cpu_model` field and
/// `__cpu_features2` word) answer on this machine as gcc's code answers:
/// c17 compiles one unit, the host compiler the same questions and `main`.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn builtins_x86_cpu_names_agree_with_gcc() {
    const IS: &[&str] = &[
        "core2",
        "bonnell",
        "raptorlake",
        "emeraldrapids",
        "gracemont",
        "intel",
        "amd",
        "corei7",
        "haswell",
    ];
    const SUPPORTS: &[&str] = &[
        "cmpxchg8b",
        "cmpxchg16b",
        "sse2",
        "avx2",
        "lm",
        "x86-64",
        "x86-64-v2",
        "x86-64-v3",
        "amx-complex",
    ];
    let probe = |func: &str| {
        let mut body = format!("void {func}(int *out) {{\n    __builtin_cpu_init();\n");
        let calls = IS
            .iter()
            .map(|n| ("is", n))
            .chain(SUPPORTS.iter().map(|n| ("supports", n)));
        for (i, (kind, name)) in calls.enumerate() {
            body += &format!("    out[{i}] = __builtin_cpu_{kind}(\"{name}\") != 0;\n");
        }
        body + "}\n"
    };
    let n = IS.len() + SUPPORTS.len();
    let host = format!(
        "{}void c17_probe(int *);\nint main(void) {{\n    int a[{n}], b[{n}];\n    \
         c17_probe(a);\n    gcc_probe(b);\n    for (int i = 0; i < {n}; i++)\n        \
         if (a[i] != b[i]) return 10 + i;\n    return 0;\n}}\n",
        probe("gcc_probe")
    );
    // A host compiler older than gcc 13 rejects some of these names.
    let host_file = create_c_file("cpu_names_host_check", &host);
    let knows = Command::new("cc")
        .args(["-fsyntax-only", "-w"])
        .arg(host_file.path())
        .status()
        .is_ok_and(|st| st.success());
    if !knows {
        eprintln!("skipping: the host cc does not know gcc 13's CPU names");
        return;
    }
    let rc = compile_with_host_cc("cpu_names", &probe("c17_probe"), &host);
    assert_eq!(rc, Some(0), "c17 and the host compiler disagree");
}
