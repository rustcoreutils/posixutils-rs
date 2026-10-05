//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/mem_expand.rs, in process.
//

use super::asm_probe::mentions;
use crate::test_compile::asm_for;

/// What `<string.h>` declares, spelled out: the aarch64 programs are built
/// without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    void *memcpy(void *restrict, const void *restrict, size_t);\n\
    void *memset(void *, int, size_t);\n\
    void *memmove(void *, const void *, size_t);\n";

/// Compile `body` at each level for the host and for aarch64, and hand the
/// assembly to `check`.
pub(super) fn for_each_target(prefix: &str, src: &str, extra: &[&str], check: impl Fn(&str, &str)) {
    for opt in ["-O0", "-O2"] {
        for target in [None, Some("aarch64-unknown-linux-gnu")] {
            let mut args = vec![opt];
            if target.is_some() {
                args.push("--target=aarch64-unknown-linux-gnu");
            }
            args.extend_from_slice(extra);
            let asm = asm_for(prefix, src, &args);
            check(&asm, &format!("{opt} {}", target.unwrap_or("host")));
        }
    }
}

#[test]
fn builtins_mem_expand_no_call_for_small_constant_length() {
    let cases: &[(&str, &str)] = &[
        (
            "memcpy",
            "void f(void *d, const void *s) { memcpy(d, s, 16); }\n\
             void g(void *d, const void *s) { memcpy(d, s, 7); }\n\
             void h(void *d, const void *s) { __builtin_memcpy(d, s, 128); }\n",
        ),
        (
            "memset",
            "void f(void *d) { memset(d, 0, 32); }\n\
             void g(void *d, int c) { memset(d, c, 13); }\n\
             void h(void *d) { __builtin_memset(d, 0xab, 128); }\n",
        ),
        (
            "memmove",
            "void f(void *d, const void *s) { memmove(d, s, 16); }\n\
             void g(void *d, const void *s) { __builtin_memmove(d, s, 64); }\n",
        ),
    ];
    for (name, body) in cases {
        let src = format!("{PROTOTYPES}{body}");
        for_each_target("mem_expand_small", &src, &[], |asm, what| {
            assert!(!mentions(asm, name), "{what}: {name} was called:\n{asm}");
        });
    }
}

#[test]
fn builtins_mem_expand_keeps_the_call_otherwise() {
    let cases: &[(&str, &str)] = &[
        // Above the limit.
        (
            "memcpy",
            "void f(void *d, const void *s) { memcpy(d, s, 129); }",
        ),
        ("memset", "void f(void *d) { memset(d, 0, 129); }"),
        (
            "memmove",
            "void f(void *d, const void *s) { memmove(d, s, 65); }",
        ),
        // A length only known at run time.
        (
            "memcpy",
            "void f(void *d, const void *s, size_t n) { memcpy(d, s, n); }",
        ),
        (
            "memset",
            "void f(void *d, size_t n) { __builtin_memset(d, 0, n); }",
        ),
        (
            "memmove",
            "void f(void *d, const void *s, size_t n) { memmove(d, s, n); }",
        ),
    ];
    for (name, body) in cases {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("mem_expand_call", &src, &[], |asm, what| {
            assert!(mentions(asm, name), "{what}: no call to {name}:\n{asm}");
        });
    }
}

/// `-fno-builtin-memcpy` makes the bare name an ordinary function, called
/// whatever its length; the reserved spelling is still expanded.
#[test]
fn builtins_mem_expand_fno_builtin() {
    let bare = format!("{PROTOTYPES}void f(void *d, const void *s) {{ memcpy(d, s, 8); }}\n");
    for_each_target(
        "mem_expand_nb",
        &bare,
        &["-fno-builtin-memcpy"],
        |asm, what| {
            assert!(mentions(asm, "memcpy"), "{what}: no call to memcpy:\n{asm}");
        },
    );
    for_each_target(
        "mem_expand_nb_all",
        &bare,
        &["-fno-builtin"],
        |asm, what| {
            assert!(mentions(asm, "memcpy"), "{what}: no call to memcpy:\n{asm}");
        },
    );
    let reserved = "void f(void *d, const void *s) { __builtin_memcpy(d, s, 8); }\n";
    for_each_target(
        "mem_expand_nb_res",
        reserved,
        &["-fno-builtin"],
        |asm, what| {
            assert!(
                !mentions(asm, "memcpy"),
                "{what}: memcpy was called:\n{asm}"
            );
        },
    );
}

/// A length that becomes a constant only once a function is inlined is
/// expanded too.
#[test]
fn builtins_mem_expand_after_inlining() {
    let src = format!(
        "{PROTOTYPES}\
               static void cp(void *d, const void *s, size_t n) {{ memcpy(d, s, n); }}\n\
               void f(void *d, const void *s) {{ cp(d, s, 24); }}\n"
    );
    let asm = asm_for("mem_expand_inl", &src, &["-O2"]);
    assert!(!mentions(&asm, "memcpy"), "memcpy was called:\n{asm}");
}

/// A copy between locals is ordinary loads and stores, which the optimizer
/// forwards: the copied value folds into the arithmetic after it, and the
/// function returns a constant.
#[test]
fn builtins_mem_expand_forwards_through_locals() {
    let src = format!(
        "{PROTOTYPES}unsigned f(void) {{ unsigned x = 5, y; memcpy(&y, &x, sizeof y); return y + 1; }}\n"
    );
    for (target, six) in [(None, "$6"), (Some("aarch64-unknown-linux-gnu"), "#6")] {
        let mut args = vec!["-O2"];
        if target.is_some() {
            args.push("--target=aarch64-unknown-linux-gnu");
        }
        let asm = asm_for("mem_expand_fwd", &src, &args);
        assert!(!mentions(&asm, "memcpy"), "memcpy was called:\n{asm}");
        if target.is_some() || cfg!(target_arch = "x86_64") {
            assert!(asm.contains(six), "the copy did not fold to 6:\n{asm}");
        }
    }
}
