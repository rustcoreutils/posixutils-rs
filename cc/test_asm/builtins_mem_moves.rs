//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/mem_moves.rs, in process.
//

use super::asm_probe::mentions;
use super::builtins_mem_expand::for_each_target;

/// What `<string.h>` and `<strings.h>` declare, spelled out: the aarch64
/// programs are built without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    void *memcpy(void *restrict, const void *restrict, size_t);\n\
    void *memmove(void *, const void *, size_t);\n\
    void *mempcpy(void *restrict, const void *restrict, size_t);\n\
    void bcopy(const void *, void *, size_t);\n\
    int memcmp(const void *, const void *, size_t);\n";

/// `mempcpy` of any length calls `memcpy` if anything, and `bcopy` calls
/// `memmove`, at every level.
#[test]
fn builtins_mem_moves_call_memcpy_and_memmove() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "void *f(void *d, const void *s, size_t n) { return mempcpy(d, s, n); }",
            "memcpy",
            "mempcpy",
        ),
        (
            "void f(const void *s, void *d, size_t n) { bcopy(s, d, n); }",
            "memmove",
            "bcopy",
        ),
    ];
    for (body, called, not) in cases {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("mem_moves_call", &src, &[], |asm, what| {
            assert!(mentions(asm, called), "{what}: no call to {called}:\n{asm}");
            assert!(!mentions(asm, not), "{what}: {not} was called:\n{asm}");
        });
    }
    // A constant length small enough calls nothing at all.
    let src = format!(
        "{PROTOTYPES}void *f(void *d, const void *s) {{ return mempcpy(d, s, 16); }}\n\
         void g(const void *s, void *d) {{ bcopy(s, d, 16); }}\n"
    );
    for_each_target("mem_moves_small", &src, &[], |asm, what| {
        for name in ["memcpy", "mempcpy", "memmove", "bcopy"] {
            assert!(!mentions(asm, name), "{what}: {name} was called:\n{asm}");
        }
    });
}

/// Optimizing, a `memmove` whose blocks cannot overlap calls `memcpy`; one
/// whose blocks might stays a `memmove`, and so does every one at `-O0`.
#[test]
fn builtins_mem_moves_disjoint_move_is_memcpy() {
    let src = format!(
        "{PROTOTYPES}\
         static const long table[20] = {{ 1, 2, 3 }};\n\
         long g[20], h[20];\n\
         void from_const(void *d) {{ memmove(d, table, sizeof table); }}\n\
         void from_literal(void *d) {{ memmove(d, \"{long}\", 150); }}\n\
         void between_locals(void (*use)(char *, char *)) {{\n\
             char a[200], b[200]; use(a, b); memmove(a, b, sizeof a); use(a, b); }}\n\
         void local_and_global(void (*use)(long *)) {{\n\
             long a[20]; use(a); memmove(g, a, sizeof a); }}\n",
        long = "x".repeat(160)
    );
    for_each_target("mem_moves_disjoint", &src, &[], |asm, what| {
        let optimizing = what.starts_with("-O2");
        assert_eq!(mentions(asm, "memcpy"), optimizing, "{what}:\n{asm}");
        assert_eq!(mentions(asm, "memmove"), !optimizing, "{what}:\n{asm}");
    });

    let src = format!(
        "{PROTOTYPES}\
         long g[20], h[20];\n\
         void unknown(void *d, const void *s) {{ memmove(d, s, 160); }}\n\
         void two_globals(void) {{ memmove(g, h, sizeof h); }}\n\
         void one_local(void (*use)(char *)) {{\n\
             char a[200]; use(a); memmove(a + 1, a, 150); use(a); }}\n"
    );
    for_each_target("mem_moves_overlap", &src, &[], |asm, what| {
        assert!(!mentions(asm, "memcpy"), "{what}:\n{asm}");
        assert!(mentions(asm, "memmove"), "{what}:\n{asm}");
    });
}
