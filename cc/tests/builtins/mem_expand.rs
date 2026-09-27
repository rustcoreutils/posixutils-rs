//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// memcpy, memset and memmove of a small constant length, expanded in place
//
// Each is a sequence of loads and stores rather than a call into libc when
// its length is a constant no larger than the inline limit (128 bytes for
// memcpy and memset, 64 for memmove), whether spelled bare or `__builtin_`.
// A longer or a variable length is still a call.
//

use crate::common::{asm_for_at, compile_and_run, compile_and_run_aarch64};

/// What `<string.h>` declares, spelled out: the aarch64 programs are built
/// without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    void *memcpy(void *restrict, const void *restrict, size_t);\n\
    void *memset(void *, int, size_t);\n\
    void *memmove(void *, const void *, size_t);\n";

/// The lengths the correctness program tries: every one up to 72, which
/// crosses every chunk boundary more than once, and a few around each limit.
const LENGTHS: &[u32] = &[
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 99,
    127, 128, 129, 200,
];

/// A program that checks every length in [`LENGTHS`] byte for byte against a
/// reference loop: every destination byte, and the guard bytes on both sides
/// that must be left alone, at four destination alignments.
fn correctness_program() -> String {
    let mut src = String::from(PROTOTYPES);
    src.push_str(
        r#"
int printf(const char *, ...);
int memcmp(const void *, const void *, size_t);

#define SZ 300
#define GUARD 0xee
static unsigned char src[SZ], buf[SZ], ref[SZ];

static void reset(void) {
    for (int i = 0; i < SZ; i++) {
        src[i] = (unsigned char)(i * 7 + 1);
        buf[i] = GUARD;
    }
}

/* buf[at, at+n) holds want[0, n), and every other byte is still GUARD. */
static int copied(int at, int n, const unsigned char *want) {
    for (int i = 0; i < SZ; i++) {
        unsigned char w = (i >= at && i < at + n) ? want[i - at] : GUARD;
        if (buf[i] != w) return 1;
    }
    return 0;
}

static int filled(int at, int n, unsigned char v) {
    for (int i = 0; i < SZ; i++) {
        unsigned char w = (i >= at && i < at + n) ? v : GUARD;
        if (buf[i] != w) return 1;
    }
    return 0;
}

/* The same move as memmove, a byte at a time through a temporary. */
static void ref_move(int d, int s, int n) {
    unsigned char t[SZ];
    for (int i = 0; i < n; i++) t[i] = ref[s + i];
    for (int i = 0; i < n; i++) ref[d + i] = t[i];
}

static void pattern(void) {
    for (int i = 0; i < SZ; i++) buf[i] = ref[i] = (unsigned char)(i * 13 + 5);
}

static int same(void) { return memcmp(buf, ref, SZ) == 0; }

#define CASE(N)                                                              \
static int case_##N(int vb) {                                                \
    for (int at = 0; at < 4; at++) {                                         \
        reset();                                                             \
        if (memcpy(buf + at, src + 3 - at, N) != buf + at) return 1;         \
        if (copied(at, N, src + 3 - at)) return 2;                           \
        reset();                                                             \
        if (__builtin_memcpy(buf + at, src + at, N) != buf + at) return 3;   \
        if (copied(at, N, src + at)) return 4;                               \
        reset();                                                             \
        if (memset(buf + at, 0, N) != buf + at) return 5;                    \
        if (filled(at, N, 0)) return 6;                                      \
        reset();                                                             \
        memset(buf + at, 0x80, N);                                           \
        if (filled(at, N, 0x80)) return 7;                                   \
        reset();                                                             \
        __builtin_memset(buf + at, 0xff, N);                                 \
        if (filled(at, N, 0xff)) return 8;                                   \
        reset();                                                             \
        memset(buf + at, 0x15a, N);                                          \
        if (filled(at, N, 0x5a)) return 9;                                   \
        reset();                                                             \
        if (memset(buf + at, vb, N) != buf + at) return 10;                  \
        if (filled(at, N, (unsigned char)vb)) return 11;                     \
        static const int dist[] = { 1, 3, 8, 9 };                            \
        for (int k = 0; k < 4; k++) {                                        \
            int d = dist[k];                                                 \
            pattern();                                                       \
            if (memmove(buf + at + d, buf + at, N) != buf + at + d)          \
                return 12;                                                   \
            ref_move(at + d, at, N);                                         \
            if (!same()) return 13;                                          \
            pattern();                                                       \
            if (__builtin_memmove(buf + at, buf + at + d, N) != buf + at)    \
                return 14;                                                   \
            ref_move(at, at + d, N);                                         \
            if (!same()) return 15;                                          \
        }                                                                    \
        pattern();                                                           \
        memmove(buf + at, buf + at, N);                                      \
        if (!same()) return 16;                                              \
    }                                                                        \
    {                                                                        \
        unsigned char a[N + 1], b[N + 1];                                    \
        memcpy(a, src + 5, N);                                               \
        memcpy(b, a, N);                                                     \
        for (int i = 0; i < N; i++)                                          \
            if (b[i] != src[5 + i]) return 17;                               \
        memset(a, vb, N);                                                    \
        for (int i = 0; i < N; i++)                                          \
            if (a[i] != (unsigned char)vb) return 18;                        \
    }                                                                        \
    return 0;                                                                \
}
"#,
    );
    for n in LENGTHS {
        src.push_str(&format!("CASE({n})\n"));
    }
    src.push_str(
        r#"
int main(void) {
    static volatile int bytes[] = { 0x3c7f, 0x80, -1, 0 };
    for (int v = 0; v < 4; v++) {
        int vb = bytes[v];
        int rc;
"#,
    );
    for n in LENGTHS {
        src.push_str(&format!(
            "        if ((rc = case_{n}(vb)) != 0) {{ printf(\"N={n} vb=%#x step %d\\n\", vb, rc); return 1; }}\n"
        ));
    }
    src.push_str(
        r#"    }

    /* A copy that reinterprets: the bits arrive whole, whatever the types. */
    double d = 1.5;
    unsigned long long u;
    memcpy(&u, &d, sizeof u);
    if (u != 0x3ff8000000000000ULL) return 2;
    float f;
    unsigned int w = 0x40490fdbu;
    memcpy(&f, &w, 4);
    if (f != 3.14159274101257324f) return 3;
    struct S { char c; short s; int i; long long l; } x = { 1, 2, 3, 4 }, y;
    memcpy(&y, &x, sizeof x);
    if (y.c != 1 || y.s != 2 || y.i != 3 || y.l != 4) return 4;
    return 0;
}
"#,
    );
    src
}

#[test]
fn builtins_mem_expand_every_small_length() {
    let code = correctness_program();
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("mem_expand{}", opt.replace('-', "_")),
                &code,
                &[opt.to_string()]
            ),
            0,
            "host {opt}"
        );
    }
}

#[test]
fn builtins_mem_expand_every_small_length_aarch64() {
    let code = correctness_program();
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64(
            &format!("mem_expand_a64{}", opt.replace('-', "_")),
            &code,
            opt,
        ) {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

/// Whether the assembly for `src` mentions the library function `name` at
/// all -- a call, a tail jump or an address taken.
fn mentions(asm: &str, name: &str) -> bool {
    asm.lines()
        .filter(|l| !l.trim_start().starts_with(".file"))
        .any(|l| {
            l.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                .any(|w| w == name || w.strip_prefix('_') == Some(name))
        })
}

/// Compile `body` at each level for the host and for aarch64, and hand the
/// assembly to `check`.
fn for_each_target(prefix: &str, src: &str, extra: &[&str], check: impl Fn(&str, &str)) {
    for opt in ["-O0", "-O2"] {
        for target in [None, Some("aarch64-unknown-linux-gnu")] {
            let mut args = vec![opt];
            if let Some(t) = target {
                args.extend(["--target", t]);
            }
            args.extend_from_slice(extra);
            let asm = asm_for_at(prefix, src, &args);
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
    let asm = asm_for_at("mem_expand_inl", &src, &["-O2"]);
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
        if let Some(t) = target {
            args.extend(["--target", t]);
        }
        let asm = asm_for_at("mem_expand_fwd", &src, &args);
        assert!(!mentions(&asm, "memcpy"), "memcpy was called:\n{asm}");
        if target.is_some() || cfg!(target_arch = "x86_64") {
            assert!(asm.contains(six), "the copy did not fold to 6:\n{asm}");
        }
    }
}

/// A declaration that disagrees with the library's prototype makes the name
/// an ordinary function, which is then called.
#[test]
fn builtins_mem_expand_incompatible_declaration_is_ordinary() {
    let src = r#"
static int calls;
char *memcpy(char *d, char *s, int n) { calls++; while (n--) d[n] = s[n]; return d; }
int main(void) {
    char a[4] = "abc", b[4];
    memcpy(b, a, 4);
    return calls == 1 && b[1] == 'b' ? 0 : 1;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("mem_expand_decl{}", opt.replace('-', "_")),
                src,
                &[opt.to_string()]
            ),
            0,
            "{opt}"
        );
    }
}

/// This translation unit's own non-weak definition of `memcpy` is the one
/// called, compatible prototype and all, wherever it is -- above its caller
/// or below it, as for `sqrt`: glibc's fortify headers define an
/// `always_inline` `gnu_inline` wrapper that checks the object size, and it
/// must not be bypassed.
#[test]
fn builtins_mem_expand_own_definition_is_called() {
    let body = "{ char *dd = d; const char *ss = s; calls++; \
                while (n--) *dd++ = *ss++; return d; }";
    for (name, qualifiers, below) in [
        ("plain", "", false),
        (
            "wrapper",
            "__attribute__((gnu_inline, always_inline)) extern inline ",
            false,
        ),
        ("static", "static inline ", false),
        ("below", "", true),
    ] {
        let def = format!(
            "{qualifiers}void *memcpy(void *restrict d, const void *restrict s, size_t n)\n{body}\n"
        );
        let (above, after) = if below {
            (String::new(), def)
        } else {
            (def, String::new())
        };
        let src = format!(
            "typedef unsigned long size_t;\n\
             int calls;\n\
             {above}\
             int main(void) {{\n\
                 char a[8] = \"abcdefg\", b[8];\n\
                 /* A static link's libc calls a global one before main. */\n\
                 calls = 0;\n\
                 if (memcpy(b, a, 8) != b || b[6] != 'g') return 1;\n\
                 return calls == 1 ? 0 : 2;\n\
             }}\n\
             {after}"
        );
        for opt in ["-O0", "-O2"] {
            let tag = format!("mem_expand_own_{name}{}", opt.replace('-', "_"));
            assert_eq!(
                compile_and_run(&tag, &src, &[opt.to_string()]),
                0,
                "{name} {opt}"
            );
            if let Some(rc) = compile_and_run_aarch64(&format!("{tag}_a64"), &src, opt) {
                assert_eq!(rc, 0, "aarch64 {name} {opt}");
            }
        }
    }
}
