//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// mempcpy and bcopy, and the memmove that is a memcpy
//
// `mempcpy` is a `memcpy` and an addition, whatever its length, and never a
// call to `mempcpy`; `bcopy` is a `memmove` with its arguments the other way
// round. Once optimizing, a `memmove` whose source is a string literal or a
// `const` object, or whose two blocks are different local objects, is a
// `memcpy`: the blocks cannot overlap.
//

// The assembly cases are unit tests in cc/test_asm/builtins_mem_moves.rs.

use crate::common::{compile_and_run, compile_and_run_aarch64};

/// What `<string.h>` and `<strings.h>` declare, spelled out: the aarch64
/// programs are built without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    void *memcpy(void *restrict, const void *restrict, size_t);\n\
    void *memmove(void *, const void *, size_t);\n\
    void *mempcpy(void *restrict, const void *restrict, size_t);\n\
    void bcopy(const void *, void *, size_t);\n\
    int memcmp(const void *, const void *, size_t);\n";

/// Every shape against the library's answer, returning the number of the
/// first check that fails.
fn correctness_program() -> String {
    let mut src = String::from(PROTOTYPES);
    src.push_str(
        r#"
static const struct rec { const char *s; double d; long l; } table[] = {
    { "a", 1.0, 1 }, { "b", 2.0, 2 }, { "c", 3.0, 3 },
    { "d", 4.0, 4 }, { "e", 5.0, 5 }, { "f", 6.0, 6 },
};
static const int ints[40] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, [39] = 40 };
char g[64] = "0123456789abcdefghijklmnopqrstuvwxyz";
size_t one = 1, zero = 0, five = 5;

int main(void) {
    char p[32] = "", *r;
    struct rec copy[6];
    int bz[40];
    char a[200], b[200];

    /* mempcpy: a constant length, a variable one, zero, and chained. */
    if (mempcpy(p, "ABCDE", 6) != p + 6 || memcmp(p, "ABCDE", 6)) return 1;
    if (mempcpy(p + 1, "xyzuv", five) != p + 6 || memcmp(p, "Axyzuv", 7)) return 2;
    r = mempcpy(p + 3, "!", zero);
    if (r != p + 3 || memcmp(p, "Axyzuv", 7)) return 3;
    if (mempcpy(mempcpy(p, "abcdEFG", 4), "efg", 4) != p + 8
        || memcmp(p, "abcdefg", 8)) return 4;
    if (__builtin_mempcpy(p, "QRS", one) != p + 1 || p[0] != 'Q' || p[1] != 'b')
        return 5;

    /* bcopy: source first, and right when the blocks overlap either way. */
    bcopy("hello", p, 6);
    if (memcmp(p, "hello", 6)) return 6;
    bcopy(g, g + 2, 10);
    if (memcmp(g, "010123456789cdef", 16)) return 7;
    bcopy(g + 4, g + 1, five);
    if (memcmp(g, "023456456789cdef", 16)) return 8;
    __builtin_bcopy(ints, bz, sizeof ints);
    if (memcmp(bz, ints, sizeof ints)) return 9;
    bcopy(g, p, zero);
    if (p[0] != 'h') return 10;

    /* memmove out of a const object and a literal, longer than any
       expansion, and between two locals. */
    if (memmove(copy, table, sizeof table) != copy
        || memcmp(copy, table, sizeof table)) return 11;
    if (copy[5].l != 6 || copy[2].s[0] != 'c') return 12;
    for (int i = 0; i < 200; i++) b[i] = (char)i;
    if (memmove(a, b, sizeof a) != a || memcmp(a, b, sizeof a)) return 13;
    if (memmove(a, "literal", 8) != a || memcmp(a, "literal", 8)) return 14;

    /* A move within one object is still a move. */
    memmove(a + 1, a, 150);
    if (a[0] != 'l' || a[1] != 'l' || a[2] != 'i' || a[150] != (char)149) return 15;
    memmove(a, a + 1, 150);
    if (a[0] != 'l' || a[1] != 'i' || a[149] != (char)149) return 16;
    return 0;
}
"#,
    );
    src
}

#[test]
fn builtins_mem_moves_match_the_library() {
    let code = correctness_program();
    for opt in ["-O0", "-O2"] {
        let tag = format!("mem_moves{}", opt.replace('-', "_"));
        assert_eq!(
            compile_and_run(&tag, &code, &[opt.to_string()]),
            0,
            "host {opt}"
        );
    }
}

#[test]
fn builtins_mem_moves_match_the_library_aarch64() {
    let code = correctness_program();
    for opt in ["-O0", "-O2"] {
        let tag = format!("mem_moves_a64{}", opt.replace('-', "_"));
        if let Some(rc) = compile_and_run_aarch64(&tag, &code, opt) {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

/// The unit's own `mempcpy` or `bcopy` is the one called, with its
/// arguments in the order the program wrote them.
#[test]
fn builtins_mem_moves_own_definition_is_called() {
    let src = format!(
        "{PROTOTYPES}\
         int calls;\n\
         int main(void) {{\n\
             char a[8] = \"abcdefg\", b[8] = \"\";\n\
             calls = 0;\n\
             if (mempcpy(b, a, 3) != b + 3 || b[2] != 'c') return 1;\n\
             bcopy(a + 4, b, 2);\n\
             if (b[0] != 'e' || b[1] != 'f' || b[2] != 'c') return 2;\n\
             return calls == 2 ? 0 : 3;\n\
         }}\n\
         void *mempcpy(void *restrict d, const void *restrict s, size_t n) {{\n\
             char *dd = d; const char *ss = s; calls++;\n\
             while (n--) *dd++ = *ss++;\n\
             return dd;\n\
         }}\n\
         void bcopy(const void *s, void *d, size_t n) {{\n\
             char *dd = d; const char *ss = s; calls++;\n\
             while (n--) *dd++ = *ss++;\n\
         }}\n"
    );
    for opt in ["-O0", "-O2"] {
        let tag = format!("mem_moves_own{}", opt.replace('-', "_"));
        assert_eq!(
            compile_and_run(&tag, &src, &[opt.to_string()]),
            0,
            "host {opt}"
        );
        let tag = format!("mem_moves_own_a64{}", opt.replace('-', "_"));
        if let Some(rc) = compile_and_run_aarch64(&tag, &src, opt) {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}
