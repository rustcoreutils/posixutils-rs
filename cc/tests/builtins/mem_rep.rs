//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `memset` and `memcpy` of a constant length on x86-64, as gcc 13 emits
// them at every level: past what is unrolled, up to 8 KB, `rep stosq` and
// `rep movsq` with the ragged tail stored by hand; past 8 KB, the call.
// glibc's configure asks for exactly this ("checking for
// __builtin_memset") and records "no" when the output names `memset`.
//

use crate::common::{asm_for_at, compile_and_run_everywhere};

/// glibc's probe, and the cutoff on either side of it.
#[test]
fn mem_rep_constant_blocks_are_inline_up_to_8k() {
    let x86 = "--target=x86_64-unknown-linux-gnu";
    for (n, inline) in [
        (1000, true),
        (129, true),
        (8192, true),
        (8193, false),
        (65536, false),
    ] {
        let src = format!(
            "void zero(void *x) {{ __builtin_memset(x, 0, {n}); }}\n\
             void fill(void *x, int c) {{ __builtin_memset(x, c, {n}); }}\n\
             void copy(void *d, const void *s) {{ __builtin_memcpy(d, s, {n}); }}\n"
        );
        for opt in ["-O0", "-O3"] {
            let asm = asm_for_at("c17_mem_rep_", &src, &[x86, opt]);
            assert_eq!(!asm.contains("memset"), inline, "{n} {opt}:\n{asm}");
            assert_eq!(!asm.contains("memcpy"), inline, "{n} {opt}:\n{asm}");
            if inline {
                assert!(
                    asm.contains("rep stosq") && asm.contains("rep movsq"),
                    "{asm}"
                );
            }
        }
    }
}

/// What the inline blocks write: every byte asked for, none past it, at
/// every tail length, with the call's value -- the destination.
#[test]
fn mem_rep_writes_exactly_the_block() {
    let src = r#"
static unsigned char buf[9000], src[9000];
static int check_set(unsigned n, unsigned char *got, unsigned char v) {
    for (unsigned i = 0; i < 9000; i++)
        if (buf[i] != (i >= 3 && i < 3 + n ? v : 0x5a))
            return 1;
    return got != buf + 3;
}
static int check_cpy(unsigned n, unsigned char *got) {
    for (unsigned i = 0; i < 9000; i++)
        if (buf[i] != (i >= 3 && i < 3 + n ? src[i + 2] : 0x5a))
            return 1;
    return got != buf + 3;
}
static void reset(void) { for (unsigned i = 0; i < 9000; i++) buf[i] = 0x5a; }
#define SET(N, V) do { reset(); volatile int c = (V); \
    unsigned char *r = __builtin_memset(buf + 3, c, N); \
    if (check_set(N, r, (unsigned char)(V))) return __LINE__ % 200 + 1; \
    reset(); r = __builtin_memset(buf + 3, V, N); \
    if (check_set(N, r, (unsigned char)(V))) return __LINE__ % 200 + 1; } while (0)
#define CPY(N) do { reset(); unsigned char *r = __builtin_memcpy(buf + 3, src + 5, N); \
    if (check_cpy(N, r)) return __LINE__ % 200 + 1; } while (0)
int main(void) {
    for (unsigned i = 0; i < 9000; i++) src[i] = (unsigned char)(i * 7 + 1);
    SET(129, 0xab); SET(130, 0x01); SET(131, 0); SET(132, 0x80); SET(133, 0xff);
    SET(134, 0x12); SET(135, 0x34); SET(136, 0x56); SET(1000, 0xcd); SET(8192, 0xee);
    SET(8191, 0x7f);
    CPY(129); CPY(130); CPY(131); CPY(132); CPY(133); CPY(134); CPY(135); CPY(136);
    CPY(1000); CPY(8192); CPY(8189);
    return 0;
}
"#;
    compile_and_run_everywhere("mem_rep", src);
}
