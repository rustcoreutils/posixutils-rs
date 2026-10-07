//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A register output of an asm statement never shares a register with the
// address of one of its memory outputs: the template writes through that
// address, possibly after it has written the register output.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// gprofng's `__collector_subget_32`, as libgp-collector.so has it. The
/// output `r` and the address of `*ptr` were given one register, so `negl`
/// overwrote the address and `xaddl` wrote to `-len`: every `gprofng collect`
/// run died in the collector before the program it profiled started.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_register_output_keeps_off_a_memory_outputs_address() {
    let src = r#"
#include <stdint.h>
struct buf { char *p; uint32_t left; uint32_t state; };
struct h { struct buf *bufs; };

static inline __attribute__((always_inline)) uint32_t
subget(uint32_t *ptr, uint32_t off) {
    uint32_t r;
    __asm__ __volatile__("movl %2, %0; negl %0; lock; xaddl %0, %1"
                         : "=r"(r), "=m"(*ptr) : "a"(off), "r"(*ptr));
    return r - off;
}

__attribute__((noinline)) int put(struct h *h, int i, int len, int *idx) {
    struct buf *b = &h->bufs[idx[i]];
    __builtin_memcpy(b->p, "xyzw", 4);
    return subget(&b->left, len) == 0;
}

int main(void) {
    static char mem[16];
    struct buf bs[2] = {{mem, 10, 0}, {mem, 7, 0}};
    struct h h = {bs};
    int idx[2] = {1, 0};
    if (put(&h, 0, 7, idx) != 1) return 1;
    if (bs[1].left != 0 || bs[0].left != 10) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_mem_output_address", src, &[]), 0);
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("asm_mem_output_address{level}"),
                src,
                &[level.to_string()]
            ),
            0,
            "{level}"
        );
    }
}

/// The same rule on aarch64: the template writes `%w0` and then stores it
/// through `%1`.
#[test]
fn asm_register_output_keeps_off_a_memory_outputs_address_aarch64() {
    let src = r#"
__attribute__((noinline)) unsigned put(unsigned **pp, int i) {
    unsigned r;
    unsigned *p = pp[i];
    __asm__ __volatile__("mov %w0, #7\n\tstr %w0, %1" : "=r"(r), "=m"(*p));
    return r;
}

int main(void) {
    unsigned a = 0, b = 0;
    unsigned *pp[2] = {&a, &b};
    if (put(pp, 1) != 7) return 1;
    if (b != 7 || a != 0) return 2;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("asm_mem_output_address_a64", src, level) {
            assert_eq!(rc, 0, "{level}");
        }
    }
}
