//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 inline-asm operands pinned to one register (`"=a"`, `"=d"`) whose
// values outlive a later asm pinned to the same register.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;

/// longlong.h's `umul_ppmm`, three in a row as mpfr's `mpfr_mul_2` has them:
/// each product's halves are live while the next `mulq` writes %rax and
/// %rdx. The allocator homed every pinned output in its register for its
/// whole life, so each asm overwrote the products before it, and all three
/// read back as the last one -- mpfr's tcheck failed for every 2-limb mul.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_pinned_outputs_survive_a_later_asm_on_the_same_register() {
    let src = r#"
typedef unsigned long UDItype;
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulq\t%3" : "=a" (w0), "=d" (w1) : "%0" ((UDItype)(u)), "rm" ((UDItype)(v)))

__attribute__((noinline)) void f(const UDItype *bp, const UDItype *cp, UDItype *o) {
    UDItype h, l, u, v, sb, sb2;
    umul_ppmm(h, l, bp[1], cp[1]);
    umul_ppmm(u, v, bp[1], cp[0]);
    umul_ppmm(sb, sb2, bp[0], cp[0]);
    o[0] = h; o[1] = l; o[2] = u; o[3] = v; o[4] = sb; o[5] = sb2;
}

int main(void) {
    UDItype b[2] = { 3, 5 }, c[2] = { 7, 0x8000000000000000UL }, o[6];
    f(b, c, o);
    if (o[0] != 2 || o[1] != 0x8000000000000000UL) return 1;
    if (o[2] != 0 || o[3] != 35) return 2;
    if (o[4] != 0 || o[5] != 21) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_pinned_outputs", src, &[]), 0);
    assert_eq!(
        compile_and_run("asm_pinned_outputs_o2", src, &["-O2".to_string()]),
        0
    );
}
