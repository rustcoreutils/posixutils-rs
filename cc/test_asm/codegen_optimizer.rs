//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/optimizer.rs, in process: an exactly
// sized string initializer stores no terminator, and an aarch64 prologue does
// not grow with its frame, and a narrow constant is extended at compile time.
//

use crate::test_asm::asm_probe::{asm_for_at, asm_for_with, body_of, X86_64_LINUX};

/// A narrow constant is extended when it is compiled, not when it runs.
///
/// `char buf[16] = {0}` spent a `shll $24; sarl $24` pair on the zero byte it
/// stored, even at `-O2`: the copy of the constant to a `char` survived, and
/// the back end sign-extended whatever it held at run time. Copy propagation
/// now stores the constant directly. (The program half, the values a
/// narrowing conversion produces, is in tests/codegen/optimizer.rs.)
#[test]
fn codegen_a_narrow_constant_is_extended_at_compile_time() {
    let src = "void use(char *);\nvoid zeroed(void) { char buf[16] = {0}; use(buf); }\n";
    let asm = asm_for_with("narrow_const", X86_64_LINUX, src, &["-O2"]);
    let body = body_of(&asm, "zeroed");
    assert!(!body.contains("sarl $24"), "{body}");
    assert!(!body.contains("shll $24"), "{body}");
}

/// C17 6.7.9p14 lets an array of character type be exactly as long as the
/// string literal initializing it, dropping the terminating null:
/// `char b[2] = "hi"` holds two characters and no terminator.
///
/// The store loop wrote the null unconditionally, one byte past the object.
/// A runtime check cannot see that -- the byte lands in stack padding -- so
/// this counts the stores instead. Both spellings share one routine, so both
/// are pinned; the six-byte case shows the terminator and the zero fill are
/// still written when there is room for them.
#[test]
fn codegen_exactly_sized_string_initializer_stays_in_bounds() {
    let src = r#"
void sink(char *);
void exact(void)  { char b[2] = "hi";   sink(b); }
void braced(void) { char b[2] = {"hi"}; sink(b); }
void roomy(void)  { char b[6] = "hi";   sink(b); }
"#;
    let asm = crate::test_asm::asm_probe::asm_for(
        "exact_string_init",
        crate::test_asm::asm_probe::X86_64_LINUX,
        src,
    );
    for func in ["exact", "braced"] {
        assert_eq!(
            crate::test_asm::asm_probe::count_in_body(&asm, func, "movb"),
            2,
            "{func}: char b[2] = \"hi\" must store exactly two bytes, not three\n{asm}"
        );
    }
    // Six bytes of room: two characters, then the null and the zero fill.
    assert!(
        crate::test_asm::asm_probe::count_in_body(&asm, "roomy", "movb") > 2,
        "roomy: a longer array must still get its terminator\n{asm}"
    );
}

/// A prologue's size does not depend on its frame's. It once zeroed the
/// whole locals area, unrolled -- a megabyte of locals was 125,000 lines of
/// assembly -- and stores nothing into the frame at all now: C gives an
/// uninitialized object no value, and every value is read back at the width
/// it was stored at.
#[test]
fn codegen_aarch64_prologue_does_not_grow_with_the_frame() {
    let target = ["--target=aarch64-unknown-linux-gnu"];
    let big = asm_for_at(
        "a64_big_prologue",
        "long f(void) { volatile char a[1000000]; a[0] = 1; return a[0]; }\n",
        &target,
    );
    assert!(
        big.lines().count() < 200,
        "a 1 MB frame's prologue should not grow with the frame: {} lines",
        big.lines().count()
    );
    assert!(
        !big.contains("xzr, xzr"),
        "nothing zeroes the frame:\n{big}"
    );
}
