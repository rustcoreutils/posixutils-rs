//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Named arguments that overflow their registers, on both aarch64 platforms:
// the assembly half, compiled in process. The interop half, c17 against
// the platform's own compiler, is `tests/codegen/stacked_args.rs`, whose
// header gives the two conventions.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX};

/// Eight ints and eight doubles use up x0-x7 and v0-v7, so the six
/// arguments after them are stacked.
const MIXED: &str = r#"
int take(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c, short s, char c2, int i, double d, char c3)
{ return c + s + c2 + i + (int)d + c3; }

/* Declared only, so the optimizer cannot fold the call away. */
int sink(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c, short s, char c2, int i, double d, char c3);

int give(void)
{ return sink(0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7,
              'a', 300, 'b', 70000, 2.5, 'c'); }
"#;

/// The SP offset of every store in `body` that writes to the outgoing
/// argument area, with its mnemonic, in program order.
fn sp_stores(body: &str) -> Vec<(String, i32)> {
    body.lines()
        .filter_map(|line| {
            let line = line.trim();
            let (op, rest) = line.split_once(' ')?;
            if !op.starts_with("str") || op.starts_with("stp") {
                return None;
            }
            let addr = rest.split_once("[sp")?.1;
            let off = match addr.strip_prefix(", #") {
                Some(tail) => tail.trim_end_matches(']').parse().ok()?,
                None if addr.starts_with(']') => 0,
                None => return None,
            };
            Some((op.to_string(), off))
        })
        .collect()
}

/// The caller writes each stacked argument at the platform's offset, and no
/// wider than its slot: on Apple the next argument begins at the very next
/// byte, so a `char` written as a doubleword would be overwritten -- or,
/// last in the area, would overwrite the caller's own frame.
#[test]
fn codegen_aarch64_stacked_named_arguments_caller() {
    for opt in ["-O0", "-O2"] {
        let darwin = asm_for_with("stacked_mixed_darwin", AARCH64_DARWIN, MIXED, &[opt]);
        assert_eq!(
            sp_stores(body_of(&darwin, "give")),
            [
                ("strb".to_string(), 0),
                ("strh".to_string(), 2),
                ("strb".to_string(), 4),
                ("str".to_string(), 8),
                ("str".to_string(), 16),
                ("strb".to_string(), 24),
            ],
            "Apple packs stacked arguments at their natural size and alignment ({opt}):\n{darwin}"
        );

        let linux = asm_for_with("stacked_mixed_linux", AARCH64_LINUX, MIXED, &[opt]);
        let offsets: Vec<i32> = sp_stores(body_of(&linux, "give"))
            .into_iter()
            .map(|(_, off)| off)
            .collect();
        assert_eq!(
            offsets,
            [0, 8, 16, 24, 32, 40],
            "AAPCS64 gives each stacked argument an eight-byte granule ({opt}):\n{linux}"
        );
    }
}

/// The callee reads each stacked parameter back where its caller put it:
/// the incoming area starts at x29+16, past the saved frame record.
#[test]
fn codegen_aarch64_stacked_named_arguments_callee() {
    for (triple, offsets) in [
        (AARCH64_DARWIN, [16, 18, 20, 24, 32, 40]),
        (AARCH64_LINUX, [16, 24, 32, 40, 48, 56]),
    ] {
        let asm = asm_for_with("stacked_mixed_callee", triple, MIXED, &["-O0"]);
        let body = body_of(&asm, "take");
        for off in offsets {
            assert!(
                body.contains(&format!("[x29, #{off}]")),
                "{triple}: a stacked parameter is read at x29+{off}:\n{body}"
            );
        }
    }
    // And nothing between Apple's packed slots: +17 would be the char read
    // as if the short that follows it began there.
    let asm = asm_for_with("stacked_mixed_callee", AARCH64_DARWIN, MIXED, &["-O0"]);
    assert!(!body_of(&asm, "take").contains("[x29, #17]"));
}

/// A Darwin variadic call stacks its named overflow packed and the variadic
/// arguments after it, each on an eight-byte granule; the callee's
/// `va_start` has to point past the named ones. It pointed at the start of
/// the area, so the first `va_arg` read the named `char`.
#[test]
fn codegen_darwin_variadics_follow_packed_named_arguments() {
    let src = r#"
int vf(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7, char c, ...);
int call(void) { return vf(0, 1, 2, 3, 4, 5, 6, 7, 'c', 5, 6L); }
"#;
    let asm = asm_for_with("darwin_va_after_named", AARCH64_DARWIN, src, &["-O0"]);
    assert_eq!(
        sp_stores(body_of(&asm, "call")),
        [
            ("strb".to_string(), 0),
            ("str".to_string(), 8),
            ("str".to_string(), 16)
        ],
        "one packed byte, then each variadic argument on a granule:\n{asm}"
    );
}

/// A Darwin variadic call that also returns a struct in memory keeps its
/// named arguments in registers.
///
/// `variadic_arg_start` counts the call's *own* arguments, but `insn.src`
/// carries the hidden sret pointer ahead of them, so the two are one apart
/// for a call that returns in memory. `setup_darwin_variadic_args` compared
/// them directly -- `.max(args_start)` raises the index but never shifts it
/// -- so the named range came out empty and every argument, `n` included,
/// was stacked as though it were variadic:
///
/// ```text
///     str x9, [sp]        ; 7, the named argument, belongs in w0
///     str x9, [sp, #8]    ; 11
///     str x9, [sp, #16]   ; 22
/// ```
///
/// Apple clang emits `mov w0, #7` and stacks only the two variadic ones, so
/// the callee read garbage for `n`. The non-sret call beside it is the
/// control: it was always right, and `args_start` is zero there, so the fix
/// must not move it.
#[test]
fn codegen_darwin_sret_variadic_keeps_its_named_argument_in_a_register() {
    let src = r#"
struct Big { long a, b, c, d; };
struct Big f(int n, ...);
struct Big sret_call(void) { return f(7, 11, 22); }

int g(int n, ...);
int plain_call(void) { return g(7, 11, 22); }
"#;
    let asm = asm_for_with("darwin_sret_va", AARCH64_DARWIN, src, &["-O0"]);

    // The named argument is in w0, and only the two variadic ones are stacked.
    let body = body_of(&asm, "sret_call");
    assert!(
        body.contains("movz w0, #7") || body.contains("mov w0, #7"),
        "the named argument of an sret variadic call goes in w0:\n{body}"
    );
    assert_eq!(
        sp_stores(body).len(),
        2,
        "only the two variadic arguments are stacked:\n{body}"
    );

    // The control: without sret the same call was already correct.
    let body = body_of(&asm, "plain_call");
    assert!(
        body.contains("movz w0, #7") || body.contains("mov w0, #7"),
        "a non-sret variadic call still passes its named argument in w0:\n{body}"
    );
    assert_eq!(
        sp_stores(body).len(),
        2,
        "and still stacks only the variadic ones:\n{body}"
    );
}
