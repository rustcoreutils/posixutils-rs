//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Operands pinned to one register (x86-64 `"a"` ... `"D"`) that cannot all
// have it: gcc refuses the statement with "'asm' operand has impossible
// constraints", and c17 accepted it, moving two values into one register so
// the template read the second twice. Inputs of one value may share the
// register, which gcc finds out by optimizing: refused at -O0, accepted from
// -O1. Every verdict here is gcc 13's.
//

use super::asm_constraints::{expect_accepted, expect_rejected, X86};

const IMPOSSIBLE: &str = "'asm' operand has impossible constraints";

/// `f(a, b)` with `stmt` as its body before `return x;`, the asm on line 3.
fn body(stmt: &str) -> String {
    format!("int f(int a, int b) {{\n    int x = a, y = b;\n    {stmt}\n    return x + y;\n}}\n")
}

/// Statements gcc refuses at every level.
#[test]
fn asm_pinned_registers_impossible_at_every_level() {
    let cases = [
        // Two inputs need %eax with different values: the issue's shape.
        r#"__asm__("" : "=a"(x) : "0"(a), "a"(b));"#,
        r#"__asm__("" : "=r"(x) : "a"(a), "a"(b));"#,
        r#"__asm__("" : "+a"(x) : "a"(b));"#,
        r#"__asm__("" : "=a"(x), "=a"(y) : "r"(a));"#,
        // An early-clobber output shares its register with no other input.
        r#"__asm__("" : "=&a"(x) : "a"(b));"#,
        // A clobbered register holds no operand.
        r#"__asm__("" : "=a"(x) : "0"(a), "r"(b) : "eax");"#,
        r#"__asm__("" : "=r"(x) : "a"(a) : "eax");"#,
        r#"__asm__("" : "=a"(x) : "r"(a) : "rax");"#,
        r#"__asm__("" : "=a"(x) : "a"(a), "c"(b) : "ecx");"#,
        r#"__asm__("" : "=a"(x), "=r"(y) : "0"(a), "1"(b), "a"(b));"#,
        // A register variable in %eax and another value there.
        r#"register int r __asm__("eax") = a; __asm__("" : "=a"(x) : "r"(r), "a"(b));"#,
        // A memory alternative: gcc refuses this from -O1, where `a` is in
        // a register, and takes it at -O0, where it is on the stack. c17
        // never moves a register-pinned value to memory to free its
        // register, so it refuses it at -O0 too.
        r#"__asm__("" : "=a"(x) : "am"(a), "a"(b));"#,
        // Two floating constants are one value only with one encoding: not
        // 1.0f and 2.0f, and not 1.0f and 1.0, whose bits differ.
        r#"__asm__("" : "=r"(x) : "a"(1.0f), "a"(2.0f));"#,
        r#"__asm__("" : "=r"(x) : "a"(1.0f), "a"(1.0));"#,
        r#"__asm__("" : "=r"(x) : "a"(-1), "a"(-1L));"#,
    ];
    for (n, stmt) in cases.iter().enumerate() {
        for opt in ["-O0", "-O1", "-O2"] {
            let name = format!("asm_pin_impossible_{n}");
            expect_rejected(&name, &body(stmt), X86, opt, IMPOSSIBLE, 3);
        }
    }
}

/// Inputs of one value in one register: gcc gives each operand its own
/// pseudo at -O0 and refuses them, and accepts once the optimizer has made
/// them one.
#[test]
fn asm_pinned_registers_one_value_shares_when_optimizing() {
    let cases = [
        r#"__asm__("" : "=a"(x) : "0"(a), "a"(a));"#,
        r#"__asm__("" : "+a"(x) : "a"(x));"#,
        r#"__asm__("" : "=a"(x) : "a"(1), "a"(1));"#,
        r#"__asm__("" : "=r"(x) : "a"(a + 1), "a"(a + 1));"#,
        r#"__asm__("" : "=a"(x) : "a"(a), "a"(a));"#,
        r#"__asm__("" : "=r"(x) : "a"(1.5f), "a"(1.5f));"#,
    ];
    for (n, stmt) in cases.iter().enumerate() {
        let name = format!("asm_pin_share_{n}");
        expect_rejected(&name, &body(stmt), X86, "-O0", IMPOSSIBLE, 3);
        for opt in ["-O1", "-O2"] {
            expect_accepted(&name, &body(stmt), X86, opt);
        }
    }
}

/// What gcc takes at every level: an output and an untied input in one
/// register, an early-clobber output with its own tied input, two register
/// variables of one register, a register no other operand wants, and a
/// constraint offering any register beside the one taken.
#[test]
fn asm_pinned_registers_accepted() {
    let cases = [
        r#"__asm__("" : "=a"(x) : "a"(b));"#,
        r#"__asm__("" : "=&a"(x) : "0"(a));"#,
        r#"__asm__("" : "=b"(x) : "a"(a), "b"(b));"#,
        r#"__asm__("" : "=S"(x) : "S"(a), "D"(b));"#,
        r#"__asm__("" : "=a"(x), "=&r"(y) : "a"(a));"#,
        r#"register int r __asm__("ebx") = a; register int s __asm__("ebx") = b; __asm__("" : "=a"(x) : "r"(r), "r"(s));"#,
        r#"__asm__("" : "=a"(x) : "aq"(a), "a"(b));"#,
        r#"__asm__("" : "=a"(x) : "ar"(a), "a"(b));"#,
    ];
    for (n, stmt) in cases.iter().enumerate() {
        for opt in ["-O0", "-O1", "-O2"] {
            expect_accepted(&format!("asm_pin_ok_{n}"), &body(stmt), X86, opt);
        }
    }
}

/// An operand with any register beside the pinned one goes to another when
/// the pinned one is taken, so the template reads two registers: before,
/// both values were moved into %eax and the second overwrote the first.
#[test]
fn asm_pinned_register_alternative_takes_another_register() {
    let src = body(r##"__asm__("# %1 %2" : "=a"(x) : "ar"(a), "a"(b));"##);
    for opt in ["-O0", "-O2"] {
        let asm = expect_accepted("asm_pin_alt", &src, X86, opt);
        let line = asm
            .lines()
            .find(|l| l.trim_start().starts_with("# %"))
            .unwrap_or_else(|| panic!("{opt}: no template line in:\n{asm}"));
        assert!(
            line.contains("%eax") && !line.trim_start().starts_with("# %eax %eax"),
            "{opt}: operand 1 must leave %eax to operand 2: {line}"
        );
        assert!(
            line.ends_with("%eax"),
            "{opt}: operand 2 is in %eax: {line}"
        );
    }
}
