//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for call lowering: the callee is resolved, the arguments
// are lowered against its type, and one call instruction is built for every
// kind of return.
//

use super::test_linearize::linearize_source_with_types;
use crate::ir::{Function, Instruction, Module, Opcode};
use crate::target::{Arch, Os, Target};
use crate::types::{TypeKind, TypeTable};

fn x86() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

fn func<'a>(module: &'a Module, name: &str) -> &'a Function {
    module
        .functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("no function {name}"))
}

fn insns(f: &Function) -> Vec<&Instruction> {
    f.blocks.iter().flat_map(|b| b.insns.iter()).collect()
}

fn calls(f: &Function) -> Vec<&Instruction> {
    insns(f)
        .into_iter()
        .filter(|i| i.op == Opcode::Call)
        .collect()
}

fn kinds(types: &TypeTable, call: &Instruction) -> Vec<TypeKind> {
    call.extra()
        .arg_types
        .iter()
        .map(|&t| types.kind(t))
        .collect()
}

/// The prototype is read through a pointer as it is off a function: an
/// `int` bound to a `double` parameter converts either way, and a call
/// through a pointer to an unprototyped function promotes a `float`.
#[test]
fn arguments_convert_by_the_callee_function_type() {
    let src = "double f(double);\n\
               double (*p)(double);\n\
               double (*q)();\n\
               double g(float x) { return f(1) + p(1) + q(x); }\n";
    let (module, types) = linearize_source_with_types(src, &x86());
    let g = func(&module, "g");
    let calls = calls(g);
    assert_eq!(calls.len(), 3);
    for call in &calls {
        assert_eq!(kinds(&types, call), [TypeKind::Double]);
    }
    assert_eq!(calls[0].extra().func_name.as_deref(), Some("f"));
    assert!(calls[0].extra().indirect_target.is_none());
    assert!(calls[1].extra().indirect_target.is_some());
    assert!(calls[2].extra().indirect_target.is_some());
}

/// A call returning through the hidden pointer is typed as that pointer, and
/// one returning a scalar as the scalar; both carry the classification, and
/// a `_Noreturn` callee's call is followed by `Unreachable`.
#[test]
fn every_return_kind_builds_one_classified_call() {
    let src = "struct Big { long a, b, c; };\n\
               struct Big big(void);\n\
               _Noreturn void stop(void);\n\
               long g(int c) { struct Big b = big(); if (c) stop(); return b.a; }\n";
    let (module, types) = linearize_source_with_types(src, &x86());
    let g = func(&module, "g");
    let calls = calls(g);
    assert_eq!(calls.len(), 2);
    let (big, stop) = (calls[0], calls[1]);
    assert_eq!(types.kind(big.typ.unwrap()), TypeKind::Pointer);
    assert_eq!(big.size, 64);
    assert_eq!(
        kinds(&types, big),
        [TypeKind::Pointer],
        "the hidden pointer"
    );
    assert!(big.extra().abi_info.is_some());
    assert!(!big.extra().is_noreturn_call);
    assert!(stop.extra().abi_info.is_some());
    assert!(stop.extra().is_noreturn_call);
    let all = insns(g);
    let at = all
        .iter()
        .position(|i| std::ptr::eq(*i, stop))
        .expect("the call");
    assert_eq!(all[at + 1].op, Opcode::Unreachable);
}

/// A sixteen-byte struct keeps its own type when it travels in registers
/// or in memory, both decided by its classification.
#[test]
fn a_medium_struct_argument_keeps_its_type() {
    let src = "struct Two { long a, b; };\n\
               struct Fp { double a, b; };\n\
               struct Ld { long double a; };\n\
               void two(struct Two); void fp(struct Fp); void ld(struct Ld);\n\
               void g(struct Two t, struct Fp f, struct Ld l) { two(t); fp(f); ld(l); }\n";
    let (module, types) = linearize_source_with_types(src, &x86());
    for call in calls(func(&module, "g")) {
        assert_eq!(kinds(&types, call), [TypeKind::Struct]);
    }
}

/// The callee's variadic start and `noreturn` are the function type's,
/// read through a pointer as they are off a function.
#[test]
fn variadic_and_noreturn_are_read_through_a_pointer() {
    let src = "int v(int, ...);\n\
               _Noreturn void stop(void);\n\
               int (*p)(int, ...);\n\
               __typeof__(stop) *np;\n\
               void g(void) { v(1, 2); p(1, 2); np(); }\n";
    let (module, _) = linearize_source_with_types(src, &x86());
    let g = func(&module, "g");
    let calls = calls(g);
    assert_eq!(calls.len(), 3);
    for call in &calls[..2] {
        assert_eq!(call.extra().variadic_arg_start, Some(1));
    }
    assert!(calls[2].extra().is_noreturn_call);
    let all = insns(g);
    let at = all
        .iter()
        .position(|i| std::ptr::eq(*i, calls[2]))
        .expect("the call");
    assert_eq!(all[at + 1].op, Opcode::Unreachable);
}
