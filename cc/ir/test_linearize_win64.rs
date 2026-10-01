//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for `__attribute__((ms_abi))`: a call is classified by the
// callee's convention, and a definition by its own.
//

use super::test_linearize::{linearize_source, linearize_source_with_types};
use crate::abi::{ArgClass, CallingConv};
use crate::ir::{Function, Instruction, Module, Opcode};
use crate::target::{Arch, Os, Target};

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

fn insns(f: &Function) -> impl Iterator<Item = &Instruction> {
    f.blocks.iter().flat_map(|b| b.insns.iter())
}

fn calls(f: &Function) -> Vec<&Instruction> {
    insns(f).filter(|i| i.op == Opcode::Call).collect()
}

fn conv_of(call: &Instruction) -> CallingConv {
    call.extra().abi_info.as_ref().expect("classified").conv
}

const DECLS: &str = "#define MS __attribute__((ms_abi))\n\
    struct S24 { long p, q, r; };\n\
    MS long take24(long, struct S24);\n\
    MS long double ld(long double);\n\
    long plain(long);\n";

/// A call takes the callee's convention -- direct, through a pointer, or to
/// an ordinary function from an `ms_abi` one -- and a by-reference argument
/// is a pointer to a fresh copy.
#[test]
fn calls_are_classified_by_the_callee() {
    let src = format!(
        "{DECLS}\
         MS long (*fp)(long);\n\
         MS long caller(struct S24 s) {{ return take24(1, s) + fp(2) + plain(3); }}\n"
    )
    .replace("#define MS __attribute__((ms_abi))\n", "")
    .replace("MS", "__attribute__((ms_abi))");
    let module = linearize_source(&src, &x86());
    let caller = func(&module, "caller");
    assert_eq!(caller.conv, CallingConv::Win64);
    let convs: Vec<CallingConv> = calls(caller).iter().map(|c| conv_of(c)).collect();
    assert_eq!(
        convs,
        [CallingConv::Win64, CallingConv::Win64, CallingConv::C]
    );
    let take = calls(caller)[0];
    let classes = &take.extra().abi_info.as_ref().unwrap().params;
    assert!(matches!(
        classes[1],
        ArgClass::Indirect { size_bytes: 24, .. }
    ));
    // The struct goes by reference, to a copy the callee owns.
    assert!(caller.locals.keys().any(|n| n.starts_with("__argcopy")));
}

/// A Win64 `long double` comes back through the hidden pointer and goes in
/// by reference: the call passes two pointers and reads the value back.
#[test]
fn a_win64_long_double_travels_by_reference_both_ways() {
    let src = DECLS
        .replace("#define MS __attribute__((ms_abi))\n", "")
        .replace("MS", "__attribute__((ms_abi))")
        + "long double use(long double x) { return ld(x); }\n";
    let module = linearize_source(&src, &x86());
    let user = func(&module, "use");
    let call = calls(user)[0];
    let abi = call.extra().abi_info.as_ref().unwrap();
    assert!(matches!(abi.ret, ArgClass::Indirect { .. }));
    assert_eq!(
        call.src.len(),
        2,
        "hidden pointer, then the argument's copy"
    );
    assert!(matches!(abi.params[1], ArgClass::Indirect { .. }));
    // The value is read back out of the hidden return slot.
    let after: Vec<&Instruction> = insns(user)
        .skip_while(|i| i.op != Opcode::Call)
        .skip(1)
        .collect();
    assert!(after.iter().any(|i| i.op == Opcode::Load));
}

/// An `ms_abi` definition is compiled under its own type's convention: a
/// parameter passed by reference arrives as a pointer, recorded as one, and
/// a `long double` result is stored through the hidden pointer.
#[test]
fn a_definition_takes_its_own_convention() {
    let src = "__attribute__((ms_abi)) long double f(long double a, long b) { return a + b; }\n\
               long double g(long double a) { return a; }\n";
    let (module, types) = linearize_source_with_types(src, &x86());
    let f = func(&module, "f");
    assert_eq!(f.conv, CallingConv::Win64);
    assert!(
        f.sret_arg().is_some(),
        "returned through the hidden pointer"
    );
    use crate::types::TypeKind;
    assert_eq!(types.kind(f.params[0].1), TypeKind::Pointer);
    assert_eq!(types.kind(f.params[1].1), TypeKind::Long);
    let g = func(&module, "g");
    assert_eq!(g.conv, CallingConv::C);
    assert!(g.sret_arg().is_none());
    assert_eq!(types.kind(g.params[0].1), TypeKind::LongDouble);
}

/// `__builtin_va_arg` on a `__builtin_ms_va_list` is ordinary IR -- a
/// pointer stepped eight bytes at a time -- and leaves no `VaArg` for a back
/// end; `__builtin_ms_va_start` is the function's `VaStart`.
#[test]
fn a_microsoft_va_arg_is_plain_ir() {
    let src = "__attribute__((ms_abi)) int f(int n, ...) {\n\
               __builtin_ms_va_list ap;\n\
               __builtin_ms_va_start(ap, n);\n\
               int r = __builtin_va_arg(ap, int);\n\
               __builtin_ms_va_end(ap);\n\
               return r;\n}\n";
    let module = linearize_source(src, &x86());
    let f = func(&module, "f");
    assert!(insns(f).any(|i| i.op == Opcode::VaStart));
    assert!(!insns(f).any(|i| i.op == Opcode::VaArg));
    assert!(insns(f).any(|i| i.op == Opcode::Add));
}

/// libgcc's complex routines are System V functions whoever calls them.
#[test]
fn runtime_library_calls_keep_the_target_convention() {
    let src = "__attribute__((ms_abi)) double _Complex f(double _Complex a, double _Complex b) \
               { return a * b; }\n";
    let module = linearize_source(src, &x86());
    let f = func(&module, "f");
    let rtlib: Vec<&Instruction> = calls(f)
        .into_iter()
        .filter(|c| c.extra().func_name.as_deref() == Some("__muldc3"))
        .collect();
    assert_eq!(rtlib.len(), 1);
    assert_eq!(conv_of(rtlib[0]), CallingConv::C);
}
