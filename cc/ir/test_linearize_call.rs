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

use super::test_linearize::{insns_of, linearize_source, linearize_source_with_types};
use crate::ir::{Function, Instruction, Module, Opcode, PseudoKind};
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

/// A call records each argument at the type the callee receives it as, which
/// is what the ABI places it by: the parameter's type under a prototype --
/// narrower included -- and the default argument promotions without one
/// (C17 6.5.2.2p6-7). Apple arm64 stacks a `char` parameter in one byte, so
/// `p('a')` recorded as `int` took four; and an unprototyped call passed a
/// `float` where a gcc-compiled K&R callee reads a `double`.
#[test]
fn test_call_argument_types_follow_the_callee() {
    use crate::target::{Arch, Os};
    let src = "void p(char, short, int);\n\
               int u();\n\
               void f(long l, char c, float x) { p(l, 300, l); u(c, x); }\n";
    let target = Target::new(Arch::Aarch64, Os::MacOS);
    let (module, types) = linearize_source_with_types(src, &target);
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let calls: Vec<&Instruction> = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Call)
        .collect();
    assert_eq!(calls.len(), 2);
    let kinds = |call: &Instruction| -> Vec<TypeKind> {
        call.extra()
            .arg_types
            .iter()
            .map(|&t| types.kind(t))
            .collect()
    };
    assert_eq!(
        kinds(calls[0]),
        [TypeKind::Char, TypeKind::Short, TypeKind::Int],
        "a prototyped call passes each argument as its parameter's type"
    );
    assert_eq!(
        kinds(calls[1]),
        [TypeKind::Int, TypeKind::Double],
        "an unprototyped call passes the promoted types"
    );
}

/// An identifier-list definition receives each parameter as its default
/// argument promotion -- what every caller passes, having no prototype to
/// convert to -- and converts it to the declared type on entry (C17
/// 6.9.1p10). It read a `float` parameter out of the register a caller had
/// put a `double` in.
#[test]
fn test_identifier_list_parameters_arrive_promoted() {
    use crate::target::{Arch, Os};
    let src = "int kr(f, c, s, l) float f; char c; short s; long l;\n\
               { return (int)f + c + s + (int)l; }\n\
               int pr(float f, char c) { return (int)f + c; }\n";
    let (module, types) = linearize_source_with_types(src, &Target::new(Arch::Aarch64, Os::Linux));
    let params = |name: &str| -> Vec<TypeKind> {
        let f = module.functions.iter().find(|f| f.name == name).unwrap();
        f.params.iter().map(|(_, t)| types.kind(*t)).collect()
    };
    assert_eq!(
        params("kr"),
        [
            TypeKind::Double,
            TypeKind::Int,
            TypeKind::Int,
            TypeKind::Long
        ]
    );
    assert_eq!(params("pr"), [TypeKind::Float, TypeKind::Char]);
    let kr = module.functions.iter().find(|f| f.name == "kr").unwrap();
    assert!(
        kr.blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .any(|i| i.op == Opcode::FCvtF),
        "the double must be converted to the float parameter on entry"
    );
}

/// The translation unit's own `floor` and `memcpy`, defined below the call,
/// are called -- `fabs`, which gcc folds as it parses, stays computed in
/// place.
#[test]
fn test_own_definition_below_the_call_is_called() {
    let src = "double floor(double); double fabs(double);\n\
               double f(double x, char *d, char *s) {\n\
                   memcpy(d, s, 8); return floor(x) + fabs(x); }\n\
               double floor(double x) { return x; }\n\
               double fabs(double x) { return x; }\n\
               void *memcpy(void *d, const void *s, unsigned long n) { return d; }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let insns = insns_of(&module, "f");
    let calls: Vec<_> = insns
        .iter()
        .filter(|i| i.op == Opcode::Call)
        .map(|i| i.extra().func_name.as_deref())
        .collect();
    assert_eq!(calls, [Some("memcpy"), Some("floor")]);
    assert!(!insns
        .iter()
        .any(|i| i.op.is_libm() || i.op == Opcode::Memcpy));
    assert!(insns.iter().any(|i| i.op == Opcode::Fabs));
}

/// AAPCS64 B.4 passes a composite over sixteen bytes as a pointer to a copy
/// the caller makes; System V puts the bytes themselves in the argument area.
/// So only the aarch64 lowering copies the argument into a frame temporary
/// before the call -- and passes that temporary's address, not the global's.
#[test]
fn test_large_composite_argument_copied_only_where_the_abi_passes_a_reference() {
    use crate::target::{Arch, Os};
    let src = "struct big { long a, b, c; };\n\
               struct big g;\n\
               long f(struct big s);\n\
               long t(void) { return f(g); }\n";
    let has_copy = |target: Target| {
        let module = linearize_source(src, &target);
        let t = module.functions.iter().find(|f| f.name == "t").unwrap();
        t.locals.keys().any(|name| name.starts_with("__argcopy_"))
    };
    assert!(has_copy(Target::new(Arch::Aarch64, Os::Linux)));
    assert!(has_copy(Target::new(Arch::Aarch64, Os::MacOS)));
    assert!(!has_copy(Target::new(Arch::X86_64, Os::Linux)));
}

/// Every opcode a backend lowers to a library call carries that function's
/// assembler name, resolved through the program's declarations: an asm-label
/// rename of `memcpy` reaches `__builtin_memcpy` and a structure copy alike,
/// and a function the program did not rename keeps its own name.
#[test]
fn test_library_callee_honours_asm_label() {
    let src = "typedef unsigned long size_t;\n\
               void *memcpy(void *, const void *, size_t) __asm(\"my_memcpy\");\n\
               void *memset(void *, int, size_t) __asm(\"my_memset\");\n\
               struct big { long a[32]; };\n\
               void t(struct big *d, struct big *s, char *p, size_t n) {\n\
                   __builtin_memcpy(p, p + 1, n);\n\
                   __builtin_memset(p, 0, n);\n\
                   __builtin_memmove(p, p + 1, n);\n\
                   *d = *s;\n\
               }\n";
    let module = linearize_source(src, &Target::host());
    let t = module.functions.iter().find(|f| f.name == "t").unwrap();
    let callees: Vec<(Opcode, &str)> = t
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .filter(|i| matches!(i.op, Opcode::Memcpy | Opcode::Memset | Opcode::Memmove))
        .map(|i| (i.op, i.library_callee()))
        .collect();
    let my_memcpy = crate::arch::lir::verbatim("my_memcpy");
    let my_memset = crate::arch::lir::verbatim("my_memset");
    assert_eq!(
        callees,
        vec![
            (Opcode::Memcpy, my_memcpy.as_str()),
            (Opcode::Memset, my_memset.as_str()),
            (Opcode::Memmove, "memmove"),
            (Opcode::Memcpy, my_memcpy.as_str()),
        ]
    );
}

/// A complex `*` calls `__mul?c3` by the ABI's classification of the complex
/// type, as any other call returning it would be: on x86-64 a
/// `_Float128 _Complex` is MEMORY class, so `__multc3` is handed a hidden
/// pointer ahead of its four halves and the function itself returns through
/// one; `long double _Complex` is COMPLEX_X87 and comes back in st(0)/st(1),
/// with no hidden pointer. On aarch64 `_Float128` is `long double` and its
/// complex an HFA, so neither has one.
#[test]
fn test_complex_multiply_routine_follows_the_return_class() {
    use crate::target::{Arch, Os};
    let cases = [
        (Arch::X86_64, "_Float128", "__multc3", true),
        (Arch::X86_64, "long double", "__mulxc3", false),
        (Arch::Aarch64, "_Float128", "__multc3", false),
    ];
    for (arch, base, routine, sret) in cases {
        let src = format!(
            "{base} _Complex m({base} _Complex x, {base} _Complex y) {{ return x * y; }}\n"
        );
        let module = linearize_source(&src, &Target::new(arch, Os::Linux));
        let m = module.functions.iter().find(|f| f.name == "m").unwrap();
        let sret_pseudo = m
            .pseudos
            .iter()
            .find(|p| p.kind == PseudoKind::Arg(0) && p.name.as_deref() == Some("__sret"))
            .map(|p| p.id);
        assert_eq!(
            sret_pseudo.is_some(),
            sret,
            "{base} on {arch:?}: the function's own return"
        );
        assert_eq!(
            m.sret, sret_pseudo,
            "{base} on {arch:?}: the function records the pointer it takes"
        );
        let call = m
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .find(|i| i.op == Opcode::Call && i.extra().func_name.as_deref() == Some(routine))
            .unwrap_or_else(|| panic!("{base} on {arch:?}: no {routine} call"));
        assert_eq!(
            call.returns_via_sret(),
            sret,
            "{base} on {arch:?}: {routine}"
        );
        assert_eq!(
            call.src.len(),
            4 + usize::from(sret),
            "{base} on {arch:?}: {routine} takes the four halves, after any hidden pointer"
        );
    }
}

/// A known library call keeps its tag in the IR under whatever name the
/// program gave it: `strstr` renamed by an asm label is called as
/// `my_strstr` and is still `strstr` to the optimizer. A call through a
/// pointer to it is not tagged.
#[test]
fn test_known_call_keeps_its_tag_under_an_asm_label() {
    let src = "char *strstr(const char *, const char *) __asm__(\"my_strstr\");\n\
               char *f(const char *s) { return strstr(s, \"o\"); }\n\
               char *g(const char *s) { return (&strstr)(s, \"o\"); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let call_in = |name: &str| {
        insns_of(&module, name)
            .into_iter()
            .find(|i| i.op == Opcode::Call)
            .unwrap_or_else(|| panic!("{name}: no call"))
    };
    let f = call_in("f");
    let label = crate::arch::lir::verbatim("my_strstr");
    assert_eq!(f.extra().func_name.as_deref(), Some(label.as_str()));
    assert_eq!(f.extra().known, Some(crate::parse::ast::LibFn::Strstr));
    assert_eq!(call_in("g").extra().known, None);
}

/// Every aggregate returned in registers has its `Ret` classified, at any
/// size: a four-`double` HFA is thirty-two bytes on aarch64 and still comes
/// back in `d0`-`d3`, by address in the IR. The callee's side stopped
/// classifying at sixteen bytes, so this `Ret` carried no ABI record and the
/// function had to be kept from the inliner; it is neither now.
#[test]
fn test_every_register_returned_aggregate_carries_its_class() {
    let target = Target::new(Arch::Aarch64, Os::Linux);
    let src = "struct H4 { double v[4]; };\n\
               struct H4 mk(double s) { struct H4 r = { { s, s, s, s } }; return r; }\n";
    let module = linearize_source(src, &target);
    let f = module.functions.iter().find(|f| f.name == "mk").unwrap();
    let ret = f
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .find(|i| i.op == Opcode::Ret)
        .expect("a ret");
    assert!(
        matches!(
            ret.extra().abi_info.as_ref().map(|a| &a.ret),
            Some(crate::abi::ArgClass::Hfa { count: 4, .. })
        ),
        "{:?}",
        ret.extra().abi_info
    );
    assert!(ret.returns_aggregate_address());
    assert!(
        !f.ret_is_address,
        "an address-returned aggregate is inlinable"
    );
}

/// An array or a function designator bound to a `_Bool` parameter is passed
/// as whether its address is null -- the result of a comparison, classified
/// as `_Bool` -- not as the address it decays to. Under no prototype it
/// travels as the pointer.
#[test]
fn a_decaying_argument_converts_to_its_parameter() {
    let src = "_Bool take(_Bool);\n\
               int old();\n\
               long h(void);\n\
               int arr[4];\n\
               int f(void) { return take(arr) + take(h) + old(arr); }\n";
    let (module, types) = linearize_source_with_types(src, &x86());
    let f = func(&module, "f");
    let all = insns(f);
    let calls = calls(f);
    assert_eq!(calls.len(), 3);
    for call in &calls[..2] {
        assert_eq!(kinds(&types, call), vec![TypeKind::Bool]);
        let arg = call.src[0];
        let def = all.iter().find(|i| i.target == Some(arg)).unwrap();
        assert!(
            def.op.is_comparison(),
            "a `_Bool` argument is a comparison, not {:?}",
            def.op
        );
    }
    assert_eq!(kinds(&types, calls[2]), vec![TypeKind::Pointer]);
}

/// A small integer vector is returned as a general-register value on Linux,
/// as gcc does, and in V0 on Darwin, as clang does: one lane as a `float`'s
/// bits, several widened to fill a `double`'s. A call narrows the widened
/// lanes back, so the caller sees the vector it declared.
#[test]
fn test_darwin_returns_small_integer_vectors_in_v0() {
    let src = "typedef short v2hi __attribute__((vector_size(4)));\n\
        typedef int v1si __attribute__((vector_size(4)));\n\
        v2hi r2(v2hi a) { return a + a; }\n\
        v1si r1(v1si a) { return a; }\n\
        v2hi ext(v2hi);\n\
        int c2(v2hi a) { return ext(a)[1]; }\n";
    for os in [Os::Linux, Os::MacOS] {
        let (module, types) = linearize_source_with_types(src, &Target::new(Arch::Aarch64, os));
        let ret = |name: &str| {
            let f = module.functions.iter().find(|f| f.name == name).unwrap();
            f.return_type
        };
        let darwin = os == Os::MacOS;
        assert_eq!(types.is_float(ret("r2")), darwin, "{os:?} r2");
        assert_eq!(types.is_float(ret("r1")), darwin, "{os:?} r1");
        if darwin {
            assert_eq!(types.size_bytes(ret("r2")), 8);
            assert_eq!(types.size_bytes(ret("r1")), 4);
        }
        // The call's result is taken at the width it was returned.
        let call = insns_of(&module, "c2")
            .into_iter()
            .find(|i| i.op == Opcode::Call)
            .unwrap();
        assert_eq!(types.is_float(call.typ.unwrap()), darwin, "{os:?} call");
    }
}

/// A call reaches the declaration it names, not whatever this unit defines
/// under the same assembler name. glibc's fortified `open` calls
/// `__open_alias`, labelled like the wrapper itself: that call is the
/// library's, so the wrapper is not recursive; the program's own call to `w`
/// is the wrapper's.
#[test]
fn call_through_a_labelled_alias_reaches_the_library() {
    use crate::parse::ast::CalleeBinding;
    let src = "extern int w(int, ...) __asm__(\"lib\");\n\
               extern int w_alias(int, ...) __asm__(\"lib\");\n\
               extern __inline __attribute__((always_inline, gnu_inline)) int\n\
               w(int a, ...) { return w_alias(a, __builtin_va_arg_pack()); }\n\
               int main(void) { return w(1, 2); }\n";
    let module = linearize_source(src, &x86());
    let label = crate::arch::lir::verbatim("lib");
    let wrapper = func(&module, &label);
    let in_main = calls(func(&module, "main"));
    assert_eq!(in_main.len(), 1);
    assert_eq!(in_main[0].extra().callee_binding, CalleeBinding::Declared);
    assert_eq!(in_main[0].local_callee(), Some(label.as_str()));
    let in_wrapper = calls(wrapper);
    assert_eq!(in_wrapper.len(), 1);
    assert_eq!(in_wrapper[0].extra().callee_binding, CalleeBinding::Library);
    assert_eq!(in_wrapper[0].local_callee(), None);
}
