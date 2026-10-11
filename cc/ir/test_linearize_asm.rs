//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for inline `asm`: memory operands and `asm goto` outputs.
//

use super::test_linearize::{linearize_no_ssa, linearize_source, test_pos, TestContext};
use super::*;
use crate::parse::ast::{
    AsmOperand, BlockItem, ExternalDecl, FunctionDef, Label, LabelId, ParamStyle, Parameter, Stmt,
};
use crate::target::Target;

/// An `asm goto` output is written back on every path out of the statement.
///
/// The label edge gets a block of its own that stores the output and then
/// branches to the label; the asm names that block, not the label. Before,
/// the store sat only on the fall-through, so a jump reached the label with
/// the output never written.
#[test]
fn test_asm_goto_output_written_back_on_the_label_edge() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let out_id = ctx.str("out");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // { asm goto("" : "=r"(x) : : : out); return 0; out: return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Asm {
            pos: Position::default(),
            template: String::new(),
            outputs: vec![AsmOperand {
                name: None,
                constraint: "=r".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![LabelId::function(out_id)],
        })),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Named {
                label: LabelId::function(out_id),
                pos: test_pos(),
            }],
            stmt: Box::new(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
        })),
    ]);
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body,
        pos: test_pos(),
        is_static: false,
        storage_class: TypeModifiers::empty(),
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    // Before SSA: once it runs, `x` is promoted and the store on the edge
    // becomes the phi argument the label block reads.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let func = &module.functions[0];

    let asm = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::Asm)
        .expect("an asm instruction");
    let out_pseudo = asm.extra().asm_data.as_ref().unwrap().outputs[0].pseudo;
    let (edge, _) = asm.extra().asm_data.as_ref().unwrap().goto_labels[0];
    let edge = func.get_block(edge).expect("the label edge block");
    assert!(
        edge.insns
            .iter()
            .any(|i| i.op == Opcode::Store && i.src.contains(&out_pseudo)),
        "the label edge must store the output: {:?}",
        edge.insns
    );
    assert!(
        matches!(edge.insns.last().map(|i| i.op), Some(Opcode::Br)),
        "the label edge must end by branching to the label"
    );
}

/// A memory operand naming an object is handed over as the object's `Sym`,
/// not as an address computed into a pseudo.
///
/// Addressed in place it needs no register; as an address value it
/// competed for one and, once spilled, the backend substituted the spill
/// slot as the operand. The address arithmetic the lvalue produced is left
/// for DCE, which removes it once nothing reads it.
#[test]
fn test_asm_memory_operand_names_its_object() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // { asm("" : "=m"(x) : "m"(x)); return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Asm {
            pos: Position::default(),
            template: String::new(),
            outputs: vec![AsmOperand {
                name: None,
                constraint: "=m".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            inputs: vec![AsmOperand {
                name: None,
                constraint: "m".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            clobbers: vec![],
            goto_labels: vec![],
        })),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
            x_sym, int_type,
        ))))),
    ]);
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body,
        pos: test_pos(),
        is_static: false,
        storage_class: TypeModifiers::empty(),
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let func = &module.functions[0];

    let asm = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::Asm)
        .expect("an asm instruction");
    let data = asm.extra().asm_data.as_ref().unwrap();
    for c in data.outputs.iter().chain(data.inputs.iter()) {
        assert!(
            matches!(
                func.get_pseudo(c.pseudo).map(|p| &p.kind),
                Some(crate::ir::PseudoKind::Sym(_))
            ),
            "{c:?} should name the object"
        );
        assert_eq!(c.offset, 0);
    }
    let mut func = func.clone();
    crate::ir::dce::run(&mut func);
    assert!(
        !func
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .any(|i| i.op == Opcode::SymAddr),
        "nothing reads the operand's address arithmetic, so DCE removes it"
    );
}

/// The address arithmetic behind a memory operand that names an object is
/// not the linearizer's to delete: `"=m"(*(q = &arr[2]))` also stores that
/// address into `q`, before the asm. Deleting it left the store reading an
/// undefined register, so `q == &arr[2]` was false.
#[test]
fn test_asm_memory_operand_keeps_an_address_something_else_reads() {
    let src = "int arr[4]; int *q;\n\
               void f(void) { __asm__ volatile(\"\" : \"=m\"(*(q = &arr[2]))); }\n";
    let module = linearize_source(src, &Target::host());
    let func = module.functions.iter().find(|f| f.name == "f").expect("f");
    let insns: Vec<&Instruction> = func.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    // Every pseudo a live instruction reads must be defined by a live one.
    let defined: std::collections::HashSet<PseudoId> = insns
        .iter()
        .filter(|i| i.op != Opcode::Nop)
        .filter_map(|i| i.target)
        .chain(
            func.pseudos
                .iter()
                .filter(|p| {
                    !matches!(
                        p.kind,
                        crate::ir::PseudoKind::Reg(_) | crate::ir::PseudoKind::Phi(_)
                    )
                })
                .map(|p| p.id),
        )
        .collect();
    for insn in insns.iter().filter(|i| i.op == Opcode::Store) {
        for s in &insn.src {
            assert!(
                defined.contains(s),
                "{:?} reads {s:?}, which nothing defines",
                insn.op
            );
        }
    }
}

/// A file-scope asm reaches the module in order, and every function and
/// object records how many of them came before it -- including an object
/// a function body defines, which belongs where the function is.
#[test]
fn test_file_scope_asm_places_definitions_between_them() {
    let src = "
int a = 1;
__asm__(\".globl first\");
int f(void) { static int s = 2; return a + s; }
__asm__(\"second\" \"\\n\");
__asm__(\"third\");
int b = 3;
";
    let module = linearize_source(src, &Target::host());
    assert_eq!(module.toplevel_asm, [".globl first", "second\n", "third"]);
    let placed: Vec<(&str, usize)> = module
        .globals
        .iter()
        .map(|g| (g.name.as_str(), g.asm_before))
        .collect();
    // `a`, then the function's static, then `b`.
    let [("a", 0), (_, 1), ("b", 3)] = placed.as_slice() else {
        panic!("{placed:?}");
    };
    let f = module.functions.iter().find(|f| f.name == "f").expect("f");
    assert_eq!(f.asm_before, 1);
}

/// With no file-scope asm there is one run of definitions, all at 0.
#[test]
fn test_no_file_scope_asm_leaves_one_run() {
    let module = linearize_source("int a = 1; int f(void) { return a; }", &Target::host());
    assert!(module.toplevel_asm.is_empty());
    assert!(module.globals.iter().all(|g| g.asm_before == 0));
    assert!(module.functions.iter().all(|f| f.asm_before == 0));
}

/// A register variable as a register operand is pinned to its register in
/// the operand's own class, after SSA has promoted the variable -- the class
/// is the only place the binding survives that and inlining. Here the
/// operand has become the parameter's value. A tied input follows its
/// output, an operand with its own letter keeps it, and a plain operand is
/// left to the allocator. A file-scope register variable defines no global:
/// it is read and written by empty asms pinned to it.
#[test]
fn test_register_variable_operands_are_pinned() {
    use crate::arch::aarch64::regalloc::Reg as A64Reg;
    use crate::arch::asm_constraints::{AsmRegClass, PinnedGp};
    use crate::arch::x86_64::regalloc::Reg as X86Reg;
    use crate::ir::{AsmConstraint, AsmData};
    use crate::target::{Arch, Os};
    let cases = [
        (
            Arch::X86_64,
            "rsp",
            "r10",
            PinnedGp::X86_64(X86Reg::Rsp),
            PinnedGp::X86_64(X86Reg::R10),
        ),
        (
            Arch::Aarch64,
            "sp",
            "x8",
            PinnedGp::Aarch64(A64Reg::SP),
            PinnedGp::Aarch64(A64Reg::X8),
        ),
    ];
    for (arch, global, local, sp, reg) in cases {
        let src = format!(
            "register unsigned long cur asm(\"{global}\");\n\
             long f(long a) {{\n\
                 register long r asm(\"{local}\") = a;\n\
                 long out;\n\
                 asm(\"\" : \"=r\"(out), \"+r\"(cur) : \"r\"(r), \"g\"(a), \"0\"(r));\n\
                 return out + (long)cur;\n\
             }}\n"
        );
        let module = linearize_source(&src, &Target::new(arch, Os::Linux));
        assert!(module
            .globals
            .iter()
            .all(|g| g.name != "cur" && g.name != global));
        let f = module.functions.iter().find(|f| f.name == "f").unwrap();
        let asms: Vec<&AsmData> = f
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .filter_map(|i| i.extra().asm_data.as_deref())
            .collect();
        let pinned = |c: &AsmConstraint| match c.class.reg {
            Some(AsmRegClass::Pinned(p)) => Some(p),
            _ => None,
        };
        let stmt = asms
            .iter()
            .find(|a| a.outputs.len() == 2)
            .expect("the statement");
        assert_eq!(pinned(&stmt.outputs[0]), None, "{arch:?}");
        assert_eq!(pinned(&stmt.outputs[1]), Some(sp), "{arch:?}");
        let ins: Vec<Option<PinnedGp>> = stmt.inputs.iter().map(pinned).collect();
        // The hidden input of "+r"(cur), then r, a and the tied "0"(r).
        assert_eq!(ins, vec![Some(sp), Some(reg), None, None], "{arch:?}");
        assert_eq!(stmt.inputs[3].matching_output, Some(0), "{arch:?}");
        // `cur` is read before the statement, written after it, and read
        // again in the return: one empty asm each, pinned to the register.
        let accesses: Vec<(usize, usize)> = asms
            .iter()
            .filter(|a| a.template.is_empty() && a.outputs.len() + a.inputs.len() == 1)
            .map(|a| {
                let c = a.outputs.first().or(a.inputs.first()).unwrap();
                assert_eq!(pinned(c), Some(sp), "{arch:?}");
                (a.outputs.len(), a.inputs.len())
            })
            .collect();
        assert_eq!(accesses, vec![(1, 0), (0, 1), (1, 0)], "{arch:?}");
    }
}

/// An output two inputs are tied to is reported, and the second tie is
/// dropped: the input becomes an operand of its own. Both ties used to copy
/// their value into the output's pseudo, which then had two definitions --
/// an internal error after linearization.
#[test]
fn test_second_tie_to_an_output_is_dropped() {
    let src = "long f(long a, long b) {\n\
               long sum;\n\
               __asm__(\"\" : \"=r\"(sum) : \"0\"(a), \"0\"(b));\n\
               return sum;\n\
               }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let asm = f
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .find_map(|i| i.extra().asm_data.as_deref())
        .expect("the statement");
    let ties: Vec<Option<usize>> = asm.inputs.iter().map(|c| c.matching_output).collect();
    assert_eq!(ties, [Some(0), None]);
    let out = asm.outputs[0].pseudo;
    let defs = f
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .filter(|i| i.target == Some(out))
        .count();
    assert_eq!(defs, 1, "the output's pseudo is defined once");
}
