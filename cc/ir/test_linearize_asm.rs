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
        is_inline: false,
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
        is_inline: false,
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
