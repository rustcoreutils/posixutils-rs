//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Optimization infrastructure for pcc C99 compiler
//
// This module provides the optimization pass runner and common utilities
// used by optimization passes (InstCombine, DCE, etc.).
//

use crate::ir::dce;
use crate::ir::instcombine;
use crate::ir::{Function, Module};

#[cfg(test)]
use crate::ir::{Pseudo, PseudoId, PseudoKind};

/// Maximum iterations for the optimization fixed-point loop.
/// Prevents infinite loops if passes keep making changes.
const MAX_ITERATIONS: usize = 10;

// ============================================================================
// Helper Functions (used by tests and future passes)
// ============================================================================

/// Get a pseudo by its ID from a function's pseudo list.
#[cfg(test)]
fn get_pseudo(func: &Function, id: PseudoId) -> Option<&Pseudo> {
    func.pseudos.iter().find(|p| p.id == id)
}

/// Get the constant integer value of a pseudo, if it is a constant.
#[cfg(test)]
fn get_const_val(func: &Function, id: PseudoId) -> Option<i64> {
    get_pseudo(func, id).and_then(|p| match &p.kind {
        PseudoKind::Val(v) => Some(*v),
        _ => None,
    })
}

/// Get the constant float value of a pseudo, if it is a float constant.
#[cfg(test)]
fn get_const_fval(func: &Function, id: PseudoId) -> Option<f64> {
    get_pseudo(func, id).and_then(|p| match &p.kind {
        PseudoKind::FVal(v) => Some(*v),
        _ => None,
    })
}

/// Check if a pseudo is a constant (integer or float).
#[cfg(test)]
fn is_const(func: &Function, id: PseudoId) -> bool {
    get_pseudo(func, id)
        .map(|p| matches!(&p.kind, PseudoKind::Val(_) | PseudoKind::FVal(_)))
        .unwrap_or(false)
}

// ============================================================================
// Pass Runner
// ============================================================================

/// Optimize a module at the given optimization level.
///
/// Level 0: No optimization
/// Level 1+: Run InstCombine + DCE passes
pub fn optimize_module(module: &mut Module, level: u32) {
    if level == 0 {
        return;
    }

    for func in &mut module.functions {
        optimize_function(func);
    }
}

/// Optimize a single function by running passes until fixed point.
fn optimize_function(func: &mut Function) {
    for _ in 0..MAX_ITERATIONS {
        let ic_changed = instcombine::run(func);
        let dce_changed = dce::run(func);

        if !ic_changed && !dce_changed {
            break;
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Opcode};
    use crate::types::TypeTable;

    fn make_test_func() -> Function {
        let types = TypeTable::new();
        let mut func = Function::new("test", types.int_id);

        // Add some pseudos
        func.add_pseudo(Pseudo::val(PseudoId(0), 42));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::fval(PseudoId(2), 3.14));

        // Add a basic block
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        func
    }

    #[test]
    fn test_get_pseudo() {
        let func = make_test_func();

        assert!(get_pseudo(&func, PseudoId(0)).is_some());
        assert!(get_pseudo(&func, PseudoId(1)).is_some());
        assert!(get_pseudo(&func, PseudoId(99)).is_none());
    }

    #[test]
    fn test_get_const_val() {
        let func = make_test_func();

        assert_eq!(get_const_val(&func, PseudoId(0)), Some(42));
        assert_eq!(get_const_val(&func, PseudoId(1)), None); // register, not const
        assert_eq!(get_const_val(&func, PseudoId(2)), None); // float, not int
        assert_eq!(get_const_val(&func, PseudoId(99)), None); // doesn't exist
    }

    #[test]
    fn test_get_const_fval() {
        let func = make_test_func();

        assert_eq!(get_const_fval(&func, PseudoId(2)), Some(3.14));
        assert_eq!(get_const_fval(&func, PseudoId(0)), None); // int, not float
        assert_eq!(get_const_fval(&func, PseudoId(1)), None); // register
    }

    #[test]
    fn test_is_const() {
        let func = make_test_func();

        assert!(is_const(&func, PseudoId(0))); // int constant
        assert!(is_const(&func, PseudoId(2))); // float constant
        assert!(!is_const(&func, PseudoId(1))); // register
        assert!(!is_const(&func, PseudoId(99))); // doesn't exist
    }
}
