//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Optimization infrastructure for c17 C17 compiler
//
// This module provides the optimization pass runner and common utilities
// used by optimization passes (InstCombine, DCE, etc.).
//

use crate::ir::dce;
use crate::ir::inline;
use crate::ir::instcombine;
use crate::ir::validate;
use crate::ir::{Function, Module};

/// Maximum iterations for the optimization fixed-point loop.
/// Prevents infinite loops if passes keep making changes.
const MAX_ITERATIONS: usize = 10;

// ============================================================================
// Pass Runner
// ============================================================================

/// Optimize a module at the given optimization level.
///
/// Level 0: nothing but `__attribute__((always_inline))` inlining
/// Level 1+: Run inlining, InstCombine, and DCE passes
pub fn optimize_module(module: &mut Module, level: u32) {
    // Phase 1: Function inlining (module-level pass)
    // This inlines small functions at their call sites and removes
    // dead static functions that were fully inlined.
    //
    // Runs even at -O0, where it admits only `__attribute__((always_inline))`
    // functions -- gcc honours that attribute with optimization off. It is a
    // no-op for a module that has none.
    inline::run(module, level);

    if level == 0 {
        return;
    }

    // Phase 2: Per-function optimization (InstCombine + DCE)
    for func in &mut module.functions {
        optimize_function(func);
    }

    // Phase 3 (debug builds only): structural IR validation.
    // Runs at the end of optimization, BEFORE `ir::lower::lower_module`
    // which intentionally introduces multi-def Copies as part of φ-
    // elimination. Any invariant we want to enforce on optimizer-stage
    // IR (currently: SSA single-def of every target pseudo) is checked
    // here. Production builds skip the call entirely.
    debug_assert!(
        validate::validate_module(module).is_ok(),
        "IR validation failed after optimization: {}",
        validate::validate_module(module)
            .err()
            .map(|errs| errs
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n  "))
            .unwrap_or_default()
    );
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
