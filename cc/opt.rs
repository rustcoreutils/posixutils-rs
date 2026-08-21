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

// ============================================================================
// What optimization was asked for
// ============================================================================

/// The optimization the command line asked for.
///
/// Built once from `-O...` and handed to *both* the optimizer and the
/// preprocessor, so that `__OPTIMIZE__` and its siblings are derived from the
/// value that drives behaviour rather than from a parallel copy of it. A
/// capability macro that can disagree with the capability is exactly the
/// failure this arrangement exists to prevent -- see the `#C163` block in
/// `cc/tests/preprocessor/std_dialect.rs`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Optimization {
    /// 0-3, as in `-O0`..`-O3`. `-Og` is 1 and `-Os` is 2.
    level: u8,
    /// `-Os`: prefer smaller code where the choice arises. Read by
    /// `__OPTIMIZE_SIZE__`; c17 makes no size-vs-speed choice of its own yet.
    for_size: bool,
    /// `-fno-inline`: general inlining is off whatever the level says.
    ///
    /// Separate from the level because GCC keeps them separate: `-O2
    /// -fno-inline` still optimizes, and still defines `__OPTIMIZE__`, while
    /// also defining `__NO_INLINE__`.
    no_inline: bool,
}

impl Optimization {
    /// Parse the argument of `-O`, as clap's `value_parser`.
    ///
    /// Deliberately narrower than [`is_valid_opt_level`], which recognises
    /// every spelling GCC and Clang accept so that `-Ofast` is still routed
    /// here and can be turned down by name, rather than being mistaken for a
    /// source file operand.
    pub fn from_flag(spelling: &str) -> Result<Self, String> {
        let (level, for_size) = match spelling {
            "0" => (0, false),
            "1" => (1, false),
            "2" => (2, false),
            "3" => (3, false),
            // GCC's "optimize for debugging": -O1 minus the passes that
            // confuse a debugger. c17 has no such pass to drop, so the level
            // is all that carries over.
            "g" => (1, false),
            // -Os is -O2 without the size-increasing choices. c17 makes no
            // size-vs-speed choice yet, so the flag records the intent and
            // sets `__OPTIMIZE_SIZE__` for the headers that read it.
            "s" => (2, true),
            "fast" => {
                return Err(
                    "-Ofast is not supported: it relaxes IEEE arithmetic, and c17 has no \
                     fast-math mode to relax it into. Use -O3."
                        .to_string(),
                )
            }
            "z" => return Err("-Oz is not supported. Use -Os.".to_string()),
            other => return Err(format!("invalid optimization level '{other}'")),
        };
        Ok(Self {
            level,
            for_size,
            no_inline: false,
        })
    }

    /// Whether any optimization runs at all. Drives `__OPTIMIZE__`.
    pub fn optimizes(&self) -> bool {
        self.level > 0
    }

    /// Whether a single-call-site function may be inlined up to the large
    /// ceiling rather than the small one -- what `-O2` buys over `-O1`.
    pub fn inlines_aggressively(&self) -> bool {
        self.level >= 2
    }

    /// Whether functions are inlined on their merits.
    ///
    /// False at `-O0` and under `-fno-inline`. `__attribute__((always_inline))`
    /// is *not* covered by this and still fires either way -- that is GCC's
    /// behaviour, and glibc's `__fortify_function` depends on it. Drives
    /// `__NO_INLINE__`, which GCC likewise defines while still honouring the
    /// attribute.
    pub fn inlines_generally(&self) -> bool {
        self.optimizes() && !self.no_inline
    }

    /// Record `-fno-inline` / `-finline`, last one winning.
    pub fn set_inlining(&mut self, enabled: bool) {
        self.no_inline = !enabled;
    }
}

/// Maximum iterations for the optimization fixed-point loop.
/// Prevents infinite loops if passes keep making changes.
const MAX_ITERATIONS: usize = 10;

// ============================================================================
// Pass Runner
// ============================================================================

/// Optimize a module as `opt` asks.
///
/// Level 0: nothing but `__attribute__((always_inline))` inlining
/// Level 1+: Run inlining, InstCombine, and DCE passes
pub fn optimize_module(module: &mut Module, opt: Optimization) {
    // Phase 1: Function inlining (module-level pass)
    // This inlines small functions at their call sites and removes
    // dead static functions that were fully inlined.
    //
    // Runs even at -O0, where it admits only `__attribute__((always_inline))`
    // functions -- gcc honours that attribute with optimization off. It is a
    // no-op for a module that has none.
    inline::run(module, opt);

    if !opt.optimizes() {
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The spellings GCC and Clang accept, and what each means here.
    #[test]
    fn optimization_parses_every_supported_spelling() {
        for (flag, optimizes, aggressive) in [
            ("0", false, false),
            ("1", true, false),
            ("2", true, true),
            ("3", true, true),
            // -Og is -O1 minus the debugger-hostile passes; c17 has none to
            // drop, so only the level carries over.
            ("g", true, false),
            // -Os is -O2 without the size-increasing choices.
            ("s", true, true),
        ] {
            let opt = Optimization::from_flag(flag)
                .unwrap_or_else(|e| panic!("-O{flag} should parse: {e}"));
            assert_eq!(opt.optimizes(), optimizes, "-O{flag} optimizes()");
            assert_eq!(
                opt.inlines_aggressively(),
                aggressive,
                "-O{flag} inlines_aggressively()"
            );
        }

        // Not supported, and turned down by name rather than by clap's
        // "invalid digit found in string".
        for (flag, expect) in [("fast", "-Ofast"), ("z", "-Oz")] {
            let err = Optimization::from_flag(flag).expect_err("-O{flag} must be refused");
            assert!(
                err.contains(expect),
                "{flag}: message should name the flag, got {err}"
            );
        }
        assert!(Optimization::from_flag("9").is_err());
        assert!(Optimization::from_flag("").is_err());

        // No -O at all is no optimization.
        assert!(!Optimization::default().optimizes());
    }

    /// `-fno-inline` is orthogonal to the level: it stops general inlining
    /// without stopping optimization, which is why GCC defines `__OPTIMIZE__`
    /// and `__NO_INLINE__` together for `-O2 -fno-inline`.
    #[test]
    fn optimization_separates_inlining_from_the_level() {
        let mut o2 = Optimization::from_flag("2").unwrap();
        assert!(o2.optimizes() && o2.inlines_generally());

        o2.set_inlining(false);
        assert!(o2.optimizes(), "-fno-inline must not stop optimization");
        assert!(!o2.inlines_generally());
        assert!(o2.inlines_aggressively(), "still level 2");

        // Last one wins.
        o2.set_inlining(true);
        assert!(o2.inlines_generally());

        // At -O0 nothing is inlined generally, with or without the flag.
        let mut o0 = Optimization::from_flag("0").unwrap();
        assert!(!o0.inlines_generally());
        o0.set_inlining(true);
        assert!(
            !o0.inlines_generally(),
            "-finline does not turn on inlining at -O0, as in GCC"
        );
    }
}
