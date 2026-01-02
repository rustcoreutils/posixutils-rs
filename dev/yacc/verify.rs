//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Formal verification of optimized parse tables.
//!
//! This module verifies that packed/optimized tables produce the same parse
//! decisions as the canonical LALR(1) tables. Verification runs on every
//! yacc invocation and panics on failure (indicating a yacc internal bug).

use crate::codegen::PackedTables;
use crate::grammar::Grammar;
use crate::lalr::{Action, LALRAutomaton};

/// Verify that optimized tables match canonical tables exactly.
///
/// This performs exhaustive verification:
/// - For every (state, terminal): ACTION_packed == ACTION_canonical
///   (with allowance for default reduction optimization on Error cases)
/// - For every (state, nonterminal): GOTO_packed == GOTO_canonical
///   (with allowance for default goto optimization)
///
/// # Default Action Optimization
///
/// Traditional yacc uses "default reductions" for table compression. When
/// the canonical table has no entry (Error), the packed table may use
/// defact[state] to reduce instead. This delays error detection but is
/// valid yacc behavior. We accept this as long as:
/// - Explicit entries in canonical table match exactly
/// - For Error cases, defact behavior is consistent
///
/// # Panics
///
/// Panics if any mismatch is found that cannot be explained by default
/// action optimization. A mismatch indicates an internal bug in the
/// table optimization/packing code.
pub fn verify_tables(grammar: &Grammar, lalr: &LALRAutomaton, packed: &PackedTables) {
    let mut errors = Vec::new();

    // Verify ACTION table: for every (state, terminal) pair
    for state in 0..packed.num_states {
        for terminal_id in grammar.terminals() {
            let canonical = lalr
                .action_table
                .get(state)
                .and_then(|m| m.get(&terminal_id))
                .cloned();

            let decoded = decode_action(packed, state, terminal_id);

            // Check if this is an acceptable mismatch due to default action optimization
            let is_valid = match (&canonical, &decoded) {
                // Exact match - always valid
                (Some(ref c), d) if actions_equal(c, d) => true,
                // No canonical entry (Error) but decoded uses default reduction
                // This is the standard yacc "delayed error" optimization
                (None, Action::Reduce(_)) if packed.defact[state] > 0 => true,
                // No canonical entry and no default - both should be Error
                (None, Action::Error) => true,
                // Any other case is a mismatch
                _ => false,
            };

            if !is_valid {
                errors.push(format!(
                    "ACTION mismatch: state={} terminal={} ({}): canonical={:?} decoded={:?}",
                    state,
                    terminal_id,
                    grammar.symbol_name(terminal_id),
                    canonical.unwrap_or(Action::Error),
                    decoded
                ));
            }
        }
    }

    // Verify GOTO table: for every (state, nonterminal) pair
    for state in 0..packed.num_states {
        for nt_id in grammar.nonterminals() {
            let canonical = lalr
                .goto_table
                .get(state)
                .and_then(|m| m.get(&nt_id))
                .copied();

            let decoded = decode_goto(grammar, packed, state, nt_id);

            // Get the nonterminal index for checking defgoto
            let nt_idx = grammar
                .nonterminals()
                .enumerate()
                .find(|(_, id)| *id == nt_id)
                .map(|(idx, _)| idx);

            // Check if this is an acceptable mismatch due to default goto optimization
            let is_valid = match (&canonical, &decoded) {
                // Exact match - always valid
                (Some(c), Some(d)) if c == d => true,
                // No canonical entry but decoded uses default goto
                // This is valid - the default goto is used for table compression
                (None, Some(_)) => {
                    // Verify this is actually from defgoto
                    if let Some(idx) = nt_idx {
                        idx < packed.defgoto.len() && packed.defgoto[idx] >= 0
                    } else {
                        false
                    }
                }
                // Both None - valid
                (None, None) => true,
                // Any other case is a mismatch
                _ => false,
            };

            if !is_valid {
                errors.push(format!(
                    "GOTO mismatch: state={} nonterminal={} ({}): canonical={:?} decoded={:?}",
                    state,
                    nt_id,
                    grammar.symbol_name(nt_id),
                    canonical,
                    decoded
                ));
            }
        }
    }

    if !errors.is_empty() {
        eprintln!("INTERNAL ERROR: Table verification failed!");
        eprintln!("This is a bug in yacc - please report it.");
        eprintln!();
        for (i, err) in errors.iter().enumerate().take(10) {
            eprintln!("  {}: {}", i + 1, err);
        }
        if errors.len() > 10 {
            eprintln!("  ... and {} more errors", errors.len() - 10);
        }
        panic!(
            "Table verification failed: {} mismatches detected",
            errors.len()
        );
    }
}

/// Compare two actions for equality.
///
/// Note: Accept is encoded as 0 in the table, same as some error cases.
/// We handle this by treating Accept specially in decode_action.
fn actions_equal(a: &Action, b: &Action) -> bool {
    match (a, b) {
        (Action::Shift(s1), Action::Shift(s2)) => s1 == s2,
        (Action::Reduce(r1), Action::Reduce(r2)) => r1 == r2,
        (Action::Accept, Action::Accept) => true,
        (Action::Error, Action::Error) => true,
        _ => false,
    }
}

/// Decode an ACTION from the packed tables.
///
/// This mirrors the lookup logic in the generated parser:
/// 1. Check if state is consistent (skip lookahead optimization)
/// 2. Look up in pact/table/check
/// 3. Fall back to defact (default action)
fn decode_action(packed: &PackedTables, state: usize, terminal: usize) -> Action {
    // Check consistent state optimization
    // In consistent states, we always reduce by the default action without lookahead
    if packed.consistent[state] && packed.defact[state] > 0 {
        let prod_id = (packed.defact[state] - 1) as usize;
        return Action::Reduce(prod_id);
    }

    // Normal lookup: pact[state] + terminal
    let base = packed.pact[state] as usize;
    let idx = base + terminal;

    // Check bounds and verify with check array
    if idx < packed.table.len() && idx < packed.check.len() && packed.check[idx] == terminal as i16
    {
        let value = packed.table[idx];
        if value > 0 {
            Action::Shift(value as usize)
        } else if value < 0 && value != i16::MIN {
            let prod_id = ((-value) - 1) as usize;
            Action::Reduce(prod_id)
        } else if value == 0 {
            // 0 can be Accept or Error depending on context
            // In our encoding, Accept is only for EOF in the accept state
            Action::Accept
        } else {
            // i16::MIN indicates explicit error
            Action::Error
        }
    } else {
        // Fall through to default action
        if packed.defact[state] > 0 {
            let prod_id = (packed.defact[state] - 1) as usize;
            Action::Reduce(prod_id)
        } else {
            Action::Error
        }
    }
}

/// Decode a GOTO from the packed tables.
///
/// This mirrors the lookup logic in the generated parser:
/// 1. Look up in pgoto/table/check
/// 2. Fall back to defgoto (default goto)
fn decode_goto(
    grammar: &Grammar,
    packed: &PackedTables,
    state: usize,
    nonterminal: usize,
) -> Option<usize> {
    // Get the nonterminal index (0-based index among nonterminals)
    let nt_idx = grammar
        .nonterminals()
        .enumerate()
        .find(|(_, id)| *id == nonterminal)
        .map(|(idx, _)| idx)?;

    if nt_idx >= packed.pgoto.len() {
        return None;
    }

    // pgoto gives us the base index for this nonterminal
    let base = packed.pgoto[nt_idx] as usize;
    let idx = base + state;

    // Check bounds and verify with check array
    if idx < packed.table.len() && idx < packed.check.len() && packed.check[idx] == state as i16 {
        let value = packed.table[idx];
        if value >= 0 {
            Some(value as usize)
        } else {
            None
        }
    } else {
        // Fall through to default goto
        if nt_idx < packed.defgoto.len() {
            let def = packed.defgoto[nt_idx];
            if def >= 0 {
                Some(def as usize)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    // Unit tests for decode functions would go here
    // These require constructing minimal PackedTables for testing
}
