//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! LALR(1) lookahead computation and parse table construction.
//!
//! Converts an LR(0) automaton to LALR(1) by computing lookahead sets for
//! reduce actions. Uses a two-phase approach:
//! 1. Lookahead propagation: tracks how lookaheads flow between states
//! 2. Table construction: builds ACTION/GOTO tables with conflict resolution

use crate::first_follow::FirstFollow;
use crate::grammar::{Grammar, ProductionId, SymbolId, AUGMENTED_START, EOF_SYMBOL};
use crate::lr0::{Item, LR0Automaton, StateId};
use crate::parser::Associativity;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Type alias for the result of build_tables to reduce type complexity
type BuildTablesResult = (
    Vec<BTreeMap<SymbolId, Action>>,
    Vec<BTreeMap<SymbolId, StateId>>,
    HashMap<(StateId, SymbolId), Vec<Action>>,
);

/// Action in the parse table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Shift and go to state
    Shift(StateId),
    /// Reduce by production
    Reduce(ProductionId),
    /// Accept the input
    Accept,
    /// Error (no action)
    Error,
}

/// LALR(1) automaton with lookahead information
#[derive(Debug)]
pub struct LALRAutomaton {
    /// LR(0) automaton
    pub lr0: LR0Automaton,
    /// ACTION table: state -> (terminal -> action)
    pub action_table: Vec<BTreeMap<SymbolId, Action>>,
    /// GOTO table: state -> (non-terminal -> state)
    pub goto_table: Vec<BTreeMap<SymbolId, StateId>>,
    /// Conflict information: (state, symbol) -> list of conflicting actions
    pub conflicts: HashMap<(StateId, SymbolId), Vec<Action>>,
}

impl LALRAutomaton {
    /// Count unresolved conflicts
    pub fn count_conflicts(&self) -> (usize, usize) {
        let mut sr = 0;
        let mut rr = 0;

        for actions in self.conflicts.values() {
            let shifts = actions
                .iter()
                .filter(|a| matches!(a, Action::Shift(_)))
                .count();
            let reduces = actions
                .iter()
                .filter(|a| matches!(a, Action::Reduce(_)))
                .count();

            if shifts > 0 && reduces > 0 {
                sr += 1;
            }
            if reduces > 1 {
                rr += 1;
            }
        }

        (sr, rr)
    }

    /// Get action for state and terminal
    #[cfg(test)]
    pub fn action(&self, state: StateId, terminal: SymbolId) -> &Action {
        self.action_table
            .get(state)
            .and_then(|m| m.get(&terminal))
            .unwrap_or(&Action::Error)
    }
}

/// Compute LALR(1) automaton from LR(0) automaton
pub fn compute(lr0: &LR0Automaton, grammar: &Grammar, ff: &FirstFollow) -> LALRAutomaton {
    // Compute lookaheads using propagation algorithm
    let lookaheads = compute_lookaheads(lr0, grammar, ff);

    // Build ACTION and GOTO tables
    let (action_table, goto_table, conflicts) = build_tables(lr0, grammar, &lookaheads);

    LALRAutomaton {
        lr0: lr0.clone(),
        action_table,
        goto_table,
        conflicts,
    }
}

/// Compute LALR(1) lookahead sets using propagation algorithm.
///
/// For each kernel item in each state, determines which terminals can follow
/// a reduction. Uses a marker symbol to distinguish:
/// - Spontaneous generation: lookahead derived from FIRST(suffix)
/// - Propagation: lookahead inherited from another item
///
/// Fixed-point iteration propagates lookaheads until stable.
fn compute_lookaheads(
    lr0: &LR0Automaton,
    grammar: &Grammar,
    ff: &FirstFollow,
) -> HashMap<(StateId, Item), HashSet<SymbolId>> {
    // Phase 1: Determine propagation edges and spontaneous lookaheads

    let mut lookaheads: HashMap<(StateId, Item), HashSet<SymbolId>> = HashMap::new();
    let mut propagates: Vec<((StateId, Item), (StateId, Item))> = Vec::new();

    // Initialize lookahead sets
    for state in &lr0.states {
        for &item in &state.kernel {
            lookaheads.insert((state.id, item), HashSet::new());
        }
    }

    // Add EOF to initial state's initial item
    let aug_prod = grammar
        .productions
        .iter()
        .position(|p| p.lhs == AUGMENTED_START)
        .expect("augmented production not found");
    let initial_item = Item::new(aug_prod, 0);
    lookaheads
        .get_mut(&(0, initial_item))
        .unwrap()
        .insert(EOF_SYMBOL);

    // Determine spontaneous lookaheads and propagation
    for state in &lr0.states {
        for &kernel_item in &state.kernel {
            // Use a marker lookahead (#) to detect propagation
            // We'll use a placeholder symbol ID that doesn't exist
            let marker: SymbolId = usize::MAX;

            // Compute closure of {[kernel_item, #]}
            let mut closure_items: HashMap<Item, HashSet<SymbolId>> = HashMap::new();
            closure_items.insert(kernel_item, [marker].into_iter().collect());

            let mut worklist = vec![kernel_item];

            while let Some(item) = worklist.pop() {
                if let Some(sym) = item.symbol_after_dot(grammar) {
                    if !grammar.is_terminal(sym) {
                        // For item [A -> α.Bβ, a], add [B -> .γ, b] for each b in FIRST(βa)
                        let beta = &grammar.productions[item.production].rhs[item.dot + 1..];
                        let item_lookaheads = closure_items.get(&item).unwrap().clone();

                        for &prod_id in grammar.productions_for_symbol(sym) {
                            let new_item = Item::new(prod_id, 0);

                            // Compute FIRST(β a) for each a in item's lookahead
                            let mut new_lookaheads = HashSet::new();

                            for &la in &item_lookaheads {
                                let first = ff.first_of_sequence_with_lookahead(beta, la, grammar);
                                new_lookaheads.extend(first);
                            }

                            let entry = closure_items.entry(new_item).or_default();
                            let old_size = entry.len();
                            entry.extend(new_lookaheads);

                            if (entry.len() > old_size || old_size == 0)
                                && !worklist.contains(&new_item)
                            {
                                worklist.push(new_item);
                            }
                        }
                    }
                }
            }

            // For each item in closure with symbol X after dot
            for (&item, las) in &closure_items {
                if let Some(sym) = item.symbol_after_dot(grammar) {
                    if let Some(&goto_state) = state.transitions.get(&sym) {
                        let goto_item = item.advance();

                        // Ensure target item exists in lookaheads
                        lookaheads.entry((goto_state, goto_item)).or_default();

                        for &la in las {
                            if la == marker {
                                // Propagation
                                propagates.push(((state.id, kernel_item), (goto_state, goto_item)));
                            } else {
                                // Spontaneous generation
                                lookaheads
                                    .get_mut(&(goto_state, goto_item))
                                    .unwrap()
                                    .insert(la);
                            }
                        }
                    }
                }
            }
        }
    }

    // Phase 2: Fixed-point propagation of lookaheads
    let mut changed = true;
    while changed {
        changed = false;

        for &(from, to) in &propagates {
            if let Some(from_las) = lookaheads.get(&from).cloned() {
                if let Some(to_las) = lookaheads.get_mut(&to) {
                    let old_size = to_las.len();
                    to_las.extend(from_las);
                    if to_las.len() > old_size {
                        changed = true;
                    }
                }
            }
        }
    }

    // Phase 3: Extend kernel lookaheads to closure items via FIRST computation
    let mut full_lookaheads: HashMap<(StateId, Item), HashSet<SymbolId>> = HashMap::new();

    for state in &lr0.states {
        // For each kernel item, compute lookaheads for closure items
        for &kernel_item in &state.kernel {
            let kernel_las = lookaheads
                .get(&(state.id, kernel_item))
                .cloned()
                .unwrap_or_default();

            // Compute closure with lookaheads
            let mut closure_items: HashMap<Item, HashSet<SymbolId>> = HashMap::new();
            closure_items.insert(kernel_item, kernel_las);

            let mut worklist = vec![kernel_item];

            while let Some(item) = worklist.pop() {
                if let Some(sym) = item.symbol_after_dot(grammar) {
                    if !grammar.is_terminal(sym) {
                        let beta = &grammar.productions[item.production].rhs[item.dot + 1..];
                        let item_lookaheads = closure_items.get(&item).unwrap().clone();

                        for &prod_id in grammar.productions_for_symbol(sym) {
                            let new_item = Item::new(prod_id, 0);

                            let mut new_lookaheads = HashSet::new();
                            for &la in &item_lookaheads {
                                let first = ff.first_of_sequence_with_lookahead(beta, la, grammar);
                                new_lookaheads.extend(first);
                            }

                            let entry = closure_items.entry(new_item).or_default();
                            let old_size = entry.len();
                            entry.extend(new_lookaheads);

                            if entry.len() > old_size {
                                worklist.push(new_item);
                            }
                        }
                    }
                }
            }

            // Merge into full_lookaheads
            for (item, las) in closure_items {
                full_lookaheads
                    .entry((state.id, item))
                    .or_default()
                    .extend(las);
            }
        }
    }

    full_lookaheads
}

/// Build ACTION and GOTO tables from LR(0) states and lookahead sets.
///
/// ACTION table: maps (state, terminal) to shift/reduce/accept/error.
/// GOTO table: maps (state, nonterminal) to next state after reduction.
fn build_tables(
    lr0: &LR0Automaton,
    grammar: &Grammar,
    lookaheads: &HashMap<(StateId, Item), HashSet<SymbolId>>,
) -> BuildTablesResult {
    let num_states = lr0.states.len();
    let mut action_table: Vec<BTreeMap<SymbolId, Action>> = vec![BTreeMap::new(); num_states];
    let mut goto_table: Vec<BTreeMap<SymbolId, StateId>> = vec![BTreeMap::new(); num_states];
    let mut conflicts: HashMap<(StateId, SymbolId), Vec<Action>> = HashMap::new();

    for state in &lr0.states {
        // Build GOTO entries from transitions
        for (&sym, &next_state) in &state.transitions {
            if grammar.is_terminal(sym) {
                // Shift action
                add_action(
                    &mut action_table[state.id],
                    sym,
                    Action::Shift(next_state),
                    grammar,
                    &mut conflicts,
                    state.id,
                );
            } else {
                // GOTO entry
                goto_table[state.id].insert(sym, next_state);
            }
        }

        // Build reduce/accept actions from complete items
        for &item in &state.items {
            if item.is_complete(grammar) {
                let prod = &grammar.productions[item.production];

                if prod.lhs == AUGMENTED_START {
                    // Accept action
                    add_action(
                        &mut action_table[state.id],
                        EOF_SYMBOL,
                        Action::Accept,
                        grammar,
                        &mut conflicts,
                        state.id,
                    );
                } else {
                    // Reduce action for each lookahead
                    if let Some(las) = lookaheads.get(&(state.id, item)) {
                        for &la in las {
                            add_action(
                                &mut action_table[state.id],
                                la,
                                Action::Reduce(item.production),
                                grammar,
                                &mut conflicts,
                                state.id,
                            );
                        }
                    }
                }
            }
        }
    }

    (action_table, goto_table, conflicts)
}

/// Add action to table, handling conflicts via precedence/associativity.
///
/// On conflict: attempts resolution using operator precedence. If unresolved,
/// records conflict and applies default (shift > reduce, earlier rule wins).
fn add_action(
    table: &mut BTreeMap<SymbolId, Action>,
    symbol: SymbolId,
    action: Action,
    grammar: &Grammar,
    conflicts: &mut HashMap<(StateId, SymbolId), Vec<Action>>,
    state_id: StateId,
) {
    if let Some(existing) = table.get(&symbol) {
        if existing == &action {
            return; // Same action, no conflict
        }

        // Conflict detected
        let resolved = resolve_conflict(existing, &action, symbol, grammar);

        if let Some(winner) = resolved {
            table.insert(symbol, winner);
        } else {
            // Unresolved conflict
            let entry = conflicts.entry((state_id, symbol)).or_default();
            if !entry.contains(existing) {
                entry.push(existing.clone());
            }
            if !entry.contains(&action) {
                entry.push(action.clone());
            }

            // Default resolution: prefer shift over reduce, earlier rule for reduce/reduce
            match (existing, &action) {
                (Action::Shift(_), Action::Reduce(_)) => {
                    // Keep shift (already in table)
                }
                (Action::Reduce(_), Action::Shift(s)) => {
                    table.insert(symbol, Action::Shift(*s));
                }
                (Action::Reduce(r1), Action::Reduce(r2)) => {
                    // Keep earlier rule
                    if r2 < r1 {
                        table.insert(symbol, Action::Reduce(*r2));
                    }
                }
                _ => {}
            }
        }
    } else {
        table.insert(symbol, action);
    }
}

/// Resolve shift/reduce conflict using precedence and associativity.
///
/// Compares rule precedence vs terminal precedence:
/// - Higher precedence wins
/// - Equal precedence: left-assoc reduces, right-assoc shifts, nonassoc errors
///
/// Returns None for reduce/reduce conflicts (no precedence-based resolution).
fn resolve_conflict(
    existing: &Action,
    new: &Action,
    symbol: SymbolId,
    grammar: &Grammar,
) -> Option<Action> {
    match (existing, new) {
        (Action::Shift(_), Action::Reduce(prod_id))
        | (Action::Reduce(prod_id), Action::Shift(_)) => {
            let shift_action = if matches!(existing, Action::Shift(_)) {
                existing.clone()
            } else {
                new.clone()
            };

            let prod = &grammar.productions[*prod_id];
            let symbol_info = &grammar.symbols[symbol];

            // Check if both have precedence
            if prod.precedence > 0 && symbol_info.precedence > 0 {
                use std::cmp::Ordering;
                match prod.precedence.cmp(&symbol_info.precedence) {
                    Ordering::Greater => {
                        // Reduce wins
                        return Some(Action::Reduce(*prod_id));
                    }
                    Ordering::Less => {
                        // Shift wins
                        return Some(shift_action);
                    }
                    Ordering::Equal => {
                        // Same precedence, use associativity
                        match symbol_info.associativity {
                            Some(Associativity::Left) => {
                                return Some(Action::Reduce(*prod_id));
                            }
                            Some(Associativity::Right) => {
                                return Some(shift_action);
                            }
                            Some(Associativity::NonAssoc) => {
                                return Some(Action::Error);
                            }
                            None => {}
                        }
                    }
                }
            }

            // Cannot resolve by precedence
            None
        }
        _ => None, // Cannot resolve reduce/reduce by precedence
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::first_follow;
    use crate::lexer::lex;
    use crate::lr0;
    use crate::parser::parse;

    fn make_lalr(input: &str) -> (Grammar, LALRAutomaton) {
        let tokens = lex(input).unwrap();
        let parsed = parse(&tokens).unwrap();
        let grammar = Grammar::from_parsed(parsed).unwrap();
        let ff = first_follow::compute(&grammar);
        let lr0 = lr0::build(&grammar);
        let lalr = compute(&lr0, &grammar, &ff);
        (grammar, lalr)
    }

    #[test]
    fn test_simple_grammar() {
        let (grammar, lalr) = make_lalr(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        // Should have no conflicts
        let (sr, rr) = lalr.count_conflicts();
        assert_eq!(sr, 0);
        assert_eq!(rr, 0);

        // Check that we can shift A in initial state
        let a_id = *grammar.symbol_map.get("A").unwrap();
        let initial_action = lalr.action(0, a_id);
        assert!(matches!(initial_action, Action::Shift(_)));
    }

    #[test]
    fn test_expression_grammar_with_prec() {
        let (_grammar, lalr) = make_lalr(
            r#"
%token NUM
%left '+' '-'
%left '*' '/'
%%
expr : expr '+' expr
     | expr '-' expr
     | expr '*' expr
     | expr '/' expr
     | NUM
     ;
"#,
        );

        // With proper precedence, should have no conflicts
        let (sr, rr) = lalr.count_conflicts();
        assert_eq!(
            sr, 0,
            "should have no shift/reduce conflicts with precedence"
        );
        assert_eq!(rr, 0);
    }

    #[test]
    fn test_ambiguous_grammar() {
        let (_grammar, lalr) = make_lalr(
            r#"
%token NUM
%%
expr : expr '+' expr
     | NUM
     ;
"#,
        );

        // Without precedence, should have shift/reduce conflict
        let (sr, _rr) = lalr.count_conflicts();
        assert!(
            sr > 0,
            "should have shift/reduce conflict without precedence"
        );
    }

    #[test]
    fn test_accept_action() {
        let (_grammar, lalr) = make_lalr(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        // Verify that there is an Accept action somewhere in the parse table
        let has_accept = lalr
            .action_table
            .iter()
            .any(|actions| actions.values().any(|a| matches!(a, Action::Accept)));
        assert!(has_accept, "should have at least one Accept action");
    }

    #[test]
    fn test_reduce_action() {
        let (grammar, lalr) = make_lalr(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        // Find state where we have A shifted
        let a_id = *grammar.symbol_map.get("A").unwrap();
        let shift_a = lalr.action(0, a_id);
        if let Action::Shift(state) = shift_a {
            // In this state, action on EOF should be Reduce
            let reduce_action = lalr.action(*state, EOF_SYMBOL);
            assert!(matches!(reduce_action, Action::Reduce(_)));
        }
    }
}
