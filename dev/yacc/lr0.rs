//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! LR(0) automaton construction

use crate::grammar::{Grammar, ProductionId, SymbolId, AUGMENTED_START};
use std::collections::{BTreeSet, HashMap, HashSet};

/// An LR(0) item: a production with a dot position
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item {
    /// Production index
    pub production: ProductionId,
    /// Dot position (0 = before first symbol, len = after last symbol)
    pub dot: usize,
}

impl Item {
    pub fn new(production: ProductionId, dot: usize) -> Self {
        Item { production, dot }
    }

    /// Check if dot is at the end
    pub fn is_complete(&self, grammar: &Grammar) -> bool {
        self.dot >= grammar.productions[self.production].rhs.len()
    }

    /// Get the symbol after the dot, if any
    pub fn symbol_after_dot(&self, grammar: &Grammar) -> Option<SymbolId> {
        let prod = &grammar.productions[self.production];
        prod.rhs.get(self.dot).copied()
    }

    /// Get the symbols after the dot (beta)
    #[allow(dead_code)]
    pub fn symbols_after_dot<'a>(&self, grammar: &'a Grammar) -> &'a [SymbolId] {
        let prod = &grammar.productions[self.production];
        if self.dot < prod.rhs.len() {
            &prod.rhs[self.dot..]
        } else {
            &[]
        }
    }

    /// Advance the dot by one position
    pub fn advance(&self) -> Self {
        Item {
            production: self.production,
            dot: self.dot + 1,
        }
    }
}

/// Index type for LR(0) states
pub type StateId = usize;

/// An LR(0) state (set of items)
#[derive(Debug, Clone)]
pub struct State {
    /// State index
    pub id: StateId,
    /// Kernel items (items with dot not at start, except initial item)
    pub kernel: BTreeSet<Item>,
    /// All items (kernel + closure)
    pub items: BTreeSet<Item>,
    /// Transitions: symbol -> state
    pub transitions: HashMap<SymbolId, StateId>,
}

/// The LR(0) automaton
#[derive(Debug, Clone)]
pub struct LR0Automaton {
    /// All states
    pub states: Vec<State>,
    /// Mapping from kernel item set to state index
    pub kernel_to_state: HashMap<BTreeSet<Item>, StateId>,
}

impl LR0Automaton {
    /// Get the initial state
    #[allow(dead_code)]
    pub fn initial_state(&self) -> StateId {
        0
    }

    /// Get a state by ID
    #[allow(dead_code)]
    pub fn state(&self, id: StateId) -> &State {
        &self.states[id]
    }

    /// Get transition from a state on a symbol
    #[allow(dead_code)]
    pub fn goto(&self, state: StateId, symbol: SymbolId) -> Option<StateId> {
        self.states[state].transitions.get(&symbol).copied()
    }
}

/// Build the LR(0) automaton for a grammar
pub fn build(grammar: &Grammar) -> LR0Automaton {
    let mut automaton = LR0Automaton {
        states: Vec::new(),
        kernel_to_state: HashMap::new(),
    };

    // Find the augmented start production
    let start_prod = grammar
        .productions
        .iter()
        .position(|p| p.lhs == AUGMENTED_START)
        .expect("augmented start production not found");

    // Initial item: S' -> . S $
    let initial_item = Item::new(start_prod, 0);
    let mut initial_kernel = BTreeSet::new();
    initial_kernel.insert(initial_item);

    // Create initial state
    let initial_items = closure(&initial_kernel, grammar);
    let initial_state = State {
        id: 0,
        kernel: initial_kernel.clone(),
        items: initial_items,
        transitions: HashMap::new(),
    };

    automaton.states.push(initial_state);
    automaton.kernel_to_state.insert(initial_kernel, 0);

    // Build states using worklist algorithm
    let mut worklist = vec![0usize];

    while let Some(state_id) = worklist.pop() {
        // Find all symbols after dots in this state
        let symbols: HashSet<SymbolId> = automaton.states[state_id]
            .items
            .iter()
            .filter_map(|item| item.symbol_after_dot(grammar))
            .collect();

        for symbol in symbols {
            // Compute GOTO(state, symbol)
            let goto_kernel =
                compute_goto_kernel(&automaton.states[state_id].items, symbol, grammar);

            if goto_kernel.is_empty() {
                continue;
            }

            // Check if this state already exists
            let next_state_id =
                if let Some(&existing_id) = automaton.kernel_to_state.get(&goto_kernel) {
                    existing_id
                } else {
                    // Create new state
                    let next_id = automaton.states.len();
                    let next_items = closure(&goto_kernel, grammar);
                    let next_state = State {
                        id: next_id,
                        kernel: goto_kernel.clone(),
                        items: next_items,
                        transitions: HashMap::new(),
                    };
                    automaton.states.push(next_state);
                    automaton.kernel_to_state.insert(goto_kernel, next_id);
                    worklist.push(next_id);
                    next_id
                };

            // Add transition
            automaton.states[state_id]
                .transitions
                .insert(symbol, next_state_id);
        }
    }

    automaton
}

/// Compute closure of a set of items
fn closure(items: &BTreeSet<Item>, grammar: &Grammar) -> BTreeSet<Item> {
    let mut result = items.clone();
    let mut worklist: Vec<Item> = items.iter().copied().collect();

    while let Some(item) = worklist.pop() {
        // If item is A -> α . B β, add all productions B -> . γ
        if let Some(sym) = item.symbol_after_dot(grammar) {
            if !grammar.is_terminal(sym) {
                // Add all productions for this non-terminal
                for &prod_id in grammar.productions_for_symbol(sym) {
                    let new_item = Item::new(prod_id, 0);
                    if !result.contains(&new_item) {
                        result.insert(new_item);
                        worklist.push(new_item);
                    }
                }
            }
        }
    }

    result
}

/// Compute kernel of GOTO(items, symbol)
fn compute_goto_kernel(
    items: &BTreeSet<Item>,
    symbol: SymbolId,
    grammar: &Grammar,
) -> BTreeSet<Item> {
    let mut result = BTreeSet::new();

    for &item in items {
        if item.symbol_after_dot(grammar) == Some(symbol) {
            result.insert(item.advance());
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse;

    fn make_grammar(input: &str) -> Grammar {
        let tokens = lex(input).unwrap();
        let parsed = parse(&tokens).unwrap();
        Grammar::from_parsed(parsed).unwrap()
    }

    #[test]
    fn test_simple_automaton() {
        let grammar = make_grammar(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        let automaton = build(&grammar);

        // Should have at least 3 states:
        // 0: S' -> . s $, s -> . A
        // 1: s -> A .
        // 2: S' -> s . $
        assert!(automaton.states.len() >= 3);
    }

    #[test]
    fn test_expression_grammar() {
        let grammar = make_grammar(
            r#"
%token NUM PLUS TIMES
%%
expr : expr PLUS term
     | term
     ;
term : term TIMES factor
     | factor
     ;
factor : NUM
       ;
"#,
        );

        let automaton = build(&grammar);

        // This grammar should produce several states
        assert!(automaton.states.len() > 5);

        // Initial state should exist
        let initial = automaton.state(0);
        assert!(!initial.items.is_empty());
    }

    #[test]
    fn test_transitions() {
        let grammar = make_grammar(
            r#"
%token A B
%%
s : A B
  ;
"#,
        );

        let automaton = build(&grammar);

        // From initial state, should be able to transition on 's' and 'A'
        let initial = automaton.state(0);

        let s_id = *grammar.symbol_map.get("s").unwrap();
        let a_id = *grammar.symbol_map.get("A").unwrap();

        // Check that transitions exist
        assert!(initial.transitions.contains_key(&s_id) || initial.transitions.contains_key(&a_id));
    }

    #[test]
    fn test_item_complete() {
        let grammar = make_grammar(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        // Find production s -> A
        let prod_id = grammar
            .productions
            .iter()
            .position(|p| grammar.symbol_name(p.lhs) == "s")
            .unwrap();

        let item0 = Item::new(prod_id, 0);
        let item1 = Item::new(prod_id, 1);

        assert!(!item0.is_complete(&grammar));
        assert!(item1.is_complete(&grammar));
    }

    #[test]
    fn test_closure() {
        let grammar = make_grammar(
            r#"
%token A
%%
s : a
  ;
a : A
  ;
"#,
        );

        // Find augmented production and s production
        let aug_prod = grammar
            .productions
            .iter()
            .position(|p| p.lhs == AUGMENTED_START)
            .unwrap();

        let mut kernel = BTreeSet::new();
        kernel.insert(Item::new(aug_prod, 0));

        let closed = closure(&kernel, &grammar);

        // Should include items for s and a productions
        assert!(closed.len() > 1);
    }
}
