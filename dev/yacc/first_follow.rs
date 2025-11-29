//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! FIRST and FOLLOW set computation for grammar analysis

use crate::grammar::{Grammar, SymbolId, EOF_SYMBOL};
use std::collections::{HashMap, HashSet};

/// FIRST and FOLLOW sets for a grammar
#[derive(Debug)]
pub struct FirstFollow {
    /// FIRST sets for each symbol
    pub first: HashMap<SymbolId, HashSet<SymbolId>>,
    /// FOLLOW sets for each non-terminal
    #[allow(dead_code)]
    pub follow: HashMap<SymbolId, HashSet<SymbolId>>,
    /// Nullable symbols
    pub nullable: HashSet<SymbolId>,
}

impl FirstFollow {
    /// Compute FIRST set for a sequence of symbols
    #[allow(dead_code)]
    pub fn first_of_sequence(&self, symbols: &[SymbolId], grammar: &Grammar) -> HashSet<SymbolId> {
        let mut result = HashSet::new();

        for &sym in symbols {
            // Add FIRST(sym) - {epsilon}
            if let Some(first_sym) = self.first.get(&sym) {
                for &f in first_sym {
                    result.insert(f);
                }
            } else if grammar.is_terminal(sym) {
                result.insert(sym);
            }

            // If sym is not nullable, stop
            if !self.nullable.contains(&sym) {
                return result;
            }
        }

        // If we get here, all symbols were nullable
        result
    }

    /// Compute FIRST set for a sequence of symbols, plus a lookahead
    pub fn first_of_sequence_with_lookahead(
        &self,
        symbols: &[SymbolId],
        lookahead: SymbolId,
        grammar: &Grammar,
    ) -> HashSet<SymbolId> {
        let mut result = HashSet::new();

        for &sym in symbols {
            if let Some(first_sym) = self.first.get(&sym) {
                for &f in first_sym {
                    result.insert(f);
                }
            } else if grammar.is_terminal(sym) {
                result.insert(sym);
            }

            if !self.nullable.contains(&sym) {
                return result;
            }
        }

        // All symbols were nullable, add the lookahead
        result.insert(lookahead);
        result
    }

    /// Check if a sequence of symbols is nullable
    #[allow(dead_code)]
    pub fn is_sequence_nullable(&self, symbols: &[SymbolId]) -> bool {
        symbols.iter().all(|&s| self.nullable.contains(&s))
    }
}

/// Compute FIRST and FOLLOW sets for a grammar
pub fn compute(grammar: &Grammar) -> FirstFollow {
    let nullable = compute_nullable(grammar);
    let first = compute_first(grammar, &nullable);
    let follow = compute_follow(grammar, &nullable, &first);

    FirstFollow {
        first,
        follow,
        nullable,
    }
}

/// Compute nullable symbols (symbols that can derive epsilon)
fn compute_nullable(grammar: &Grammar) -> HashSet<SymbolId> {
    let mut nullable = HashSet::new();
    let mut changed = true;

    while changed {
        changed = false;

        for prod in &grammar.productions {
            // Skip if LHS is already nullable
            if nullable.contains(&prod.lhs) {
                continue;
            }

            // Check if RHS is all nullable (or empty)
            let rhs_nullable = prod.rhs.iter().all(|&sym| nullable.contains(&sym));

            if rhs_nullable {
                nullable.insert(prod.lhs);
                changed = true;
            }
        }
    }

    nullable
}

/// Compute FIRST sets for all symbols
fn compute_first(
    grammar: &Grammar,
    nullable: &HashSet<SymbolId>,
) -> HashMap<SymbolId, HashSet<SymbolId>> {
    let mut first: HashMap<SymbolId, HashSet<SymbolId>> = HashMap::new();

    // Initialize FIRST sets
    for (id, sym) in grammar.symbols.iter().enumerate() {
        let mut set = HashSet::new();
        if sym.is_terminal {
            // FIRST(terminal) = {terminal}
            set.insert(id);
        }
        first.insert(id, set);
    }

    // Fixed-point iteration
    let mut changed = true;
    while changed {
        changed = false;

        for prod in &grammar.productions {
            // For each production A -> X1 X2 ... Xn
            for &sym in &prod.rhs {
                // Add FIRST(Xi) to FIRST(A)
                if let Some(first_sym) = first.get(&sym).cloned() {
                    let first_lhs = first.get_mut(&prod.lhs).unwrap();
                    let old_size = first_lhs.len();
                    first_lhs.extend(first_sym);
                    if first_lhs.len() > old_size {
                        changed = true;
                    }
                }

                // If Xi is not nullable, stop
                if !nullable.contains(&sym) {
                    break;
                }
            }
        }
    }

    first
}

/// Compute FOLLOW sets for all non-terminals
fn compute_follow(
    grammar: &Grammar,
    nullable: &HashSet<SymbolId>,
    first: &HashMap<SymbolId, HashSet<SymbolId>>,
) -> HashMap<SymbolId, HashSet<SymbolId>> {
    let mut follow: HashMap<SymbolId, HashSet<SymbolId>> = HashMap::new();

    // Initialize FOLLOW sets for non-terminals
    for (id, sym) in grammar.symbols.iter().enumerate() {
        if !sym.is_terminal {
            follow.insert(id, HashSet::new());
        }
    }

    // Add $ to FOLLOW(start symbol)
    if let Some(follow_start) = follow.get_mut(&grammar.start_symbol) {
        follow_start.insert(EOF_SYMBOL);
    }

    // Fixed-point iteration
    let mut changed = true;
    while changed {
        changed = false;

        for prod in &grammar.productions {
            // For each production A -> α B β
            for (i, &sym) in prod.rhs.iter().enumerate() {
                // Only consider non-terminals
                if grammar.is_terminal(sym) {
                    continue;
                }

                let beta = &prod.rhs[i + 1..];

                // Add FIRST(β) to FOLLOW(B)
                for &beta_sym in beta {
                    if let Some(first_beta) = first.get(&beta_sym) {
                        if let Some(follow_sym) = follow.get_mut(&sym) {
                            let old_size = follow_sym.len();
                            follow_sym.extend(first_beta.iter().copied());
                            if follow_sym.len() > old_size {
                                changed = true;
                            }
                        }
                    }

                    // If beta_sym is not nullable, stop
                    if !nullable.contains(&beta_sym) {
                        break;
                    }
                }

                // If β is nullable (or empty), add FOLLOW(A) to FOLLOW(B)
                let beta_nullable = beta.iter().all(|&s| nullable.contains(&s));
                if beta_nullable {
                    if let Some(follow_lhs) = follow.get(&prod.lhs).cloned() {
                        if let Some(follow_sym) = follow.get_mut(&sym) {
                            let old_size = follow_sym.len();
                            follow_sym.extend(follow_lhs);
                            if follow_sym.len() > old_size {
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
    }

    follow
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
    fn test_nullable() {
        let grammar = make_grammar(
            r#"
%token A B
%%
s : a B
  ;
a : A
  |
  ;
"#,
        );

        let ff = compute(&grammar);

        let a_id = *grammar.symbol_map.get("a").unwrap();
        let s_id = *grammar.symbol_map.get("s").unwrap();

        assert!(ff.nullable.contains(&a_id), "a should be nullable");
        assert!(!ff.nullable.contains(&s_id), "s should not be nullable");
    }

    #[test]
    fn test_first_terminal() {
        let grammar = make_grammar(
            r#"
%token A B
%%
s : A B
  ;
"#,
        );

        let ff = compute(&grammar);

        let a_id = *grammar.symbol_map.get("A").unwrap();
        let s_id = *grammar.symbol_map.get("s").unwrap();

        assert!(ff.first[&s_id].contains(&a_id));
    }

    #[test]
    fn test_first_with_nullable() {
        let grammar = make_grammar(
            r#"
%token A B C
%%
s : a B
  ;
a : A
  |
  ;
"#,
        );

        let ff = compute(&grammar);

        let a_id = *grammar.symbol_map.get("A").unwrap();
        let b_id = *grammar.symbol_map.get("B").unwrap();
        let s_id = *grammar.symbol_map.get("s").unwrap();

        // FIRST(s) should contain both A and B (since a is nullable)
        assert!(ff.first[&s_id].contains(&a_id), "FIRST(s) should contain A");
        assert!(ff.first[&s_id].contains(&b_id), "FIRST(s) should contain B");
    }

    #[test]
    fn test_follow_start() {
        let grammar = make_grammar(
            r#"
%token A
%%
s : A
  ;
"#,
        );

        let ff = compute(&grammar);

        let s_id = *grammar.symbol_map.get("s").unwrap();

        // FOLLOW(s) should contain $end
        assert!(ff.follow[&s_id].contains(&EOF_SYMBOL));
    }

    #[test]
    fn test_follow_propagation() {
        let grammar = make_grammar(
            r#"
%token A B
%%
s : a B
  ;
a : A
  ;
"#,
        );

        let ff = compute(&grammar);

        let a_sym = *grammar.symbol_map.get("a").unwrap();
        let b_id = *grammar.symbol_map.get("B").unwrap();

        // FOLLOW(a) should contain B
        assert!(ff.follow[&a_sym].contains(&b_id));
    }

    #[test]
    fn test_expression_grammar() {
        let grammar = make_grammar(
            r#"
%token NUM PLUS TIMES LPAREN RPAREN
%%
expr : expr PLUS term
     | term
     ;
term : term TIMES factor
     | factor
     ;
factor : LPAREN expr RPAREN
       | NUM
       ;
"#,
        );

        let ff = compute(&grammar);

        let expr_id = *grammar.symbol_map.get("expr").unwrap();
        let term_id = *grammar.symbol_map.get("term").unwrap();
        let factor_id = *grammar.symbol_map.get("factor").unwrap();
        let num_id = *grammar.symbol_map.get("NUM").unwrap();
        let lparen_id = *grammar.symbol_map.get("LPAREN").unwrap();
        let plus_id = *grammar.symbol_map.get("PLUS").unwrap();
        let times_id = *grammar.symbol_map.get("TIMES").unwrap();
        let rparen_id = *grammar.symbol_map.get("RPAREN").unwrap();

        // FIRST(expr) = FIRST(term) = FIRST(factor) = {NUM, LPAREN}
        assert!(ff.first[&expr_id].contains(&num_id));
        assert!(ff.first[&expr_id].contains(&lparen_id));
        assert!(ff.first[&term_id].contains(&num_id));
        assert!(ff.first[&factor_id].contains(&num_id));

        // FOLLOW(expr) = {$end, PLUS, RPAREN}
        assert!(ff.follow[&expr_id].contains(&EOF_SYMBOL));
        assert!(ff.follow[&expr_id].contains(&plus_id));
        assert!(ff.follow[&expr_id].contains(&rparen_id));

        // FOLLOW(term) = {PLUS, TIMES, $end, RPAREN}
        assert!(ff.follow[&term_id].contains(&plus_id));
        assert!(ff.follow[&term_id].contains(&times_id));
    }
}
