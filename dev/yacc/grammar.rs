//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Grammar representation and symbol table.
//!
//! Core data structures: symbols (terminals/nonterminals with precedence and
//! type tags), productions (rules with semantic actions), and the complete
//! grammar with augmented start symbol and dense symbol indexing for tables.

use crate::error::YaccError;
use crate::parser::{Associativity, CodeBlock, ParsedGrammar, RhsElement, Symbol as ParsedSymbol};
use gettextrs::gettext;
use plib::diag;
use std::collections::HashMap;

/// Create a grammar error, also logging it via diag
fn grammar_error(msg: String) -> YaccError {
    diag::error_at(diag::Position::line_only(0), &msg);
    YaccError::Grammar(msg)
}

/// Create a grammar error with line number, also logging it via diag
fn grammar_error_at(line: usize, msg: String) -> YaccError {
    diag::error_at(diag::Position::line_only(line as u32), &msg);
    YaccError::Grammar(msg)
}

/// Index type for symbols
pub type SymbolId = usize;

/// Index type for productions
pub type ProductionId = usize;

/// The special end-of-input marker
pub const EOF_SYMBOL: SymbolId = 0;

/// The special error token
pub const ERROR_SYMBOL: SymbolId = 1;

/// The placeholder terminal that every token number without a declared
/// terminal translates to.
///
/// It appears in no rule, so no state ever has an action for it and any token
/// mapping to it reaches the parser's error path. Without it, unknown token
/// numbers translated to `error`, which some states *can* shift -- the parser
/// would then enter recovery with no `yyerror()` call and no `yynerrs`
/// increment.
pub const UNDEF_SYMBOL: SymbolId = 2;

/// The augmented start symbol (S')
pub const AUGMENTED_START: SymbolId = 3;

/// A symbol in the grammar
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// Symbol name
    pub name: String,
    /// Is this a terminal?
    pub is_terminal: bool,
    /// Type tag for semantic value
    pub tag: Option<String>,
    /// Token number (for terminals)
    pub token_number: Option<i32>,
    /// Precedence level (0 = no precedence)
    pub precedence: usize,
    /// Associativity
    pub associativity: Option<Associativity>,
}

/// A production rule (A -> α)
#[derive(Debug, Clone)]
pub struct Production {
    /// Production number
    pub id: ProductionId,
    /// Left-hand side non-terminal
    pub lhs: SymbolId,
    /// Right-hand side symbols
    pub rhs: Vec<SymbolId>,
    /// Semantic action (C code)
    pub action: Option<String>,
    /// Precedence of this rule
    pub precedence: usize,
    /// Line number in source file
    pub line: usize,
    /// For a lowered mid-rule action: the enclosing rule's RHS symbols that
    /// precede the action. `None` for an ordinary production.
    ///
    /// A mid-rule action becomes an empty production for a synthetic
    /// non-terminal, so its own `rhs` says nothing about what `$1`..`$n` mean
    /// inside it. They refer to the enclosing rule's elements to the left,
    /// which is what this records.
    pub mid_rule_prefix: Option<Vec<SymbolId>>,
}

/// The complete grammar
#[derive(Debug)]
pub struct Grammar {
    /// All symbols (terminals and non-terminals)
    pub symbols: Vec<SymbolInfo>,
    /// Symbol name to ID mapping
    pub symbol_map: HashMap<String, SymbolId>,
    /// All production rules
    pub productions: Vec<Production>,
    /// Start symbol (the user's start symbol, not the augmented one)
    pub start_symbol: SymbolId,
    /// C prologue code with line information
    pub prologue: Vec<CodeBlock>,
    /// Union definition with line information
    pub union_def: Option<CodeBlock>,
    /// C epilogue code with line information
    pub epilogue: Option<CodeBlock>,
    /// Productions indexed by LHS symbol
    pub productions_for: HashMap<SymbolId, Vec<ProductionId>>,
    /// First terminal symbol ID
    pub first_terminal: SymbolId,
    /// First non-terminal symbol ID
    pub first_nonterminal: SymbolId,
    /// Number of terminals
    pub num_terminals: usize,
    /// Number of non-terminals
    pub num_nonterminals: usize,
    /// Next available token number
    next_token_number: i32,
    /// Counter for mid-rule action non-terminals
    mid_action_counter: usize,
    /// Map of token numbers to symbol names (for duplicate detection)
    token_number_map: HashMap<i32, String>,

    // Dense terminal/non-terminal indexing for optimized table generation
    /// Dense terminal index mapping: symbol_id -> term_idx (0..num_terminals-1)
    term_idx: HashMap<SymbolId, usize>,
    /// Dense non-terminal index mapping: symbol_id -> nt_idx (0..num_nonterminals-1)
    nt_idx: HashMap<SymbolId, usize>,
    /// Reverse mapping: term_idx -> symbol_id
    pub term_idx_to_symbol: Vec<SymbolId>,
    /// Reverse mapping: nt_idx -> symbol_id
    pub nt_idx_to_symbol: Vec<SymbolId>,
}

impl Grammar {
    /// Create a grammar from a parsed grammar file
    pub fn from_parsed(parsed: ParsedGrammar) -> Result<Self, YaccError> {
        let mut grammar = Grammar {
            symbols: Vec::new(),
            symbol_map: HashMap::new(),
            productions: Vec::new(),
            start_symbol: 0,
            prologue: parsed.prologue,
            union_def: parsed.union_def,
            epilogue: parsed.epilogue,
            productions_for: HashMap::new(),
            first_terminal: 0,
            first_nonterminal: 0,
            num_terminals: 0,
            num_nonterminals: 0,
            next_token_number: 257, // Start after 256 (reserved for error)
            mid_action_counter: 0,
            token_number_map: HashMap::new(),
            term_idx: HashMap::new(),
            nt_idx: HashMap::new(),
            term_idx_to_symbol: Vec::new(),
            nt_idx_to_symbol: Vec::new(),
        };

        // Add special symbols (these cannot fail - they are the first symbols added)
        grammar.add_symbol("$end", true, None, Some(0), 0, None)?; // EOF_SYMBOL = 0
        grammar.add_symbol("error", true, None, Some(256), 0, None)?; // ERROR_SYMBOL = 1
                                                                      // $undefined gets no token number: nothing maps to it except
                                                                      // yytranslate's default fill.
        grammar.add_symbol("$undefined", true, None, None, 0, None)?; // UNDEF_SYMBOL = 2
        grammar.add_symbol("$accept", false, None, None, 0, None)?; // AUGMENTED_START = 3

        // Add declared tokens - two pass approach per POSIX:
        // 1. First pass: register all tokens with explicit numbers
        // 2. Second pass: auto-assign numbers to tokens without explicit numbers

        // Pass 1: Add all tokens, registering explicit numbers
        for token in &parsed.tokens {
            // POSIX: Allow %token error <number> to override error token value
            // The error token was already added above with default value 256,
            // so we update it if the user provides an explicit number
            if token.name == "error" {
                if let Some(new_num) = token.number {
                    // Update the error token's number
                    let error_id = ERROR_SYMBOL;
                    let old_num = grammar.symbols[error_id]
                        .token_number
                        .expect("error should have token number");
                    grammar.token_number_map.remove(&old_num);
                    grammar.symbols[error_id].token_number = Some(new_num);
                    grammar
                        .token_number_map
                        .insert(new_num, "error".to_string());
                }
                // Update other attributes if specified
                if let Some(ref tag) = token.tag {
                    grammar.symbols[ERROR_SYMBOL].tag = Some(tag.clone());
                }
                if token.precedence.unwrap_or(0) > 0 {
                    grammar.symbols[ERROR_SYMBOL].precedence = token.precedence.unwrap_or(0);
                }
                if token.associativity.is_some() {
                    grammar.symbols[ERROR_SYMBOL].associativity = token.associativity;
                }
                continue;
            }

            let number = token.number.or_else(|| {
                // Check if it's a character literal (has implicit number)
                if token.name.starts_with('\'') && token.name.ends_with('\'') {
                    let chars: Vec<char> = token.name[1..token.name.len() - 1].chars().collect();
                    if chars.len() == 1 {
                        return Some(chars[0] as i32);
                    }
                }
                None
            });

            grammar.add_symbol(
                &token.name,
                true,
                token.tag.clone(),
                number,
                token.precedence.unwrap_or(0),
                token.associativity,
            )?;
        }

        // Pass 2: Auto-assign numbers to tokens that don't have one yet
        // This happens after all explicit assignments are known
        for token in &parsed.tokens {
            let id = *grammar.symbol_map.get(&token.name).unwrap();

            if grammar.symbols[id].token_number.is_none() {
                // Skip any numbers that are already in use
                while grammar
                    .token_number_map
                    .contains_key(&grammar.next_token_number)
                {
                    grammar.next_token_number += 1;
                }
                let auto_num = grammar.next_token_number;
                grammar.symbols[id].token_number = Some(auto_num);
                grammar
                    .token_number_map
                    .insert(auto_num, token.name.clone());
                grammar.next_token_number += 1;
            }
        }

        // First pass: identify all non-terminals from LHS of rules
        for rule in &parsed.rules {
            match grammar.symbol_map.get(&rule.lhs) {
                None => {
                    grammar.add_symbol(&rule.lhs, false, None, None, 0, None)?;
                }
                // A name declared with %token (or %left/%right/%nonassoc) stays
                // a terminal, so the rule would build a production the LR
                // closure never expands and the parser would silently drop it.
                // The rule tables would also fall back to the raw symbol id for
                // its LHS, yielding a negative non-terminal index.
                Some(&id) if grammar.symbols[id].is_terminal => {
                    return Err(grammar_error_at(
                        rule.line,
                        format!(
                            "{} '{}', {}",
                            gettext("rule given for"),
                            rule.lhs,
                            gettext("which is a token")
                        ),
                    ));
                }
                Some(_) => {}
            }
        }

        // Add type declarations
        for type_decl in &parsed.types {
            for symbol_name in &type_decl.symbols {
                if let Some(&id) = grammar.symbol_map.get(symbol_name) {
                    grammar.symbols[id].tag = Some(type_decl.tag.clone());
                } else {
                    // Symbol not yet declared, add as non-terminal
                    grammar.add_symbol(
                        symbol_name,
                        false,
                        Some(type_decl.tag.clone()),
                        None,
                        0,
                        None,
                    )?;
                }
            }
        }

        // Determine start symbol
        let user_start = if let Some(ref start) = parsed.start {
            if let Some(&id) = grammar.symbol_map.get(start) {
                id
            } else {
                return Err(grammar_error(format!(
                    "{} '{}' {}",
                    gettext("start symbol"),
                    start,
                    gettext("not defined")
                )));
            }
        } else if !parsed.rules.is_empty() {
            // Default: LHS of first rule
            *grammar.symbol_map.get(&parsed.rules[0].lhs).unwrap()
        } else {
            return Err(grammar_error(gettext("no rules defined")));
        };

        grammar.start_symbol = user_start;

        // Add productions
        for rule in &parsed.rules {
            grammar.add_production(rule)?;
        }

        // Add augmented start production: $accept -> start $end
        let augmented_prod = Production {
            id: grammar.productions.len(),
            lhs: AUGMENTED_START,
            rhs: vec![grammar.start_symbol, EOF_SYMBOL],
            action: None,
            precedence: 0,
            line: 0,
            mid_rule_prefix: None,
        };
        let prod_id = grammar.productions.len();
        grammar.productions.push(augmented_prod);
        grammar
            .productions_for
            .entry(AUGMENTED_START)
            .or_default()
            .push(prod_id);

        // Calculate terminal/non-terminal counts
        grammar.first_terminal = 0;
        grammar.first_nonterminal = grammar.symbols.len();
        for (i, sym) in grammar.symbols.iter().enumerate() {
            if sym.is_terminal {
                grammar.num_terminals += 1;
            } else {
                grammar.num_nonterminals += 1;
                if i < grammar.first_nonterminal {
                    grammar.first_nonterminal = i;
                }
            }
        }

        // Compute dense terminal indices (0..num_terminals-1)
        // Iterates in symbol ID order, so term_idx 0 = EOF_SYMBOL, term_idx 1 = ERROR_SYMBOL
        for (sym_id, sym) in grammar.symbols.iter().enumerate() {
            if sym.is_terminal {
                let idx = grammar.term_idx_to_symbol.len();
                grammar.term_idx.insert(sym_id, idx);
                grammar.term_idx_to_symbol.push(sym_id);
            }
        }

        // Compute dense non-terminal indices (0..num_nonterminals-1)
        // Iterates in symbol ID order, so nt_idx 0 = AUGMENTED_START
        for (sym_id, sym) in grammar.symbols.iter().enumerate() {
            if !sym.is_terminal {
                let idx = grammar.nt_idx_to_symbol.len();
                grammar.nt_idx.insert(sym_id, idx);
                grammar.nt_idx_to_symbol.push(sym_id);
            }
        }

        // Validate: check that all RHS symbols are defined
        for prod in &grammar.productions {
            for &sym_id in &prod.rhs {
                if sym_id >= grammar.symbols.len() {
                    return Err(grammar_error_at(
                        prod.line,
                        format!("{} {}", gettext("undefined symbol in production"), prod.id),
                    ));
                }
            }
        }

        // Validate: check that all non-terminals have at least one production
        for (id, sym) in grammar.symbols.iter().enumerate() {
            if !sym.is_terminal
                && id != AUGMENTED_START
                && !grammar.productions_for.contains_key(&id)
            {
                return Err(grammar_error(format!(
                    "{} '{}' {}",
                    gettext("non-terminal"),
                    sym.name,
                    gettext("has no rules")
                )));
            }
        }

        grammar.validate_productive()?;

        Ok(grammar)
    }

    fn add_symbol(
        &mut self,
        name: &str,
        is_terminal: bool,
        tag: Option<String>,
        token_number: Option<i32>,
        precedence: usize,
        associativity: Option<Associativity>,
    ) -> Result<SymbolId, YaccError> {
        if let Some(&id) = self.symbol_map.get(name) {
            // Update existing symbol if needed
            if precedence > 0 {
                self.symbols[id].precedence = precedence;
                self.symbols[id].associativity = associativity;
            }
            if tag.is_some() {
                self.symbols[id].tag = tag;
            }
            // POSIX: Once a token number is assigned, it shall not be changed
            if let Some(new_num) = token_number {
                if let Some(existing_num) = self.symbols[id].token_number {
                    if existing_num != new_num {
                        return Err(grammar_error(format!(
                            "{} '{}' {} {}, {} {}",
                            gettext("token"),
                            name,
                            gettext("already has number"),
                            existing_num,
                            gettext("cannot reassign to"),
                            new_num
                        )));
                    }
                } else {
                    // Assigning number to symbol that didn't have one yet
                    // Check for duplicate token numbers
                    if let Some(existing_name) = self.token_number_map.get(&new_num) {
                        return Err(grammar_error(format!(
                            "{} {}: {} '{}', {} '{}'",
                            gettext("duplicate token number"),
                            new_num,
                            gettext("already assigned to"),
                            existing_name,
                            gettext("cannot assign to"),
                            name
                        )));
                    }
                    self.symbols[id].token_number = Some(new_num);
                    self.token_number_map.insert(new_num, name.to_string());
                }
            }
            return Ok(id);
        }

        // POSIX: "Conforming applications shall not use names beginning in yy or YY"
        // We warn (not error) since it's the application's responsibility
        if !name.starts_with('@') && !name.starts_with('\'') {
            // Skip internal symbols like @1 (mid-rule actions) and 'c' (char literals)
            if name.starts_with("yy") || name.starts_with("YY") {
                diag::warning_at(
                    diag::Position::line_only(0),
                    &format!(
                        "{} '{}' {}",
                        gettext("symbol"),
                        name,
                        gettext("begins with 'yy' or 'YY' which is reserved")
                    ),
                );
            }
        }

        // New symbol - check for duplicate token number
        if let Some(num) = token_number {
            if let Some(existing_name) = self.token_number_map.get(&num) {
                return Err(grammar_error(format!(
                    "{} {}: {} '{}', {} '{}'",
                    gettext("duplicate token number"),
                    num,
                    gettext("already assigned to"),
                    existing_name,
                    gettext("cannot assign to"),
                    name
                )));
            }
            self.token_number_map.insert(num, name.to_string());
        }

        let id = self.symbols.len();
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            is_terminal,
            tag,
            token_number,
            precedence,
            associativity,
        });
        self.symbol_map.insert(name.to_string(), id);
        Ok(id)
    }

    fn add_production(&mut self, rule: &crate::parser::Rule) -> Result<(), YaccError> {
        let lhs = *self.symbol_map.get(&rule.lhs).ok_or_else(|| {
            grammar_error_at(
                rule.line,
                format!("{}: {}", gettext("undefined non-terminal"), rule.lhs),
            )
        })?;

        let mut rhs = Vec::new();
        let mut last_terminal_prec = 0;

        for elem in &rule.rhs {
            match elem {
                RhsElement::Symbol(sym) => {
                    let sym_id = self.get_or_add_symbol(sym)?;
                    rhs.push(sym_id);

                    // POSIX 124050: a rule's precedence is that of "the last
                    // token or literal in the body of the rule". A trailing
                    // terminal without precedence therefore clears whatever an
                    // earlier operator contributed -- taking the last terminal
                    // that *has* precedence instead would silently resolve a
                    // conflict the spec says to report. Non-terminals are
                    // neither tokens nor literals, so they leave it alone.
                    if self.symbols[sym_id].is_terminal {
                        last_terminal_prec = self.symbols[sym_id].precedence;
                    }
                }
                RhsElement::MidAction(code) => {
                    // Create a new non-terminal for mid-rule action
                    self.mid_action_counter += 1;
                    let mid_name = format!("@{}", self.mid_action_counter);
                    let mid_id = self.add_symbol(&mid_name, false, None, None, 0, None)?;

                    // Add empty production for mid-rule action
                    let mid_prod = Production {
                        id: self.productions.len(),
                        lhs: mid_id,
                        rhs: Vec::new(),
                        action: Some(code.clone()),
                        precedence: 0,
                        line: rule.line,
                        mid_rule_prefix: Some(rhs.clone()),
                    };
                    let prod_id = self.productions.len();
                    self.productions.push(mid_prod);
                    self.productions_for
                        .entry(mid_id)
                        .or_default()
                        .push(prod_id);

                    rhs.push(mid_id);
                }
            }
        }

        // Determine rule precedence
        let precedence = if let Some(ref prec_name) = rule.prec {
            // Explicit %prec
            if let Some(&id) = self.symbol_map.get(prec_name) {
                self.symbols[id].precedence
            } else {
                return Err(grammar_error_at(
                    rule.line,
                    format!("{}: {}", gettext("undefined token in %prec"), prec_name),
                ));
            }
        } else {
            // Default: precedence of rightmost terminal
            last_terminal_prec
        };

        let prod = Production {
            id: self.productions.len(),
            lhs,
            rhs,
            action: rule.action.clone(),
            precedence,
            line: rule.line,
            mid_rule_prefix: None,
        };

        let prod_id = self.productions.len();
        self.productions.push(prod);
        self.productions_for.entry(lhs).or_default().push(prod_id);

        Ok(())
    }

    fn get_or_add_symbol(&mut self, sym: &ParsedSymbol) -> Result<SymbolId, YaccError> {
        match sym {
            ParsedSymbol::NonTerminal(name) => {
                if let Some(&id) = self.symbol_map.get(name) {
                    Ok(id)
                } else {
                    // Unknown symbol - assume it's a non-terminal (will be validated later)
                    self.add_symbol(name, false, None, None, 0, None)
                }
            }
            ParsedSymbol::CharLiteral(c) => {
                // POSIX RATIONALE 124342-6: multi-byte characters should be
                // returned as tokens by the lexer, not as multi-byte character
                // literals. A literal codepoint must fit in a single byte
                // (1..=255); 0 (NUL) is reserved for end-of-input.
                let cp = *c as u32;
                if !(1..=255).contains(&cp) {
                    return Err(grammar_error(format!(
                        "{} '{}' (U+{:04X}) {}",
                        gettext("character literal"),
                        c,
                        cp,
                        gettext("is out of range; only single-byte characters 1..=255 are allowed")
                    )));
                }
                let name = format!("'{}'", c);
                if let Some(&id) = self.symbol_map.get(&name) {
                    Ok(id)
                } else {
                    // Add character literal as terminal
                    self.add_symbol(&name, true, None, Some(*c as i32), 0, None)
                }
            }
            ParsedSymbol::Error => Ok(ERROR_SYMBOL),
        }
    }

    /// Get symbol name by ID
    pub fn symbol_name(&self, id: SymbolId) -> &str {
        &self.symbols[id].name
    }

    /// Reject non-terminals that derive no string of terminals.
    ///
    /// Such a non-terminal can never take part in a successful parse, so every
    /// rule mentioning it is dead. Diagnosing it here also keeps the LALR
    /// lookahead closure well-behaved: a non-productive non-terminal has an
    /// empty FIRST set and is not nullable, which is the one way
    /// `first_of_sequence_with_lookahead` can return nothing.
    fn validate_productive(&self) -> Result<(), YaccError> {
        let mut productive = vec![false; self.symbols.len()];
        for (id, sym) in self.symbols.iter().enumerate() {
            productive[id] = sym.is_terminal;
        }

        let mut changed = true;
        while changed {
            changed = false;
            for prod in &self.productions {
                if !productive[prod.lhs] && prod.rhs.iter().all(|&s| productive[s]) {
                    productive[prod.lhs] = true;
                    changed = true;
                }
            }
        }

        // Warn about every offender: one non-productive non-terminal makes each
        // of its users non-productive too, so naming only the first would often
        // name a symptom rather than the cause. `$accept` is excluded -- it is
        // non-productive exactly when the start symbol is, and it is synthetic.
        //
        // These are warnings, not errors: the rules involved are dead, but the
        // rest of the grammar is still a usable parser, and that is what other
        // yacc implementations generate. POSIX requires a diagnostic only for a
        // non-terminal with no rules at all (123825-26), which is a separate
        // check above.
        for (id, sym) in self.symbols.iter().enumerate() {
            if sym.is_terminal || productive[id] || id == AUGMENTED_START {
                continue;
            }
            diag::warning_at(
                diag::Position::line_only(self.first_rule_line(id) as u32),
                &format!(
                    "{} '{}' {}",
                    gettext("non-terminal"),
                    sym.name,
                    gettext("derives no string of tokens; its rules are unusable")
                ),
            );
        }

        // A non-productive start symbol is fatal: the grammar accepts nothing,
        // so there is no parser to generate.
        if !productive[self.start_symbol] {
            return Err(grammar_error_at(
                self.first_rule_line(self.start_symbol),
                format!(
                    "{} '{}' {}",
                    gettext("start symbol"),
                    self.symbols[self.start_symbol].name,
                    gettext("does not derive any sentence")
                ),
            ));
        }

        Ok(())
    }

    /// Line of the first rule defining `sym`, for diagnostics. 0 if it has none.
    fn first_rule_line(&self, sym: SymbolId) -> usize {
        self.productions_for
            .get(&sym)
            .and_then(|ids| ids.first())
            .map(|&pid| self.productions[pid].line)
            .unwrap_or(0)
    }

    /// Check if a symbol is a terminal
    pub fn is_terminal(&self, id: SymbolId) -> bool {
        self.symbols[id].is_terminal
    }

    /// Get all terminal symbol IDs
    pub fn terminals(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.symbols
            .iter()
            .enumerate()
            .filter(|(_, s)| s.is_terminal)
            .map(|(i, _)| i)
    }

    /// Get all non-terminal symbol IDs
    pub fn nonterminals(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.symbols
            .iter()
            .enumerate()
            .filter(|(_, s)| !s.is_terminal)
            .map(|(i, _)| i)
    }

    /// Get productions for a non-terminal
    pub fn productions_for_symbol(&self, sym: SymbolId) -> &[ProductionId] {
        self.productions_for
            .get(&sym)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get dense terminal index for a terminal symbol.
    /// Returns None if the symbol is not a terminal.
    pub fn terminal_index(&self, sym_id: SymbolId) -> Option<usize> {
        self.term_idx.get(&sym_id).copied()
    }

    /// Get dense non-terminal index for a non-terminal symbol.
    /// Returns None if the symbol is not a non-terminal.
    pub fn nonterminal_index(&self, sym_id: SymbolId) -> Option<usize> {
        self.nt_idx.get(&sym_id).copied()
    }

    /// Get the augmented start production (should be the last one added)
    #[cfg(test)]
    pub fn augmented_production(&self) -> &Production {
        // Find production with lhs = AUGMENTED_START
        self.productions
            .iter()
            .find(|p| p.lhs == AUGMENTED_START)
            .expect("augmented production not found")
    }
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
    fn test_simple_grammar() {
        let grammar = make_grammar(
            r#"
%token NUM
%%
expr : NUM
     ;
"#,
        );

        assert!(grammar.symbol_map.contains_key("NUM"));
        assert!(grammar.symbol_map.contains_key("expr"));
        assert!(grammar.symbols[*grammar.symbol_map.get("NUM").unwrap()].is_terminal);
        assert!(!grammar.symbols[*grammar.symbol_map.get("expr").unwrap()].is_terminal);
    }

    #[test]
    fn test_augmented_grammar() {
        let grammar = make_grammar(
            r#"
%token NUM
%%
expr : NUM
     ;
"#,
        );

        // Should have augmented production
        let aug_prod = grammar.augmented_production();
        assert_eq!(aug_prod.lhs, AUGMENTED_START);
        assert_eq!(aug_prod.rhs.len(), 2);
        assert_eq!(aug_prod.rhs[1], EOF_SYMBOL);
    }

    #[test]
    fn test_precedence() {
        let grammar = make_grammar(
            r#"
%token NUM
%left '+' '-'
%left '*' '/'
%%
expr : expr '+' expr
     | expr '*' expr
     | NUM
     ;
"#,
        );

        let plus_id = *grammar.symbol_map.get("'+'").unwrap();
        let times_id = *grammar.symbol_map.get("'*'").unwrap();

        assert!(grammar.symbols[plus_id].precedence < grammar.symbols[times_id].precedence);
    }

    #[test]
    fn test_token_numbers() {
        let grammar = make_grammar(
            r#"
%token NUM 300
%token ID
%%
expr : NUM
     | ID
     ;
"#,
        );

        let num_id = *grammar.symbol_map.get("NUM").unwrap();
        let id_id = *grammar.symbol_map.get("ID").unwrap();

        assert_eq!(grammar.symbols[num_id].token_number, Some(300));
        // ID should get an auto-assigned unique number (not necessarily > 300)
        let id_num = grammar.symbols[id_id].token_number.unwrap();
        assert_ne!(id_num, 300); // Must be different from NUM's number
        assert!(id_num >= 257); // Must be >= 257 (first auto-assign range)
    }

    #[test]
    fn test_char_literal_tokens() {
        let grammar = make_grammar(
            r#"
%token NUM
%%
expr : expr '+' expr
     | NUM
     ;
"#,
        );

        let plus_id = *grammar.symbol_map.get("'+'").unwrap();
        assert!(grammar.symbols[plus_id].is_terminal);
        assert_eq!(grammar.symbols[plus_id].token_number, Some('+' as i32));
    }

    fn try_make_grammar(input: &str) -> Result<Grammar, YaccError> {
        let tokens = lex(input).unwrap();
        let parsed = parse(&tokens).unwrap();
        Grammar::from_parsed(parsed)
    }

    #[test]
    fn test_duplicate_token_number_error() {
        // Two different tokens with the same explicit number should fail
        let result = try_make_grammar(
            r#"
%token FOO 300
%token BAR 300
%%
expr : FOO
     | BAR
     ;
"#,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("duplicate token number 300"),
            "error should mention duplicate token number: {}",
            err
        );
    }

    #[test]
    fn test_token_number_reassignment_error() {
        // Trying to change an already-assigned token number should fail
        let result = try_make_grammar(
            r#"
%token FOO 300
%token FOO 400
%%
expr : FOO
     ;
"#,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("already has number 300"),
            "error should mention token already has number: {}",
            err
        );
    }

    #[test]
    fn test_same_token_same_number_ok() {
        // Same token with same number should be OK (redeclaration)
        let grammar = make_grammar(
            r#"
%token FOO 300
%left FOO 300
%%
expr : FOO
     ;
"#,
        );

        let foo_id = *grammar.symbol_map.get("FOO").unwrap();
        assert_eq!(grammar.symbols[foo_id].token_number, Some(300));
    }

    #[test]
    fn test_char_literal_nul_rejected() {
        // NUL character (codepoint 0) is reserved for end-of-input and is
        // rejected as out of the single-byte 1..=255 character-literal range.
        let result = try_make_grammar(
            r#"
%token NUM
%%
expr : '\0'
     | NUM
     ;
"#,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("out of range"),
            "error should reject the NUL character literal: {}",
            err
        );
    }

    #[test]
    fn test_auto_assigned_numbers_avoid_conflicts() {
        // Auto-assigned numbers should skip over explicitly assigned ones
        let grammar = make_grammar(
            r#"
%token A 257
%token B
%token C 258
%token D
%%
expr : A | B | C | D
     ;
"#,
        );

        let a_id = *grammar.symbol_map.get("A").unwrap();
        let b_id = *grammar.symbol_map.get("B").unwrap();
        let c_id = *grammar.symbol_map.get("C").unwrap();
        let d_id = *grammar.symbol_map.get("D").unwrap();

        // A and C have explicit numbers
        assert_eq!(grammar.symbols[a_id].token_number, Some(257));
        assert_eq!(grammar.symbols[c_id].token_number, Some(258));

        // B and D should get unique auto-assigned numbers
        let b_num = grammar.symbols[b_id].token_number.unwrap();
        let d_num = grammar.symbols[d_id].token_number.unwrap();

        assert_ne!(b_num, 257);
        assert_ne!(b_num, 258);
        assert_ne!(d_num, 257);
        assert_ne!(d_num, 258);
        assert_ne!(b_num, d_num);
    }
}
