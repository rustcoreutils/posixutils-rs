//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! NFA (Non-deterministic Finite Automaton) construction using Thompson's algorithm.
//!
//! This module implements Thompson's construction to convert regular expressions
//! into NFAs, which can then be converted to DFAs for efficient lexical analysis.

use regex_syntax::hir::{Class, ClassUnicode, Hir, HirKind, Literal, Repetition};
use std::collections::{BTreeMap, BTreeSet};

/// Represents a transition in the NFA
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Transition {
    /// Epsilon transition (no input consumed)
    Epsilon,
    /// Match a single character
    Char(char),
    /// Match any character in a set of ranges
    CharClass(Vec<(char, char)>),
}

/// A state in the NFA
#[derive(Debug, Clone)]
pub struct NfaState {
    /// Transitions from this state: (transition, target_state)
    pub transitions: Vec<(Transition, usize)>,
    /// If this is an accepting state, contains the rule index (lower = higher priority)
    pub accepting: Option<usize>,
}

impl NfaState {
    fn new() -> Self {
        NfaState {
            transitions: Vec::new(),
            accepting: None,
        }
    }
}

/// The complete NFA
#[derive(Debug)]
pub struct Nfa {
    /// All states in the NFA
    pub states: Vec<NfaState>,
    /// The start state
    pub start: usize,
    /// For trailing context: maps NFA state index to rule indices for which
    /// this state marks the end of the main pattern (where yytext should end)
    /// Used for variable-length trailing context support
    pub main_pattern_end: BTreeMap<usize, Vec<usize>>,
}

impl Nfa {
    /// Create a new empty NFA with a single start state
    fn new() -> Self {
        let mut nfa = Nfa {
            states: Vec::new(),
            start: 0,
            main_pattern_end: BTreeMap::new(),
        };
        nfa.add_state(); // Add start state
        nfa
    }

    /// Add a new state and return its index
    fn add_state(&mut self) -> usize {
        let idx = self.states.len();
        self.states.push(NfaState::new());
        idx
    }

    /// Add a transition from one state to another
    fn add_transition(&mut self, from: usize, trans: Transition, to: usize) {
        self.states[from].transitions.push((trans, to));
    }

    /// Build an NFA from a list of rules (patterns with rule indices)
    /// Note: Kept for backward compatibility and testing. Use from_rules_with_trailing_context
    /// for rules that may have trailing context.
    #[cfg(test)]
    pub fn from_rules(rules: &[(Hir, usize)]) -> Result<Self, String> {
        let mut nfa = Nfa::new();
        let start = nfa.start;

        for (hir, rule_idx) in rules {
            // Build sub-NFA for this rule
            let (rule_start, rule_end) = nfa.build_hir(hir)?;

            // Connect start state to rule's start via epsilon
            nfa.add_transition(start, Transition::Epsilon, rule_start);

            // Mark rule's end as accepting with rule index
            nfa.states[rule_end].accepting = Some(*rule_idx);
        }

        Ok(nfa)
    }

    /// Build an NFA from rules with explicit trailing context support
    /// Each rule is (main_pattern, optional_trailing_context, rule_index)
    /// For rules with trailing context, tracks the main pattern end state
    pub fn from_rules_with_trailing_context(
        rules: &[(Hir, Option<Hir>, usize)],
    ) -> Result<Self, String> {
        let mut nfa = Nfa::new();
        let start = nfa.start;

        for (main_hir, trailing_opt, rule_idx) in rules {
            match trailing_opt {
                Some(trailing_hir) => {
                    // Build NFA for main pattern
                    let (main_start, main_end) = nfa.build_hir(main_hir)?;

                    // Mark the main pattern end state for this rule
                    // This is where yytext should end for variable-length TC
                    nfa.main_pattern_end
                        .entry(main_end)
                        .or_default()
                        .push(*rule_idx);

                    // Build NFA for trailing context
                    let (tc_start, tc_end) = nfa.build_hir(trailing_hir)?;

                    // Connect main pattern end to trailing context start
                    nfa.add_transition(main_end, Transition::Epsilon, tc_start);

                    // Connect NFA start to main pattern start
                    nfa.add_transition(start, Transition::Epsilon, main_start);

                    // Mark trailing context end as accepting
                    nfa.states[tc_end].accepting = Some(*rule_idx);
                }
                None => {
                    // No trailing context - simple rule
                    let (rule_start, rule_end) = nfa.build_hir(main_hir)?;
                    nfa.add_transition(start, Transition::Epsilon, rule_start);
                    nfa.states[rule_end].accepting = Some(*rule_idx);
                }
            }
        }

        Ok(nfa)
    }

    /// Build NFA fragment for a HIR node, returns (start_state, end_state)
    fn build_hir(&mut self, hir: &Hir) -> Result<(usize, usize), String> {
        match hir.kind() {
            HirKind::Empty => {
                // Empty matches empty string - just epsilon transition
                let start = self.add_state();
                let end = self.add_state();
                self.add_transition(start, Transition::Epsilon, end);
                Ok((start, end))
            }

            HirKind::Literal(lit) => self.build_literal(lit),

            HirKind::Class(class) => self.build_class(class),

            HirKind::Look(_) => {
                // Look-around assertions (^, $, \b, etc.)
                // For lex, we handle ^ and $ specially at the rule level
                // For now, treat as epsilon (matches empty string at position)
                let start = self.add_state();
                let end = self.add_state();
                self.add_transition(start, Transition::Epsilon, end);
                Ok((start, end))
            }

            HirKind::Repetition(rep) => self.build_repetition(rep),

            HirKind::Capture(cap) => {
                // Capture groups don't affect matching, just recurse
                self.build_hir(&cap.sub)
            }

            HirKind::Concat(concat) => {
                if concat.is_empty() {
                    let start = self.add_state();
                    let end = self.add_state();
                    self.add_transition(start, Transition::Epsilon, end);
                    return Ok((start, end));
                }

                // Build first element
                let (first_start, mut prev_end) = self.build_hir(&concat[0])?;

                // Chain remaining elements
                for hir in &concat[1..] {
                    let (curr_start, curr_end) = self.build_hir(hir)?;
                    self.add_transition(prev_end, Transition::Epsilon, curr_start);
                    prev_end = curr_end;
                }

                Ok((first_start, prev_end))
            }

            HirKind::Alternation(alt) => {
                let start = self.add_state();
                let end = self.add_state();

                for hir in alt.iter() {
                    let (alt_start, alt_end) = self.build_hir(hir)?;
                    self.add_transition(start, Transition::Epsilon, alt_start);
                    self.add_transition(alt_end, Transition::Epsilon, end);
                }

                Ok((start, end))
            }
        }
    }

    /// Build NFA for a literal (sequence of bytes)
    fn build_literal(&mut self, lit: &Literal) -> Result<(usize, usize), String> {
        let bytes = &lit.0;
        if bytes.is_empty() {
            let start = self.add_state();
            let end = self.add_state();
            self.add_transition(start, Transition::Epsilon, end);
            return Ok((start, end));
        }

        // Convert bytes to string for character-by-character processing
        let s = String::from_utf8(bytes.to_vec())
            .map_err(|e| format!("Invalid UTF-8 in literal: {}", e))?;

        let start = self.add_state();
        let mut prev = start;

        for ch in s.chars() {
            let next = self.add_state();
            self.add_transition(prev, Transition::Char(ch), next);
            prev = next;
        }

        Ok((start, prev))
    }

    /// Build NFA for a character class
    fn build_class(&mut self, class: &Class) -> Result<(usize, usize), String> {
        let start = self.add_state();
        let end = self.add_state();

        match class {
            Class::Unicode(unicode) => {
                let ranges = extract_unicode_ranges(unicode);
                if ranges.len() == 1 && ranges[0].0 == ranges[0].1 {
                    // Single character
                    self.add_transition(start, Transition::Char(ranges[0].0), end);
                } else {
                    self.add_transition(start, Transition::CharClass(ranges), end);
                }
            }
            Class::Bytes(bytes) => {
                // Convert byte ranges to char ranges (assuming ASCII)
                let ranges: Vec<(char, char)> = bytes
                    .iter()
                    .map(|r| (r.start() as char, r.end() as char))
                    .collect();
                if ranges.len() == 1 && ranges[0].0 == ranges[0].1 {
                    self.add_transition(start, Transition::Char(ranges[0].0), end);
                } else {
                    self.add_transition(start, Transition::CharClass(ranges), end);
                }
            }
        }

        Ok((start, end))
    }

    /// Build NFA for repetition (*, +, ?, {n,m})
    fn build_repetition(&mut self, rep: &Repetition) -> Result<(usize, usize), String> {
        let (sub_start, sub_end) = self.build_hir(&rep.sub)?;

        let start = self.add_state();
        let end = self.add_state();

        // Get min and max from the repetition
        let min = rep.min;
        let max = rep.max; // None means unbounded

        match (min, max) {
            // * (zero or more)
            (0, None) => {
                // start -> sub_start (can enter)
                // start -> end (can skip)
                // sub_end -> sub_start (can repeat)
                // sub_end -> end (can exit)
                self.add_transition(start, Transition::Epsilon, sub_start);
                self.add_transition(start, Transition::Epsilon, end);
                self.add_transition(sub_end, Transition::Epsilon, sub_start);
                self.add_transition(sub_end, Transition::Epsilon, end);
            }

            // + (one or more)
            (1, None) => {
                // Must match at least once
                // start -> sub_start
                // sub_end -> sub_start (can repeat)
                // sub_end -> end (can exit)
                self.add_transition(start, Transition::Epsilon, sub_start);
                self.add_transition(sub_end, Transition::Epsilon, sub_start);
                self.add_transition(sub_end, Transition::Epsilon, end);
            }

            // ? (zero or one)
            (0, Some(1)) => {
                // start -> sub_start (can enter)
                // start -> end (can skip)
                // sub_end -> end
                self.add_transition(start, Transition::Epsilon, sub_start);
                self.add_transition(start, Transition::Epsilon, end);
                self.add_transition(sub_end, Transition::Epsilon, end);
            }

            // {n} or {n,m} - bounded repetition
            (n, max_opt) => {
                if n == 0 && max_opt == Some(0) {
                    // {0} - matches empty
                    self.add_transition(start, Transition::Epsilon, end);
                    return Ok((start, end));
                }

                // Build n required copies
                let mut prev_end = start;
                for _ in 0..n {
                    let (copy_start, copy_end) = self.build_hir(&rep.sub)?;
                    self.add_transition(prev_end, Transition::Epsilon, copy_start);
                    prev_end = copy_end;
                }

                if let Some(m) = max_opt {
                    // Bounded: {n,m} - build (m-n) optional copies
                    let mut last_ends = vec![prev_end];
                    for _ in n..m {
                        let (copy_start, copy_end) = self.build_hir(&rep.sub)?;
                        self.add_transition(prev_end, Transition::Epsilon, copy_start);
                        last_ends.push(copy_end);
                        prev_end = copy_end;
                    }
                    // Connect all possible endpoints to final end
                    for ep in last_ends {
                        self.add_transition(ep, Transition::Epsilon, end);
                    }
                } else {
                    // Unbounded: {n,} - add loop for unlimited repetitions
                    // Connect required part to end (allows exactly n matches)
                    self.add_transition(prev_end, Transition::Epsilon, end);

                    // Add loop for additional matches beyond n
                    let (loop_start, loop_end) = self.build_hir(&rep.sub)?;
                    self.add_transition(prev_end, Transition::Epsilon, loop_start);
                    self.add_transition(loop_end, Transition::Epsilon, loop_start);
                    self.add_transition(loop_end, Transition::Epsilon, end);
                }
            }
        }

        Ok((start, end))
    }

    /// Compute epsilon closure of a set of states
    pub fn epsilon_closure(&self, states: &BTreeSet<usize>) -> BTreeSet<usize> {
        let mut closure = states.clone();
        let mut worklist: Vec<usize> = states.iter().copied().collect();

        while let Some(state) = worklist.pop() {
            for (trans, target) in &self.states[state].transitions {
                if *trans == Transition::Epsilon && !closure.contains(target) {
                    closure.insert(*target);
                    worklist.push(*target);
                }
            }
        }

        closure
    }

    /// Get all possible transitions from a set of states on a given character
    pub fn move_on_char(&self, states: &BTreeSet<usize>, ch: char) -> BTreeSet<usize> {
        let mut result = BTreeSet::new();

        for &state in states {
            for (trans, target) in &self.states[state].transitions {
                let matches = match trans {
                    Transition::Char(c) => *c == ch,
                    Transition::CharClass(ranges) => {
                        ranges.iter().any(|(lo, hi)| ch >= *lo && ch <= *hi)
                    }
                    Transition::Epsilon => false,
                };
                if matches {
                    result.insert(*target);
                }
            }
        }

        result
    }

    /// Get the highest priority accepting rule for a set of states (lowest index = highest priority)
    pub fn get_accepting(&self, states: &BTreeSet<usize>) -> Option<usize> {
        states
            .iter()
            .filter_map(|&s| self.states[s].accepting)
            .min()
    }

    /// Get all accepting rules for a set of NFA states, sorted by priority (lowest index first)
    /// Used for REJECT support
    pub fn get_all_accepting(&self, states: &BTreeSet<usize>) -> Vec<usize> {
        let mut rules: Vec<usize> = states
            .iter()
            .filter_map(|&s| self.states[s].accepting)
            .collect();
        rules.sort();
        rules.dedup();
        rules
    }
}

/// Extract character ranges from a Unicode character class
fn extract_unicode_ranges(cls: &ClassUnicode) -> Vec<(char, char)> {
    cls.iter().map(|r| (r.start(), r.end())).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_regex(pattern: &str) -> Hir {
        regex_syntax::parse(pattern).expect("Failed to parse regex")
    }

    #[test]
    fn test_single_char() {
        let hir = parse_regex("a");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        // Should have start state, plus states for the pattern
        assert!(nfa.states.len() >= 2);
    }

    #[test]
    fn test_concatenation() {
        let hir = parse_regex("abc");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        // Verify we can reach accepting state
        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));
        assert!(!start_closure.is_empty());
    }

    #[test]
    fn test_alternation() {
        let hir = parse_regex("a|b");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // Should be able to match 'a' from start
        let after_a = nfa.move_on_char(&start_closure, 'a');
        let after_a_closure = nfa.epsilon_closure(&after_a);
        assert!(nfa.get_accepting(&after_a_closure).is_some());

        // Should be able to match 'b' from start
        let after_b = nfa.move_on_char(&start_closure, 'b');
        let after_b_closure = nfa.epsilon_closure(&after_b);
        assert!(nfa.get_accepting(&after_b_closure).is_some());
    }

    #[test]
    fn test_kleene_star() {
        let hir = parse_regex("a*");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // Should accept empty string
        assert!(nfa.get_accepting(&start_closure).is_some());

        // Should accept "a"
        let after_a = nfa.move_on_char(&start_closure, 'a');
        let after_a_closure = nfa.epsilon_closure(&after_a);
        assert!(nfa.get_accepting(&after_a_closure).is_some());
    }

    #[test]
    fn test_plus() {
        let hir = parse_regex("a+");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // Should NOT accept empty string
        assert!(nfa.get_accepting(&start_closure).is_none());

        // Should accept "a"
        let after_a = nfa.move_on_char(&start_closure, 'a');
        let after_a_closure = nfa.epsilon_closure(&after_a);
        assert!(nfa.get_accepting(&after_a_closure).is_some());
    }

    #[test]
    fn test_char_class() {
        let hir = parse_regex("[a-z]");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();

        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // Should match 'a'
        let after_a = nfa.move_on_char(&start_closure, 'a');
        let after_a_closure = nfa.epsilon_closure(&after_a);
        assert!(nfa.get_accepting(&after_a_closure).is_some());

        // Should match 'z'
        let after_z = nfa.move_on_char(&start_closure, 'z');
        let after_z_closure = nfa.epsilon_closure(&after_z);
        assert!(nfa.get_accepting(&after_z_closure).is_some());

        // Should NOT match 'A'
        let after_upper_a = nfa.move_on_char(&start_closure, 'A');
        let after_upper_a_closure = nfa.epsilon_closure(&after_upper_a);
        assert!(nfa.get_accepting(&after_upper_a_closure).is_none());
    }

    #[test]
    fn test_multiple_rules() {
        let hir1 = parse_regex("if");
        let hir2 = parse_regex("[a-z]+");
        let nfa = Nfa::from_rules(&[(hir1, 0), (hir2, 1)]).unwrap();

        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // Match "if" - should get rule 0 (higher priority)
        let after_i = nfa.move_on_char(&start_closure, 'i');
        let after_i_closure = nfa.epsilon_closure(&after_i);
        let after_if = nfa.move_on_char(&after_i_closure, 'f');
        let after_if_closure = nfa.epsilon_closure(&after_if);

        // Both rules match, but rule 0 has higher priority
        assert_eq!(nfa.get_accepting(&after_if_closure), Some(0));
    }

    #[test]
    fn test_trailing_context_simple() {
        // Pattern: "foo/bar" - match "foo" when followed by "bar"
        let main_hir = parse_regex("foo");
        let trailing_hir = parse_regex("bar");
        let nfa =
            Nfa::from_rules_with_trailing_context(&[(main_hir, Some(trailing_hir), 0)]).unwrap();

        // Verify main_pattern_end is populated
        assert!(
            !nfa.main_pattern_end.is_empty(),
            "main_pattern_end should track end of main pattern"
        );

        // Verify that rule 0 is recorded as having main pattern end
        let has_rule_0 = nfa
            .main_pattern_end
            .values()
            .any(|rules| rules.contains(&0));
        assert!(has_rule_0, "Rule 0 should be tracked in main_pattern_end");

        // Verify the full pattern matches "foobar"
        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));

        // After "foo"
        let after_f = nfa.move_on_char(&start_closure, 'f');
        let after_fo = nfa.move_on_char(&nfa.epsilon_closure(&after_f), 'o');
        let after_foo = nfa.move_on_char(&nfa.epsilon_closure(&after_fo), 'o');
        let after_foo_closure = nfa.epsilon_closure(&after_foo);

        // After "foo", should NOT yet be accepting (need trailing context)
        assert!(
            nfa.get_accepting(&after_foo_closure).is_none(),
            "Should not accept after just main pattern"
        );

        // After "foobar"
        let after_foob = nfa.move_on_char(&after_foo_closure, 'b');
        let after_fooba = nfa.move_on_char(&nfa.epsilon_closure(&after_foob), 'a');
        let after_foobar = nfa.move_on_char(&nfa.epsilon_closure(&after_fooba), 'r');
        let after_foobar_closure = nfa.epsilon_closure(&after_foobar);

        // After full pattern, should be accepting
        assert!(
            nfa.get_accepting(&after_foobar_closure).is_some(),
            "Should accept after main + trailing context"
        );
    }

    #[test]
    fn test_trailing_context_with_simple_rule() {
        // Mix: rule 0 has trailing context, rule 1 is simple
        let main_hir = parse_regex("ab");
        let trailing_hir = parse_regex("c");
        let simple_hir = parse_regex("xyz");

        let nfa = Nfa::from_rules_with_trailing_context(&[
            (main_hir, Some(trailing_hir), 0),
            (simple_hir, None, 1),
        ])
        .unwrap();

        // Rule 0 should be in main_pattern_end
        let has_rule_0 = nfa
            .main_pattern_end
            .values()
            .any(|rules| rules.contains(&0));
        assert!(has_rule_0, "Rule 0 should be tracked in main_pattern_end");

        // Rule 1 should NOT be in main_pattern_end (no trailing context)
        let has_rule_1 = nfa
            .main_pattern_end
            .values()
            .any(|rules| rules.contains(&1));
        assert!(
            !has_rule_1,
            "Rule 1 should NOT be in main_pattern_end (no trailing context)"
        );

        // Verify "xyz" matches and accepts as rule 1
        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));
        let after_x = nfa.move_on_char(&start_closure, 'x');
        let after_xy = nfa.move_on_char(&nfa.epsilon_closure(&after_x), 'y');
        let after_xyz = nfa.move_on_char(&nfa.epsilon_closure(&after_xy), 'z');
        let after_xyz_closure = nfa.epsilon_closure(&after_xyz);

        assert_eq!(
            nfa.get_accepting(&after_xyz_closure),
            Some(1),
            "Simple rule should match"
        );
    }

    #[test]
    fn test_trailing_context_no_context() {
        // When trailing context is None, should behave like from_rules
        let hir = parse_regex("abc");
        let nfa = Nfa::from_rules_with_trailing_context(&[(hir, None, 0)]).unwrap();

        // main_pattern_end should be empty (no trailing context)
        assert!(
            nfa.main_pattern_end.is_empty(),
            "No trailing context means no main_pattern_end entries"
        );

        // Should match "abc"
        let start_closure = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));
        let after_a = nfa.move_on_char(&start_closure, 'a');
        let after_ab = nfa.move_on_char(&nfa.epsilon_closure(&after_a), 'b');
        let after_abc = nfa.move_on_char(&nfa.epsilon_closure(&after_ab), 'c');
        let after_abc_closure = nfa.epsilon_closure(&after_abc);

        assert_eq!(nfa.get_accepting(&after_abc_closure), Some(0));
    }
}
