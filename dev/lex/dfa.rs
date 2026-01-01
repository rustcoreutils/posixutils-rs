//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! DFA (Deterministic Finite Automaton) construction using subset construction.
//!
//! This module converts an NFA to a DFA using the subset (powerset) construction
//! algorithm. The resulting DFA can be used for efficient lexical analysis.

use crate::nfa::{Nfa, Transition};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// Represents an input symbol for DFA transitions
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DfaInput {
    /// A specific character
    Char(char),
}

/// A state in the DFA
#[derive(Debug, Clone)]
pub struct DfaState {
    /// Transitions from this state: input -> target_state
    pub transitions: BTreeMap<DfaInput, usize>,
    /// If this is an accepting state, contains the rule index (highest priority)
    pub accepting: Option<usize>,
    /// All accepting rules for this state, sorted by priority (lowest index first)
    /// Used for REJECT support
    pub accepting_rules: Vec<usize>,
    /// For variable-length trailing context: rules for which this state
    /// contains a main pattern end (where yytext should end)
    /// Used for runtime tracking of where to truncate matched text
    pub main_pattern_end_rules: Vec<usize>,
}

impl DfaState {
    fn new(
        accepting: Option<usize>,
        accepting_rules: Vec<usize>,
        main_pattern_end_rules: Vec<usize>,
    ) -> Self {
        DfaState {
            transitions: BTreeMap::new(),
            accepting,
            accepting_rules,
            main_pattern_end_rules,
        }
    }
}

/// The complete DFA
#[derive(Debug)]
pub struct Dfa {
    /// All states in the DFA
    pub states: Vec<DfaState>,
    /// The start state (always 0)
    pub start: usize,
    /// Character equivalence classes for table compression
    pub char_classes: CharClasses,
}

/// Character equivalence classes for efficient table representation
#[derive(Debug, Clone)]
pub struct CharClasses {
    /// Maps each character to its equivalence class
    pub char_to_class: [u8; 256],
    /// Number of distinct equivalence classes
    pub num_classes: usize,
}

impl Default for CharClasses {
    fn default() -> Self {
        Self::new()
    }
}

impl CharClasses {
    fn new() -> Self {
        CharClasses {
            char_to_class: [0; 256],
            num_classes: 1,
        }
    }

    /// Build character equivalence classes from the DFA
    fn build(dfa: &Dfa) -> Self {
        // Start with all characters in class 0
        let mut char_to_class = [0u8; 256];
        let mut class_signatures: HashMap<Vec<Option<usize>>, u8> = HashMap::new();
        let mut next_class = 0u8;

        // For each character, compute its "signature" (vector of target states from each DFA state)
        for ch in 0u8..=255 {
            let c = ch as char;
            let signature: Vec<Option<usize>> = dfa
                .states
                .iter()
                .map(|state| state.transitions.get(&DfaInput::Char(c)).copied())
                .collect();

            if let Some(&existing_class) = class_signatures.get(&signature) {
                char_to_class[ch as usize] = existing_class;
            } else {
                char_to_class[ch as usize] = next_class;
                class_signatures.insert(signature, next_class);
                next_class = next_class.saturating_add(1);
            }
        }

        CharClasses {
            char_to_class,
            num_classes: next_class as usize,
        }
    }
}

impl Dfa {
    /// Convert an NFA to a DFA using subset construction
    pub fn from_nfa(nfa: &Nfa) -> Self {
        let mut dfa = Dfa {
            states: Vec::new(),
            start: 0,
            char_classes: CharClasses::new(),
        };

        // Map from NFA state sets to DFA state indices
        let mut state_map: HashMap<BTreeSet<usize>, usize> = HashMap::new();

        // Worklist of DFA states to process
        let mut worklist: Vec<BTreeSet<usize>> = Vec::new();

        // Compute initial state (epsilon closure of NFA start state)
        let initial_nfa_states = nfa.epsilon_closure(&BTreeSet::from([nfa.start]));
        let initial_accepting = nfa.get_accepting(&initial_nfa_states);
        let initial_accepting_rules = nfa.get_all_accepting(&initial_nfa_states);
        let initial_main_end_rules = get_main_pattern_end_rules(nfa, &initial_nfa_states);

        state_map.insert(initial_nfa_states.clone(), 0);
        dfa.states.push(DfaState::new(
            initial_accepting,
            initial_accepting_rules,
            initial_main_end_rules,
        ));
        worklist.push(initial_nfa_states);

        // Build alphabet from all unique characters in NFA transitions
        let alphabet = build_alphabet(nfa);

        // Process worklist
        while let Some(nfa_states) = worklist.pop() {
            let dfa_state_idx = *state_map.get(&nfa_states).unwrap();

            // For each character in alphabet
            for ch in &alphabet {
                // Compute move on this character
                let moved = nfa.move_on_char(&nfa_states, *ch);
                if moved.is_empty() {
                    continue;
                }

                // Compute epsilon closure
                let target_nfa_states = nfa.epsilon_closure(&moved);
                if target_nfa_states.is_empty() {
                    continue;
                }

                // Get or create DFA state for this set
                let target_dfa_idx = if let Some(&idx) = state_map.get(&target_nfa_states) {
                    idx
                } else {
                    let idx = dfa.states.len();
                    let accepting = nfa.get_accepting(&target_nfa_states);
                    let accepting_rules = nfa.get_all_accepting(&target_nfa_states);
                    let main_end_rules = get_main_pattern_end_rules(nfa, &target_nfa_states);
                    state_map.insert(target_nfa_states.clone(), idx);
                    dfa.states
                        .push(DfaState::new(accepting, accepting_rules, main_end_rules));
                    worklist.push(target_nfa_states);
                    idx
                };

                // Add transition
                dfa.states[dfa_state_idx]
                    .transitions
                    .insert(DfaInput::Char(*ch), target_dfa_idx);
            }
        }

        // Build character equivalence classes
        dfa.char_classes = CharClasses::build(&dfa);

        dfa
    }

    /// Minimize the DFA using Hopcroft's algorithm
    pub fn minimize(&self) -> Dfa {
        if self.states.is_empty() {
            return Dfa {
                states: Vec::new(),
                start: 0,
                char_classes: self.char_classes.clone(),
            };
        }

        // Initial partition: separate accepting states by their full accepting rules list AND
        // main pattern end rules. This ensures states with different semantic meaning are kept separate
        let mut partitions: Vec<BTreeSet<usize>> = Vec::new();
        let mut state_to_partition: Vec<usize> = vec![0; self.states.len()];

        // Group states by (accepting_rules, main_pattern_end_rules) tuple
        // This preserves information needed for REJECT, start conditions, and trailing context
        let mut state_groups: BTreeMap<(Vec<usize>, Vec<usize>), BTreeSet<usize>> = BTreeMap::new();
        for (idx, state) in self.states.iter().enumerate() {
            state_groups
                .entry((
                    state.accepting_rules.clone(),
                    state.main_pattern_end_rules.clone(),
                ))
                .or_default()
                .insert(idx);
        }

        for (_, states) in state_groups {
            let partition_idx = partitions.len();
            for &s in &states {
                state_to_partition[s] = partition_idx;
            }
            partitions.push(states);
        }

        // Build alphabet for transitions
        let alphabet: BTreeSet<char> = self
            .states
            .iter()
            .flat_map(|s| s.transitions.keys())
            .map(|DfaInput::Char(c)| *c)
            .collect();

        // Refine partitions until fixed point
        let mut changed = true;
        while changed {
            changed = false;

            for partition_idx in 0..partitions.len() {
                if partitions[partition_idx].len() <= 1 {
                    continue;
                }

                // Try to split this partition
                for ch in &alphabet {
                    let partition = &partitions[partition_idx];
                    let mut splits: BTreeMap<Option<usize>, BTreeSet<usize>> = BTreeMap::new();

                    for &state in partition {
                        let target = self.states[state]
                            .transitions
                            .get(&DfaInput::Char(*ch))
                            .map(|&t| state_to_partition[t]);
                        splits.entry(target).or_default().insert(state);
                    }

                    if splits.len() > 1 {
                        // Need to split
                        let mut first = true;
                        for (_, states) in splits {
                            if first {
                                partitions[partition_idx] = states.clone();
                                for &s in &states {
                                    state_to_partition[s] = partition_idx;
                                }
                                first = false;
                            } else {
                                let new_idx = partitions.len();
                                for &s in &states {
                                    state_to_partition[s] = new_idx;
                                }
                                partitions.push(states);
                            }
                        }
                        changed = true;
                        break;
                    }
                }
            }
        }

        // Build minimized DFA
        let mut new_states: Vec<DfaState> = Vec::with_capacity(partitions.len());
        let mut partition_to_new_state: Vec<usize> = vec![0; partitions.len()];

        // Find new start state
        let new_start_partition = state_to_partition[self.start];

        // Reorder so start state is 0
        let mut new_idx = 0;
        partition_to_new_state[new_start_partition] = new_idx;
        new_idx += 1;
        for (i, _) in partitions.iter().enumerate() {
            if i != new_start_partition {
                partition_to_new_state[i] = new_idx;
                new_idx += 1;
            }
        }

        // Build states in new order
        let mut ordered_partitions: Vec<(usize, &BTreeSet<usize>)> = partitions
            .iter()
            .enumerate()
            .map(|(i, p)| (partition_to_new_state[i], p))
            .collect();
        ordered_partitions.sort_by_key(|(idx, _)| *idx);

        for (_, partition) in ordered_partitions {
            let representative = *partition.iter().next().unwrap();
            let old_state = &self.states[representative];

            let mut transitions = BTreeMap::new();
            for (input, &target) in &old_state.transitions {
                let new_target = partition_to_new_state[state_to_partition[target]];
                transitions.insert(input.clone(), new_target);
            }

            new_states.push(DfaState {
                transitions,
                accepting: old_state.accepting,
                accepting_rules: old_state.accepting_rules.clone(),
                main_pattern_end_rules: old_state.main_pattern_end_rules.clone(),
            });
        }

        let minimized = Dfa {
            states: new_states,
            start: 0,
            char_classes: self.char_classes.clone(),
        };

        // Rebuild character classes for minimized DFA
        Dfa {
            char_classes: CharClasses::build(&minimized),
            ..minimized
        }
    }

    /// Get the number of states
    pub fn num_states(&self) -> usize {
        self.states.len()
    }

    /// Get the number of transitions
    pub fn num_transitions(&self) -> usize {
        self.states.iter().map(|s| s.transitions.len()).sum()
    }

    /// Look up the transition for a given state and equivalence class
    /// Returns the target state index, or -1 if no transition
    pub fn lookup(&self, state: usize, class: usize) -> i16 {
        // Find which character maps to this class
        for (ch, &cls) in self.char_classes.char_to_class.iter().enumerate() {
            if cls as usize == class {
                let c = ch as u8 as char;
                if let Some(&target) = self.states[state].transitions.get(&DfaInput::Char(c)) {
                    return target as i16;
                }
                break;
            }
        }
        -1 // No transition
    }

    /// Compress the DFA transition tables using row displacement
    pub fn compress(&self) -> CompressedTables {
        let num_states = self.states.len();
        let num_classes = self.char_classes.num_classes;

        // Build dense transition table first (state x class -> target)
        let mut dense: Vec<Vec<i16>> = vec![vec![-1; num_classes]; num_states];
        for (state_idx, state) in self.states.iter().enumerate() {
            for (input, &target) in &state.transitions {
                let DfaInput::Char(c) = input;
                let class = self.char_classes.char_to_class[*c as usize] as usize;
                dense[state_idx][class] = target as i16;
            }
        }

        // Find default transition for each state (most common target)
        let mut default: Vec<i16> = Vec::with_capacity(num_states);
        for state_row in &dense {
            let mut counts: HashMap<i16, usize> = HashMap::new();
            for &target in state_row {
                *counts.entry(target).or_insert(0) += 1;
            }
            // Default is the most frequent transition (often -1 for jam)
            let most_common = counts
                .into_iter()
                .max_by_key(|&(_, count)| count)
                .map(|(target, _)| target)
                .unwrap_or(-1);
            default.push(most_common);
        }

        // Build difference entries: (class, target) pairs where target != default
        let mut state_diffs: Vec<Vec<(usize, i16)>> = Vec::with_capacity(num_states);
        for (state_idx, state_row) in dense.iter().enumerate() {
            let mut diffs = Vec::new();
            for (class, &target) in state_row.iter().enumerate() {
                if target != default[state_idx] {
                    diffs.push((class, target));
                }
            }
            state_diffs.push(diffs);
        }

        // Row displacement: pack all states into shared nxt/chk arrays
        // Start with reasonable initial size
        let initial_size = num_states * num_classes / 2 + num_classes;
        let mut nxt: Vec<i16> = vec![-1; initial_size];
        let mut chk: Vec<i16> = vec![-1; initial_size];
        let mut base: Vec<i32> = vec![0; num_states];
        let mut firstfree: usize = 0;

        for (state_idx, diffs) in state_diffs.iter().enumerate() {
            if diffs.is_empty() {
                // No non-default transitions, base can be 0
                base[state_idx] = 0;
                continue;
            }

            // Find a slot where all our entries fit without collision
            let max_class = diffs.iter().map(|(c, _)| *c).max().unwrap_or(0);
            let slot = find_table_space(&chk, firstfree, diffs, max_class, num_classes);

            // Ensure arrays are large enough
            let needed_size = slot + num_classes;
            if needed_size > nxt.len() {
                nxt.resize(needed_size, -1);
                chk.resize(needed_size, -1);
            }

            // Store entries
            base[state_idx] = slot as i32;
            for &(class, target) in diffs {
                let idx = slot + class;
                nxt[idx] = target;
                chk[idx] = state_idx as i16;
            }

            // Update firstfree hint
            while firstfree < chk.len() && chk[firstfree] != -1 {
                firstfree += 1;
            }
        }

        // Trim trailing unused entries
        let mut actual_len = nxt.len();
        while actual_len > 0 && chk[actual_len - 1] == -1 {
            actual_len -= 1;
        }
        nxt.truncate(actual_len);
        chk.truncate(actual_len);

        CompressedTables {
            base,
            default,
            nxt,
            chk,
            num_classes,
        }
    }
}

/// Find a slot in the nxt/chk arrays where entries can be placed without collision
fn find_table_space(
    chk: &[i16],
    firstfree: usize,
    diffs: &[(usize, i16)],
    _max_class: usize,
    _num_classes: usize,
) -> usize {
    if diffs.is_empty() {
        return 0;
    }

    let min_class = diffs.iter().map(|(c, _)| *c).min().unwrap_or(0);

    // Try slots starting from firstfree
    let mut slot = firstfree.saturating_sub(min_class);

    'outer: loop {
        // Check if all entries fit at this slot
        for &(class, _) in diffs {
            let idx = slot + class;
            if idx < chk.len() && chk[idx] != -1 {
                // Collision, try next slot
                slot += 1;
                continue 'outer;
            }
        }
        // No collision, use this slot
        return slot;
    }
}

/// Compressed DFA transition tables using row displacement encoding
///
/// This format achieves 3-10x size reduction compared to dense 2D tables
/// by sharing slots in the nxt/chk arrays across states.
#[derive(Debug)]
pub struct CompressedTables {
    /// Base offset into nxt/chk for each state
    pub base: Vec<i32>,
    /// Default transition for each state (most common target)
    pub default: Vec<i16>,
    /// Packed next-state array (shared across states)
    pub nxt: Vec<i16>,
    /// State ownership verification (should equal state index for valid entry)
    pub chk: Vec<i16>,
    /// Number of equivalence classes
    pub num_classes: usize,
}

impl CompressedTables {
    /// Look up transition for a given state and equivalence class
    pub fn lookup(&self, state: usize, class: usize) -> i16 {
        let idx = self.base[state] as usize + class;
        if idx < self.chk.len() && self.chk[idx] == state as i16 {
            self.nxt[idx]
        } else {
            self.default[state]
        }
    }

    /// Exhaustively verify that compressed tables produce identical results to original DFA
    /// Returns Ok(()) if all lookups match, or an error describing the first mismatch
    pub fn verify(&self, dfa: &Dfa) -> Result<(), String> {
        for state in 0..dfa.states.len() {
            for class in 0..dfa.char_classes.num_classes {
                let orig = dfa.lookup(state, class);
                let comp = self.lookup(state, class);
                if orig != comp {
                    return Err(format!(
                        "Compression mismatch at state={}, class={}: dense={}, compressed={}",
                        state, class, orig, comp
                    ));
                }
            }
        }
        Ok(())
    }

    /// Get compression statistics
    pub fn stats(&self) -> CompressionStats {
        let num_states = self.base.len();
        let dense_size = num_states * self.num_classes * 2; // 2 bytes per entry
        let compressed_size = self.base.len() * 4  // base: i32
            + self.default.len() * 2               // default: i16
            + self.nxt.len() * 2                   // nxt: i16
            + self.chk.len() * 2; // chk: i16

        CompressionStats {
            num_states,
            num_classes: self.num_classes,
            dense_size,
            compressed_size,
            ratio: dense_size as f64 / compressed_size as f64,
        }
    }
}

/// Statistics about table compression
#[derive(Debug)]
pub struct CompressionStats {
    pub num_states: usize,
    pub num_classes: usize,
    pub dense_size: usize,
    pub compressed_size: usize,
    pub ratio: f64,
}

/// Get all main pattern end rules for a set of NFA states
/// Used during DFA construction to track variable-length trailing context
fn get_main_pattern_end_rules(nfa: &Nfa, nfa_states: &BTreeSet<usize>) -> Vec<usize> {
    let mut rules: Vec<usize> = nfa_states
        .iter()
        .filter_map(|&state| nfa.main_pattern_end.get(&state))
        .flatten()
        .copied()
        .collect();
    rules.sort();
    rules.dedup();
    rules
}

/// Build the alphabet (set of all characters used in transitions) from an NFA
/// Only includes ASCII characters (0-255) for C compatibility
fn build_alphabet(nfa: &Nfa) -> Vec<char> {
    let mut chars: BTreeSet<char> = BTreeSet::new();

    for state in &nfa.states {
        for (trans, _) in &state.transitions {
            match trans {
                Transition::Char(c) => {
                    // Only include ASCII characters
                    if (*c as u32) < 256 {
                        chars.insert(*c);
                    }
                }
                Transition::CharClass(ranges) => {
                    // Add representative characters from each range (ASCII only)
                    for (lo, hi) in ranges {
                        // Clamp to ASCII range
                        let lo_clamped = std::cmp::max(*lo as u32, 0);
                        let hi_clamped = std::cmp::min(*hi as u32, 255);
                        if lo_clamped <= hi_clamped {
                            for c in lo_clamped..=hi_clamped {
                                chars.insert(char::from_u32(c).unwrap_or('\0'));
                            }
                        }
                    }
                }
                Transition::Epsilon => {}
            }
        }
    }

    chars.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nfa::Nfa;
    use regex_syntax::hir::Hir;

    fn parse_regex(pattern: &str) -> Hir {
        regex_syntax::parse(pattern).expect("Failed to parse regex")
    }

    #[test]
    fn test_simple_dfa() {
        let hir = parse_regex("ab");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Should have states for: start, after-a, after-ab (accepting)
        assert!(dfa.states.len() >= 3);

        // Check accepting state
        let accepting_states: Vec<_> = dfa
            .states
            .iter()
            .enumerate()
            .filter(|(_, s)| s.accepting.is_some())
            .collect();
        assert!(!accepting_states.is_empty());
    }

    #[test]
    fn test_alternation_dfa() {
        let hir = parse_regex("a|b");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Both 'a' and 'b' should lead to accepting states
        if let Some(&target) = dfa.states[dfa.start].transitions.get(&DfaInput::Char('a')) {
            assert!(dfa.states[target].accepting.is_some());
        }
        if let Some(&target) = dfa.states[dfa.start].transitions.get(&DfaInput::Char('b')) {
            assert!(dfa.states[target].accepting.is_some());
        }
    }

    #[test]
    fn test_kleene_star_dfa() {
        let hir = parse_regex("a*");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Start state should be accepting (matches empty string)
        assert!(dfa.states[dfa.start].accepting.is_some());
    }

    #[test]
    fn test_minimization() {
        let hir = parse_regex("a|b");
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);
        let minimized = dfa.minimize();

        // Minimized should have same or fewer states
        assert!(minimized.states.len() <= dfa.states.len());
    }

    #[test]
    fn test_multiple_rules_priority() {
        let hir1 = parse_regex("if");
        let hir2 = parse_regex("[a-z]+");
        let nfa = Nfa::from_rules(&[(hir1, 0), (hir2, 1)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Navigate through "if"
        let state_after_i = dfa.states[dfa.start]
            .transitions
            .get(&DfaInput::Char('i'))
            .copied();
        assert!(state_after_i.is_some());

        let state_after_if = dfa.states[state_after_i.unwrap()]
            .transitions
            .get(&DfaInput::Char('f'))
            .copied();
        assert!(state_after_if.is_some());

        // "if" should match rule 0 (higher priority)
        assert_eq!(dfa.states[state_after_if.unwrap()].accepting, Some(0));
    }
}
