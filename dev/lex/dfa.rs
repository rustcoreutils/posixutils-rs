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
    /// Default transition (any character not explicitly listed) - reserved for future use
    #[allow(dead_code)]
    Default,
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
    /// The set of NFA states this DFA state represents (for debugging)
    #[allow(dead_code)]
    pub nfa_states: BTreeSet<usize>,
}

impl DfaState {
    fn new(
        nfa_states: BTreeSet<usize>,
        accepting: Option<usize>,
        accepting_rules: Vec<usize>,
    ) -> Self {
        DfaState {
            transitions: BTreeMap::new(),
            accepting,
            accepting_rules,
            nfa_states,
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

        state_map.insert(initial_nfa_states.clone(), 0);
        dfa.states.push(DfaState::new(
            initial_nfa_states.clone(),
            initial_accepting,
            initial_accepting_rules,
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
                    state_map.insert(target_nfa_states.clone(), idx);
                    dfa.states.push(DfaState::new(
                        target_nfa_states.clone(),
                        accepting,
                        accepting_rules,
                    ));
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

        // Initial partition: separate accepting states by their full accepting rules list
        // This ensures states with different secondary rules (for REJECT/start conditions) are kept separate
        let mut partitions: Vec<BTreeSet<usize>> = Vec::new();
        let mut state_to_partition: Vec<usize> = vec![0; self.states.len()];

        // Group states by their full accepting rules list (not just the primary rule)
        // This preserves information needed for REJECT and start condition handling
        let mut accepting_groups: BTreeMap<Vec<usize>, BTreeSet<usize>> = BTreeMap::new();
        for (idx, state) in self.states.iter().enumerate() {
            accepting_groups
                .entry(state.accepting_rules.clone())
                .or_default()
                .insert(idx);
        }

        for (_, states) in accepting_groups {
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
            .filter_map(|i| match i {
                DfaInput::Char(c) => Some(*c),
                DfaInput::Default => None,
            })
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
                nfa_states: partition.clone(),
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
                Transition::Any => {
                    // Add all ASCII characters except newline
                    for c in 0u8..=255 {
                        if c != b'\n' {
                            chars.insert(c as char);
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
