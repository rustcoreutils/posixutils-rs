//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! DFA construction using subset construction (powerset algorithm).
//!
//! Converts NFA to DFA with Hopcroft minimization and character equivalence classes.

use crate::nfa::{Nfa, Transition};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// Index of a character equivalence class.
///
/// DFA transitions are keyed by class rather than by character: every byte in a
/// class drives the automaton identically, so determinization walks a handful of
/// symbols instead of all 256.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ClassId(pub usize);

/// A state in the DFA
#[derive(Debug, Clone)]
pub struct DfaState {
    /// Transitions from this state: input -> target_state
    pub transitions: BTreeMap<ClassId, usize>,
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
    /// The start state used when not at the beginning of a line (always 0)
    pub start: usize,
    /// The start state used at the beginning of a line.
    ///
    /// Equal to `start` when no rule is '^'-anchored, in which case the two
    /// roots have the same epsilon closure and collapse to one state.
    pub bol_start: usize,
    /// Character equivalence classes for table compression
    pub char_classes: CharClasses,
}

/// A set of bytes, as a 256-bit mask.
type ByteSet = [u64; 4];

fn byte_set_insert(set: &mut ByteSet, b: u8) {
    set[(b >> 6) as usize] |= 1u64 << (b & 63);
}

fn byte_set_contains(set: &ByteSet, b: u8) -> bool {
    (set[(b >> 6) as usize] >> (b & 63)) & 1 == 1
}

/// The bytes a single NFA transition accepts.
///
/// Characters above U+00FF cannot appear in the byte-oriented scanner, so they
/// contribute nothing; epsilon transitions consume no input at all.
fn transition_byte_set(trans: &Transition) -> Option<ByteSet> {
    let mut set: ByteSet = [0; 4];
    match trans {
        Transition::Epsilon => return None,
        Transition::Char(c) => {
            let c = *c as u32;
            if c > 255 {
                return None;
            }
            byte_set_insert(&mut set, c as u8);
        }
        Transition::CharClass(ranges) => {
            for (lo, hi) in ranges {
                let lo = *lo as u32;
                let hi = std::cmp::min(*hi as u32, 255);
                if lo > hi {
                    continue;
                }
                for b in lo..=hi {
                    byte_set_insert(&mut set, b as u8);
                }
            }
        }
    }
    Some(set)
}

/// Character equivalence classes: bytes no rule can tell apart.
#[derive(Debug, Clone)]
pub struct CharClasses {
    /// Maps each byte to its equivalence class
    pub char_to_class: [u8; 256],
    /// Number of distinct equivalence classes
    pub num_classes: usize,
    /// One representative byte per class, for driving the NFA
    reps: Vec<u8>,
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
            reps: vec![0],
        }
    }

    /// Derive equivalence classes from the NFA, before determinization.
    ///
    /// Start with every byte in one class and split by each transition's
    /// character set; two bytes end up together exactly when no transition in
    /// the automaton distinguishes them. Doing this first is what lets subset
    /// construction iterate over a handful of classes rather than 256
    /// characters -- the same ordering flex uses.
    pub fn from_nfa(nfa: &Nfa) -> Self {
        // Distinct character sets only: a rule set repeats the same class many
        // times, and each distinct set can split the partition at most once.
        let mut sets: BTreeSet<ByteSet> = BTreeSet::new();
        for state in &nfa.states {
            for (trans, _) in &state.transitions {
                if let Some(set) = transition_byte_set(trans) {
                    sets.insert(set);
                }
            }
        }

        let mut classes: Vec<Vec<u8>> = vec![(0..=255u8).collect()];
        for set in &sets {
            let mut refined: Vec<Vec<u8>> = Vec::with_capacity(classes.len() + 1);
            for class in classes.into_iter() {
                let (inside, outside): (Vec<u8>, Vec<u8>) =
                    class.into_iter().partition(|&b| byte_set_contains(set, b));
                match (inside.is_empty(), outside.is_empty()) {
                    // The set does not cut this class; keep it whole.
                    (true, _) => refined.push(outside),
                    (_, true) => refined.push(inside),
                    _ => {
                        refined.push(inside);
                        refined.push(outside);
                    }
                }
            }
            classes = refined;
        }

        // At most 256 bytes yield at most 256 classes, so a class number always
        // fits in the u8 table entry the generated scanner indexes with.
        let mut char_to_class = [0u8; 256];
        let mut reps = Vec::with_capacity(classes.len());
        for (idx, members) in classes.iter().enumerate() {
            for &b in members {
                char_to_class[b as usize] = idx as u8;
            }
            reps.push(members[0]);
        }

        CharClasses {
            char_to_class,
            num_classes: classes.len(),
            reps,
        }
    }

    /// The class a byte belongs to.
    pub fn class_of_byte(&self, b: u8) -> ClassId {
        ClassId(self.char_to_class[b as usize] as usize)
    }

    /// A byte standing for the whole class.
    pub fn representative(&self, class: ClassId) -> u8 {
        self.reps[class.0]
    }

    /// Every class, in order.
    pub fn classes(&self) -> impl Iterator<Item = ClassId> + '_ {
        (0..self.num_classes).map(ClassId)
    }
}

impl Dfa {
    /// Convert an NFA to a DFA using subset construction
    pub fn from_nfa(nfa: &Nfa) -> Self {
        let classes = CharClasses::from_nfa(nfa);
        Self::from_nfa_with_classes(nfa, classes)
    }

    /// Convert an NFA to a DFA over a given set of equivalence classes.
    ///
    /// Taking the classes as an argument lets a subsidiary automaton -- a
    /// trailing context, say -- share the main scanner's classes, so both can
    /// be driven from one `yy_ec` table.
    pub fn from_nfa_with_classes(nfa: &Nfa, char_classes: CharClasses) -> Self {
        let mut dfa = Dfa {
            states: Vec::new(),
            start: 0,
            bol_start: 0,
            char_classes,
        };

        // Map from NFA state sets to DFA state indices
        let mut state_map: HashMap<BTreeSet<usize>, usize> = HashMap::new();

        // Worklist of DFA states to process
        let mut worklist: Vec<BTreeSet<usize>> = Vec::new();

        // Seed one DFA state per NFA root. With no '^'-anchored rule the two
        // roots have the same epsilon closure, so state_map collapses them and
        // bol_start == start.
        let seed = |dfa: &mut Dfa,
                    state_map: &mut HashMap<BTreeSet<usize>, usize>,
                    worklist: &mut Vec<BTreeSet<usize>>,
                    root: usize| {
            let nfa_states = nfa.epsilon_closure(&BTreeSet::from([root]));
            if let Some(&idx) = state_map.get(&nfa_states) {
                return idx;
            }
            let idx = dfa.states.len();
            let accepting = nfa.get_accepting(&nfa_states);
            let accepting_rules = nfa.get_all_accepting(&nfa_states);
            let main_end_rules = get_main_pattern_end_rules(nfa, &nfa_states);
            state_map.insert(nfa_states.clone(), idx);
            dfa.states
                .push(DfaState::new(accepting, accepting_rules, main_end_rules));
            worklist.push(nfa_states);
            idx
        };

        dfa.start = seed(&mut dfa, &mut state_map, &mut worklist, nfa.start);
        dfa.bol_start = seed(&mut dfa, &mut state_map, &mut worklist, nfa.bol_start);

        // Every byte in a class behaves identically, so one representative
        // drives the NFA for the whole class.
        let alphabet: Vec<(ClassId, char)> = dfa
            .char_classes
            .classes()
            .map(|c| (c, dfa.char_classes.representative(c) as char))
            .collect();

        // Process worklist
        while let Some(nfa_states) = worklist.pop() {
            let dfa_state_idx = *state_map.get(&nfa_states).unwrap();

            // For each equivalence class
            for &(class, rep) in &alphabet {
                // Compute move on a representative of this class
                let moved = nfa.move_on_char(&nfa_states, rep);
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
                    .insert(class, target_dfa_idx);
            }
        }

        dfa
    }

    /// Minimize the DFA using Hopcroft's algorithm
    pub fn minimize(&self) -> Dfa {
        if self.states.is_empty() {
            return Dfa {
                states: Vec::new(),
                start: 0,
                bol_start: 0,
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

        // Refinement runs over equivalence classes, not characters: the
        // alphabet is a few dozen symbols rather than 256.
        let alphabet: Vec<ClassId> = self.char_classes.classes().collect();

        // Refine partitions until fixed point
        let mut changed = true;
        while changed {
            changed = false;

            for partition_idx in 0..partitions.len() {
                if partitions[partition_idx].len() <= 1 {
                    continue;
                }

                // Try to split this partition
                for class in &alphabet {
                    let partition = &partitions[partition_idx];
                    let mut splits: BTreeMap<Option<usize>, BTreeSet<usize>> = BTreeMap::new();

                    for &state in partition {
                        let target = self.states[state]
                            .transitions
                            .get(class)
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
            for (&class, &target) in &old_state.transitions {
                let new_target = partition_to_new_state[state_to_partition[target]];
                transitions.insert(class, new_target);
            }

            new_states.push(DfaState {
                transitions,
                accepting: old_state.accepting,
                accepting_rules: old_state.accepting_rules.clone(),
                main_pattern_end_rules: old_state.main_pattern_end_rules.clone(),
            });
        }

        // The classes come from the NFA and merging states cannot change
        // which bytes an automaton can tell apart, so they carry over as-is.
        Dfa {
            states: new_states,
            start: 0,
            // The beginning-of-line root is an entry point too, so it has to
            // follow the same partition remap as every transition target.
            bol_start: partition_to_new_state[state_to_partition[self.bol_start]],
            char_classes: self.char_classes.clone(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nfa::Nfa;
    use crate::nfa::NfaRule;
    use regex_syntax::hir::Hir;

    fn parse_regex(pattern: &str) -> Hir {
        regex_syntax::parse(pattern).expect("Failed to parse regex")
    }

    #[test]
    fn test_simple_dfa() {
        let hir = parse_regex("ab");
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)]).unwrap();
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
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Both 'a' and 'b' should lead to accepting states
        if let Some(&target) = dfa.states[dfa.start]
            .transitions
            .get(&dfa.char_classes.class_of_byte(b'a'))
        {
            assert!(dfa.states[target].accepting.is_some());
        }
        if let Some(&target) = dfa.states[dfa.start]
            .transitions
            .get(&dfa.char_classes.class_of_byte(b'b'))
        {
            assert!(dfa.states[target].accepting.is_some());
        }
    }

    #[test]
    fn test_kleene_star_dfa() {
        let hir = parse_regex("a*");
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Start state should be accepting (matches empty string)
        assert!(dfa.states[dfa.start].accepting.is_some());
    }

    #[test]
    fn test_minimization() {
        let hir = parse_regex("a|b");
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);
        let minimized = dfa.minimize();

        // Minimized should have same or fewer states
        assert!(minimized.states.len() <= dfa.states.len());
    }

    #[test]
    fn test_multiple_rules_priority() {
        let hir1 = parse_regex("if");
        let hir2 = parse_regex("[a-z]+");
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir1, 0), NfaRule::plain(hir2, 1)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        // Navigate through "if"
        let state_after_i = dfa.states[dfa.start]
            .transitions
            .get(&dfa.char_classes.class_of_byte(b'i'))
            .copied();
        assert!(state_after_i.is_some());

        let state_after_if = dfa.states[state_after_i.unwrap()]
            .transitions
            .get(&dfa.char_classes.class_of_byte(b'f'))
            .copied();
        assert!(state_after_if.is_some());

        // "if" should match rule 0 (higher priority)
        assert_eq!(dfa.states[state_after_if.unwrap()].accepting, Some(0));
    }
}
