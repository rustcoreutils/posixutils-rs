//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A fixed-size set of small integers, one bit each: the per-block sets of a
// dataflow analysis over a function's locals, numbered densely by the
// analysis. A hash set per block costs a hash per member per block per
// sweep, which a function with thousands of blocks and thousands of locals
// -- what splicing hundreds of calls into one makes -- pays in seconds.
//

/// A set of the integers `0..len`, one bit each.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct BitSet {
    words: Vec<u64>,
}

impl BitSet {
    /// The empty set of `0..len`.
    pub(crate) fn new(len: usize) -> Self {
        BitSet {
            words: vec![0; len.div_ceil(64)],
        }
    }

    /// The full set of `0..len`.
    pub(crate) fn full(len: usize) -> Self {
        let mut s = BitSet::new(len);
        for i in 0..len {
            s.insert(i);
        }
        s
    }

    pub(crate) fn insert(&mut self, i: usize) {
        self.words[i / 64] |= 1 << (i % 64);
    }

    pub(crate) fn remove(&mut self, i: usize) {
        self.words[i / 64] &= !(1 << (i % 64));
    }

    pub(crate) fn contains(&self, i: usize) -> bool {
        self.words[i / 64] & (1 << (i % 64)) != 0
    }

    /// Add every member of `other`, answering whether that added any.
    pub(crate) fn union_with(&mut self, other: &BitSet) -> bool {
        let mut changed = false;
        for (w, o) in self.words.iter_mut().zip(&other.words) {
            let merged = *w | o;
            changed |= merged != *w;
            *w = merged;
        }
        changed
    }

    /// Keep only the members `other` has too.
    pub(crate) fn intersect_with(&mut self, other: &BitSet) {
        for (w, o) in self.words.iter_mut().zip(&other.words) {
            *w &= o;
        }
    }

    /// Drop every member of `other`.
    pub(crate) fn subtract(&mut self, other: &BitSet) {
        for (w, o) in self.words.iter_mut().zip(&other.words) {
            *w &= !o;
        }
    }

    /// The members, in increasing order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.words.iter().enumerate().flat_map(|(w, &word)| {
            let mut word = word;
            std::iter::from_fn(move || {
                if word == 0 {
                    return None;
                }
                let bit = word.trailing_zeros() as usize;
                word &= word - 1;
                Some(w * 64 + bit)
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bitset_members_across_words() {
        let mut s = BitSet::new(130);
        for i in [0, 63, 64, 129] {
            s.insert(i);
        }
        assert_eq!(s.iter().collect::<Vec<_>>(), vec![0, 63, 64, 129]);
        s.remove(63);
        assert!(!s.contains(63) && s.contains(64));
    }

    #[test]
    fn bitset_set_operations() {
        let full = BitSet::full(70);
        assert_eq!(full.iter().count(), 70);
        let mut a = BitSet::new(70);
        a.insert(3);
        a.insert(69);
        let mut b = BitSet::new(70);
        b.insert(69);
        assert!(!a.clone().union_with(&b), "nothing new");
        assert!(b.union_with(&a), "3 is new");
        let mut c = full.clone();
        c.intersect_with(&a);
        assert_eq!(c, a);
        let mut d = full;
        d.subtract(&a);
        assert_eq!(d.iter().count(), 68);
        assert!(!d.contains(3) && d.contains(4));
    }
}
