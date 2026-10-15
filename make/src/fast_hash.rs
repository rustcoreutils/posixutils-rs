//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Hash maps keyed by target, file and macro names, with a fast hasher.
//!
//! The standard maps hash with SipHash, which resists an adversary choosing
//! keys that collide. A makefile's author has no need to attack the make
//! that runs it, and SipHash was a sixth of the instructions of a no-op run,
//! which looks names up at every step. This is the multiply-rotate hash
//! rustc uses for its own tables (FxHash).

use std::collections::{HashMap, HashSet};
use std::hash::{BuildHasherDefault, Hasher};

/// A `HashMap` with the fast hasher.
pub type Map<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// A `HashSet` with the fast hasher.
pub type Set<K> = HashSet<K, BuildHasherDefault<FxHasher>>;

/// FxHash: each word is mixed in by a rotate, an exclusive or and a
/// multiplication.
#[derive(Default, Clone, Copy)]
pub struct FxHasher {
    hash: u64,
}

const SEED: u64 = 0x51_7c_c1_b7_27_22_0a_95;

impl FxHasher {
    fn add(&mut self, word: u64) {
        self.hash = (self.hash.rotate_left(5) ^ word).wrapping_mul(SEED);
    }
}

impl Hasher for FxHasher {
    fn write(&mut self, bytes: &[u8]) {
        let (words, rest) = bytes.as_chunks::<8>();
        for word in words {
            self.add(u64::from_ne_bytes(*word));
        }
        if !rest.is_empty() {
            let mut buf = [0u8; 8];
            buf[..rest.len()].copy_from_slice(rest);
            self.add(u64::from_ne_bytes(buf));
        }
    }

    fn write_u8(&mut self, i: u8) {
        self.add(u64::from(i));
    }

    fn write_u32(&mut self, i: u32) {
        self.add(u64::from(i));
    }

    fn write_u64(&mut self, i: u64) {
        self.add(i);
    }

    fn write_usize(&mut self, i: usize) {
        self.add(i as u64);
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_that_differ_only_past_a_word_are_told_apart() {
        let mut set = Set::default();
        for name in ["abcdefgh1", "abcdefgh2", "abcdefgh", "abcdefg", ""] {
            assert!(set.insert(name.to_string()));
        }
        assert!(set.contains("abcdefgh2"));
        assert!(!set.contains("abcdefgh3"));
        assert_eq!(set.len(), 5);
    }
}
