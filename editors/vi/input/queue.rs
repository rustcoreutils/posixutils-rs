//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The editor's key input queue.
//!
//! Everything the editor acts on arrives here: keys the user typed, keys a
//! `:map` expansion produced, and keys an `@` buffer execution produced. POSIX
//! describes the last of those as behaving "as if the contents of the named
//! buffer were entered as standard input" (vi.md 121171), and requires the same
//! of an abbreviation's replacement — historical practice was that such
//! characters "were logically pushed onto the terminal input queue, and were
//! not a simple replacement" (ex.md 96473-96475). One queue is how all three
//! come to mean the same thing.
//!
//! The queue also holds the keys of a partially matched left-hand side. A
//! multi-key `:map` cannot be recognised until enough keys have arrived, and
//! they must not reach the editor in the meantime.

use super::Key;
use std::collections::VecDeque;

/// Where a key came from, which decides what an error discards.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeySource {
    /// The user typed it.
    Typed,
    /// A `:map` or abbreviation expansion produced it.
    MapExpansion,
    /// An `@` buffer execution produced it.
    BufferExecution,
}

impl KeySource {
    /// Whether an error should discard the rest of this key's run.
    ///
    /// vi.md 120624-120627: on an error, "if the vi command resulted from a map
    /// expansion, all characters from that map expansion shall be discarded",
    /// and if from a buffer execution, "no further commands caused by the
    /// execution of the buffer shall be executed". Typed keys have no run to
    /// discard — the user is still at the keyboard.
    pub fn is_expansion(self) -> bool {
        !matches!(self, KeySource::Typed)
    }
}

/// One key, with what may still be done to it.
#[derive(Debug, Clone, Copy)]
pub struct QueuedKey {
    /// The key itself.
    pub key: Key,
    /// Whether this key may begin a `:map` match.
    ///
    /// Cleared for the leading run of a replacement that repeats its own
    /// left-hand side (95120-95121), for a whole replacement when `remap` is
    /// unset (95871-95875), and for the key after a `^V` (95097-95098).
    pub remappable: bool,
    /// Whether this key may trigger an abbreviation check.
    pub abbrevable: bool,
    /// Where it came from.
    pub source: KeySource,
}

impl QueuedKey {
    /// A key the user typed: eligible for everything.
    pub fn typed(key: Key) -> Self {
        Self {
            key,
            remappable: true,
            abbrevable: true,
            source: KeySource::Typed,
        }
    }
}

/// Pending keys, plus the partially matched left-hand side.
#[derive(Debug, Default)]
pub struct InputQueue {
    queue: VecDeque<QueuedKey>,
    /// Keys held back because they are a strict prefix of some `:map`
    /// left-hand side (95116-95118).
    partial: Vec<QueuedKey>,
    /// A command-mode `^V` is pending, so the next key matches no map.
    literal_next: bool,
}

impl InputQueue {
    /// Add a key the user typed.
    pub fn push_typed(&mut self, key: Key) {
        self.queue.push_back(QueuedKey::typed(key));
    }

    /// Put keys at the *front*, so an expansion is consumed before whatever was
    /// already waiting behind it.
    pub fn push_front(&mut self, keys: impl DoubleEndedIterator<Item = QueuedKey>) {
        for k in keys.rev() {
            self.queue.push_front(k);
        }
    }

    /// Take the next key.
    pub fn pop(&mut self) -> Option<QueuedKey> {
        self.queue.pop_front()
    }

    /// Whether any key is waiting behind the one in hand.
    pub fn has_queued(&self) -> bool {
        !self.queue.is_empty()
    }

    /// Hold a key as part of a possible multi-key left-hand side.
    pub fn hold(&mut self, key: QueuedKey) {
        self.partial.push(key);
    }

    /// The keys held so far.
    pub fn partial(&self) -> &[QueuedKey] {
        &self.partial
    }

    /// Give up on the held keys: the first is returned, the rest go back to the
    /// front of the queue so they can begin a match of their own.
    ///
    /// `:map bc X` must still fire on `abc`, so the `b` and `c` left over after
    /// `a` fails to match cannot simply be dispatched in place.
    pub fn release_partial(&mut self) -> Option<QueuedKey> {
        if self.partial.is_empty() {
            return None;
        }
        let first = self.partial.remove(0);
        let rest = std::mem::take(&mut self.partial);
        self.push_front(rest.into_iter());
        Some(first)
    }

    /// Drop the held keys without dispatching them.
    pub fn clear_partial(&mut self) {
        self.partial.clear();
    }

    /// A `^V` was seen; the next key matches no left-hand side.
    pub fn set_literal_next(&mut self) {
        self.literal_next = true;
    }

    /// Consume the pending-literal flag.
    pub fn take_literal_next(&mut self) -> bool {
        std::mem::take(&mut self.literal_next)
    }

    /// Whether a `^V` is pending.
    pub fn literal_next(&self) -> bool {
        self.literal_next
    }

    /// Whether anything in flight came from an expansion.
    ///
    /// Typed keys arrive one at a time, so anything queued behind the key in
    /// hand is expansion or buffer-execution material by construction.
    pub fn is_expanding(&self) -> bool {
        self.queue.iter().any(|k| k.source.is_expansion())
            || self.partial.iter().any(|k| k.source.is_expansion())
    }

    /// Discard everything pending, for vi.md 120624-120627.
    pub fn discard(&mut self) {
        self.queue.clear();
        self.partial.clear();
        self.literal_next = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn q(c: char) -> QueuedKey {
        QueuedKey::typed(Key::Char(c))
    }

    #[test]
    fn test_push_front_preserves_order() {
        let mut iq = InputQueue::default();
        iq.push_typed(Key::Char('z'));
        iq.push_front(vec![q('a'), q('b'), q('c')].into_iter());
        let got: Vec<char> = std::iter::from_fn(|| iq.pop())
            .filter_map(|k| k.key.as_char())
            .collect();
        assert_eq!(got, vec!['a', 'b', 'c', 'z']);
    }

    /// The leftovers of a failed match go back to the front, so a later
    /// left-hand side that starts inside them can still fire.
    #[test]
    fn test_release_partial_requeues_the_rest() {
        let mut iq = InputQueue::default();
        iq.hold(q('a'));
        iq.hold(q('b'));
        iq.hold(q('c'));
        let first = iq.release_partial().unwrap();
        assert_eq!(first.key.as_char(), Some('a'));
        assert!(iq.partial().is_empty());
        let got: Vec<char> = std::iter::from_fn(|| iq.pop())
            .filter_map(|k| k.key.as_char())
            .collect();
        assert_eq!(got, vec!['b', 'c']);
    }

    #[test]
    fn test_discard_drops_queue_and_partial() {
        let mut iq = InputQueue::default();
        iq.push_typed(Key::Char('x'));
        iq.hold(q('y'));
        iq.set_literal_next();
        iq.discard();
        assert!(iq.pop().is_none());
        assert!(iq.partial().is_empty());
        assert!(!iq.literal_next());
    }

    /// Typed keys arrive one at a time, so only expansion material is ever
    /// found waiting.
    #[test]
    fn test_is_expanding_only_for_expansion_sources() {
        let mut iq = InputQueue::default();
        iq.push_typed(Key::Char('x'));
        assert!(!iq.is_expanding());
        iq.push_front(
            vec![QueuedKey {
                key: Key::Char('y'),
                remappable: true,
                abbrevable: true,
                source: KeySource::MapExpansion,
            }]
            .into_iter(),
        );
        assert!(iq.is_expanding());
        iq.discard();
        assert!(!iq.is_expanding());
    }
}
