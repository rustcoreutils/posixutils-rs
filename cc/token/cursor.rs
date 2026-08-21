//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The token stream a preprocessing pass reads.
//

use super::lexer::Token;

/// Where the token just read came from.
///
/// The distinction is load-bearing once macro expansion pushes its result back
/// in front of the cursor: a line marker must not be applied twice, and a `#`
/// that came out of an expansion is not a directive (C17 6.10.3p11 makes that
/// undefined), so both checks ask this rather than assuming.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Provenance {
    /// Read from the file being preprocessed.
    Main,
    /// Read from a macro expansion pushed back in front of the file.
    Expansion,
}

/// A token stream that can be pushed back onto.
///
/// This replaces a `Peekable<vec::IntoIter<Token>>`, and reads the same way --
/// `next()`, `peek()`, `for t in cursor.by_ref()` -- so that the directive
/// handlers did not have to change shape. What it adds is
/// [`push_expansion`](Self::push_expansion): the substituted body of a macro
/// goes back in front of the cursor instead of into the output, so rescanning
/// is just the same loop reading on, and an expansion that ends mid-call can
/// finish it from the rest of the file.
///
/// The pushback is one flat stack, not a stack of streams with an end marker
/// between them. That matters: `CALL(ADD)(10,32)` leaves `ADD_func` in the
/// pushback with its `(10, 32)` still in the file, so a `peek()` that stopped
/// at an exhausted expansion instead of falling through to the file would not
/// find the argument list.
pub(super) struct TokenCursor {
    /// The file, in order.
    main: std::vec::IntoIter<Token>,
    /// Pending expansion tokens, held reversed so `pop` is the next one.
    pushback: Vec<Token>,
    /// Where the token most recently returned by `next` came from.
    last: Provenance,
}

impl TokenCursor {
    pub(super) fn new(tokens: Vec<Token>) -> Self {
        Self {
            main: tokens.into_iter(),
            pushback: Vec::new(),
            last: Provenance::Main,
        }
    }

    /// The next token without consuming it, from the pushback if there is one
    /// and from the file otherwise.
    pub(super) fn peek(&self) -> Option<&Token> {
        match self.pushback.last() {
            Some(token) => Some(token),
            None => self.main.as_slice().first(),
        }
    }

    /// Where the token most recently returned by [`next`](Iterator::next) came
    /// from. Meaningless before the first `next`, which is why it starts at
    /// `Main` rather than at something that would read as "unknown".
    pub(super) fn provenance(&self) -> Provenance {
        self.last
    }
}

impl Iterator for TokenCursor {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.pushback.pop() {
            Some(token) => {
                self.last = Provenance::Expansion;
                Some(token)
            }
            None => {
                self.last = Provenance::Main;
                self.main.next()
            }
        }
    }
}
