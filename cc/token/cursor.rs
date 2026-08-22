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
    /// Pending tokens, held reversed so `pop` is the next one, each with the
    /// provenance it should be read back with. An expansion's output is not
    /// the only thing that gets pushed here: recovery from a malformed
    /// construct puts untouched *file* tokens back, and those have to stay
    /// file tokens or the rest of the file stops being preprocessed.
    pushback: Vec<(Token, Provenance)>,
    /// Where the token most recently returned by `next` came from.
    last: Provenance,
    /// Spacing an expansion left for whichever token comes next.
    pending_spacing: Option<(bool, bool)>,
}

impl TokenCursor {
    pub(super) fn new(tokens: Vec<Token>) -> Self {
        Self {
            main: tokens.into_iter(),
            pushback: Vec::new(),
            last: Provenance::Main,
            pending_spacing: None,
        }
    }

    /// The next token without consuming it, from the pushback if there is one
    /// and from the file otherwise.
    pub(super) fn peek(&self) -> Option<&Token> {
        match self.pushback.last() {
            Some((token, _)) => Some(token),
            None => self.main.as_slice().first(),
        }
    }

    /// Where the token most recently returned by [`next`](Iterator::next) came
    /// from. Meaningless before the first `next`, which is why it starts at
    /// `Main` rather than at something that would read as "unknown".
    pub(super) fn provenance(&self) -> Provenance {
        self.last
    }

    /// Put a macro expansion in front of the file.
    ///
    /// The tokens are read next, in the order given, and go through the same
    /// scanning as anything else. That is what makes rescanning free rather
    /// than a separate recursive pass over a separate vector, and it is what
    /// lets an expansion that ends mid-call finish the call from the rest of
    /// the file.
    ///
    /// `whitespace` and `newline` are the invocation's, and are handed to
    /// whichever token is yielded next -- from this expansion, or from the
    /// file if the expansion turns out to be empty. Stamping them onto
    /// `tokens[0]` instead loses them exactly when the expansion is empty or
    /// begins with a macro that expands to nothing.
    pub(super) fn push_expansion(&mut self, tokens: Vec<Token>, whitespace: bool, newline: bool) {
        self.pending_spacing = Some((whitespace, newline));
        self.pushback.reserve(tokens.len());
        self.pushback
            .extend(tokens.into_iter().rev().map(|t| (t, Provenance::Expansion)));
    }

    /// Put file tokens back, to be read again exactly as they were.
    ///
    /// This is recovery, not expansion: a construct was rejected and its tokens
    /// belong to the file, so they keep `Main` provenance and their own
    /// spacing. Pushing them as an expansion instead would leave the rest of
    /// the file unpreprocessed -- `remap_pos` skipped, and a `#` no longer
    /// recognised as a directive, since both of those ask where the token came
    /// from.
    pub(super) fn unread(&mut self, tokens: Vec<Token>) {
        self.pushback.reserve(tokens.len());
        self.pushback
            .extend(tokens.into_iter().rev().map(|t| (t, Provenance::Main)));
    }
}

impl Iterator for TokenCursor {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let token = match self.pushback.pop() {
            Some((token, provenance)) => {
                self.last = provenance;
                Some(token)
            }
            None => {
                self.last = Provenance::Main;
                self.main.next()
            }
        };
        match (token, self.pending_spacing.take()) {
            (Some(mut token), Some((whitespace, newline))) => {
                // Added to, never taken away. The flags describe where the
                // invocation stood, and the token they land on is the
                // expansion's first -- unless the expansion was empty, in
                // which case it is the next token of the *file*, which already
                // knows where it stands. Overwriting made an empty expansion
                // swallow the following line break: `#define E` with `A E` on
                // one line and `B c` on the next came out as `A B c`.
                token.pos.whitespace |= whitespace;
                token.pos.newline |= newline;
                Some(token)
            }
            (token, _) => token,
        }
    }
}
