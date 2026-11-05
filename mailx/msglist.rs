//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message lists: the `msglist` argument most commands take.
//!
//! Each blank-separated token is parsed into a [`Selector`] without looking
//! at the mailbox, then evaluated against it.

use crate::error::Result;
use crate::mailbox::{Direction, Mailbox};
use crate::message::{extract_login, Headers, Message, ReadState};
use crate::variables::Variables;

/// Which messages a list may name. Deleted messages are invisible to every
/// command but `undelete`, for which the positional forms count deleted
/// messages instead.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Want {
    Live,
    Deleted,
}

/// One message specification (spec 104525-104546).
#[derive(Debug, Clone, PartialEq, Eq)]
enum Selector {
    /// `n`
    Number(usize),
    /// `n-m`, either end also `^`, `$` or `.`.
    Range(Endpoint, Endpoint),
    /// `.`
    Current,
    /// `^`
    First,
    /// `$`
    Last,
    /// `*`
    All,
    /// `+`
    Next,
    /// `-`
    Previous,
    /// `/string`: a case-insensitive Subject search.
    Subject(String),
    /// `:c`: messages of type `c`.
    Type(char),
    /// Anything else: messages from this address.
    Address(String),
}

/// One end of an `n-m` range.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Endpoint {
    Number(usize),
    First,
    Last,
    Current,
}

impl Selector {
    fn parse(token: &str) -> Result<Selector> {
        // A range is a pair of message numbers (spec 104533): `-` separates
        // only when both halves name one, so an address such as
        // `alpha-zeta@example.net` is not a range.
        if let Some((start, end)) = token.split_once('-') {
            if let (Some(start), Some(end)) = (Endpoint::parse(start), Endpoint::parse(end)) {
                return Ok(Selector::Range(start, end));
            }
        }
        Ok(match token {
            "." => Selector::Current,
            "^" => Selector::First,
            "$" => Selector::Last,
            "*" => Selector::All,
            "+" => Selector::Next,
            "-" => Selector::Previous,
            _ if token.starts_with('/') => Selector::Subject(token[1..].to_string()),
            _ if token.starts_with(':') => {
                Selector::Type(token.chars().nth(1).ok_or("Invalid message type")?)
            }
            _ => match token.parse() {
                Ok(num) => Selector::Number(num),
                Err(_) => Selector::Address(token.to_string()),
            },
        })
    }

    /// The messages this selects, in mailbox order.
    fn select(&self, mb: &Mailbox, want: Want, allnet: bool) -> Result<Vec<usize>> {
        let visible = |m: &Message| want == Want::Deleted || !m.is_deleted();
        // What `^`, `$`, `+` and `-` step through.
        let positional = |m: &Message| m.is_deleted() == (want == Want::Deleted);
        let one =
            |found: Option<usize>, none: &str| found.map(|n| vec![n]).ok_or_else(|| none.into());
        // The diagnostic when nothing is found, for a live or a deleted search.
        let none = |live, deleted| if want == Want::Deleted { deleted } else { live };
        let current = mb.current().unwrap_or(0);

        match self {
            Selector::Number(num) if (1..=mb.message_count()).contains(num) => Ok(vec![*num]),
            Selector::Number(num) => Err(format!("Invalid message number: {}", num).into()),
            Selector::Range(start, end) => {
                let (start, end) = (start.resolve(mb), end.resolve(mb));
                if start > end {
                    return Err("Invalid range".into());
                }
                // Clamp to the mailbox rather than reject: `undelete 1-$` and
                // `undelete 1-9999` both mean "the rest of them".
                let last = mb.message_count();
                if start > last {
                    return Err(format!("Invalid message number: {}", start).into());
                }
                Ok((start..=end.min(last)).collect())
            }
            Selector::Current => one(mb.current(), "No current message"),
            Selector::First => one(
                mb.find(0, Direction::Forward, positional),
                none("No messages", "No deleted messages"),
            ),
            Selector::Last => one(
                mb.find(mb.message_count() + 1, Direction::Backward, positional),
                none("No messages", "No deleted messages"),
            ),
            Selector::All => Ok(matching(mb, visible)),
            Selector::Next => one(
                mb.find(current, Direction::Forward, positional),
                none("No more messages", "No more deleted messages"),
            ),
            Selector::Previous => one(
                mb.find(current, Direction::Backward, positional),
                none("No previous messages", "No previous deleted messages"),
            ),
            Selector::Subject(text) => {
                let lower = text.to_lowercase();
                let found =
                    searching(mb, visible, |h| h.subject().to_lowercase().contains(&lower))?;
                nonempty(found, || format!("No messages matching /{}", text))
            }
            Selector::Type(kind) => {
                let found = matching(mb, |m| is_of_type(m, *kind));
                nonempty(found, || format!("No messages of type :{}", kind))
            }
            Selector::Address(addr) => {
                let found = searching(mb, visible, |h| is_from(h, addr, allnet))?;
                nonempty(found, || format!("No messages from {}", addr))
            }
        }
    }
}

impl Endpoint {
    /// A plain number, or the `^`, `$` and `.` forms that stand for one
    /// (spec 104525-104533).
    fn parse(s: &str) -> Option<Endpoint> {
        match s {
            "^" => Some(Endpoint::First),
            "$" => Some(Endpoint::Last),
            "." => Some(Endpoint::Current),
            _ if !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit()) => {
                s.parse().ok().map(Endpoint::Number)
            }
            _ => None,
        }
    }

    fn resolve(self, mb: &Mailbox) -> usize {
        match self {
            Endpoint::Number(n) => n,
            Endpoint::First => 1,
            Endpoint::Last => mb.message_count(),
            Endpoint::Current => mb.current().unwrap_or(0),
        }
    }
}

/// The numbers of the messages that satisfy `pred`.
fn matching(mb: &Mailbox, pred: impl Fn(&Message) -> bool) -> Vec<usize> {
    (1..=mb.message_count())
        .filter(|&n| mb.get(n).is_some_and(&pred))
        .collect()
}

/// The numbers of the messages among those `candidate` accepts whose
/// headers, read from the store, satisfy `pred`.
fn searching(
    mb: &Mailbox,
    candidate: impl Fn(&Message) -> bool,
    pred: impl Fn(&Headers) -> bool,
) -> Result<Vec<usize>> {
    let mut found = Vec::new();
    for num in matching(mb, candidate) {
        if pred(&mb.headers(num)?) {
            found.push(num);
        }
    }
    Ok(found)
}

fn nonempty(found: Vec<usize>, diagnostic: impl FnOnce() -> String) -> Result<Vec<usize>> {
    if found.is_empty() {
        Err(diagnostic().into())
    } else {
        Ok(found)
    }
}

/// Whether `msg` is of the `:c` type `kind`.
fn is_of_type(msg: &Message, kind: char) -> bool {
    match kind {
        'd' => msg.is_deleted(),
        'n' => msg.read == ReadState::New,
        'o' => msg.is_old(),
        'r' => msg.read == ReadState::Read,
        'u' => msg.read == ReadState::Unread,
        _ => false,
    }
}

/// Whether `msg` is from `addr`, compared case-insensitively. With `allnet`
/// only the login parts are compared (spec 104575-104577).
fn is_from(msg: &Headers, addr: &str, allnet: bool) -> bool {
    if allnet {
        let login = extract_login(addr).to_lowercase();
        extract_login(msg.from()).to_lowercase().contains(&login)
    } else {
        msg.from().to_lowercase().contains(&addr.to_lowercase())
    }
}

/// The messages `spec` names, in order and without repeats.
pub fn parse_msglist(spec: &str, mb: &Mailbox, want: Want, vars: &Variables) -> Result<Vec<usize>> {
    let spec = spec.trim();
    if spec.is_empty() {
        return omitted(mb, want);
    }

    let allnet = vars.is_set("allnet");
    let mut result = Vec::new();
    for token in spec.split_whitespace() {
        result.extend(Selector::parse(token)?.select(mb, want, allnet)?);
    }
    result.sort_unstable();
    result.dedup();
    if want == Want::Live {
        result.retain(|&n| mb.get(n).is_some_and(|m| !m.is_deleted()));
    }

    nonempty(result, || "No applicable messages".to_string())
}

/// Whether `token` names messages by number, position or state rather than
/// by searching their headers: `3`, `2-5`, `$`, `:u`, but not `/text` or an
/// address.
pub fn is_positional(token: &str) -> bool {
    Selector::parse(token).is_ok_and(|s| !matches!(s, Selector::Subject(_) | Selector::Address(_)))
}

/// The message an omitted msglist stands for, as BSD mailx's `first()`
/// finds it: the current message, or failing that the nearest after it, or
/// failing that before it, that `want` can name -- so a deleted current
/// message is passed over by every command but `undelete`.
pub fn first(mb: &Mailbox, want: Want) -> Option<usize> {
    let wanted = |m: &Message| m.is_deleted() == (want == Want::Deleted);
    let current = mb.current().unwrap_or(0);
    mb.find(current.saturating_sub(1), Direction::Forward, wanted)
        .or_else(|| mb.find(current, Direction::Backward, wanted))
}

/// The list an omitted msglist stands for: see [`first`].
fn omitted(mb: &Mailbox, want: Want) -> Result<Vec<usize>> {
    first(mb, want)
        .map(|n| vec![n])
        .ok_or_else(|| "No applicable messages".into())
}

/// The list an omitted msglist stands for, for every command but
/// `undelete`.
pub fn current_message(mb: &Mailbox) -> Result<Vec<usize>> {
    omitted(mb, Want::Live)
}

/// The messages an optional msglist argument names, defaulting to the current
/// one.
pub fn msglist_or_current(args: &str, mb: &Mailbox, vars: &Variables) -> Result<Vec<usize>> {
    if args.is_empty() {
        current_message(mb)
    } else {
        parse_msglist(args, mb, Want::Live, vars)
    }
}

/// Parse a message list and return just the first message
pub fn parse_message(spec: &str, mb: &Mailbox, vars: &Variables) -> Result<usize> {
    let list = parse_msglist(spec, mb, Want::Live, vars)?;
    list.first().copied().ok_or_else(|| "No message".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokens_parse_to_their_selectors() {
        let cases = [
            ("3", Selector::Number(3)),
            ("+5", Selector::Number(5)),
            (
                "2-4",
                Selector::Range(Endpoint::Number(2), Endpoint::Number(4)),
            ),
            ("^-$", Selector::Range(Endpoint::First, Endpoint::Last)),
            (
                ".-3",
                Selector::Range(Endpoint::Current, Endpoint::Number(3)),
            ),
            (".", Selector::Current),
            ("^", Selector::First),
            ("$", Selector::Last),
            ("*", Selector::All),
            ("+", Selector::Next),
            ("-", Selector::Previous),
            ("/Re: x", Selector::Subject("Re: x".into())),
            (":u", Selector::Type('u')),
            (
                "alpha-zeta@example.net",
                Selector::Address("alpha-zeta@example.net".into()),
            ),
            ("bob", Selector::Address("bob".into())),
        ];
        for (token, want) in cases {
            assert_eq!(Selector::parse(token).unwrap(), want, "{}", token);
        }
    }

    #[test]
    fn a_bare_colon_is_not_a_type() {
        assert!(Selector::parse(":").is_err());
    }
}
