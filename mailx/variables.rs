//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Internal variables for mailx

use std::collections::HashMap;
use std::num::NonZeroUsize;

/// The internal variables, and the alias, alternates and header lists that
/// commands maintain beside them.
#[derive(Debug)]
pub struct Variables {
    /// Every defined variable and its value. A binary variable is one whose
    /// value nobody reads; `set name` gives it, like any variable, the null
    /// value. Unset is absence.
    values: HashMap<String, String>,
    pub aliases: HashMap<String, Vec<String>>,
    /// Other addresses of the user's, from `alternates`.
    pub alternates: Vec<String>,
    /// Header fields `discard`/`ignore` hide.
    pub ignored_headers: Vec<String>,
    /// Header fields `retain` keeps.
    pub retained_headers: Vec<String>,
}

/// The name a variable is stored under: `ask` is accepted for `asksub`.
fn canonical(name: &str) -> &str {
    if name == "ask" {
        "asksub"
    } else {
        name
    }
}

impl Variables {
    pub fn new() -> Self {
        let mut vars = Variables {
            values: HashMap::new(),
            aliases: HashMap::new(),
            alternates: Vec::new(),
            ignored_headers: Vec::new(),
            retained_headers: Vec::new(),
        };

        // Set defaults per POSIX
        vars.enable("asksub");
        vars.enable("header");
        vars.enable("save");
        vars.set("prompt", "? ");
        vars.set("SHELL", "/bin/sh");
        vars.set("VISUAL", "vi");
        vars.set("indentprefix", "\t");
        vars.set("toplines", "5");

        vars
    }

    pub fn set(&mut self, name: &str, value: &str) {
        self.values
            .insert(canonical(name).to_string(), value.to_string());
    }

    /// Define `name` with the null value, as `set name` does.
    pub fn enable(&mut self, name: &str) {
        self.set(name, "");
    }

    pub fn unset(&mut self, name: &str) {
        self.values.remove(canonical(name));
    }

    /// Whether `name` is defined, whatever its value.
    pub fn is_set(&self, name: &str) -> bool {
        self.values.contains_key(canonical(name))
    }

    /// The value of `name`, null included.
    pub fn get(&self, name: &str) -> Option<&str> {
        self.values.get(canonical(name)).map(|s| s.as_str())
    }

    /// The value of `name`, unless it is unset or null.
    ///
    /// For variables that name a file or a command, where a null value
    /// names nothing.
    pub fn get_nonempty(&self, name: &str) -> Option<&str> {
        self.get(name).filter(|v| !v.is_empty())
    }

    pub fn get_number(&self, name: &str) -> Option<i64> {
        self.get(name).and_then(|s| s.parse().ok())
    }

    /// Header summaries per screenful, for the `headers` and `z` commands.
    ///
    /// The return type carries the non-zero guarantee: `screen` comes from
    /// the user, and the callers divide by it.
    ///
    /// When `screen` is unset the spec asks for a value "based on the terminal
    /// type identified by the TERM environment variable, the window size, the
    /// baud rate, or some combination of these" (104664-66).
    pub fn screen_lines(&self) -> NonZeroUsize {
        self.get_number("screen")
            .and_then(|n| usize::try_from(n).ok())
            .and_then(NonZeroUsize::new)
            .unwrap_or_else(|| screenful_from(terminal_rows(), std::env::var("TERM").ok()))
    }

    /// The command-escape character for input mode.
    ///
    /// `~` by default; `None` when `escape` is null, which disables command
    /// escapes (spec 104610-104612).
    pub fn escape_char(&self) -> Option<char> {
        match self.get("escape") {
            None => Some('~'),
            Some(s) => s.chars().next(),
        }
    }

    /// Write every defined variable, as `set` with no arguments does: `name`
    /// for a null value, `name="value"` otherwise.
    pub fn print_all(&self) {
        let mut names: Vec<&String> = self.values.keys().collect();
        names.sort();
        for name in names {
            match self.values[name].as_str() {
                "" => println!("{}", name),
                value => println!("{}=\"{}\"", name, value),
            }
        }
    }

    /// Expand an alias
    pub fn expand_alias(&self, name: &str) -> Vec<String> {
        let mut active = Vec::new();
        self.expand_alias_inner(name, &mut active)
    }

    /// Expand `name`, with `active` naming the aliases already being expanded.
    ///
    /// An alias that refers back to one further up the chain is emitted
    /// literally rather than followed. Without this, `alias a b` plus
    /// `alias b a` -- or simply `alias a a` -- recursed until the stack died,
    /// and a start-up file is a natural place for such a definition to appear.
    fn expand_alias_inner(&self, name: &str, active: &mut Vec<String>) -> Vec<String> {
        let Some(addrs) = self.aliases.get(name) else {
            return Vec::new();
        };
        if active.iter().any(|a| a == name) {
            return Vec::new();
        }
        active.push(name.to_string());

        let mut result = Vec::new();
        for addr in addrs {
            // A leading unquoted backslash prevents expansion of this group
            // member (spec 104720-104721): strip it and take the rest as-is.
            if let Some(literal) = addr.strip_prefix('\\') {
                result.push(literal.to_string());
                continue;
            }
            // Recursively expand
            let expanded = self.expand_alias_inner(addr, active);
            if expanded.is_empty() {
                result.push(addr.clone());
            } else {
                result.extend(expanded);
            }
        }

        active.pop();
        result
    }

    /// Check if an address is an alternate for the user
    pub fn is_alternate(&self, addr: &str) -> bool {
        let addr_lower = addr.to_lowercase();
        self.alternates
            .iter()
            .any(|a| a.to_lowercase() == addr_lower)
    }

    /// Whether `addr` names the user running mailx.
    ///
    /// This decides which recipients `reply` drops when `metoo` is unset, and
    /// which messages `showto` displays by recipient. POSIX 104926 defines the
    /// set as the user's login name plus the addresses declared with
    /// `alternates`, compared case-insensitively (spec 104728).
    ///
    /// The comparison is on whole names, not substrings. It used to ask whether
    /// the address *contained* the login, which made a short login like `ed`
    /// match `edward@example.com` -- and made an empty `$USER` match every
    /// address there is, so a reply-all silently addressed no one.
    pub fn is_me(&self, addr: &str) -> bool {
        let addr = crate::message::extract_address(addr);
        let addr = addr.as_str();
        if addr.is_empty() {
            return false;
        }
        if self.is_alternate(addr) {
            return true;
        }
        let login = crate::user_login();
        if login.is_empty() {
            return false;
        }
        let local = addr.split('@').next().unwrap_or(addr);
        addr.eq_ignore_ascii_case(&login) || local.eq_ignore_ascii_case(&login)
    }
}

/// Parse a set command argument
pub fn parse_set_arg(arg: &str) -> (&str, Option<&str>) {
    if let Some(eq_pos) = arg.find('=') {
        let name = &arg[..eq_pos];
        let value = &arg[eq_pos + 1..];
        // Strip one balanced pair of surrounding quotes, not every quote at
        // each end independently: `trim_matches` mangled `x=""quoted""` and
        // `x='a"` alike.
        let value = strip_one_quote_pair(value);
        (name, Some(value))
    } else {
        (arg, None)
    }
}

/// Remove one matched pair of surrounding quotes, if present.
fn strip_one_quote_pair(s: &str) -> &str {
    for q in ['"', '\''] {
        if s.len() >= 2 && s.starts_with(q) && s.ends_with(q) {
            return &s[1..s.len() - 1];
        }
    }
    s
}

/// Split a command argument string into words, honoring POSIX quoting.
///
/// Per spec 104694-104703: an argument enclosed in a matched pair of double or
/// single quotes keeps its whitespace and backslashes literally, each quote
/// character is ordinary inside the other kind, and an unquoted backslash makes
/// the next character literal. Splitting on bare whitespace instead meant no
/// variable value could contain a blank -- `set prompt="mail > "` was simply
/// unusable.
pub fn split_args(line: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut word = String::new();
    let mut started = false;
    let mut quote: Option<char> = None;
    let mut chars = line.chars();

    while let Some(c) = chars.next() {
        match quote {
            Some(q) if c == q => quote = None,
            Some(_) => word.push(c),
            None if c == '\\' => {
                started = true;
                if let Some(next) = chars.next() {
                    word.push(next);
                }
            }
            None if c == '"' || c == '\'' => {
                started = true;
                quote = Some(c);
            }
            None if c.is_whitespace() => {
                if started {
                    words.push(std::mem::take(&mut word));
                    started = false;
                }
            }
            None => {
                started = true;
                word.push(c);
            }
        }
    }

    if started {
        words.push(word);
    }
    words
}

/// Rows the terminal reports, or `None` when standard output is not one.
///
/// The window size is the most specific of the three sources the spec names,
/// and the only one that tracks a resized window, so it is asked first.
fn terminal_rows() -> Option<u16> {
    // SAFETY: `ws` is a live, correctly sized struct; the ioctl either fills
    // it or fails, and the return value is checked.
    let ws = unsafe {
        let mut ws: libc::winsize = std::mem::zeroed();
        if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut ws) == -1 {
            return None;
        }
        ws
    };
    (ws.ws_row > 0).then_some(ws.ws_row)
}

/// Rows the terminfo entry for `term` claims, if any.
fn terminfo_rows(term: &str) -> Option<u16> {
    use terminfo::capability as cap;
    let db = terminfo::Database::from_name(term).ok()?;
    let cap::Lines(n) = db.get::<cap::Lines>()?;
    u16::try_from(n).ok().filter(|n| *n > 0)
}

/// The default screenful, given a window size and a terminal type.
///
/// Split out from `screen_lines` so the rule can be asserted directly: a test
/// with no controlling terminal cannot otherwise say what the fallback did.
/// Window size first, then the terminfo entry for `TERM`, then 20 -- which is
/// what this was unconditionally before.
///
/// One row is held back for the command prompt, so a 24-row terminal lists 23
/// summaries rather than scrolling the first one away as it prompts.
fn screenful_from(rows: Option<u16>, term: Option<String>) -> NonZeroUsize {
    const DEFAULT: NonZeroUsize = NonZeroUsize::new(20).unwrap();
    let rows = rows.or_else(|| terminfo_rows(term.as_deref()?));
    rows.map(usize::from)
        .map(|r| r.saturating_sub(1))
        .and_then(NonZeroUsize::new)
        .unwrap_or(DEFAULT)
}

#[cfg(test)]
mod screenful_tests {
    use super::*;

    #[test]
    fn the_window_size_wins_when_there_is_one() {
        // 24-row window -> 23 summaries plus the prompt line.
        assert_eq!(screenful_from(Some(24), Some("dumb".into())).get(), 23);
        assert_eq!(screenful_from(Some(50), None).get(), 49);
    }

    #[test]
    fn a_degenerate_window_falls_back_rather_than_returning_zero() {
        // A one-row window would leave no summaries at all; three call sites
        // divide by this value, so it must never reach zero.
        assert_eq!(screenful_from(Some(1), None).get(), 20);
    }

    #[test]
    fn term_is_consulted_when_there_is_no_window_size() {
        // vt100 is 24 lines in every terminfo database that has it at all.
        if terminfo_rows("vt100") == Some(24) {
            assert_eq!(screenful_from(None, Some("vt100".into())).get(), 23);
        }
    }

    #[test]
    fn an_unknown_or_absent_term_falls_back_to_twenty() {
        assert_eq!(screenful_from(None, None).get(), 20);
        assert_eq!(
            screenful_from(None, Some("zz_no_such_terminal_zz".into())).get(),
            20
        );
    }
}
