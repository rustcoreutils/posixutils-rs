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

/// Mailx internal variables
#[derive(Debug)]
pub struct Variables {
    /// String/numeric variables
    values: HashMap<String, String>,
    /// Boolean variables (set = true, unset = false)
    booleans: HashMap<String, bool>,
    /// Aliases
    pub aliases: HashMap<String, Vec<String>>,
    /// Alternate names for the user
    pub alternates: Vec<String>,
    /// Ignored header fields
    pub ignored_headers: Vec<String>,
    /// Retained header fields
    pub retained_headers: Vec<String>,
    /// Last shell command (for ! expansion with bang variable)
    pub last_shell_cmd: Option<String>,
    /// Conditional (`if`/`else`/`endif`) nesting state for command mode:
    /// each entry is (this branch matches, currently in the else branch).
    pub cond_stack: Vec<(bool, bool)>,
}

impl Variables {
    pub fn new() -> Self {
        let mut vars = Variables {
            values: HashMap::new(),
            booleans: HashMap::new(),
            aliases: HashMap::new(),
            alternates: Vec::new(),
            ignored_headers: Vec::new(),
            retained_headers: Vec::new(),
            last_shell_cmd: None,
            cond_stack: Vec::new(),
        };

        // Set defaults per POSIX
        vars.set_bool("asksub", true);
        vars.set_bool("header", true);
        vars.set_bool("save", true);
        vars.set("prompt", "? ");
        vars.set("SHELL", "/bin/sh");
        vars.set("VISUAL", "vi");
        vars.set("indentprefix", "\t");
        vars.set("toplines", "5");

        vars
    }

    /// Set a string/numeric variable
    pub fn set(&mut self, name: &str, value: &str) {
        // Handle ask/asksub synonyms
        let name = if name == "ask" { "asksub" } else { name };

        self.values.insert(name.to_string(), value.to_string());
    }

    /// Get a string variable
    pub fn get(&self, name: &str) -> Option<&str> {
        let name = if name == "ask" { "asksub" } else { name };
        self.values.get(name).map(|s| s.as_str())
    }

    /// Get a numeric variable
    pub fn get_number(&self, name: &str) -> Option<i64> {
        self.get(name).and_then(|s| s.parse().ok())
    }

    /// Header summaries per screenful, for the `headers` and `z` commands.
    ///
    /// The return type carries the non-zero guarantee rather than leaving each
    /// caller to remember it: `screen` comes from the user, and every one of
    /// the three sites that divides by it used to divide by zero on
    /// `set screen=0`.
    pub fn screen_lines(&self) -> NonZeroUsize {
        const DEFAULT: NonZeroUsize = NonZeroUsize::new(20).unwrap();
        self.get_number("screen")
            .and_then(|n| usize::try_from(n).ok())
            .and_then(NonZeroUsize::new)
            .unwrap_or(DEFAULT)
    }

    /// Set a boolean variable
    pub fn set_bool(&mut self, name: &str, value: bool) {
        // Handle ask/asksub synonyms
        let name = if name == "ask" { "asksub" } else { name };

        self.booleans.insert(name.to_string(), value);
    }

    /// Get a boolean variable
    pub fn get_bool(&self, name: &str) -> bool {
        let name = if name == "ask" { "asksub" } else { name };
        self.booleans.get(name).copied().unwrap_or(false)
    }

    /// Unset a variable
    pub fn unset(&mut self, name: &str) {
        let name = if name == "ask" { "asksub" } else { name };
        self.values.remove(name);
        self.booleans.remove(name);
    }

    /// Get the command-escape character for input mode.
    ///
    /// Returns `Some('~')` by default, `Some(c)` when `escape` names a
    /// character, and `None` when `escape` is set to null — in which case
    /// command escaping is disabled (spec 104610-104612).
    pub fn escape_char(&self) -> Option<char> {
        match self.get("escape") {
            None => Some('~'),
            Some(s) => s.chars().next(), // empty string => None => disabled
        }
    }

    /// Print all set variables
    pub fn print_all(&self) {
        let mut names: Vec<&String> = self.values.keys().collect();
        names.sort();
        for name in names {
            if let Some(value) = self.values.get(name) {
                println!("{}=\"{}\"", name, value);
            }
        }

        let mut bools: Vec<(&String, &bool)> = self.booleans.iter().collect();
        bools.sort_by_key(|(k, _)| *k);
        for (name, value) in bools {
            if *value {
                println!("{}", name);
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

    /// Whether commands should currently execute given the conditional stack
    /// (true when every enclosing `if`/`else` branch matches).
    pub fn cond_active(&self) -> bool {
        self.cond_stack.iter().all(|(matches, _)| *matches)
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
        let addr = crate::message::extract_address(addr).trim();
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
