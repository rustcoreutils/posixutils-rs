// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The makefile scanner.
//!
//! By the time `parse` runs, the preprocessor has folded continuations,
//! spliced includes, expanded macros and blanked macro-definition lines. What
//! is left is rule lines and command lines, which this walks one line at a
//! time.

use super::preprocessor::{is_include_line, is_macro_definition, preprocess, VPathEntry};
use super::scan::{self, RuleLine};
use crate::Macro;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub Vec<String>);

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for err in &self.0 {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// One target rule: its targets, its prerequisites, and its command lines.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Rule {
    targets: Vec<String>,
    prerequisites: Vec<String>,
    /// Command lines with their leading `<tab>` removed. Prefix characters
    /// (`@`, `-`, `+`) are still attached; `rule::Recipe` strips those.
    recipes: Vec<String>,
    /// 1-based line in the preprocessed text, for diagnostics.
    line: usize,
}

impl Rule {
    pub fn targets(&self) -> impl Iterator<Item = &str> {
        self.targets.iter().map(String::as_str)
    }

    pub fn prerequisites(&self) -> impl Iterator<Item = &str> {
        self.prerequisites.iter().map(String::as_str)
    }

    pub fn recipes(&self) -> impl Iterator<Item = &str> {
        self.recipes.iter().map(String::as_str)
    }

    /// Line in the preprocessed text where this rule's header appeared.
    pub fn line(&self) -> usize {
        self.line
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{}: {}",
            self.targets.join(" "),
            self.prerequisites.join(" ")
        )?;
        for recipe in &self.recipes {
            writeln!(f, "\t{recipe}")?;
        }
        Ok(())
    }
}

/// A parsed makefile: its rules, and the macros the preprocessor consumed.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Makefile {
    rules: Vec<Rule>,
    macros: Vec<Macro>,
    /// `vpath` search paths, in declaration order.
    vpaths: Vec<VPathEntry>,
}

impl Makefile {
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.rules.iter()
    }

    pub fn macros(&self) -> &[Macro] {
        &self.macros
    }

    pub fn vpaths(&self) -> &[VPathEntry] {
        &self.vpaths
    }

    /// Consume the makefile, yielding its rules, macros and search paths.
    pub fn into_parts(self) -> (Vec<Rule>, Vec<Macro>, Vec<VPathEntry>) {
        (self.rules, self.macros, self.vpaths)
    }
}

impl Display for Makefile {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for rule in &self.rules {
            write!(f, "{rule}")?;
        }
        Ok(())
    }
}

/// What one physical line of a preprocessed makefile is.
enum Line<'a> {
    /// A `<tab>`-indented command line, tab already removed.
    Command(&'a str),
    /// Blank, or a comment; neither opens nor closes an entry (POSIX 105646).
    Ignorable,
    /// A macro definition or include line, already handled upstream.
    Consumed,
    /// Any other non-empty line, which begins a new entry.
    Entry(&'a str),
}

/// Classify one physical line.
fn classify(line: &str) -> Line<'_> {
    // POSIX 105645: all lines beginning with <tab> are command lines. Checked
    // first, and never comment-stripped -- the shell sees them verbatim, where
    // `#` is meaningful.
    if let Some(body) = line.strip_prefix('\t') {
        if body.trim().is_empty() {
            return Line::Ignorable;
        }
        return Line::Command(body);
    }

    let code = scan::strip_comment(line);
    if code.trim().is_empty() {
        return Line::Ignorable;
    }
    if is_macro_definition(code) || is_include_line(code) {
        return Line::Consumed;
    }
    Line::Entry(code)
}

/// Start a rule from a parsed rule line, seeding its inline command if present.
fn rule_from(header: RuleLine, line: usize) -> Rule {
    let RuleLine {
        targets,
        prerequisites,
        inline,
    } = header;
    Rule {
        targets,
        prerequisites,
        recipes: inline.into_iter().collect(),
        line,
    }
}

/// Accumulates rules while walking the file.
#[derive(Default)]
struct Builder {
    rules: Vec<Rule>,
    errors: Vec<String>,
    current: Option<Rule>,
}

impl Builder {
    /// Close the rule under construction, if any.
    fn close(&mut self) {
        if let Some(rule) = self.current.take() {
            self.rules.push(rule);
        }
    }

    /// Add a command line to the rule under construction.
    ///
    /// A `<tab>`-indented line is a command line only inside a rule. Outside
    /// one it is ordinary text -- real makefiles indent continuation and
    /// comment lines with tabs, so `\t# note` between two macro definitions
    /// must not be an error. It is re-classified without its indentation.
    fn push_command(&mut self, body: &str, lineno: usize) {
        if self.current.is_some() {
            if let Some(rule) = self.current.as_mut() {
                rule.recipes.push(body.to_string());
            }
            return;
        }
        if let Line::Entry(code) = classify(body) {
            self.open(code, lineno);
        }
    }

    fn open(&mut self, code: &str, lineno: usize) {
        self.close();
        match scan::split_rule_line(code) {
            Ok(header) => self.current = Some(rule_from(header, lineno)),
            Err(msg) => self.errors.push(format!("{lineno}: {msg}")),
        }
    }

    fn finish(
        mut self,
        macros: Vec<Macro>,
        vpaths: Vec<VPathEntry>,
    ) -> Result<Makefile, ParseError> {
        self.close();
        // A makefile that defines no rules at all cannot be built from.
        if self.errors.is_empty() && self.rules.is_empty() {
            self.errors.push(" *** No targets. Stop.".to_string());
        }
        if self.errors.is_empty() {
            Ok(Makefile {
                rules: self.rules,
                macros,
                vpaths,
            })
        } else {
            Err(ParseError(self.errors))
        }
    }
}

/// Parse preprocessed makefile text into rules.
pub fn parse(text: &str) -> Result<Makefile, ParseError> {
    parse_scanned(text, Vec::new(), Vec::new())
}

fn parse_scanned(
    text: &str,
    macros: Vec<Macro>,
    vpaths: Vec<VPathEntry>,
) -> Result<Makefile, ParseError> {
    let mut builder = Builder::default();

    for (idx, raw) in text.split('\n').enumerate() {
        let lineno = idx + 1;
        let line = raw.strip_suffix('\r').unwrap_or(raw);
        match classify(line) {
            Line::Command(body) => builder.push_command(body, lineno),
            Line::Ignorable | Line::Consumed => {}
            Line::Entry(code) => builder.open(code, lineno),
        }
    }

    builder.finish(macros, vpaths)
}

impl FromStr for Makefile {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let scanned = preprocess(s).map_err(|e| ParseError(vec![e.to_string()]))?;
        parse_scanned(&scanned.text, scanned.macros, scanned.vpaths)
    }
}
