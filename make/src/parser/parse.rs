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

use super::preprocessor::{
    is_include_line, is_macro_definition, preprocess_with, Include, MacroTable, TargetVar,
    VPathEntry,
};
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
    /// GNU make's order-only prerequisites, written after a `|`.
    order_only: Vec<String>,
    /// Command lines with their leading `<tab>` removed. Prefix characters
    /// (`@`, `-`, `+`) are still attached; `rule::Recipe` strips those.
    recipes: Vec<String>,
    /// Written `target::`, the GNU double-colon form.
    double_colon: bool,
    /// The target pattern of a static pattern rule, until `Builder::close`
    /// makes one rule per target out of it.
    target_pattern: Option<String>,
    /// The stem a static pattern rule matched, which `$*` names.
    stem: Option<String>,
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

    pub fn order_only(&self) -> impl Iterator<Item = &str> {
        self.order_only.iter().map(String::as_str)
    }

    pub fn recipes(&self) -> impl Iterator<Item = &str> {
        self.recipes.iter().map(String::as_str)
    }

    /// The stem, for a rule made from a static pattern rule.
    pub fn stem(&self) -> Option<&str> {
        self.stem.as_deref()
    }

    /// Whether the rule was written `target::`.
    pub fn is_double_colon(&self) -> bool {
        self.double_colon
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
#[derive(Debug, Clone, Default)]
pub struct Makefile {
    rules: Vec<Rule>,
    macros: Vec<Macro>,
    /// `vpath` search paths, in declaration order.
    vpaths: Vec<VPathEntry>,
    /// Macros an `export` directive named.
    exports: Vec<String>,
    /// Every file an `include` line named; see [`Include`].
    includes: Vec<Include>,
    /// The macros again, with the source of each.
    table: MacroTable,
    /// Target- and pattern-specific macro assignments, in the order read.
    target_vars: Vec<TargetVar>,
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

    pub fn exports(&self) -> &[String] {
        &self.exports
    }

    pub fn includes(&self) -> &[Include] {
        &self.includes
    }

    /// The macros with the source of each, which decides whether a
    /// target-specific assignment may replace one.
    pub fn table(&self) -> &MacroTable {
        &self.table
    }

    pub fn target_vars(&self) -> &[TargetVar] {
        &self.target_vars
    }

    /// Consume the makefile, yielding its rules, macros, search paths and
    /// exported macro names.
    pub fn into_parts(self) -> (Vec<Rule>, Vec<Macro>, Vec<VPathEntry>, Vec<String>) {
        (self.rules, self.macros, self.vpaths, self.exports)
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
    Entry(std::borrow::Cow<'a, str>),
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
    if is_macro_definition(&code) || is_include_line(&code) {
        return Line::Consumed;
    }
    Line::Entry(code)
}

/// Start a rule from a parsed rule line, seeding its inline command if present.
///
/// Each target and prerequisite word holding a wildcard is globbed here, as
/// the line is read, as in GNU make (see [`super::func::glob_name`]).
fn rule_from(header: RuleLine, line: usize) -> Rule {
    let RuleLine {
        targets,
        prerequisites,
        order_only,
        inline,
        double_colon,
        target_pattern,
    } = header;
    let glob = |words: Vec<String>| -> Vec<String> {
        words
            .iter()
            .flat_map(|word| super::func::glob_name(word))
            .collect()
    };
    Rule {
        targets: glob(targets),
        prerequisites: glob(prerequisites),
        order_only: glob(order_only),
        recipes: inline.into_iter().collect(),
        double_colon,
        target_pattern,
        stem: None,
        line,
    }
}

/// Make one ordinary target rule per target of a static pattern rule.
///
/// GNU make: `targets: target-pattern: prereq-patterns` gives each target
/// that matches the target pattern the prerequisites the patterns name for
/// its stem, the commands, and the stem as `$*`. bzip2 builds every object
/// this way. A target that does not match is warned about and gets no
/// prerequisites, as in GNU make.
fn expand_static_pattern(rule: Rule) -> Vec<Rule> {
    let pattern = rule.target_pattern.clone().unwrap_or_default();
    rule.targets
        .iter()
        .map(|target| {
            let stem = crate::rule::pattern_stem(&pattern, target).map(String::from);
            let expand = |names: &[String]| -> Vec<String> {
                match &stem {
                    Some(stem) => names
                        .iter()
                        .map(|p| crate::rule::pattern_expand(p, stem))
                        .collect(),
                    None => Vec::new(),
                }
            };
            if stem.is_none() {
                eprintln!("make: target '{target}' doesn't match the target pattern");
            }
            Rule {
                targets: vec![target.clone()],
                prerequisites: expand(&rule.prerequisites),
                order_only: expand(&rule.order_only),
                target_pattern: None,
                stem,
                ..rule.clone()
            }
        })
        .collect()
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
    ///
    /// A rule whose target list expanded to nothing names no target, and is
    /// dropped along with its command lines: `$(EXTRA_PROGRAMS): dep` with an
    /// empty `EXTRA_PROGRAMS` is a rule for nothing, which automake output
    /// writes routinely. POSIX asks the application for a non-null list
    /// (105643) but says nothing of one that expands to null; GNU make
    /// ignores it, and rejecting it failed the whole makefile.
    ///
    /// A `%` pattern rule without commands is dropped too. In GNU make it
    /// cancels the built-in rule of that shape; it never makes anything, so
    /// keeping it would let it "make" every target it matches by running no
    /// commands. automake ends every Makefile with five of them
    /// (`%:: %,v`, `%:: SCCS/s.%`, ...), cancelling GNU rules this make does
    /// not have.
    fn close(&mut self) {
        let Some(rule) = self.current.take() else {
            return;
        };
        if rule.target_pattern.is_some() {
            self.rules.extend(expand_static_pattern(rule));
            return;
        }
        let pattern = rule.targets.iter().any(|t| t.contains('%'));
        if rule.targets.is_empty() || (pattern && rule.recipes.is_empty()) {
            return;
        }
        if rule.double_colon && pattern {
            self.errors.push(format!(
                "{}: double-colon pattern rules are not supported",
                rule.line
            ));
            return;
        }
        self.rules.push(rule);
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
            self.open(&code, lineno);
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
        exports: Vec<String>,
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
                exports,
                ..Makefile::default()
            })
        } else {
            Err(ParseError(self.errors))
        }
    }
}

/// Parse preprocessed makefile text into rules.
pub fn parse(text: &str) -> Result<Makefile, ParseError> {
    parse_scanned(text, Vec::new(), Vec::new(), Vec::new())
}

fn parse_scanned(
    text: &str,
    macros: Vec<Macro>,
    vpaths: Vec<VPathEntry>,
    exports: Vec<String>,
) -> Result<Makefile, ParseError> {
    let mut builder = Builder::default();

    for (idx, raw) in text.split('\n').enumerate() {
        let lineno = idx + 1;
        let line = raw.strip_suffix('\r').unwrap_or(raw);
        match classify(line) {
            Line::Command(body) => builder.push_command(body, lineno),
            Line::Ignorable | Line::Consumed => {}
            Line::Entry(code) => builder.open(&code, lineno),
        }
    }

    builder.finish(macros, vpaths, exports)
}

impl FromStr for Makefile {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Makefile::parse_with_macros(s, &[], &[])
    }
}

impl Makefile {
    /// Parse `text` with the definitions in force before it is read:
    /// command-line macros (POSIX source 1) and the built-ins (source 4).
    pub fn parse_with_macros(
        text: &str,
        cmdline: &[Macro],
        builtins: &[Macro],
    ) -> Result<Makefile, ParseError> {
        let scanned = preprocess_with(text, cmdline, builtins)
            .map_err(|e| ParseError(vec![e.to_string()]))?;
        let mut makefile = parse_scanned(
            &scanned.text,
            scanned.macros,
            scanned.vpaths,
            scanned.exports,
        )?;
        makefile.includes = scanned.includes;
        makefile.table = scanned.table;
        makefile.target_vars = scanned.target_vars;
        Ok(makefile)
    }
}
