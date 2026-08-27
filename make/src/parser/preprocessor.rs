//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::Macro;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::iter::Peekable;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Acquire;

#[derive(Debug)]
pub enum PreprocError {
    EmptyIdent,
    UnexpectedEOF,
    UnexpectedSymbol(char),
    TooManyColons,
    BadAssignmentOperator(char),
    CommandFailed,
    UndefinedMacro(String),
    BadMacroName,
    IncludeFailed { path: String, reason: String },
}

impl Display for PreprocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            // A missing/unreadable include is a user-facing condition, so give
            // it a readable message rather than the debug representation.
            PreprocError::IncludeFailed { path, reason } => {
                writeln!(f, "cannot open include file '{path}': {reason}")
            }
            other => writeln!(f, "{:?}", other),
        }
    }
}

impl std::error::Error for PreprocError {}

type Result<T> = std::result::Result<T, PreprocError>;

/// Returns true if `s` ends with an odd number of backslashes, i.e. its final
/// `<newline>` is escaped (a `\\` pair is a literal backslash, not a splice).
fn ends_with_odd_backslash(s: &str) -> bool {
    s.chars().rev().take_while(|&c| c == '\\').count() % 2 == 1
}

/// Splice one logical line starting at `lines[i]`, advancing `i` past every
/// physical line it consumed. Returns the joined text and how many extra
/// physical lines went into it.
fn fold_one_line(lines: &[&str], i: &mut usize) -> (String, usize) {
    let is_recipe = lines[*i].starts_with('\t');
    let mut current = lines[*i].to_string();
    let mut folded = 0;
    *i += 1;

    while ends_with_odd_backslash(&current) && *i < lines.len() {
        current.pop(); // drop the trailing backslash
        let next = lines[*i];
        *i += 1;
        folded += 1;
        if is_recipe {
            current.push_str(next.strip_prefix('\t').unwrap_or(next));
        } else {
            current.push(' ');
            current.push_str(next.trim_start());
        }
    }
    (current, folded)
}

/// Fold escaped newlines (`\<newline>`) per POSIX. Outside command (recipe)
/// lines the backslash, the newline, and any leading white space on the next
/// line are replaced by a single <space>. In a command line (tab-indented) the
/// continuation is spliced directly — a single leading <tab> of the next line
/// is removed — so the whole logical command reaches the shell intact.
///
/// Each physical line absorbed into a splice leaves an empty line behind, so
/// every later line keeps its original number. Empty lines are comments, so
/// the padding is semantically inert.
fn fold_continuations(source: &str) -> String {
    let lines: Vec<&str> = source.split('\n').collect();
    let n = lines.len();
    let mut out = String::with_capacity(source.len());
    let mut i = 0;
    while i < n {
        let (current, folded) = fold_one_line(&lines, &mut i);
        out.push_str(&current);
        for _ in 0..folded {
            out.push('\n');
        }
        if i < n {
            out.push('\n');
        }
    }
    out
}

/// Apply a `$(string:subst1=subst2)` substitution to an already-expanded macro
/// value. Handles both the suffix form (`subst1` is matched as a suffix of each
/// word) and the pattern form (`[op]%[os]=[np][%][ns]`).
fn apply_substitution(value: &str, spec: &str) -> String {
    let Some((lhs, rhs)) = spec.split_once('=') else {
        return value.to_string();
    };

    value
        .split_whitespace()
        .map(|word| {
            if let Some((op, os)) = lhs.split_once('%') {
                // Pattern form: word must match op + (stem) + os.
                if word.len() >= op.len() + os.len() && word.starts_with(op) && word.ends_with(os) {
                    let stem = &word[op.len()..word.len() - os.len()];
                    match rhs.split_once('%') {
                        Some((np, ns)) => format!("{np}{stem}{ns}"),
                        None => rhs.to_string(),
                    }
                } else {
                    word.to_string()
                }
            } else if !lhs.is_empty() && word.ends_with(lhs) {
                // Suffix form.
                format!("{}{}", &word[..word.len() - lhs.len()], rhs)
            } else {
                word.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn skip_blank(letters: &mut Peekable<impl Iterator<Item = char>>) {
    while let Some(letter) = letters.peek() {
        if !letter.is_whitespace() {
            break;
        };
        letters.next();
    }
}

fn suitable_ident(c: &char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '.')
}

/// Decide whether a whole makefile line is a macro definition, as opposed to a
/// recipe line, a rule header, or arbitrary text.
///
/// The preprocessor scans the file line-by-line for macro definitions and also
/// strips those lines before the rule parser runs. Both passes must agree, and
/// both must avoid misclassifying ordinary content: a recipe line is indented
/// with a `<tab>` and frequently contains `=` (shell assignments, `--opt=val`,
/// `test x = y`), and such a line is never a macro definition. A line is a
/// macro definition only when it is not a recipe line and the text preceding
/// the assignment operator is a single valid macro name (optionally prefixed
/// with `export`).
pub fn is_macro_definition(line: &str) -> bool {
    // Recipe lines are tab-indented and are never macro definitions.
    if line.starts_with('\t') {
        return false;
    }
    let Some(eq) = line.find('=') else {
        return false;
    };
    // Drop a trailing assignment-operator prefix (`:`, `?`, `+`, `!`) so that
    // `:=`, `::=`, `:::=`, `?=`, `+=`, and `!=` all reduce to the name.
    let mut name = line[..eq].trim_end_matches([':', '?', '+', '!']).trim();
    if let Some(rest) = name.strip_prefix("export") {
        // Require whitespace after `export` so `exported=1` is not mis-split.
        if rest.starts_with(char::is_whitespace) {
            name = rest.trim();
        }
    }
    !name.is_empty() && name.chars().all(|c| suitable_ident(&c))
}

fn get_ident(letters: &mut Peekable<impl Iterator<Item = char>>) -> Result<String> {
    let mut ident = String::new();

    while let Some(letter) = letters.peek() {
        if !suitable_ident(letter) {
            break;
        };
        ident.push(*letter);
        letters.next();
    }

    if ident.is_empty() {
        Err(PreprocError::EmptyIdent)
    } else {
        Ok(ident)
    }
}

fn take_till_eol(letters: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut content = String::new();

    while let Some(letter) = letters.peek() {
        if matches!(letter, '\n' | '#') {
            break;
        };
        content.push(*letter);
        letters.next();
    }

    content
}

/// A macro table that remembers first-definition order, so the macros handed
/// to `Make` (and any future `-p` dump) are deterministic.
#[derive(Debug, Default)]
pub struct MacroTable {
    values: HashMap<String, String>,
    order: Vec<String>,
}

impl MacroTable {
    /// The plain name→body map, for `substitute`.
    fn values(&self) -> &HashMap<String, String> {
        &self.values
    }

    fn set(&mut self, name: String, body: String) {
        if !self.values.contains_key(&name) {
            self.order.push(name.clone());
        }
        self.values.insert(name, body);
    }

    /// Current body of `name`, used by `?=` and `+=`.
    fn get(&self, name: &str) -> Option<&String> {
        self.values.get(name)
    }

    pub fn into_macros(self) -> Vec<Macro> {
        self.order
            .into_iter()
            .map(|name| {
                let body = self.values.get(&name).cloned().unwrap_or_default();
                (name, body)
            })
            .collect()
    }
}

/// POSIX.1-2024 macro assignment operators.
enum Operator {
    /// `=` — delayed expansion.
    Equals,
    /// `:=` — GNU-compatible immediate expansion.
    Colon,
    /// `::=` — immediate expansion.
    Colon2,
    /// `:::=` — immediate expansion, `$` left unmodified.
    Colon3,
    /// `!=` — shell command output.
    Bang,
    /// `?=` — assign only if not already defined.
    QuestionMark,
    /// `+=` — append.
    Plus,
}

/// Consume the assignment operator that follows a macro name.
fn parse_operator(text: &mut Peekable<impl Iterator<Item = char>>) -> Result<Operator> {
    let Some(symbol) = text.next() else {
        return Err(PreprocError::UnexpectedEOF);
    };
    let expect_equals = |text: &mut Peekable<_>, c: char| -> Result<()> {
        match text.next() {
            Some('=') => Ok(()),
            _ => Err(PreprocError::BadAssignmentOperator(c)),
        }
    };
    match symbol {
        '=' => Ok(Operator::Equals),
        ':' => {
            let mut count = 1;
            while let Some(':') = text.peek() {
                count += 1;
                text.next();
            }
            expect_equals(text, ':')?;
            match count {
                1 => Ok(Operator::Colon),
                2 => Ok(Operator::Colon2),
                3 => Ok(Operator::Colon3),
                _ => Err(PreprocError::TooManyColons),
            }
        }
        '!' => {
            expect_equals(text, '!')?;
            Ok(Operator::Bang)
        }
        '?' => {
            expect_equals(text, '?')?;
            Ok(Operator::QuestionMark)
        }
        '+' => {
            expect_equals(text, '+')?;
            Ok(Operator::Plus)
        }
        c => Err(PreprocError::UnexpectedSymbol(c)),
    }
}

/// Expand `body` against `table` until it stops changing.
fn expand_to_fixpoint(body: &str, table: &HashMap<String, String>) -> Result<String> {
    let mut body = body.to_string();
    loop {
        let (result, substitutions) = substitute(&body, table)?;
        if substitutions == 0 {
            return Ok(body);
        }
        body = result;
    }
}

/// POSIX 105746-105748: strip leading white space, drop a single trailing
/// <newline>, then turn every remaining <newline> into a <space>.
fn shell_output_to_macro_value(raw: &str) -> String {
    let trimmed = raw.trim_start();
    let trimmed = trimmed.strip_suffix('\n').unwrap_or(trimmed);
    trimmed.replace('\n', " ")
}

/// Run `body` through the shell and convert its stdout to a macro value (`!=`).
fn shell_assign(body: &str) -> Result<String> {
    let Ok(result) = std::process::Command::new("sh").args(["-c", body]).output() else {
        return Err(PreprocError::CommandFailed);
    };
    Ok(shell_output_to_macro_value(&String::from_utf8_lossy(
        &result.stdout,
    )))
}

/// Turn a raw definition body into the value the operator assigns.
fn apply_operator(
    operator: Operator,
    name: &str,
    body: String,
    table: &MacroTable,
) -> Result<String> {
    match operator {
        Operator::Equals => Ok(body),
        Operator::Colon | Operator::Colon2 => expand_to_fixpoint(&body, table.values()),
        Operator::Colon3 => Ok(substitute(&body, table.values())?.0),
        Operator::Bang => shell_assign(&substitute(&body, table.values())?.0),
        Operator::QuestionMark => Ok(match table.get(name) {
            Some(existing) => existing.clone(),
            None => body,
        }),
        Operator::Plus => Ok(match table.get(name) {
            Some(existing) => format!("{existing} {body}"),
            None => body,
        }),
    }
}

/// Read the macro name from a definition line, allowing an `export` prefix.
fn parse_macro_name(text: &mut Peekable<impl Iterator<Item = char>>) -> Result<String> {
    let name = get_ident(text)?;
    if name == "export" {
        skip_blank(text);
        return get_ident(text);
    }
    Ok(name)
}

/// Searches for all the lines in makefile that resemble macro definition
/// and creates a name→body table from them, in first-definition order.
fn generate_macro_table(source: &str) -> Result<MacroTable> {
    let mut macro_table = MacroTable::default();

    for def in source.lines().filter(|line| is_macro_definition(line)) {
        let mut text = def.chars().peekable();
        let macro_name = parse_macro_name(&mut text)?;
        skip_blank(&mut text);
        let operator = parse_operator(&mut text)?;
        skip_blank(&mut text);
        let body = take_till_eol(&mut text);
        let body = apply_operator(operator, &macro_name, body, &macro_table)?;
        macro_table.set(macro_name, body);
    }

    Ok(macro_table)
}

pub static ENV_MACROS: AtomicBool = AtomicBool::new(false);

fn substitute(source: &str, table: &HashMap<String, String>) -> Result<(String, u32)> {
    let env_macros = ENV_MACROS.load(Acquire);

    let mut substitutions = 0;
    let mut result = String::with_capacity(source.len());

    let mut letters = source.chars().peekable();
    while let Some(letter) = letters.next() {
        if letter != '$' {
            result.push(letter);
            continue;
        }

        let Some(letter) = letters.next() else {
            Err(PreprocError::UnexpectedEOF)?
        };

        match letter {
            // Internal macros - we leave them "as is"
            // yet as they will be dealt with in the
            // parsing stage with more context available
            c @ ('$' | '@' | '%' | '?' | '<' | '*' | '^' | '+') => {
                result.push('$');
                result.push(c);
                continue;
            }
            c if suitable_ident(&c) => {
                let env_macro = if env_macros {
                    std::env::var(c.to_string()).ok()
                } else {
                    None
                };
                let table_macro = table.get(&c.to_string()).cloned();
                let Some(macro_body) = env_macro.or(table_macro) else {
                    Err(PreprocError::UndefinedMacro(c.to_string()))?
                };
                result.push_str(&macro_body);
                substitutions += 1;
                continue;
            }
            open @ ('(' | '{') => {
                let close = if open == '(' { ')' } else { '}' };

                // An internal-macro reference such as `$(@)`, `$(@D)`, `$(?F)`
                // is left verbatim for the rule stage, which alone has the
                // target/prerequisite context to expand it.
                if matches!(
                    letters.peek(),
                    Some('@' | '%' | '?' | '<' | '*' | '^' | '+')
                ) {
                    result.push('$');
                    result.push(open);
                    for c in letters.by_ref() {
                        result.push(c);
                        if c == close {
                            break;
                        }
                    }
                    continue;
                }

                skip_blank(&mut letters);
                let Ok(macro_name) = get_ident(&mut letters) else {
                    Err(PreprocError::BadMacroName)?
                };

                let env_macro = if env_macros {
                    std::env::var(&macro_name).ok()
                } else {
                    None
                };
                let table_macro = table.get(&macro_name).cloned();

                // `$(name:subst1=subst2)` substitution form.
                if letters.peek() == Some(&':') {
                    letters.next();
                    let mut spec = String::new();
                    let mut closed = false;
                    for c in letters.by_ref() {
                        if c == close {
                            closed = true;
                            break;
                        }
                        spec.push(c);
                    }
                    // Like the plain `$(name)` path, require the closing
                    // delimiter rather than silently accepting EOF.
                    if !closed {
                        Err(PreprocError::UnexpectedEOF)?
                    }
                    let Some(macro_body) = env_macro.or(table_macro) else {
                        Err(PreprocError::UndefinedMacro(macro_name.to_string()))?
                    };
                    result.push_str(&apply_substitution(&macro_body, &spec));
                    substitutions += 1;
                    continue;
                }

                skip_blank(&mut letters);
                let Some(finilizer) = letters.next() else {
                    Err(PreprocError::UnexpectedEOF)?
                };
                if finilizer != close {
                    Err(PreprocError::UnexpectedSymbol(finilizer))?
                }

                let Some(macro_body) = env_macro.or(table_macro) else {
                    // The special `MAKE` macro, when not otherwise defined, is
                    // passed through to the rule stage (it expands to the make
                    // program and marks the recipe for recursive execution).
                    if macro_name == "MAKE" {
                        result.push('$');
                        result.push(open);
                        result.push_str("MAKE");
                        result.push(close);
                        continue;
                    }
                    Err(PreprocError::UndefinedMacro(macro_name.to_string()))?
                };
                result.push_str(&macro_body);
                substitutions += 1;

                continue;
            }
            c => Err(PreprocError::UnexpectedSymbol(c))?,
        }
    }

    Ok((result, substitutions))
}

/// Recognize an include directive: the word `include`, optionally prefixed with
/// a `-` (whose missing file is ignored), at the start of a line and followed by
/// one or more blanks. Returns `(path_spec, ignore_missing)`. Requiring the
/// trailing blank avoids matching `includedir=...` and similar.
fn parse_include_directive(line: &str) -> Option<(&str, bool)> {
    let (rest, ignore_missing) = if let Some(rest) = line.strip_prefix("-include") {
        (rest, true)
    } else {
        let rest = line.strip_prefix("include")?;
        (rest, false)
    };
    if rest.starts_with([' ', '\t']) {
        Some((rest.trim(), ignore_missing))
    } else {
        None
    }
}

/// Copy-pastes included makefiles into single one recursively.
/// Pretty much the same as C preprocessor and `#include` directive
fn process_include_lines(source: &str, table: &HashMap<String, String>) -> Result<(String, usize)> {
    let mut counter = 0;
    let mut result = String::new();
    for line in source.lines() {
        let expanded = if let Some((path_spec, ignore_missing)) = parse_include_directive(line) {
            counter += 1;
            // Propagate a substitution error (e.g. an undefined macro in the
            // path) rather than defaulting to an empty, misleading path.
            let (path, _) = substitute(path_spec, table)?;
            match fs::read_to_string(Path::new(&path)) {
                Ok(contents) => contents,
                // `-include` silently ignores a missing/unreadable file;
                // plain `include` is a hard error.
                Err(_) if ignore_missing => String::new(),
                Err(err) => {
                    return Err(PreprocError::IncludeFailed {
                        path,
                        reason: err.to_string(),
                    })
                }
            }
        } else {
            line.to_string()
        };
        result.push_str(&expanded);
        result.push('\n');
    }
    Ok((result, counter))
}

/// Blank out macro-definition lines. `generate_macro_table` has already taken
/// their content, and the rule parser must not see them. Blanking rather than
/// deleting keeps every later line at its original number.
fn blank_macro_lines(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    for line in source.lines() {
        if !is_macro_definition(line) {
            out.push_str(line);
        }
        out.push('\n');
    }
    out
}

/// Splice `include` lines until none remain, returning the macro table of the
/// fully-spliced text.
fn expand_includes(source: &mut String) -> Result<MacroTable> {
    let mut table = generate_macro_table(source)?;
    loop {
        // The real table, not an empty one: a path spec may reference macros,
        // e.g. `include $(TOP)/config.mk`.
        let (spliced, found) = process_include_lines(source, table.values())?;
        *source = spliced;
        table = generate_macro_table(source)?;
        if found == 0 {
            return Ok(table);
        }
    }
}

/// Expand every macro reference in `source` until it stops changing.
fn substitute_to_fixpoint(source: &str, table: &HashMap<String, String>) -> Result<String> {
    let mut source = source.to_string();
    loop {
        let (result, substitutions) = substitute(&source, table)?;
        if substitutions == 0 {
            return Ok(result);
        }
        source = result;
    }
}

/// Resolve `include`s and macros, returning the text the rule parser sees and
/// the macro definitions it must not (they are consumed here, but `Make` needs
/// them for `SHELL` and for the recipe environment).
pub fn preprocess(source: &str) -> Result<(String, Vec<Macro>)> {
    let mut source = fold_continuations(source);
    let table = expand_includes(&mut source)?;
    let source = blank_macro_lines(&source);
    let text = substitute_to_fixpoint(&source, table.values())?;
    Ok((text, table.into_macros()))
}
