//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::directive::{parse_directive, split_condition_args, Directive};
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
    IncludeFailed {
        path: String,
        reason: String,
    },
    /// A conditional whose arguments could not be split.
    BadConditional(String),
    /// `else`/`endif` without a matching `if`, or an unterminated `if`.
    UnmatchedConditional(String),
    /// A macro whose expansion re-introduces its own reference.
    RecursiveMacro,
}

impl Display for PreprocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            // A missing/unreadable include is a user-facing condition, so give
            // it a readable message rather than the debug representation.
            PreprocError::IncludeFailed { path, reason } => {
                writeln!(f, "cannot open include file '{path}': {reason}")
            }
            PreprocError::BadConditional(args) => {
                writeln!(f, "malformed conditional: {args}")
            }
            PreprocError::UnmatchedConditional(word) => {
                writeln!(f, "unmatched '{word}'")
            }
            PreprocError::RecursiveMacro => {
                writeln!(
                    f,
                    "macro expansion does not terminate (recursive definition?)"
                )
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
/// How many expansion rounds before we call a macro self-referential. A value
/// that re-introduces its own reference (`A = $(A)x`) never converges.
const MAX_EXPANSION_ROUNDS: usize = 256;

fn expand_to_fixpoint(body: &str, table: &HashMap<String, String>) -> Result<String> {
    let mut body = body.to_string();
    for _ in 0..MAX_EXPANSION_ROUNDS {
        let (result, substitutions) = substitute(&body, table)?;
        if substitutions == 0 {
            return Ok(body);
        }
        body = result;
    }
    Err(PreprocError::RecursiveMacro)
}

/// Expand every macro reference in `source` until it stops changing.
fn substitute_to_fixpoint(source: &str, table: &HashMap<String, String>) -> Result<String> {
    let mut source = source.to_string();
    for _ in 0..MAX_EXPANSION_ROUNDS {
        let (result, substitutions) = substitute(&source, table)?;
        if substitutions == 0 {
            return Ok(result);
        }
        source = result;
    }
    Err(PreprocError::RecursiveMacro)
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

/// Parse one macro-definition line into the name and the value its operator
/// assigns, resolving against what is already defined.
fn parse_macro_definition(line: &str, table: &MacroTable) -> Result<(String, String)> {
    let mut text = line.chars().peekable();
    // A definition may be indented with spaces, which is common inside a
    // conditional. (A <tab> would make it a command line, handled elsewhere.)
    skip_blank(&mut text);
    let name = parse_macro_name(&mut text)?;
    skip_blank(&mut text);
    let operator = parse_operator(&mut text)?;
    skip_blank(&mut text);
    let body = take_till_eol(&mut text);
    let body = apply_operator(operator, &name, body, table)?;
    Ok((name, body))
}

pub static ENV_MACROS: AtomicBool = AtomicBool::new(false);

/// Resolve a macro reference to its value.
///
/// POSIX 105845 makes the environment macro source 3, unconditionally -- `-e`
/// changes which source *wins*, not whether the environment is consulted at
/// all. POSIX 105833: "if the macro named by string1 does not exist, the final
/// result shall be an empty string", so an undefined name is not an error.
fn lookup_macro(name: &str, table: &HashMap<String, String>, env_wins: bool) -> String {
    let from_env = std::env::var(name).ok();
    let from_table = table.get(name).cloned();
    let resolved = if env_wins {
        from_env.or(from_table)
    } else {
        from_table.or(from_env)
    };
    resolved.unwrap_or_default()
}

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
                let macro_body = lookup_macro(&c.to_string(), table, env_macros);
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
                    let macro_body = lookup_macro(&macro_name, table, env_macros);
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

                // The special `MAKE` macro, when not otherwise defined, is
                // passed through to the rule stage (it expands to the make
                // program and marks the recipe for recursive execution).
                let defined = table.contains_key(&macro_name) || std::env::var(&macro_name).is_ok();
                if !defined && macro_name == "MAKE" {
                    result.push('$');
                    result.push(open);
                    result.push_str("MAKE");
                    result.push(close);
                    continue;
                }
                let macro_body = lookup_macro(&macro_name, table, env_macros);
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

/// True if `line` is an `include` / `-include` directive.
fn is_include_directive(line: &str) -> bool {
    parse_include_directive(line).is_some()
}

/// True if `line` is an `include` / `-include` directive. The reader has
/// already spliced these, but `parse` may be handed text directly.
pub(crate) fn is_include_line(line: &str) -> bool {
    is_include_directive(line)
}

/// One level of `if.../else/endif` nesting.
struct Branch {
    /// Whether this level's currently selected arm is live.
    active: bool,
    /// Whether any arm at this level has already been taken, so a later
    /// `else` must not fire.
    taken: bool,
    /// Whether a bare `else` has been seen, so a second one is an error.
    closed: bool,
}

/// How deep `include` may nest before we call it a cycle. POSIX 105611 requires
/// at least 16; the cap exists so a self-including file terminates.
const MAX_INCLUDE_DEPTH: usize = 64;

/// Walks makefile text once, evaluating directives in order.
///
/// Macro definitions, conditionals and includes are interleaved because they
/// depend on each other: a conditional's outcome can decide which macros exist,
/// and a macro can name the file an `include` reads. Every consumed line is
/// replaced by an empty one so later lines keep their original numbers.
struct Reader {
    table: MacroTable,
    branches: Vec<Branch>,
    out: String,
    depth: usize,
}

impl Reader {
    fn new() -> Self {
        Reader {
            table: MacroTable::default(),
            branches: Vec::new(),
            out: String::new(),
            depth: 0,
        }
    }

    /// Whether lines are currently being kept.
    fn active(&self) -> bool {
        self.branches.iter().all(|b| b.active)
    }

    /// Emit a line.
    ///
    /// A rule header is expanded here, against what is defined at that point.
    /// A command line is left raw and expanded once the whole file has been
    /// read, because a macro may be defined after the recipe that uses it --
    /// notably a command-line `macro=value` operand, which `main` appends to
    /// the end of the makefile text.
    fn keep(&mut self, line: &str) -> Result<()> {
        if line.starts_with('\t') {
            self.out.push_str(line);
        } else {
            let expanded = substitute_to_fixpoint(line, self.table.values())?;
            self.out.push_str(&expanded);
        }
        self.out.push('\n');
        Ok(())
    }

    /// Consume a line without emitting it, preserving line numbering.
    fn drop_line(&mut self) {
        self.out.push('\n');
    }

    /// Expand macro references in a directive's condition, using what is
    /// defined so far.
    fn expand(&self, text: &str) -> Result<String> {
        expand_to_fixpoint(text, self.table.values())
    }

    fn eval_compare(&self, equal: bool, args: &str) -> Result<bool> {
        let expanded = self.expand(args)?;
        let Some((lhs, rhs)) = split_condition_args(&expanded) else {
            return Err(PreprocError::BadConditional(args.to_string()));
        };
        Ok((lhs == rhs) == equal)
    }

    fn eval_defined(&self, defined: bool, name: &str) -> Result<bool> {
        let name = self.expand(name)?;
        let name = name.trim();
        // An empty value counts as undefined, matching GNU.
        let has = self
            .table
            .get(name)
            .map(|v| !v.is_empty())
            .unwrap_or_else(|| std::env::var(name).map(|v| !v.is_empty()).unwrap_or(false));
        Ok(has == defined)
    }

    fn eval_condition(&self, directive: &Directive) -> Result<bool> {
        match directive {
            Directive::IfCompare { equal, args } => self.eval_compare(*equal, args),
            Directive::IfDefined { defined, name } => self.eval_defined(*defined, name),
            other => Err(PreprocError::BadConditional(format!("{other:?}"))),
        }
    }

    /// Open a new conditional level. A condition inside an inactive branch is
    /// not evaluated -- it may reference macros that only the live arm defines.
    fn push_branch(&mut self, directive: &Directive) -> Result<()> {
        let live = self.active();
        let taken = if live {
            self.eval_condition(directive)?
        } else {
            false
        };
        self.branches.push(Branch {
            active: taken,
            taken,
            closed: false,
        });
        Ok(())
    }

    fn handle_else(&mut self, chained: Option<&Directive>) -> Result<()> {
        let Some(branch) = self.branches.pop() else {
            return Err(PreprocError::UnmatchedConditional("else".to_string()));
        };
        if branch.closed {
            return Err(PreprocError::UnmatchedConditional("else".to_string()));
        }
        let outer_live = self.active();
        let (active, closed) = match chained {
            // `else ifeq (...)`: eligible only if no arm has been taken yet.
            Some(cond) => {
                let eligible = outer_live && !branch.taken;
                let hit = eligible && {
                    self.branches.push(Branch {
                        active: true,
                        taken: branch.taken,
                        closed: false,
                    });
                    let r = self.eval_condition(cond);
                    self.branches.pop();
                    r?
                };
                (hit, false)
            }
            None => (outer_live && !branch.taken, true),
        };
        self.branches.push(Branch {
            active,
            taken: branch.taken || active,
            closed,
        });
        Ok(())
    }

    fn handle_endif(&mut self) -> Result<()> {
        self.branches
            .pop()
            .map(|_| ())
            .ok_or_else(|| PreprocError::UnmatchedConditional("endif".to_string()))
    }

    /// Capture a `define ... endef` body, returning the lines consumed.
    fn capture_define(&mut self, name: &str, lines: &[&str], start: usize) -> usize {
        let mut body: Vec<&str> = Vec::new();
        let mut i = start + 1;
        while i < lines.len() {
            if matches!(parse_directive(lines[i]), Some(Directive::EndDef)) {
                break;
            }
            body.push(lines[i]);
            i += 1;
        }
        if self.active() {
            self.table.set(name.to_string(), body.join("\n"));
        }
        // Blank out the whole construct, header and terminator included.
        for _ in start..=i.min(lines.len() - 1) {
            self.drop_line();
        }
        i
    }

    fn splice_include(&mut self, line: &str) -> Result<()> {
        let Some((path_spec, ignore_missing)) = parse_include_directive(line) else {
            return Ok(());
        };
        let path = self.expand(path_spec)?;
        let path = path.trim();
        self.drop_line();

        if self.depth >= MAX_INCLUDE_DEPTH {
            return Err(PreprocError::IncludeFailed {
                path: path.to_string(),
                reason: format!("include nested more than {MAX_INCLUDE_DEPTH} deep (cycle?)"),
            });
        }

        let contents = match fs::read_to_string(Path::new(path)) {
            Ok(contents) => contents,
            Err(_) if ignore_missing => return Ok(()),
            Err(err) => {
                return Err(PreprocError::IncludeFailed {
                    path: path.to_string(),
                    reason: err.to_string(),
                })
            }
        };

        self.depth += 1;
        let folded = fold_continuations(&contents);
        self.read(&folded)?;
        self.depth -= 1;
        Ok(())
    }

    /// Handle one line; returns the index of the last line it consumed.
    fn handle(&mut self, lines: &[&str], i: usize) -> Result<usize> {
        let line = lines[i];

        if let Some(directive) = parse_directive(line) {
            match directive {
                Directive::Define(name) => return Ok(self.capture_define(&name, lines, i)),
                Directive::EndDef => self.drop_line(),
                Directive::Else(chained) => {
                    self.handle_else(chained.as_deref())?;
                    self.drop_line();
                }
                Directive::EndIf => {
                    self.handle_endif()?;
                    self.drop_line();
                }
                other => {
                    self.push_branch(&other)?;
                    self.drop_line();
                }
            }
            return Ok(i);
        }

        if !self.active() {
            self.drop_line();
            return Ok(i);
        }

        if is_include_directive(line) {
            self.splice_include(line)?;
        } else if is_macro_definition(line) {
            self.define_macro(line)?;
            self.drop_line();
        } else {
            self.keep(line)?;
        }
        Ok(i)
    }

    fn define_macro(&mut self, line: &str) -> Result<()> {
        let (name, body) = parse_macro_definition(line, &self.table)?;
        self.table.set(name, body);
        Ok(())
    }

    fn read(&mut self, text: &str) -> Result<()> {
        let mut lines: Vec<&str> = text.split('\n').collect();
        // Text ending in a newline splits with a trailing empty element; it is
        // the terminator of the last line, not a line of its own.
        if lines.last() == Some(&"") {
            lines.pop();
        }
        let mut i = 0;
        while i < lines.len() {
            i = self.handle(&lines, i)? + 1;
        }
        Ok(())
    }
}

/// Expand the command lines held back during the read, now that every macro is
/// known.
///
/// A multi-line value landing in a recipe gets each of its newlines followed by
/// the recipe's <tab>, so every line stays a command line rather than becoming
/// a bogus rule.
fn expand_command_lines(text: &str, table: &HashMap<String, String>) -> Result<String> {
    let mut out = String::with_capacity(text.len());
    for line in text.lines() {
        if line.starts_with('\t') {
            let expanded = substitute_to_fixpoint(line, table)?;
            out.push_str(&expanded.replace('\n', "\n\t"));
        } else {
            out.push_str(line);
        }
        out.push('\n');
    }
    Ok(out)
}

/// Resolve directives, includes and macros, returning the text the rule parser
/// sees and the macro definitions it must not (they are consumed here, but
/// `Make` needs them for `SHELL` and for the recipe environment).
pub fn preprocess(source: &str) -> Result<(String, Vec<Macro>)> {
    let mut reader = Reader::new();
    reader.read(&fold_continuations(source))?;
    if !reader.branches.is_empty() {
        return Err(PreprocError::UnmatchedConditional("endif".to_string()));
    }
    let text = expand_command_lines(&reader.out, reader.table.values())?;
    Ok((text, reader.table.into_macros()))
}
