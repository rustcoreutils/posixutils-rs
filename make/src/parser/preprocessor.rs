//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::directive::{parse_directive, split_condition_args, Directive};
use super::func;
use crate::Macro;
use std::borrow::Cow;
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
    /// A function reported an error, or does not exist.
    FunctionFailed(String),
    /// `$(eval ...)` re-entered the reader too many times.
    EvalTooDeep,
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
            // `write!`, not `writeln!`: a function error crosses the
            // `Result<String, String>` boundary between `func` and here once
            // per nesting level, and each `to_string()` would otherwise append
            // another newline -- 200 of them for a deep recursion.
            PreprocError::FunctionFailed(msg) => write!(f, "{msg}"),
            PreprocError::EvalTooDeep => {
                writeln!(f, "$(eval ...) nested too deeply (recursive definition?)")
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

/// Stands for the newline of an escaped newline in a command line, from the
/// time the line is folded until the rule stage puts the newline back (see
/// [`restore_escaped_newlines`]). A private-use character, so it cannot be
/// confused with anything a makefile means.
pub(crate) const ESCAPED_NEWLINE: char = '\u{E000}';

/// Put back the newlines [`ESCAPED_NEWLINE`] stands for.
pub(crate) fn restore_escaped_newlines(text: &str) -> String {
    text.replace(ESCAPED_NEWLINE, "\n")
}

/// Inside a macro expansion in a command line, an escaped newline and the
/// blanks after it are a single space, as anywhere else in the makefile
/// (POSIX 105556-105559: the newline is kept only "after macro expansion").
pub(crate) fn join_escaped_newlines(raw: &str) -> Cow<'_, str> {
    if !raw.contains(ESCAPED_NEWLINE) {
        return Cow::Borrowed(raw);
    }
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' && chars.peek() == Some(&ESCAPED_NEWLINE) {
            chars.next();
            while matches!(chars.peek(), Some(' ' | '\t')) {
                chars.next();
            }
            while out.ends_with([' ', '\t']) {
                out.pop();
            }
            out.push(' ');
        } else {
            out.push(c);
        }
    }
    Cow::Owned(out)
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
            // POSIX 105559-105562: the command executed keeps the backslash,
            // the newline and the next line, less a leading <tab>. The
            // newline is held as a marker until the rule stage, because
            // everything up to there works a line at a time.
            current.push('\\');
            current.push(ESCAPED_NEWLINE);
            current.push_str(next.strip_prefix('\t').unwrap_or(next));
        } else {
            // The continuation, the newline, and the next line's leading white
            // space collapse to a *single* space -- so any blank already
            // sitting before the backslash goes too (audit #48).
            while current.ends_with([' ', '\t']) {
                current.pop();
            }
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
pub(crate) fn apply_substitution(value: &str, spec: &str) -> String {
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
            } else if let Some(base) = word.strip_suffix(lhs) {
                // Suffix form. An empty `subst1` is a suffix of every word,
                // so `$(TESTS:=.log)` appends `.log` to each (POSIX
                // 105806-105807). automake's test harness writes exactly that,
                // and then runs `rm -f` on the result: returning the words
                // unchanged deleted gzip's test scripts.
                format!("{base}{rhs}")
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

/// A character of a macro name. POSIX 105772: portable applications select
/// macro names from the portable filename character set, which has `-` as
/// well as letters, digits, `_` and `.`; binutils' top-level Makefile has
/// `PGO-TRAINING-TARGETS = ...`.
fn suitable_ident(c: &char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '.' | '-')
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
    if let Some(rest) = name.strip_prefix("override") {
        if rest.starts_with(char::is_whitespace) {
            name = rest.trim();
        }
    }
    if let Some(rest) = name.strip_prefix("export") {
        // Require whitespace after `export` so `exported=1` is not mis-split.
        if rest.starts_with(char::is_whitespace) {
            name = rest.trim();
        }
    }
    !name.is_empty() && name.chars().all(|c| suitable_ident(&c))
}

/// The byte offsets of the bytes of `line` that are outside every `$(...)`
/// and `${...}` reference, with the bytes.
///
/// An iterator over bytes rather than a vector of characters: every line of
/// every makefile is scanned this way, most only as far as their first `:`.
/// Every byte it looks for is ASCII, and in UTF-8 no byte of a multibyte
/// character is, so an offset found is always a character boundary.
fn top_level_chars(line: &str) -> impl Iterator<Item = (usize, u8)> + '_ {
    let mut depth = 0usize;
    let mut bytes = line.bytes().enumerate();
    std::iter::from_fn(move || {
        while let Some((i, c)) = bytes.next() {
            if depth > 0 {
                match c {
                    b'(' | b'{' => depth += 1,
                    b')' | b'}' => depth -= 1,
                    _ => {}
                }
                continue;
            }
            if c != b'$' {
                return Some((i, c));
            }
            // A reference opens, or `$$` / `$x` is one token either way.
            if let Some((_, next)) = bytes.next() {
                if matches!(next, b'(' | b'{') {
                    depth = 1;
                }
            }
        }
        None
    })
}

/// Split a target-specific assignment, `targets: [override] [export] name op
/// value`, into its targets and its assignment (GNU make).
///
/// It is a rule line whose prerequisite side holds an assignment operator
/// before any `;`, outside every macro reference: `foo: CFLAGS += -g` is
/// one, `all: $(SRC:.c=.o)` and `x: ; echo a=b` are not. A macro definition
/// whose value has a `:` in it never gets here; `is_macro_definition`
/// claims it first.
fn split_target_var(line: &str) -> Option<(&str, &str)> {
    if line.starts_with('\t') {
        return None;
    }
    let colon = top_level_chars(line).find(|&(_, c)| c == b':')?.0;
    let (targets, rest) = (&line[..colon], &line[colon + 1..]);
    // `targets:: ...` is a double-colon rule, never an assignment.
    if rest.starts_with([':', '=']) {
        return None;
    }
    for (_, c) in top_level_chars(rest) {
        match c {
            b';' => return None,
            b'=' => return Some((targets, rest)),
            _ => {}
        }
    }
    None
}

/// Split a rule line before the `;` that begins its inline command (POSIX
/// 105644), if it has one: the first `;` after the first `:`, outside every
/// macro reference. The second part keeps the `;`.
fn split_inline_command(line: &str) -> (&str, &str) {
    let mut chars = top_level_chars(line);
    if !chars.any(|(_, c)| c == b':') {
        return (line, "");
    }
    match chars.find(|&(_, c)| c == b';') {
        Some((at, _)) => line.split_at(at),
        None => (line, ""),
    }
}

/// `text` without a leading `word` and the blanks after it, if it starts
/// with that word.
fn strip_word<'a>(text: &'a str, word: &str) -> Option<&'a str> {
    let rest = text.strip_prefix(word)?;
    rest.starts_with([' ', '\t'])
        .then(|| rest.trim_start_matches([' ', '\t']))
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

/// The text between the delimiters of one `$(...)` or `${...}` reference
/// that is not an internal macro: a function call, a macro name, or a
/// substitution `name:subst1=subst2`.
struct Reference<'a> {
    raw: &'a str,
    open: char,
    close: char,
}

impl Reference<'_> {
    /// The function this reference calls, and its argument text, if it is a
    /// call. A function name is followed by at least one blank; `$(shell)`
    /// alone is a macro named `shell`, as in GNU make.
    fn function(&self) -> Option<(&str, &str)> {
        let text = self.raw.trim_start_matches([' ', '\t']);
        let end = text.find([' ', '\t'])?;
        let (name, args) = text.split_at(end);
        func::is_function(name).then(|| (name, args.trim_start_matches([' ', '\t'])))
    }

    /// Split `name:subst1=subst2` at its top-level `:`. A `:` with no `=`
    /// after it is part of the name, as is one inside a nested reference.
    fn substitution(&self) -> (&str, Option<&str>) {
        let mut depth = 0usize;
        for (i, c) in self.raw.char_indices() {
            match c {
                '(' | '{' => depth += 1,
                ')' | '}' => depth = depth.saturating_sub(1),
                ':' if depth == 0 && self.raw[i + 1..].contains('=') => {
                    return (&self.raw[..i], Some(&self.raw[i + 1..]));
                }
                _ => {}
            }
        }
        (self.raw, None)
    }

    /// Expand the reference onto `result`, returning how many substitutions
    /// it made.
    ///
    /// POSIX 105801-105803: "If string1 in a macro expansion contains a macro
    /// expansion, that inner macro expansion shall be performed first and the
    /// result substituted into string1 to produce the macro name". That
    /// covers a name computed in part, as automake writes it --
    /// `$(am__v_CC_$(V))` -- as well as wholly, `$($(X))`, and at any depth.
    /// Only the inner references were taken before, so a name with a literal
    /// prefix was rejected outright.
    fn expand(
        &self,
        table: &MacroTable,
        state: &func::Expansion,
        env_macros: bool,
        result: &mut String,
    ) -> Result<u32> {
        if let Some((name, args)) = self.function() {
            return self.call(name, args, table, state, env_macros, result);
        }

        let (name_part, spec) = self.substitution();
        // A name computed from an automatic variable, `$($@)` or
        // `$(TEST_$@)`, is left for the rule stage, which knows the target,
        // as a function call reading one is. dpkg's tests of its own .mk
        // files check every variable with `test '$($@)' = '$(TEST_$@)'`.
        if mentions_automatic(name_part) {
            result.push('$');
            result.push(self.open);
            result.push_str(self.raw);
            result.push(self.close);
            return Ok(0);
        }
        let name = match name_part.contains('$') {
            true => expand_to_fixpoint(name_part, table, state)?,
            false => name_part.to_string(),
        };
        // POSIX 105833 makes a reference to a macro that does not exist the
        // empty string, and a name that expands to nothing names none.
        let name = name.trim();
        if name.is_empty() {
            return Ok(1);
        }

        let Some(spec) = spec else {
            // The special `MAKE` macro, when not otherwise defined, is
            // passed through to the rule stage (it expands to the make
            // program and marks the recipe for recursive execution).
            if name == "MAKE" && table.resolve(name, env_macros).is_none() {
                result.push('$');
                result.push(self.open);
                result.push_str(name);
                result.push(self.close);
                return Ok(0);
            }
            result.push_str(&lookup_macro(name, table, env_macros));
            return Ok(1);
        };

        // `$(name:subst1=subst2)`. Expand the body before substituting: a
        // delayed macro holds unexpanded text, and `$(SRC:.c=.o)` has to see
        // the words, not `$(wildcard *.c)`. The replacement may itself contain
        // a reference, as in `$(SRC:%.c=$(D)/%.o)` (audit #70); it is
        // expanded by the next round.
        let body = lookup_macro(name, table, env_macros);
        let body = expand_to_fixpoint(&body, table, state)?;
        result.push_str(&apply_substitution(&body, spec));
        Ok(1)
    }

    /// `$(fn args...)` -- a function call, not a macro reference.
    fn call(
        &self,
        name: &str,
        args: &str,
        table: &MacroTable,
        state: &func::Expansion,
        env_macros: bool,
        result: &mut String,
    ) -> Result<u32> {
        // Defer the call if it reads an automatic variable; only the rule
        // stage can supply one (audit #62).
        if mentions_automatic(args) {
            result.push('$');
            result.push(self.open);
            result.push_str(name);
            result.push(' ');
            result.push_str(args);
            result.push(self.close);
            return Ok(0);
        }
        let ctx = func::Ctx {
            table,
            env_wins: env_macros,
            state,
        };
        let out = func::call(name, args, &ctx, &expand_for_function)
            .map_err(PreprocError::FunctionFailed)?;
        result.push_str(&out);
        Ok(1)
    }
}

/// Consume a reference's text up to the delimiter that closes it, honouring
/// nested `$(...)`/`${...}`: the text before it, with `text` moved past it.
///
/// Both delimiters are ASCII, which no byte of a multibyte character is.
fn read_balanced<'a>(text: &mut &'a str, open: char, close: char) -> Result<&'a str> {
    let (open, close) = (open as u8, close as u8);
    let mut depth = 0usize;
    for (i, &b) in text.as_bytes().iter().enumerate() {
        if b == open {
            depth += 1;
        } else if b == close {
            if depth == 0 {
                let raw = &text[..i];
                *text = &text[i + 1..];
                return Ok(raw);
            }
            depth -= 1;
        }
    }
    Err(PreprocError::UnexpectedEOF)
}

/// The automatic variables, which only the rule stage can resolve.
const AUTOMATIC: [char; 7] = ['@', '%', '?', '<', '*', '^', '+'];

/// True if `text` mentions an automatic variable.
///
/// A function whose argument does cannot be evaluated while reading: `$^` is
/// still literal here, so `$(notdir $^)` would apply `notdir` to the two
/// characters `$^` and return them unchanged (audit #62). Such a call is left
/// verbatim for the rule stage, which knows the target.
fn mentions_automatic(text: &str) -> bool {
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '$' {
            continue;
        }
        match chars.peek() {
            // `$$` is a literal dollar, so `$$?` hands the shell `$?`.
            Some('$') => {
                chars.next();
            }
            Some(&next) if AUTOMATIC.contains(&next) => return true,
            // `$(@D)`, `${?F}` and friends.
            Some(&('(' | '{')) => {
                let mut rest = chars.clone();
                rest.next();
                if matches!(rest.peek(), Some(n) if AUTOMATIC.contains(n)) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// Expand text on a function's behalf.
fn expand_for_function(text: &str, ctx: &func::Ctx) -> std::result::Result<String, String> {
    expand_to_fixpoint(text, ctx.table, ctx.state).map_err(|e| e.to_string())
}

fn take_till_eol(letters: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut content = String::new();

    while let Some(letter) = letters.peek() {
        if *letter == '\n' {
            break;
        };
        content.push(*letter);
        letters.next();
    }

    content
}

/// Where a macro definition came from.
///
/// POSIX 105837-105847 numbers the sources -- 1 the command line, 2 MAKEFLAGS,
/// 3 the environment, 4 the built-in rules -- and 105848 forbids a definition
/// from overriding one from a lower-numbered (stronger) source. Recording the
/// source is what lets one comparison answer every precedence question;
/// without it the ordering is emergent, and every site that reaches for a
/// macro has to re-derive it and can disagree.
///
/// Macros from MAKEFLAGS (source 2) arrive as command-line macros: they are
/// merged into the argument list before parsing, and sources 1 and 2 differ
/// only in their order relative to each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroSource {
    /// Source 4: defined by the inference rules built into make.
    BuiltIn,
    /// Source 3: the environment make was started with.
    Environment,
    /// Defined in the makefile. POSIX 105852-105855 puts this above source 4
    /// always, above source 3 unless `-e`, and never above source 1 or 2.
    Makefile,
    /// Source 1 (and 2): a command-line macro operand.
    CommandLine,
    /// Not a POSIX source: a makefile's `override` assignment, whose whole
    /// purpose is to beat a command-line macro. POSIX has no `override`, so
    /// it has no source number; it sits above source 1 because that is what
    /// the directive means.
    Override,
    /// Not a POSIX source: a `foreach` loop variable or a `call` argument,
    /// in force only while that function's body is expanded. It shadows every
    /// other source, which is what makes the binding a binding.
    Local,
}

impl MacroSource {
    /// Precedence, where higher wins.
    ///
    /// `-e` swaps the makefile and the environment, and *only* those two: a
    /// command-line macro still outranks both (105855) and the built-ins still
    /// rank below both.
    fn rank(self, env_wins: bool) -> u8 {
        match self {
            MacroSource::BuiltIn => 0,
            MacroSource::Environment => {
                if env_wins {
                    2
                } else {
                    1
                }
            }
            MacroSource::Makefile => {
                if env_wins {
                    1
                } else {
                    2
                }
            }
            MacroSource::CommandLine => 3,
            MacroSource::Override => 4,
            MacroSource::Local => 5,
        }
    }
}

#[derive(Debug, Clone)]
struct MacroEntry {
    value: String,
    source: MacroSource,
}

/// A macro table that remembers first-definition order, so the macros handed
/// to `Make` (and the `-p` dump) are deterministic, and the source of each
/// definition, so precedence is a comparison rather than an emergent property.
#[derive(Debug, Default, Clone)]
pub struct MacroTable {
    values: HashMap<String, MacroEntry>,
    order: Vec<String>,
}

impl MacroTable {
    /// A table pre-loaded with `macros`, all from `source`.
    pub fn from_macros(macros: &[Macro], source: MacroSource) -> Self {
        let mut table = Self::default();
        for (name, body) in macros {
            table.force(name.clone(), body.clone(), source);
        }
        table
    }

    /// Assign, unless a stronger source already defined `name`.
    ///
    /// Equal ranks assign, which is POSIX 105851: "Macro definitions from a
    /// single source ... shall override previous macro definitions from the
    /// same source."
    fn set(&mut self, name: String, body: String, source: MacroSource) {
        let env_wins = ENV_MACROS.load(Acquire);
        if let Some(existing) = self.values.get(&name) {
            if source.rank(env_wins) < existing.source.rank(env_wins) {
                return;
            }
        }
        self.force(name, body, source);
    }

    /// Assign regardless of precedence, recording `source` as given.
    ///
    /// Used to establish a seed, whose rank is then honoured by every later
    /// assignment, and by `override`, which passes [`MacroSource::Override`]
    /// so that what it establishes outranks a command-line macro.
    fn force(&mut self, name: String, body: String, source: MacroSource) {
        if !self.values.contains_key(&name) {
            self.order.push(name.clone());
        }
        self.values.insert(
            name,
            MacroEntry {
                value: body,
                source,
            },
        );
    }

    /// Seed a definition from `source` before the makefile is read.
    pub fn seed(&mut self, name: String, body: String, source: MacroSource) {
        self.force(name, body, source);
    }

    /// Bind `name` for the duration of a `foreach` or `call` expansion,
    /// shadowing any definition whatever its source.
    pub(crate) fn bind_local(&mut self, name: String, value: String) {
        self.force(name, value, MacroSource::Local);
    }

    /// What `name` holds, weighing the table against the environment.
    ///
    /// The environment is macro source 3 unconditionally -- `-e` changes which
    /// source *wins*, not whether the environment is consulted -- so it is
    /// compared here by rank rather than by a blanket "environment first"
    /// switch. That distinction is the point: the environment must beat a
    /// built-in even without `-e`, and must lose to a command-line macro even
    /// with it.
    ///
    /// `None` means undefined, which is what `?=` and `ifdef` test; an
    /// explicitly empty value is defined.
    pub(crate) fn resolve(&self, name: &str, env_wins: bool) -> Option<String> {
        let entry = self.values.get(name);
        // The environment is not read for a definition it cannot outrank,
        // which is nearly every one; `getenv` walks the whole environment.
        let env_rank = MacroSource::Environment.rank(env_wins);
        if let Some(entry) = entry.filter(|entry| entry.source.rank(env_wins) >= env_rank) {
            return Some(entry.value.clone());
        }
        let from_env = std::env::var(name).ok();
        match entry {
            None => from_env,
            Some(entry) => from_env.or_else(|| Some(entry.value.clone())),
        }
    }

    /// Replace every entry the environment outranks with the environment's
    /// value, so what the table holds is the value that wins.
    ///
    /// [`Self::resolve`] weighs the environment live, but the environment is
    /// never *stored*, so until this runs the table still has one precedence
    /// question open and [`Self::into_macros`] would hand on the loser. That is
    /// not a question `Make` can answer later: it receives values, not sources.
    ///
    /// Concretely, it is what made everything downstream of the parse disagree
    /// with the makefile itself. With `CC=envcc` in the environment, `$(CC)` in
    /// a makefile expanded to `envcc` -- `resolve` saw the environment outrank
    /// the built-in -- while the built-in `.c.o` rule ran `c17` and `make -p`
    /// reported `CC = c17`, both reading the stored value.
    pub fn settle_environment(&mut self) {
        let env_wins = ENV_MACROS.load(Acquire);
        let env_rank = MacroSource::Environment.rank(env_wins);
        for name in &self.order {
            let Some(entry) = self.values.get_mut(name) else {
                continue;
            };
            if env_rank <= entry.source.rank(env_wins) {
                continue;
            }
            if let Ok(value) = std::env::var(name) {
                entry.value = value;
                entry.source = MacroSource::Environment;
            }
        }
    }

    /// Whether `name`'s winning definition is one a target-specific
    /// assignment not written `override` must yield to: a command-line
    /// macro, or under `-e` the environment.
    ///
    /// A makefile `override` is not one of them: in GNU make
    /// `sefcontext_compile: LDLIBS += ../src/libselinux.a` adds to
    /// libselinux's `override LDLIBS += -lselinux`.
    fn pins(&self, name: &str, env_wins: bool) -> bool {
        let in_env = env_wins && std::env::var_os(name).is_some();
        match self.values.get(name) {
            Some(entry) => match entry.source {
                MacroSource::CommandLine => true,
                MacroSource::Override | MacroSource::Local => false,
                MacroSource::Environment => env_wins,
                MacroSource::Makefile | MacroSource::BuiltIn => in_env,
            },
            None => in_env,
        }
    }

    /// Apply a target-specific assignment, for the target being made, to
    /// this copy of `global`, the makefile's own table.
    ///
    /// It replaces whatever the makefile, the environment (without `-e`), a
    /// makefile `override`, or a target it is made for gave the name. Unless
    /// it is written `override`, a command-line macro -- or the environment
    /// under `-e` -- replaces it in turn, `+=` and all, as in GNU make, where
    /// `make CFLAGS=-O0` holds for `foo: CFLAGS += -g` too.
    pub fn apply(&mut self, var: &TargetVar, global: &MacroTable) {
        let env_wins = ENV_MACROS.load(Acquire);
        if !var.overriding && global.pins(&var.name, env_wins) {
            if let Some(value) = global.resolve(&var.name, env_wins) {
                self.force(var.name.clone(), value, MacroSource::CommandLine);
            }
            return;
        }
        let current = self.resolve(&var.name, env_wins);
        let value = match (var.op, current) {
            (TargetAssign::IfUnset, Some(_)) => return,
            (TargetAssign::Append, Some(current)) if !current.is_empty() => {
                format!("{current} {}", var.value)
            }
            _ => var.value.clone(),
        };
        let source = match var.overriding {
            true => MacroSource::Override,
            false => MacroSource::Makefile,
        };
        self.force(var.name.clone(), value, source);
    }

    /// The value the makefile, the command line or the built-ins gave
    /// `name`, ignoring the environment.
    pub fn defined(&self, name: &str) -> Option<&str> {
        self.values.get(name).map(|entry| entry.value.as_str())
    }

    /// Every definition, in first-definition order.
    pub fn entries(&self) -> impl Iterator<Item = (&str, &str)> {
        self.order.iter().filter_map(|name| {
            self.values
                .get(name)
                .map(|entry| (name.as_str(), entry.value.as_str()))
        })
    }

    pub fn into_macros(self) -> Vec<Macro> {
        // The source is dropped here, which is why `settle_environment` has to
        // have run: `Make` consumes values, and every precedence decision must
        // already have been made against them.
        self.order
            .into_iter()
            .map(|name| {
                let body = self
                    .values
                    .get(&name)
                    .map(|e| e.value.clone())
                    .unwrap_or_default();
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

fn expand_to_fixpoint(body: &str, table: &MacroTable, state: &func::Expansion) -> Result<String> {
    let mut body = body.to_string();
    for _ in 0..MAX_EXPANSION_ROUNDS {
        let (result, substitutions) = substitute(&body, table, state)?;
        if substitutions == 0 {
            return Ok(body);
        }
        body = result;
    }
    Err(PreprocError::RecursiveMacro)
}

/// Expand every macro reference in `source` until it stops changing.
fn substitute_to_fixpoint(
    source: &str,
    table: &MacroTable,
    state: &func::Expansion,
) -> Result<String> {
    let mut source = source.to_string();
    for _ in 0..MAX_EXPANSION_ROUNDS {
        let (result, substitutions) = substitute(&source, table, state)?;
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

/// Turn each `$$` of fully expanded text into the `$` it stands for.
///
/// Expansion here leaves `$$` in place, because most text is expanded again
/// later -- a command line by the rule stage, which does this itself. Text
/// handed to the shell while the makefile is read is not: POSIX 105799-105800
/// makes `$$` a single `$`, so `$(shell test 5 -lt 8; echo $$?)` must run
/// `echo $?`.
pub(crate) fn unescape_dollars(text: &str) -> String {
    text.replace("$$", "$")
}

/// Run `body` through the shell and convert its stdout to a macro value (`!=`).
fn shell_assign(body: &str) -> Result<String> {
    let Ok(result) = func::shell_command(body).output() else {
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
    state: &func::Expansion,
) -> Result<String> {
    match operator {
        Operator::Equals => Ok(body),
        Operator::Colon | Operator::Colon2 => expand_to_fixpoint(&body, table, state),
        Operator::Colon3 => Ok(substitute(&body, table, state)?.0),
        Operator::Bang => {
            shell_assign(&unescape_dollars(&expand_to_fixpoint(&body, table, state)?))
        }
        // `?=` and `+=` ask what the macro is *now*, and the environment is a
        // macro source (POSIX 105845) -- consulting only the table made
        // `CC ?= gcc` ignore an inherited `CC=clang`, and `CC += -Wall` drop it
        // entirely (audit #73).
        Operator::QuestionMark => Ok(match existing_value(name, table) {
            Some(existing) => existing,
            None => body,
        }),
        Operator::Plus => Ok(match existing_value(name, table) {
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

/// What `name` holds now, from the makefile or from the environment.
///
/// `None` means undefined, which is what `?=` tests; an explicitly empty value
/// is defined and is left alone.
fn existing_value(name: &str, table: &MacroTable) -> Option<String> {
    table.resolve(name, ENV_MACROS.load(Acquire))
}

/// Parse one macro-definition line into the name and the value its operator
/// assigns, resolving against what is already defined.
fn parse_macro_definition(
    line: &str,
    table: &MacroTable,
    state: &func::Expansion,
) -> Result<(String, String)> {
    let mut text = line.chars().peekable();
    // A definition may be indented with spaces, which is common inside a
    // conditional. (A <tab> would make it a command line, handled elsewhere.)
    skip_blank(&mut text);
    let name = parse_macro_name(&mut text)?;
    skip_blank(&mut text);
    let operator = parse_operator(&mut text)?;
    skip_blank(&mut text);
    let body = take_till_eol(&mut text);
    let body = apply_operator(operator, &name, body, table, state)?;
    Ok((name, body))
}

pub static ENV_MACROS: AtomicBool = AtomicBool::new(false);

/// Resolve a macro reference to its value.
///
/// POSIX 105845 makes the environment macro source 3, unconditionally -- `-e`
/// changes which source *wins*, not whether the environment is consulted at
/// all. POSIX 105833: "if the macro named by string1 does not exist, the final
/// result shall be an empty string", so an undefined name is not an error.
fn lookup_macro(name: &str, table: &MacroTable, env_wins: bool) -> String {
    table.resolve(name, env_wins).unwrap_or_default()
}

fn substitute(source: &str, table: &MacroTable, state: &func::Expansion) -> Result<(String, u32)> {
    let env_macros = ENV_MACROS.load(Acquire);

    let mut substitutions = 0;
    let mut result = String::with_capacity(source.len());

    // The text up to each `$` is copied a run at a time.
    let mut rest = source;
    while let Some(at) = rest.find('$') {
        result.push_str(&rest[..at]);
        let mut after = rest[at + 1..].chars();

        // A `$` with nothing after it is an ordinary character. Rejecting it
        // failed the whole makefile over a recipe as ordinary as
        // `@echo end$` (audit #87); the rule stage already emits it literally.
        let Some(letter) = after.next() else {
            result.push('$');
            rest = "";
            break;
        };
        rest = after.as_str();

        match letter {
            // Internal macros - we leave them "as is"
            // yet as they will be dealt with in the
            // parsing stage with more context available
            c @ ('$' | '@' | '%' | '?' | '<' | '*' | '^' | '+') => {
                result.push('$');
                result.push(c);
            }
            open @ ('(' | '{') => {
                let close = if open == '(' { ')' } else { '}' };

                // An internal-macro reference such as `$(@)`, `$(@D)`, `$(?F)`
                // is left verbatim for the rule stage, which alone has the
                // target/prerequisite context to expand it.
                if matches!(
                    rest.chars().next(),
                    Some('@' | '%' | '?' | '<' | '*' | '^' | '+')
                ) {
                    // To the matching close: a substitution may nest a
                    // reference, `$(@:.html=$(EXT))`.
                    result.push('$');
                    result.push(open);
                    result.push_str(read_balanced(&mut rest, open, close)?);
                    result.push(close);
                    continue;
                }

                let raw = join_escaped_newlines(read_balanced(&mut rest, open, close)?);
                let reference = Reference {
                    raw: &raw,
                    open,
                    close,
                };
                substitutions += reference.expand(table, state, env_macros, &mut result)?;
            }
            // A single character names a macro, as GNU does -- so a stray
            // `$` in a recipe expands to nothing rather than rejecting the
            // whole makefile (audit #72).
            c => {
                let mut name = [0; 4];
                let name = c.encode_utf8(&mut name);
                result.push_str(&lookup_macro(name, table, env_macros));
                substitutions += 1;
            }
        }
    }
    result.push_str(rest);

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

/// One `vpath` entry: a `%` pattern and the directories searched for it.
///
/// Not POSIX -- the standard has only the blanket `VPATH` macro -- but the
/// per-pattern form is how a makefile says "sources here, headers there".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VPathEntry {
    pub pattern: String,
    pub dirs: Vec<String>,
}

/// A file an `include` line named, whether or not it could be read.
///
/// Kept so that once the makefiles are read, make can bring each one up to
/// date and read the makefiles again if any changed (POSIX 105587-105600).
/// An `include` whose file is still missing then, and that no rule can
/// make, is an error; a `-include` (`optional`) is not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include {
    pub path: String,
    pub optional: bool,
}

/// How a target-specific assignment combines with the value already in
/// force for the target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetAssign {
    /// `=`, `:=`, `::=`, `:::=` and `!=`: the value replaces it.
    Set,
    /// `+=`: the value is appended to it.
    Append,
    /// `?=`: the value is used only where there is none.
    IfUnset,
}

/// A target-specific (or, with a `%` target, pattern-specific) macro
/// assignment, `targets: [override] [export] name op value` (GNU make).
///
/// It is in force while the commands of each target it names are expanded,
/// and of every target made as a prerequisite of one of them, unless that
/// target assigns the name itself. sysvinit writes
/// `init: LDLIBS += $(INITLIBS) $(STATIC)`; libzstd writes
/// `%-mt : CPPFLAGS_DYNLIB := -DZSTD_MULTITHREAD`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetVar {
    pub targets: Vec<String>,
    pub name: String,
    pub op: TargetAssign,
    /// For `=`, `+=` and `?=` the text as written, expanded with the
    /// target's macros when used; for the other operators the value the
    /// operator gave when the line was read.
    pub value: String,
    /// Written `override`: beats a command-line macro.
    pub overriding: bool,
    /// Written `export`: the macro goes into the environment of the
    /// target's commands.
    pub exported: bool,
}

/// What the reader produced: the text the rule parser sees, plus the state it
/// consumed on the way.
#[derive(Debug, Default)]
pub struct Preprocessed {
    pub text: String,
    pub macros: Vec<Macro>,
    /// The same macros with the source of each, which decides whether a
    /// target-specific assignment may replace one.
    pub table: MacroTable,
    pub vpaths: Vec<VPathEntry>,
    /// Macros an `export` directive asked to put in the recipe environment.
    pub exports: Vec<String>,
    /// Every file an `include` line named, in the order read.
    pub includes: Vec<Include>,
    /// Target- and pattern-specific macro assignments, in the order read.
    pub target_vars: Vec<TargetVar>,
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

/// The macro naming the makefiles read so far (GNU make).
pub const MAKEFILE_LIST: &str = "MAKEFILE_LIST";

/// How deep `include` may nest before we call it a cycle. POSIX 105611 requires
/// at least 16; the cap exists so a self-including file terminates.
const MAX_INCLUDE_DEPTH: usize = 64;

/// How deep `$(eval ...)` may re-enter the reader. Separate from the include
/// cap: they are independent resources, and sharing one would make an `include`
/// inside eval'd text trip the eval limit.
const MAX_EVAL_DEPTH: usize = 64;

/// A ceiling on how much text one line's evals may inject, so a self-producing
/// eval that stays shallow still terminates.
const MAX_EVAL_CHUNKS: usize = 1000;

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
    eval_depth: usize,
    vpaths: Vec<VPathEntry>,
    exports: Vec<String>,
    includes: Vec<Include>,
    target_vars: Vec<TargetVar>,
    /// Whether a rule line was the last line read that was not blank, a
    /// comment or a conditional: only then is a `<tab>` line a command line
    /// (POSIX 105643-105646). See `handle_line`.
    in_rule: bool,
    /// Shared by every expansion this reader performs, so recursion is bounded
    /// and `$(eval ...)` has somewhere to leave text.
    state: func::Expansion,
}

impl Reader {
    fn new() -> Self {
        Reader {
            table: MacroTable::default(),
            branches: Vec::new(),
            out: String::new(),
            depth: 0,
            eval_depth: 0,
            vpaths: Vec::new(),
            exports: Vec::new(),
            includes: Vec::new(),
            target_vars: Vec::new(),
            in_rule: false,
            state: func::Expansion::new(),
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
            // A command line reaches the shell verbatim, `#` included.
            self.out.push_str(line);
        } else {
            // `handle_line` stripped the comment *before* this expansion, so a
            // reference inside one is never evaluated -- a commented-out
            // `$(shell rm -rf ...)` must not run (audit #51).
            let code = line;
            // A command after the rule line's `;` is a command line, and like
            // any other is expanded by the rule stage, not here: expanding it
            // with the rest of the line used macros defined only so far.
            let (header, command) = split_inline_command(code);
            let expanded = substitute_to_fixpoint(header, &self.table, &self.state)?;
            // Anything `$(eval ...)` queued belongs *before* this line. A rule
            // header emitted by an eval on a rule line would otherwise close
            // the enclosing rule and steal its recipe.
            self.drain_evals()?;
            // A `#` here is literal -- it was written `\#`, or came from a
            // macro -- and the rule parser strips comments again.
            self.out.push_str(&super::scan::escape_hashes(&expanded));
            self.out.push_str(&super::scan::escape_hashes(command));
            // A blank line or a comment leaves a rule open; a line that
            // expands to nothing closes it, as in GNU make.
            if !code.trim().is_empty() {
                self.in_rule = !expanded.trim().is_empty() || !command.is_empty();
            }
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
        expand_to_fixpoint(text, &self.table, &self.state)
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
            .resolve(name, ENV_MACROS.load(Acquire))
            .is_some_and(|v| !v.is_empty());
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

    /// `vpath` with no arguments: forget every search path.
    fn clear_all_vpaths(&mut self) {
        self.vpaths.clear();
    }

    /// `vpath PATTERN`: forget the search path for one pattern.
    fn clear_vpath(&mut self, pattern: &str) {
        self.vpaths.retain(|entry| entry.pattern != pattern);
    }

    /// `vpath PATTERN DIRS`: add directories for a pattern, appending to any
    /// already recorded for it. Directories are `:`- or blank-separated.
    fn add_vpath(&mut self, pattern: &str, dirs: &str) {
        let dirs: Vec<String> = dirs
            .split([':', ' ', '\t'])
            .filter(|d| !d.is_empty())
            .map(String::from)
            .collect();
        if dirs.is_empty() {
            return;
        }
        match self.vpaths.iter_mut().find(|e| e.pattern == pattern) {
            Some(entry) => entry.dirs.extend(dirs),
            None => self.vpaths.push(VPathEntry {
                pattern: pattern.to_string(),
                dirs,
            }),
        }
    }

    /// Dispatch a `vpath` directive to one of its three forms.
    fn handle_vpath(&mut self, args: &str) -> Result<()> {
        let expanded = self.expand(args)?;
        let trimmed = expanded.trim();
        match trimmed.split_once([' ', '\t']) {
            // `vpath PATTERN DIRS`
            Some((pattern, dirs)) => self.add_vpath(pattern, dirs),
            // `vpath PATTERN` / bare `vpath`
            None if trimmed.is_empty() => self.clear_all_vpaths(),
            None => self.clear_vpath(trimmed),
        }
        Ok(())
    }

    /// `export NAME...` / `unexport NAME...`.
    ///
    /// A bare `export` with no names exports everything, which is GNU's meaning;
    /// recorded as the marker `*` so the rule stage can tell it apart.
    fn handle_export(&mut self, names: &str, exporting: bool) -> Result<()> {
        let expanded = self.expand(names)?;
        let names: Vec<String> = super::scan::split_words(&expanded);
        if names.is_empty() && exporting {
            self.exports.push("*".to_string());
            return Ok(());
        }
        for name in names {
            self.exports.retain(|e| *e != name);
            if exporting {
                self.exports.push(name);
            }
        }
        Ok(())
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
            self.table
                .set(name.to_string(), body.join("\n"), MacroSource::Makefile);
        }
        // Blank out the whole construct, header and terminator included.
        for _ in start..=i.min(lines.len() - 1) {
            self.drop_line();
        }
        i
    }

    /// Read the files an `include` line names.
    ///
    /// POSIX 105572-105577: the expanded remainder of the line is split at
    /// blanks into one or more pathnames, read in order, so
    /// `-include $(wildcard *.d)` reads every file the wildcard found.
    fn splice_include(&mut self, line: &str) -> Result<()> {
        let Some((path_spec, optional)) = parse_include_directive(line) else {
            return Ok(());
        };
        let paths = self.expand(path_spec)?;
        self.drop_line();
        for path in super::scan::split_words(&paths) {
            self.include_file(path, optional)?;
        }
        Ok(())
    }

    /// Read one included file, or note that it does not exist yet.
    ///
    /// POSIX 105587-105600, the "delayed remaking" method: a file that does
    /// not exist is read as empty, and once the makefiles have been read, any
    /// rule for it is used to create it -- automake's
    /// `include ./$(DEPDIR)/foo.Po` is made by `am--depfiles`. Whether a
    /// missing file without a rule is an error is decided then, because a
    /// rule later in the makefile may still supply it; see [`Include`].
    fn include_file(&mut self, path: String, optional: bool) -> Result<()> {
        if self.depth >= MAX_INCLUDE_DEPTH {
            return Err(PreprocError::IncludeFailed {
                path,
                reason: format!("include nested more than {MAX_INCLUDE_DEPTH} deep (cycle?)"),
            });
        }

        let read = fs::read_to_string(Path::new(&path));
        self.includes.push(Include {
            path: path.clone(),
            optional,
        });
        let contents = match read {
            Ok(contents) => contents,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(_) if optional => return Ok(()),
            Err(err) => {
                return Err(PreprocError::IncludeFailed {
                    path,
                    reason: err.to_string(),
                })
            }
        };

        // GNU make's MAKEFILE_LIST names every makefile read so far, the one
        // being read last: libzstd.mk finds its own directory with
        // `$(lastword $(MAKEFILE_LIST))`.
        let list = match self.table.resolve(MAKEFILE_LIST, ENV_MACROS.load(Acquire)) {
            Some(list) if !list.is_empty() => format!("{list} {path}"),
            _ => path.clone(),
        };
        self.table
            .set(MAKEFILE_LIST.to_string(), list, MacroSource::Makefile);

        self.depth += 1;
        let folded = fold_continuations(&contents);
        self.read(&folded)?;
        self.depth -= 1;
        Ok(())
    }

    /// Read text produced by `$(eval ...)` with its own conditional stack.
    ///
    /// Sharing the enclosing stack lets an eval'd `endif` close a conditional
    /// belonging to the outer file, silently un-gating it. GNU isolates here
    /// for the same reason, and reports an unbalanced conditional on exit.
    /// Taking the stack also makes the eval'd text unconditionally active,
    /// which is right: the queue can only be filled while active.
    fn read_isolated(&mut self, text: &str) -> Result<()> {
        let saved = std::mem::take(&mut self.branches);
        let in_rule = std::mem::replace(&mut self.in_rule, false);
        let result = self.read(text);
        let balanced = self.branches.is_empty();
        self.branches = saved;
        self.in_rule = in_rule;
        result?;
        if !balanced {
            return Err(PreprocError::UnmatchedConditional("endif".to_string()));
        }
        Ok(())
    }

    /// Read back everything `$(eval ...)` queued, and anything those reads
    /// queue in turn.
    fn drain_evals(&mut self) -> Result<()> {
        let mut chunks = 0usize;
        loop {
            let pending = self.state.take_pending();
            if pending.is_empty() {
                return Ok(());
            }
            if self.eval_depth >= MAX_EVAL_DEPTH {
                return Err(PreprocError::EvalTooDeep);
            }
            self.eval_depth += 1;
            let result = self.read_chunks(pending, &mut chunks);
            self.eval_depth -= 1;
            result?;
        }
    }

    fn read_chunks(&mut self, pending: Vec<String>, chunks: &mut usize) -> Result<()> {
        for text in pending {
            *chunks += 1;
            if *chunks > MAX_EVAL_CHUNKS {
                return Err(PreprocError::EvalTooDeep);
            }
            // Deliberately not folded: eval'd text usually comes from a
            // `define` body, whose lines were already folded when the file was
            // read. Folding again would eat a literal backslash.
            self.read_isolated(&text)?;
        }
        Ok(())
    }

    /// Handle one line, then read back anything its expansion queued.
    ///
    /// Wrapping rather than draining inside each arm covers every path that can
    /// expand: a conditional's condition, an include path, and a macro
    /// definition's body can all contain `$(eval ...)`.
    fn handle(&mut self, lines: &[&str], i: usize) -> Result<usize> {
        let last = self.handle_line(lines, i)?;
        self.drain_evals()?;
        Ok(last)
    }

    /// Handle one line; returns the index of the last line it consumed.
    fn handle_line(&mut self, lines: &[&str], i: usize) -> Result<usize> {
        // Every line but a command line loses its comment first. POSIX 105629
        // hands a command line to the shell verbatim, where `#` is meaningful;
        // everywhere else a `#` ends the line. Stripping here rather than at
        // `keep` time is what lets `ifeq ($(X),1)  # why` be read as a
        // conditional at all -- the comment used to reach `parse_directive`
        // as part of the condition, so the makefile failed outright, and
        // `ifdef X  # why` silently took the `else` arm (audit #81).
        //
        // A `<tab>` line is a command line only in a rule (POSIX 105643-
        // 105646: the lines after a target entry's rule line that begin
        // with a `<tab>`); anything else, a macro definition or a directive
        // included, ends the rule. Elsewhere POSIX gives a `<tab>` line no
        // meaning, and GNU make reads it as any other line, so it is read
        // here without its indentation, its continuations joined as outside
        // a command line. libselinux indents macro definitions inside
        // conditionals, `ifeq ($(USE_PCRE2),y)` / `<tab>PCRE_CFLAGS :=
        // -DUSE_PCRE2 ...`.
        let detabbed;
        let raw = match lines[i].starts_with('\t') && !self.in_rule {
            true => {
                detabbed = join_escaped_newlines(lines[i].trim_start_matches([' ', '\t']));
                detabbed.as_ref()
            }
            false => lines[i],
        };
        let stripped;
        let line = match raw.starts_with('\t') {
            true => raw,
            false => {
                stripped = super::scan::strip_comment(raw);
                stripped.as_ref()
            }
        };

        if let Some(directive) = parse_directive(line) {
            // Only a conditional leaves a rule open, as in GNU make.
            if self.active() && !directive.is_conditional() {
                self.in_rule = false;
            }
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
                Directive::VPath(args) => {
                    if self.active() {
                        self.handle_vpath(&args)?;
                    }
                    self.drop_line();
                }
                Directive::Export(names) => {
                    if self.active() {
                        self.handle_export(&names, true)?;
                    }
                    self.drop_line();
                }
                Directive::Unexport(names) => {
                    if self.active() {
                        self.handle_export(&names, false)?;
                    }
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
            self.in_rule = false;
            self.splice_include(line)?;
            self.in_rule = false;
        } else if is_macro_definition(line) {
            self.in_rule = false;
            self.define_macro(line)?;
            self.drop_line();
        } else if let Some((targets, assignment)) = split_target_var(line) {
            self.in_rule = false;
            self.define_target_var(targets, assignment)?;
            self.drop_line();
        } else {
            self.keep(line)?;
        }
        Ok(i)
    }

    /// Record a target-specific assignment, `targets: [override] [export]
    /// name op value`.
    ///
    /// The targets are expanded now, as a rule's are. The value is kept as
    /// written for `=`, `+=` and `?=`, to be expanded with the target's
    /// macros when its commands are; the other operators do their work now,
    /// against what is defined at this point, as for any macro definition.
    fn define_target_var(&mut self, targets: &str, assignment: &str) -> Result<()> {
        let targets = super::scan::split_words(&self.expand(targets)?);
        let mut text = assignment.trim_start();
        let (mut overriding, mut exported) = (false, false);
        loop {
            if let Some(rest) = strip_word(text, "override") {
                (overriding, text) = (true, rest);
            } else if let Some(rest) = strip_word(text, "export") {
                (exported, text) = (true, rest);
            } else {
                break;
            }
        }
        let mut letters = text.chars().peekable();
        let name = get_ident(&mut letters)?;
        skip_blank(&mut letters);
        let operator = parse_operator(&mut letters)?;
        skip_blank(&mut letters);
        let body: String = letters.collect();
        let (op, value) = match operator {
            Operator::Equals => (TargetAssign::Set, body),
            Operator::Plus => (TargetAssign::Append, body),
            Operator::QuestionMark => (TargetAssign::IfUnset, body),
            other => (
                TargetAssign::Set,
                apply_operator(other, &name, body, &self.table, &self.state)?,
            ),
        };
        self.target_vars.push(TargetVar {
            targets,
            name,
            op,
            value,
            overriding,
            exported,
        });
        Ok(())
    }

    fn define_macro(&mut self, line: &str) -> Result<()> {
        // `override NAME = v` is the documented way for a makefile to beat a
        // command-line macro; without it `override CFLAGS += -fPIC` would be
        // silently ignored.
        let (line, overriding) = match line.trim_start().strip_prefix("override ") {
            Some(rest) => (rest, true),
            None => (line, false),
        };
        let (name, body) = parse_macro_definition(line, &self.table, &self.state)?;
        match overriding {
            true => self.table.force(name, body, MacroSource::Override),
            false => self.table.set(name, body, MacroSource::Makefile),
        }
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

/// Expand one command line's macros against `table`: what the rule stage
/// does to a command line just before it runs, with the macros in force for
/// the target being made (target-specific ones included).
///
/// The automatic macros, `$$`, and any function call that reads an
/// automatic macro are left for the rule stage, which knows the target.
/// `$(eval ...)` is refused: a command line is expanded after the makefile
/// has been read, so text it queued would have no reader.
pub fn expand_command(text: &str, table: &MacroTable) -> std::result::Result<String, String> {
    let state = func::Expansion::without_eval();
    substitute_to_fixpoint(text, table, &state).map_err(|e| e.to_string())
}

/// Expand every command line of preprocessed text against `table`, as the
/// rule stage would for a target with no target-specific macros. A
/// multi-line value gets each of its newlines followed by the <tab> of a
/// command line.
fn expand_command_lines(text: &str, table: &MacroTable) -> Result<String> {
    let state = func::Expansion::without_eval();
    let mut out = String::with_capacity(text.len());
    for line in text.lines() {
        if line.starts_with('\t') {
            let expanded = substitute_to_fixpoint(line, table, &state)?;
            out.push_str(&expanded.replace('\n', "\n\t"));
        } else {
            out.push_str(line);
        }
        out.push('\n');
    }
    Ok(out)
}

/// Preprocess `source`, and expand its command lines too.
///
/// [`preprocess_with`] leaves command lines as written, for the rule stage
/// to expand with each target's macros. This shows what they expand to for
/// a target that has none of its own.
pub fn preprocess(source: &str) -> Result<Preprocessed> {
    let mut scanned = preprocess_with(source, &[], &[])?;
    scanned.text = expand_command_lines(&scanned.text, &scanned.table)?;
    Ok(scanned)
}

/// POSIX 105884-105893: `CURDIR` is provided by make, set to the absolute
/// pathname of the working directory make was executed in.
///
/// It ranks as a makefile definition, which places it exactly where the
/// standard does: the environment's `CURDIR` does not affect it unless `-e`
/// is given, and a definition in the makefile or on the command line replaces
/// it. Where the environment does carry a `CURDIR`, the recipe environment
/// receives the macro's value, as for any inherited name.
fn seed_curdir(table: &mut MacroTable) {
    if let Ok(dir) = std::env::current_dir() {
        let dir = dir.to_string_lossy().into_owned();
        table.seed("CURDIR".to_string(), dir, MacroSource::Makefile);
    }
}

/// Preprocess with the definitions that are in force before the makefile is
/// read: command-line macros (POSIX source 1) and the built-ins (source 4).
///
/// Both are seeded before the read rather than reconciled after it, because a
/// rule header is expanded where it appears -- appending left `all: $(OBJ)`
/// already expanded by the time the operand was seen (audit #71). The built-ins
/// had the same problem and it went unnoticed for longer: seeded only into the
/// finished `Make`, they were invisible to the makefile, so `$(CC)` expanded to
/// nothing while `make -p` reported `CC = c17`.
///
/// Recorded sources are what keep a later assignment in its place: the
/// makefile may replace a built-in but not a command-line macro, without
/// anyone having to track that separately.
///
/// Command lines are returned as written. They are expanded by the rule
/// stage just before they run, against the macros in force for the target
/// being made, which may include target-specific ones.
pub fn preprocess_with(
    source: &str,
    cmdline: &[Macro],
    builtins: &[Macro],
) -> Result<Preprocessed> {
    let mut reader = Reader::new();
    for (name, value) in builtins {
        reader
            .table
            .seed(name.clone(), value.clone(), MacroSource::BuiltIn);
    }
    seed_curdir(&mut reader.table);
    for (name, value) in cmdline {
        reader
            .table
            .seed(name.clone(), value.clone(), MacroSource::CommandLine);
    }
    reader.read(&fold_continuations(source))?;
    if !reader.branches.is_empty() {
        return Err(PreprocError::UnmatchedConditional("endif".to_string()));
    }
    // Settle the one precedence question the table still has open; see
    // `settle_environment`.
    reader.table.settle_environment();
    Ok(Preprocessed {
        text: reader.out,
        macros: reader.table.clone().into_macros(),
        table: reader.table,
        vpaths: reader.vpaths,
        exports: reader.exports,
        includes: reader.includes,
        target_vars: reader.target_vars,
    })
}
