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
use crate::fast_hash::Map as HashMap;
use crate::Macro;
use std::borrow::Cow;
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
    /// A delayed-expansion macro whose value refers to itself, directly or
    /// through other macros: `A = $(A)x`.
    SelfReference(String),
    /// Delayed-expansion macros nested deeper than make allows.
    NestedTooDeep,
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
            // GNU make's words. `write!` for the reason given below.
            PreprocError::SelfReference(name) => {
                write!(
                    f,
                    "Recursive variable '{name}' references itself (eventually)"
                )
            }
            PreprocError::NestedTooDeep => write!(
                f,
                "macro references nested more than {} deep",
                func::MAX_MACRO_NESTING
            ),
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

/// The function a reference calls, and its argument text, if it is a call.
/// A function name is followed by at least one blank; `$(shell)` alone is a
/// macro named `shell`, as in GNU make.
fn function_call(raw: &str) -> Option<(&str, &str)> {
    let text = raw.trim_start_matches([' ', '\t']);
    let end = text.find([' ', '\t'])?;
    let (name, args) = text.split_at(end);
    func::is_function(name).then(|| (name, args.trim_start_matches([' ', '\t'])))
}

/// Split the expanded text of a reference, `name:subst1=subst2`, at its
/// first `:`, if an `=` follows it; otherwise the whole text is the name
/// (GNU make).
fn split_substitution(text: &str) -> (&str, Option<&str>) {
    match text.split_once(':') {
        Some((name, spec)) if spec.contains('=') => (name, Some(spec)),
        _ => (text, None),
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

/// Write `text` with every `$` doubled: what a function or the shell
/// returns is data, and expansion writes a literal `$` as `$$` (see
/// [`expand_into`]).
pub(crate) fn escape_dollars(text: &str) -> Cow<'_, str> {
    match text.contains('$') {
        true => Cow::Owned(text.replace('$', "$$")),
        false => Cow::Borrowed(text),
    }
}

/// Expand every macro reference in `text` onto `out`, once.
///
/// POSIX 105776-105784: a delayed-expansion macro's value is expanded where
/// it is referenced, its result scanned for further macros -- which is what
/// expanding the value recursively, straight onto `out`, does -- and an
/// immediate-expansion macro's value, expanded when it was defined, is not
/// scanned again. Nothing is rescanned: what a function returns, the output
/// of `$(shell ...)` or a file name `$(wildcard ...)` found, is data.
///
/// The output is in "escaped form": a literal `$` is written `$$`, so text
/// can be expanded in stages and each consumer turns `$$` into `$` once, at
/// the end ([`unescape_dollars`], and the rule stage for a command line).
/// `$(MAKE)` is left as written when nothing defines it, for the rule stage
/// to recognise. While the makefile is read, so is what only a target can
/// give a value: the automatic macros, and a reference or call that reads
/// one. An immediate-expansion macro stores its value in this form, so it
/// expands to itself but for what was left.
fn expand_into(text: &str, ctx: &func::Ctx, out: &mut String) -> Result<()> {
    // The text up to each `$` is copied a run at a time.
    let mut rest = text;
    while let Some(at) = rest.find('$') {
        out.push_str(&rest[..at]);
        let mut after = rest[at + 1..].chars();

        // A `$` with nothing after it is an ordinary character. Rejecting it
        // failed the whole makefile over a recipe as ordinary as
        // `@echo end$` (audit #87); the rule stage already emits it literally.
        let Some(letter) = after.next() else {
            out.push('$');
            return Ok(());
        };
        rest = after.as_str();
        expand_dollar(letter, &mut rest, ctx, out)?;
    }
    out.push_str(rest);
    Ok(())
}

/// Expand what one `$` followed by `letter` begins onto `out`, moving
/// `rest` past the text of a bracketed reference.
///
/// Apart from [`expand_into`] so that its loop, which every level of
/// nesting passes through, keeps a small stack frame.
fn expand_dollar(letter: char, rest: &mut &str, ctx: &func::Ctx, out: &mut String) -> Result<()> {
    match letter {
        '$' => out.push_str("$$"),
        c if AUTOMATIC.contains(&c) => match ctx.automatic {
            Some(automatic) => out.push_str(&escape_dollars(&automatic(c, None))),
            // Left for the rule stage, which has the target and its
            // prerequisites.
            None => {
                out.push('$');
                out.push(c);
            }
        },
        open @ ('(' | '{') => {
            let close = if open == '(' { ')' } else { '}' };
            // An internal-macro reference such as `$(@)`, `$(@D)`,
            // `$(@:.html=$(EXT))`, read to its matching close.
            if rest.starts_with(AUTOMATIC) {
                let inner = read_balanced(rest, open, close)?;
                match ctx.automatic {
                    Some(automatic) => expand_automatic(inner, automatic, ctx, out)?,
                    None => {
                        out.push('$');
                        out.push(open);
                        out.push_str(inner);
                        out.push(close);
                    }
                }
                return Ok(());
            }
            let raw = join_escaped_newlines(read_balanced(rest, open, close)?);
            expand_reference(&raw, open, close, ctx, out)?;
        }
        // A single character names a macro, as GNU does -- so a stray `$` in
        // a recipe expands to nothing rather than rejecting the whole
        // makefile (audit #72).
        c => {
            let mut name = [0; 4];
            expand_macro(c.encode_utf8(&mut name), '(', ')', ctx, out)?;
        }
    }
    Ok(())
}

/// Expand a bracketed automatic macro, the text between the delimiters of
/// `$(@)`, `$(@D)` or `$(<F)`, for the target being made.
///
/// A name the automatic macros give is a file name, data like a
/// `$(wildcard)` result: each `$` in it is a `$`.
fn expand_automatic(
    inner: &str,
    automatic: func::Automatic,
    ctx: &func::Ctx,
    out: &mut String,
) -> Result<()> {
    let inner = join_escaped_newlines(inner);
    let mut chars = inner.chars();
    let Some(sigil) = chars.next() else {
        return Ok(());
    };
    let rest = chars.as_str();
    let modifier = rest.chars().next().filter(|m| matches!(m, 'D' | 'F'));
    let rest = &rest[modifier.map_or(0, char::len_utf8)..];
    let value = automatic(sigil, modifier);
    let value = escape_dollars(&value);
    match rest.strip_prefix(':') {
        // `$(@:.html=.htp)`: an internal macro is a macro, and takes a
        // substitution like any other (POSIX 105804). automake's
        // `.texi.html` rule writes its output to `$(@:.html=.htp)` and then
        // moves it.
        Some(spec) => {
            let spec = expand_to_string(spec, ctx)?;
            out.push_str(&apply_substitution(&value, &spec));
        }
        None => out.push_str(&value),
    }
    Ok(())
}

/// Expand `text` into a string of its own; see [`expand_into`].
fn expand_to_string(text: &str, ctx: &func::Ctx) -> Result<String> {
    let mut out = String::with_capacity(text.len());
    expand_into(text, ctx, &mut out)?;
    Ok(out)
}

/// Expand the text between the delimiters of one `$(...)` or `${...}`
/// reference that is not an internal macro: a function call, a macro name,
/// or a substitution `name:subst1=subst2`.
///
/// POSIX 105801-105803 and 105830-105832: a reference inside the name or
/// on the right-hand side of the `:` is expanded first, and the result
/// examined. That covers a name computed in part, as automake writes it --
/// `$(am__v_CC_$(V))` -- or wholly, `$($(X))`, and a computed substitution,
/// `$(SRC:%.c=$(D)/%.o)`, as GNU make expands the whole reference text.
fn expand_reference(
    raw: &str,
    open: char,
    close: char,
    ctx: &func::Ctx,
    out: &mut String,
) -> Result<()> {
    if let Some((name, args)) = function_call(raw) {
        return expand_call(name, args, open, close, ctx, out);
    }
    let expanded;
    let text = match raw.contains('$') {
        // A reference reading an automatic variable, `$($@)`, `$(TEST_$@)`
        // or `$(SRC:.c=$@)`, is left for the rule stage, which knows the
        // target, as a function call reading one is. dpkg's tests of its
        // own .mk files check every variable with `test '$($@)' =
        // '$(TEST_$@)'`.
        true if ctx.automatic.is_none() && mentions_automatic(raw) => {
            out.push('$');
            out.push(open);
            out.push_str(raw);
            out.push(close);
            return Ok(());
        }
        true => {
            expanded = expand_to_string(raw, ctx)?;
            expanded.as_str()
        }
        false => raw,
    };
    let (name, spec) = split_substitution(text);
    // POSIX 105833 makes a reference to a macro that does not exist the
    // empty string, and a name that expands to nothing names none.
    let name = name.trim();
    if name.is_empty() {
        return Ok(());
    }
    match spec {
        None => expand_macro(name, open, close, ctx, out),
        Some(spec) => {
            // `$(name:subst1=subst2)` substitutes in the macro's expanded
            // value: `$(SRC:.c=.o)` has to see the words, not
            // `$(wildcard *.c)`.
            let mut body = String::new();
            expand_macro(name, open, close, ctx, &mut body)?;
            out.push_str(&apply_substitution(&body, spec));
            Ok(())
        }
    }
}

/// Expand a reference to the macro `name` onto `out`.
fn expand_macro(
    name: &str,
    open: char,
    close: char,
    ctx: &func::Ctx,
    out: &mut String,
) -> Result<()> {
    let Some(found) = ctx.lookup(name) else {
        // The special `MAKE` macro, when not otherwise defined, is passed
        // through to the rule stage (it expands to the make program and
        // marks the command line for recursive execution).
        if name == "MAKE" {
            out.push('$');
            out.push(open);
            out.push_str(name);
            out.push(close);
        }
        // POSIX 105833: an undefined macro is the empty string, not an error.
        return Ok(());
    };
    let value = found.value;
    match found.flavour {
        // A value with no `$` in it is what it expands to.
        _ if !value.contains('$') => out.push_str(&value),
        // What expanding the value left for the rule stage is expanded
        // there; the rest of it expands to itself.
        Flavour::Simple if ctx.automatic.is_some() => expand_into(&value, ctx, out)?,
        Flavour::Simple => out.push_str(&value),
        // A macro met again while its own value is being expanded would be
        // expanded forever: an error, as in GNU make, when it is used.
        Flavour::Recursive => {
            let _expanding = ctx
                .state
                .enter_macro(name, found.index)
                .map_err(|reentry| match reentry {
                    func::Reentry::SelfReference => PreprocError::SelfReference(name.to_string()),
                    func::Reentry::TooDeep => PreprocError::NestedTooDeep,
                })?;
            expand_into(&value, ctx, out)?;
        }
    }
    Ok(())
}

/// `$(fn args...)` -- a function call, not a macro reference.
fn expand_call(
    name: &str,
    args: &str,
    open: char,
    close: char,
    ctx: &func::Ctx,
    out: &mut String,
) -> Result<()> {
    // Defer the call if it reads an automatic variable; only the rule
    // stage can supply one (audit #62).
    if ctx.automatic.is_none() && mentions_automatic(args) {
        out.push('$');
        out.push(open);
        out.push_str(name);
        out.push(' ');
        out.push_str(args);
        out.push(close);
        return Ok(());
    }
    let result =
        func::call(name, args, ctx, &expand_for_function).map_err(PreprocError::FunctionFailed)?;
    out.push_str(&result);
    Ok(())
}

/// Expand text on a function's behalf.
fn expand_for_function(text: &str, ctx: &func::Ctx) -> std::result::Result<String, String> {
    expand_to_string(text, ctx).map_err(|e| e.to_string())
}

/// Expand `text` against `table`, with nothing bound by a function.
fn expand_text(text: &str, table: &MacroTable, state: &func::Expansion) -> Result<String> {
    let ctx = func::Ctx {
        table,
        env_wins: ENV_MACROS.load(Acquire),
        state,
        locals: None,
        automatic: None,
    };
    expand_to_string(text, &ctx)
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
        }
    }
}

/// How a macro's value is used when the macro is referenced (POSIX
/// 105730-105749).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Flavour {
    /// Delayed expansion -- `=`, `?=`, `!=`, `:::=`, `define`, the
    /// environment and the command line: the value is expanded each time
    /// the macro is referenced.
    Recursive,
    /// Immediate expansion -- `:=` and `::=`: the value was expanded when it
    /// was assigned, each literal `$` written `$$`, and is not expanded
    /// again (POSIX 105779-105781).
    Simple,
}

#[derive(Debug, Clone)]
struct MacroEntry {
    value: String,
    source: MacroSource,
    flavour: Flavour,
    /// Where the name is in the table's definition order: a number for the
    /// macro that stays the same when it is defined again.
    index: usize,
}

/// What a reference to a macro finds.
pub(crate) struct Found<'a> {
    pub value: Cow<'a, str>,
    pub flavour: Flavour,
    /// The table entry's number, unless only the environment defines the
    /// macro or a function binds it.
    pub index: Option<usize>,
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
            table.force(name.clone(), body.clone(), source, Flavour::Recursive);
        }
        table
    }

    /// Assign, unless a stronger source already defined `name`.
    ///
    /// Equal ranks assign, which is POSIX 105851: "Macro definitions from a
    /// single source ... shall override previous macro definitions from the
    /// same source."
    fn set(&mut self, name: String, body: String, source: MacroSource, flavour: Flavour) {
        let env_wins = ENV_MACROS.load(Acquire);
        if let Some(existing) = self.values.get(&name) {
            if source.rank(env_wins) < existing.source.rank(env_wins) {
                return;
            }
        }
        self.force(name, body, source, flavour);
    }

    /// Assign regardless of precedence, recording `source` as given.
    ///
    /// Used to establish a seed, whose rank is then honoured by every later
    /// assignment, and by `override`, which passes [`MacroSource::Override`]
    /// so that what it establishes outranks a command-line macro.
    fn force(&mut self, name: String, body: String, source: MacroSource, flavour: Flavour) {
        let index = match self.values.get(&name) {
            Some(entry) => entry.index,
            None => {
                self.order.push(name.clone());
                self.order.len() - 1
            }
        };
        self.values.insert(
            name,
            MacroEntry {
                value: body,
                source,
                flavour,
                index,
            },
        );
    }

    /// Seed a delayed-expansion definition from `source` before the
    /// makefile is read.
    pub fn seed(&mut self, name: String, body: String, source: MacroSource) {
        self.force(name, body, source, Flavour::Recursive);
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
        self.resolve_flavoured(name, env_wins)
            .map(|(value, _)| value)
    }

    /// What `name` holds, as [`Self::resolve`] says, and its flavour. A
    /// value from the environment is a delayed-expansion one.
    pub(crate) fn resolve_flavoured(
        &self,
        name: &str,
        env_wins: bool,
    ) -> Option<(String, Flavour)> {
        self.lookup(name, env_wins)
            .map(|found| (found.value.into_owned(), found.flavour))
    }

    /// [`Self::resolve_flavoured`], borrowing the value from the table when
    /// it comes from there, as it nearly always does: expansion asks this
    /// for every reference.
    pub(crate) fn lookup(&self, name: &str, env_wins: bool) -> Option<Found<'_>> {
        let entry = self.values.get(name);
        fn from_table(entry: &MacroEntry) -> Found<'_> {
            Found {
                value: Cow::Borrowed(entry.value.as_str()),
                flavour: entry.flavour,
                index: Some(entry.index),
            }
        }
        // The environment is not read for a definition it cannot outrank,
        // which is nearly every one; `getenv` walks the whole environment.
        let env_rank = MacroSource::Environment.rank(env_wins);
        if let Some(entry) = entry.filter(|entry| entry.source.rank(env_wins) >= env_rank) {
            return Some(from_table(entry));
        }
        let from_env = std::env::var(name).ok().map(|value| Found {
            value: Cow::Owned(value),
            flavour: Flavour::Recursive,
            index: entry.map(|entry| entry.index),
        });
        match entry {
            None => from_env,
            Some(entry) => from_env.or_else(|| Some(from_table(entry))),
        }
    }

    /// Replace every entry the environment outranks with the environment's
    /// value, so what the table holds is the value that wins.
    ///
    /// [`Self::resolve`] weighs the environment live, but the environment is
    /// never *stored*, so until this runs the table still has one precedence
    /// question open and [`Self::to_macros`] would hand on the loser. That is
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
                entry.flavour = Flavour::Recursive;
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
                MacroSource::Override => false,
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
    ///
    /// A `+=` defines a delayed-expansion macro whatever it adds to, as in
    /// GNU make, where `foo: A += $(X)` reads `X` when the commands are
    /// expanded even if `A` was defined with `:=`. An immediate macro's
    /// value expands to itself, so it can begin a delayed one.
    pub fn apply(&mut self, var: &TargetVar, global: &MacroTable) {
        let env_wins = ENV_MACROS.load(Acquire);
        if !var.overriding && global.pins(&var.name, env_wins) {
            if let Some((value, flavour)) = global.resolve_flavoured(&var.name, env_wins) {
                self.force(var.name.clone(), value, MacroSource::CommandLine, flavour);
            }
            return;
        }
        let current = self.resolve(&var.name, env_wins);
        let (value, flavour) = match (var.op, current) {
            (TargetAssign::IfUnset, Some(_)) => return,
            (TargetAssign::Append, Some(current)) if !current.is_empty() => {
                (format!("{current} {}", var.value), Flavour::Recursive)
            }
            _ => (var.value.clone(), var.flavour),
        };
        let source = match var.overriding {
            true => MacroSource::Override,
            false => MacroSource::Makefile,
        };
        self.force(var.name.clone(), value, source, flavour);
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

    pub fn to_macros(&self) -> Vec<Macro> {
        // The source is dropped here, which is why `settle_environment` has to
        // have run: `Make` consumes values, and every precedence decision must
        // already have been made against them.
        self.order
            .iter()
            .map(|name| {
                let body = self
                    .values
                    .get(name)
                    .map(|e| e.value.clone())
                    .unwrap_or_default();
                (name.clone(), body)
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

/// Turn a raw definition body into the value the operator assigns, and the
/// flavour of the macro it defines (POSIX 105730-105764).
fn apply_operator(
    operator: Operator,
    name: &str,
    body: String,
    table: &MacroTable,
    state: &func::Expansion,
) -> Result<(String, Flavour)> {
    let recursive = |value| Ok((value, Flavour::Recursive));
    match operator {
        Operator::Equals => recursive(body),
        Operator::Colon | Operator::Colon2 => {
            Ok((expand_text(&body, table, state)?, Flavour::Simple))
        }
        // A delayed-expansion macro whose value is expanded now as well,
        // with `$$` left as it is (POSIX 105739-105740, 105799-105801).
        Operator::Colon3 => recursive(expand_text(&body, table, state)?),
        // A delayed-expansion macro too: the command's output is expanded
        // when the macro is, unlike what `$(shell ...)` returns.
        Operator::Bang => {
            let command = unescape_dollars(&expand_text(&body, table, state)?);
            recursive(shell_assign(&command)?)
        }
        // `?=` and `+=` ask what the macro is *now*, and the environment is a
        // macro source (POSIX 105845) -- consulting only the table made
        // `CC ?= gcc` ignore an inherited `CC=clang`, and `CC += -Wall` drop it
        // entirely (audit #73).
        Operator::QuestionMark => match existing_value(name, table) {
            Some(existing) => Ok(existing),
            None => recursive(body),
        },
        // POSIX 105759-105764: what is appended to an immediate-expansion
        // macro is evaluated now, to a delayed-expansion one when used.
        Operator::Plus => match existing_value(name, table) {
            Some((existing, Flavour::Simple)) => {
                let body = expand_text(&body, table, state)?;
                Ok((format!("{existing} {body}"), Flavour::Simple))
            }
            Some((existing, Flavour::Recursive)) => recursive(format!("{existing} {body}")),
            None => recursive(body),
        },
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
fn existing_value(name: &str, table: &MacroTable) -> Option<(String, Flavour)> {
    table.resolve_flavoured(name, ENV_MACROS.load(Acquire))
}

/// Parse one macro-definition line into the name, the value its operator
/// assigns and the flavour that gives the macro, resolving against what is
/// already defined.
fn parse_macro_definition(
    line: &str,
    table: &MacroTable,
    state: &func::Expansion,
) -> Result<(String, String, Flavour)> {
    let mut text = line.chars().peekable();
    // A definition may be indented with spaces, which is common inside a
    // conditional. (A <tab> would make it a command line, handled elsewhere.)
    skip_blank(&mut text);
    let name = parse_macro_name(&mut text)?;
    skip_blank(&mut text);
    let operator = parse_operator(&mut text)?;
    skip_blank(&mut text);
    let body = take_till_eol(&mut text);
    let (body, flavour) = apply_operator(operator, &name, body, table, state)?;
    Ok((name, body, flavour))
}

pub static ENV_MACROS: AtomicBool = AtomicBool::new(false);

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
    /// The flavour of the macro it defines: immediate for `:=` and `::=`.
    pub flavour: Flavour,
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
            let expanded = expand_text(header, &self.table, &self.state)?;
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
        expand_text(text, &self.table, &self.state)
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

    /// Capture a `define ... endef` body, returning the index of the last
    /// line consumed. `op` is the operator written after the name, which
    /// assigns the body as it would a one-line definition's value:
    /// `define NAME :=` expands it now.
    fn capture_define(
        &mut self,
        name: &str,
        op: &str,
        lines: &[&str],
        start: usize,
    ) -> Result<usize> {
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
            let operator = match op.is_empty() {
                true => Operator::Equals,
                false => parse_operator(&mut op.chars().peekable())?,
            };
            let body = body.join("\n");
            let (value, flavour) = apply_operator(operator, name, body, &self.table, &self.state)?;
            self.table
                .set(name.to_string(), value, MacroSource::Makefile, flavour);
        }
        // Blank out the whole construct, header and terminator included.
        for _ in start..=i.min(lines.len() - 1) {
            self.drop_line();
        }
        Ok(i)
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
        let (source, flavour) = (MacroSource::Makefile, Flavour::Recursive);
        self.table
            .set(MAKEFILE_LIST.to_string(), list, source, flavour);

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
                Directive::Define { name, op } => {
                    return self.capture_define(&name, &op, lines, i);
                }
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
        let recursive = Flavour::Recursive;
        let (op, value, flavour) = match operator {
            Operator::Equals => (TargetAssign::Set, body, recursive),
            Operator::Plus => (TargetAssign::Append, body, recursive),
            Operator::QuestionMark => (TargetAssign::IfUnset, body, recursive),
            other => {
                let (value, flavour) =
                    apply_operator(other, &name, body, &self.table, &self.state)?;
                (TargetAssign::Set, value, flavour)
            }
        };
        self.target_vars.push(TargetVar {
            targets,
            name,
            op,
            value,
            flavour,
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
        let (name, body, flavour) = parse_macro_definition(line, &self.table, &self.state)?;
        match overriding {
            true => self.table.force(name, body, MacroSource::Override, flavour),
            false => self.table.set(name, body, MacroSource::Makefile, flavour),
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

/// Expand `text` against `table`, the macros in force for a target, with
/// no automatic macros: what a macro exported to the target's commands
/// holds.
///
/// The automatic macros and any reference or call that reads one are left
/// as written, and `$$` stands for `$`. `$(eval ...)` is refused: this is
/// done after the makefile has been read, so text it queued would have no
/// reader.
pub fn expand_command(text: &str, table: &MacroTable) -> std::result::Result<String, String> {
    let state = func::Expansion::without_eval();
    expand_text(text, table, &state).map_err(|e| e.to_string())
}

/// Expand one command line for the target being made, against `table`, the
/// macros in force for it: what the rule stage does to a command line just
/// before it runs. `automatic` gives the automatic macros.
///
/// The result is in the escaped form [`expand_into`] describes: the rule
/// stage turns `$$` into `$`, and an undefined `$(MAKE)` into the make
/// program. `$(eval ...)` is refused, as in [`expand_command`].
pub(crate) fn expand_command_for(
    text: &str,
    table: &MacroTable,
    automatic: func::Automatic,
) -> std::result::Result<String, String> {
    let state = func::Expansion::without_eval();
    let ctx = func::Ctx {
        table,
        env_wins: ENV_MACROS.load(Acquire),
        state: &state,
        locals: None,
        automatic: Some(automatic),
    };
    expand_to_string(text, &ctx).map_err(|e| e.to_string())
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
            let expanded = expand_text(line, table, &state)?;
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
        macros: reader.table.to_macros(),
        table: reader.table,
        vpaths: reader.vpaths,
        exports: reader.exports,
        includes: reader.includes,
        target_vars: reader.target_vars,
    })
}
