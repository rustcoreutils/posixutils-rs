//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Macro functions: `$(subst ...)`, `$(wildcard ...)`, `$(shell ...)` and so on.
//!
//! Not POSIX -- the standard has no functions -- but real makefiles lean on
//! them heavily. The set implemented here was chosen by counting occurrences
//! across a sample of hand-written Makefiles rather than by mirroring GNU's
//! full roster.
//!
//! Each function is its own small routine taking already-split arguments. The
//! three that need their arguments *unexpanded* (`if`, `foreach`, `call`) are
//! handled separately, before argument expansion.

use crate::parser::preprocessor::{escape_dollars, unescape_dollars, Flavour, Found, MacroTable};
use std::borrow::Cow;
use std::cell::{Cell, RefCell};

/// How deep function calls may nest before we call the expansion
/// non-terminating.
///
/// Expansion -> `func::call` -> expansion is a real recursion, and a
/// self-referential `$(call ...)` used to exhaust the stack and abort the
/// process rather than report anything.
const MAX_EXPANSION_DEPTH: usize = 200;

/// How deep delayed-expansion macros may nest, one expanded inside the
/// value of another. POSIX 105785 asks for at least 100 levels. Each level
/// is a few stack frames, and a thread Rust starts, a test's among them, has
/// 2 MiB of stack: in a debug build about 550 levels fill it.
pub(crate) const MAX_MACRO_NESTING: usize = 400;

/// Per-expansion state shared by every nested `Ctx`.
///
/// `pending` is where `$(eval ...)` leaves text for the reader to read back as
/// makefile source. A function cannot reach the reader -- it owns the macro
/// table and the output buffer, and expansion runs inside a `&mut self` call on
/// it -- so eval records its expansion here instead and the reader drains it.
///
/// `None` means eval is not usable in this context: command lines are expanded
/// after the reader has finished, where queued text would have no consumer.
#[derive(Default)]
pub(crate) struct Expansion {
    depth: Cell<usize>,
    macro_depth: Cell<usize>,
    /// The delayed-expansion macros whose values are being expanded, one
    /// bit per table entry, as GNU make flags a variable while it expands
    /// it: meeting one again means the value refers to itself.
    expanding: RefCell<Vec<u64>>,
    /// The same for macros only the environment defines, by name.
    expanding_env: RefCell<Vec<String>>,
    pending: Option<RefCell<Vec<String>>>,
}

/// Why a macro's value cannot be expanded where it is referenced.
pub(crate) enum Reentry {
    /// Its value is being expanded already: it refers to itself.
    SelfReference,
    /// Macros are nested more than [`MAX_MACRO_NESTING`] deep.
    TooDeep,
}

impl Expansion {
    /// State for a context that can accept `$(eval ...)`.
    pub(crate) fn new() -> Self {
        Expansion {
            pending: Some(RefCell::new(Vec::new())),
            ..Expansion::default()
        }
    }

    /// State for a context that cannot -- a command line.
    pub(crate) fn without_eval() -> Self {
        Expansion::default()
    }

    /// Queue text for the reader. Whitespace-only text is dropped: an eval that
    /// expanded to nothing has nothing to read back.
    fn queue(&self, text: String) -> Result<(), String> {
        let Some(pending) = &self.pending else {
            return Err("$(eval ...) is not usable in a command line".to_string());
        };
        if !text.trim().is_empty() {
            pending.borrow_mut().push(text);
        }
        Ok(())
    }

    /// Take everything queued so far.
    ///
    /// Borrowing is confined here so no caller can hold a `RefMut` across a
    /// recursive expansion, which would panic.
    pub(crate) fn take_pending(&self) -> Vec<String> {
        match &self.pending {
            Some(pending) => pending.borrow_mut().drain(..).collect(),
            None => Vec::new(),
        }
    }

    /// Enter one level of nested expansion, or report that it is too deep.
    ///
    /// The returned guard restores the previous depth however the caller
    /// leaves, including via `?`.
    fn enter(&self) -> Result<DepthGuard<'_>, String> {
        let depth = self.depth.get();
        if depth >= MAX_EXPANSION_DEPTH {
            return Err(format!(
                "expansion nested more than {MAX_EXPANSION_DEPTH} deep (recursive definition?)"
            ));
        }
        self.depth.set(depth + 1);
        Ok(DepthGuard(&self.depth))
    }

    /// Enter the value of the delayed-expansion macro `name`, whose table
    /// entry is `index` (`None` for one only the environment defines),
    /// until the guard returned is dropped.
    pub(crate) fn enter_macro(
        &self,
        name: &str,
        index: Option<usize>,
    ) -> Result<MacroGuard<'_>, Reentry> {
        let depth = self.macro_depth.get();
        if depth >= MAX_MACRO_NESTING {
            return Err(Reentry::TooDeep);
        }
        match index {
            Some(index) => {
                let mut expanding = self.expanding.borrow_mut();
                let (word, bit) = (index / 64, 1u64 << (index % 64));
                if word >= expanding.len() {
                    expanding.resize(word + 1, 0);
                }
                if expanding[word] & bit != 0 {
                    return Err(Reentry::SelfReference);
                }
                expanding[word] |= bit;
            }
            None => {
                let mut expanding = self.expanding_env.borrow_mut();
                if expanding.iter().any(|n| n == name) {
                    return Err(Reentry::SelfReference);
                }
                expanding.push(name.to_string());
            }
        }
        self.macro_depth.set(depth + 1);
        Ok(MacroGuard { state: self, index })
    }
}

/// Marks a macro as no longer being expanded when dropped.
pub(crate) struct MacroGuard<'a> {
    state: &'a Expansion,
    index: Option<usize>,
}

impl Drop for MacroGuard<'_> {
    fn drop(&mut self) {
        let state = self.state;
        state.macro_depth.set(state.macro_depth.get() - 1);
        match self.index {
            Some(index) => state.expanding.borrow_mut()[index / 64] &= !(1u64 << (index % 64)),
            None => {
                state.expanding_env.borrow_mut().pop();
            }
        }
    }
}

/// Restores an expansion depth when dropped.
pub(crate) struct DepthGuard<'a>(&'a Cell<usize>);

impl Drop for DepthGuard<'_> {
    fn drop(&mut self) {
        self.0.set(self.0.get() - 1);
    }
}

/// The macros a `foreach` loop or a `call` binds while its body is
/// expanded, in front of those bound by the functions it is inside.
pub(crate) struct Locals<'a> {
    bindings: Vec<(String, String)>,
    outer: Option<&'a Locals<'a>>,
}

impl Locals<'_> {
    /// The innermost binding of `name`.
    fn get(&self, name: &str) -> Option<&str> {
        match self.bindings.iter().find(|(bound, _)| bound == name) {
            Some((_, value)) => Some(value),
            None => self.outer?.get(name),
        }
    }
}

/// What a function needs to expand text of its own.
pub(crate) struct Ctx<'a> {
    pub table: &'a MacroTable,
    pub env_wins: bool,
    /// Shared across every nested `Ctx`, so recursion is bounded end to end.
    pub state: &'a Expansion,
    /// What the `foreach` loops and `call`s being expanded bind.
    pub locals: Option<&'a Locals<'a>>,
    /// The automatic macros of the target whose command line this is, or
    /// `None` while the makefile is read, when there is no target yet.
    pub automatic: Option<Automatic<'a>>,
}

/// The value of an automatic macro, `@ % ? < * ^ +`, with the `D` or `F`
/// modifier of `$(@D)` and `$(@F)` if there is one.
pub(crate) type Automatic<'a> = &'a dyn Fn(char, Option<char>) -> String;

impl<'a> Ctx<'a> {
    /// What `name` holds here, and its flavour: a `foreach` or `call`
    /// binding shadows every definition, and its value is already
    /// expanded, as GNU make binds it.
    pub(crate) fn lookup(&self, name: &str) -> Option<Found<'a>> {
        if let Some(value) = self.locals.and_then(|locals| locals.get(name)) {
            return Some(Found {
                value: Cow::Borrowed(value),
                flavour: Flavour::Simple,
                index: None,
            });
        }
        self.table.lookup(name, self.env_wins)
    }

    /// This context with `locals` bound in front of what it binds already.
    fn with<'b>(&'b self, locals: &'b Locals<'b>) -> Ctx<'b> {
        Ctx {
            table: self.table,
            env_wins: self.env_wins,
            state: self.state,
            locals: Some(locals),
            automatic: self.automatic,
        }
    }
}

type Expand<'a> = &'a dyn Fn(&str, &Ctx) -> Result<String, String>;

/// True if `name` is a function rather than a macro.
pub(crate) fn is_function(name: &str) -> bool {
    matches!(
        name,
        "subst"
            | "patsubst"
            | "strip"
            | "findstring"
            | "filter"
            | "filter-out"
            | "sort"
            | "dir"
            | "notdir"
            | "suffix"
            | "basename"
            | "addsuffix"
            | "addprefix"
            | "join"
            | "word"
            | "words"
            | "wordlist"
            | "firstword"
            | "lastword"
            | "wildcard"
            | "realpath"
            | "shell"
            | "if"
            | "or"
            | "and"
            | "foreach"
            | "call"
            | "eval"
            | "error"
            | "warning"
            | "info"
    )
}

/// Split on top-level commas, so a comma inside a nested `$(...)` stays put.
fn split_args(raw: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut depth = 0usize;
    let mut current = String::new();
    for c in raw.chars() {
        match c {
            '(' | '{' => {
                depth += 1;
                current.push(c);
            }
            ')' | '}' => {
                depth = depth.saturating_sub(1);
                current.push(c);
            }
            ',' if depth == 0 => {
                args.push(current);
                current = String::new();
            }
            _ => current.push(c),
        }
    }
    args.push(current);
    args
}

fn words(s: &str) -> Vec<&str> {
    s.split_whitespace().collect()
}

fn join(items: impl IntoIterator<Item = String>) -> String {
    items.into_iter().collect::<Vec<_>>().join(" ")
}

/// `%`-pattern match, returning the stem.
fn pattern_stem<'a>(pattern: &str, word: &'a str) -> Option<&'a str> {
    let (prefix, suffix) = pattern.split_once('%')?;
    if word.len() < prefix.len() + suffix.len() {
        return None;
    }
    if !word.starts_with(prefix) || !word.ends_with(suffix) {
        return None;
    }
    Some(&word[prefix.len()..word.len() - suffix.len()])
}

/// Substitute a `%`-pattern replacement, or return `replacement` verbatim when
/// it has no `%`.
fn pattern_apply(replacement: &str, stem: &str) -> String {
    match replacement.split_once('%') {
        Some((p, s)) => format!("{p}{stem}{s}"),
        None => replacement.to_string(),
    }
}

fn f_patsubst(pattern: &str, replacement: &str, text: &str) -> String {
    join(
        words(text)
            .into_iter()
            .map(|w| match pattern_stem(pattern, w) {
                Some(stem) => pattern_apply(replacement, stem),
                None if pattern == w => replacement.to_string(),
                None => w.to_string(),
            }),
    )
}

fn f_filter(patterns: &str, text: &str, keep: bool) -> String {
    let pats = words(patterns);
    join(
        words(text)
            .into_iter()
            .filter(|w| {
                let hit = pats.iter().any(|p| {
                    if p.contains('%') {
                        pattern_stem(p, w).is_some()
                    } else {
                        p == w
                    }
                });
                hit == keep
            })
            .map(String::from),
    )
}

fn f_sort(text: &str) -> String {
    let mut ws: Vec<&str> = words(text);
    ws.sort_unstable();
    ws.dedup();
    join(ws.into_iter().map(String::from))
}

fn dir_of(w: &str) -> String {
    match w.rfind('/') {
        Some(i) => w[..=i].to_string(),
        None => "./".to_string(),
    }
}

fn notdir_of(w: &str) -> String {
    match w.rfind('/') {
        Some(i) => w[i + 1..].to_string(),
        None => w.to_string(),
    }
}

/// The suffix of the last path component, `.` included; empty when there is none.
fn suffix_of(w: &str) -> String {
    let base = notdir_of(w);
    match base.rfind('.') {
        Some(i) => base[i..].to_string(),
        None => String::new(),
    }
}

/// The word with its suffix removed.
fn basename_of(w: &str) -> String {
    let suffix = suffix_of(w);
    w[..w.len() - suffix.len()].to_string()
}

fn f_word(index: &str, text: &str) -> Result<String, String> {
    let n: usize = index
        .trim()
        .parse()
        .map_err(|_| format!("word: '{index}' is not a number"))?;
    if n == 0 {
        return Err("word: index must be greater than 0".to_string());
    }
    Ok(words(text)
        .get(n - 1)
        .map(|s| s.to_string())
        .unwrap_or_default())
}

fn f_wordlist(from: &str, to: &str, text: &str) -> Result<String, String> {
    let parse = |s: &str| -> Result<usize, String> {
        s.trim()
            .parse()
            .map_err(|_| format!("wordlist: '{s}' is not a number"))
    };
    let (from, to) = (parse(from)?, parse(to)?);
    if from == 0 {
        return Err("wordlist: index must be greater than 0".to_string());
    }
    let ws = words(text);
    let end = to.min(ws.len());
    // `$(wordlist 5,1,...)` asks for an empty span, not an inverted slice.
    if from > ws.len() || from > end {
        return Ok(String::new());
    }
    Ok(join(ws[from - 1..end].iter().map(|s| s.to_string())))
}

fn f_join(a: &str, b: &str) -> String {
    let (aw, bw) = (words(a), words(b));
    let n = aw.len().max(bw.len());
    join((0..n).map(|i| {
        format!(
            "{}{}",
            aw.get(i).copied().unwrap_or(""),
            bw.get(i).copied().unwrap_or("")
        )
    }))
}

/// The shell running `command` for `$(shell ...)` or `!=`, "as if it were
/// passed as the argument to the system() interface" (POSIX 105744-105746):
/// only its standard output is captured, and it shares make's standard
/// input and standard error, so its diagnostics are seen. libselinux's
/// `$(shell $(PKG_CONFIG) --cflags ...)` said nothing when pkg-config could
/// not find the package.
pub(crate) fn shell_command(command: &str) -> std::process::Command {
    let mut shell = std::process::Command::new("sh");
    shell
        .args(["-c", command])
        .stdin(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit());
    shell
}

fn f_shell(command: &str) -> Result<String, String> {
    let output = shell_command(command)
        .output()
        .map_err(|e| format!("shell: {}", plib::diag::io_error_text(&e)))?;
    let text = String::from_utf8_lossy(&output.stdout);
    // Same convention as the `!=` operator: trailing newline dropped, the rest
    // turned into spaces.
    let text = text.strip_suffix('\n').unwrap_or(&text);
    Ok(text.replace('\n', " "))
}

/// `$(realpath names...)`: the canonical absolute name of each that exists
/// (GNU make). libzstd.mk finds its own directory with
/// `$(dir $(realpath $(lastword $(MAKEFILE_LIST))))`.
fn f_realpath(names: &str) -> String {
    join(
        words(names)
            .into_iter()
            .filter_map(|name| std::fs::canonicalize(name).ok())
            .map(|path| path.to_string_lossy().into_owned()),
    )
}

fn f_wildcard(patterns: &str) -> String {
    let mut hits: Vec<String> = Vec::new();
    for pattern in words(patterns) {
        hits.extend(glob(pattern));
    }
    join(hits)
}

/// What a target or prerequisite word names: GNU make globs a word holding
/// `*`, `?` or `[` against the files there are, and keeps it as written if
/// nothing matches. libzstd's doc/educational_decoder/Makefile writes
/// `HARNESS_FILES=*.c` and `harness: $(HARNESS_FILES)`.
pub(crate) fn glob_name(word: &str) -> Vec<String> {
    if !word.contains(['*', '?', '[']) {
        return vec![word.to_string()];
    }
    match glob(word) {
        found if found.is_empty() => vec![word.to_string()],
        found => found,
    }
}

/// Filename globbing for `$(wildcard ...)` and target and prerequisite
/// words: `*`, `?` and `[...]` in any component of the pattern, and `\` to
/// make the next character literal, as glob(3) does. sysvinit looks for
/// libcrypt with `$(wildcard $(ROOT)/$(usrdir)/lib*/libcrypt.*)`, a wildcard
/// in a directory component. As in the shell, a name starting with `.` is
/// matched only by a component that starts with a literal `.`.
///
/// Each name found is spelled as the pattern spells it, `//` included, as
/// glob(3) gives it: libzstd writes `$(LIB_SRCDIR)/decompress/*.c` with a
/// `LIB_SRCDIR` that ends in `/`, then turns the names into object names
/// with `$(subst $(LIB_SRCDIR)/decompress/,zstdd_,...)`, which matches only
/// if the `//` is still there.
fn glob(pattern: &str) -> Vec<String> {
    if !pattern.contains(['*', '?', '[']) {
        return match std::path::Path::new(pattern).exists() {
            true => vec![pattern.to_string()],
            false => Vec::new(),
        };
    }
    let components: Vec<&str> = pattern.split('/').collect();
    let mut found = vec![String::new()];
    for (i, component) in components.iter().enumerate() {
        let step = Step {
            first: i == 0,
            last: i + 1 == components.len(),
        };
        found = found
            .iter()
            .flat_map(|prefix| glob_component(prefix, component, step))
            .collect();
    }
    found.sort_unstable();
    found
}

/// Where a pattern component stands in its pattern.
#[derive(Clone, Copy)]
struct Step {
    /// Nothing precedes it: it is matched in the current directory, and no
    /// `/` joins it to what comes before.
    first: bool,
    /// Nothing follows it: what it names must exist, not just be a
    /// directory.
    last: bool,
}

/// The names `prefix` joined with each match of one pattern component, by
/// a single `/` unless the component is the first. A match that is not the
/// last component must be a directory; the last must exist.
fn glob_component(prefix: &str, component: &str, step: Step) -> Vec<String> {
    let join = |name: &str| -> String {
        match step.first {
            true => name.to_string(),
            false => format!("{prefix}/{name}"),
        }
    };
    let tokens = glob_tokens(component);
    if tokens.iter().all(|t| matches!(t, GlobToken::Literal(_))) {
        let literal: String = tokens
            .iter()
            .filter_map(|t| match t {
                GlobToken::Literal(c) => Some(*c),
                _ => None,
            })
            .collect();
        let path = join(&literal);
        let keep = match step.last {
            true => std::path::Path::new(&path).exists(),
            false => true,
        };
        return if keep { vec![path] } else { Vec::new() };
    }
    let dir = match (step.first, prefix) {
        (true, _) => ".",
        // The pattern began with `/`: the root directory.
        (false, "") => "/",
        (false, p) => p,
    };
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    entries
        .flatten()
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|name| tokens_match(&tokens, name))
        .map(|name| join(&name))
        .filter(|path| step.last || std::path::Path::new(path).is_dir())
        .collect()
}

/// One element of a glob(3) pattern component.
#[derive(Debug, PartialEq)]
enum GlobToken {
    /// A character matching itself, written plainly or after `\`.
    Literal(char),
    /// `?`: any one character.
    One,
    /// `*`: any run of characters.
    Any,
    /// `[...]`: one character in (or, after `!` or `^`, not in) the ranges;
    /// a single character is a range of one.
    Class {
        negated: bool,
        ranges: Vec<(char, char)>,
    },
}

/// Split a pattern component into its tokens. A `[` with no `]` to close
/// it is a literal, as is the character after a `\`.
fn glob_tokens(component: &str) -> Vec<GlobToken> {
    let chars: Vec<char> = component.chars().collect();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '\\' if i + 1 < chars.len() => {
                tokens.push(GlobToken::Literal(chars[i + 1]));
                i += 1;
            }
            '*' => tokens.push(GlobToken::Any),
            '?' => tokens.push(GlobToken::One),
            '[' => match glob_class(&chars, i + 1) {
                Some((class, close)) => {
                    tokens.push(class);
                    i = close;
                }
                None => tokens.push(GlobToken::Literal('[')),
            },
            c => tokens.push(GlobToken::Literal(c)),
        }
        i += 1;
    }
    tokens
}

/// The bracket expression whose body starts at `chars[start]`, and the
/// index of its closing `]`. A `]` first in the body is a member.
fn glob_class(chars: &[char], start: usize) -> Option<(GlobToken, usize)> {
    let negated = matches!(chars.get(start), Some('!' | '^'));
    let body = start + usize::from(negated);
    let mut ranges = Vec::new();
    let mut i = body;
    loop {
        let mut low = *chars.get(i)?;
        if low == ']' && i > body {
            return Some((GlobToken::Class { negated, ranges }, i));
        }
        if low == '\\' {
            i += 1;
            low = *chars.get(i)?;
        }
        let mut high = low;
        if chars.get(i + 1) == Some(&'-') {
            if let Some(&end) = chars.get(i + 2).filter(|&&c| c != ']') {
                high = end;
                i += 2;
            }
        }
        ranges.push((low, high));
        i += 1;
    }
}

/// Whether one name matches a component's tokens. `*`, `?` and a bracket
/// expression never match a leading `.`.
fn tokens_match(tokens: &[GlobToken], name: &str) -> bool {
    if name.starts_with('.') && tokens.first() != Some(&GlobToken::Literal('.')) {
        return false;
    }
    let n: Vec<char> = name.chars().collect();
    let one = |token: &GlobToken, c: char| match token {
        GlobToken::Literal(l) => *l == c,
        GlobToken::One => true,
        GlobToken::Any => false,
        GlobToken::Class { negated, ranges } => {
            ranges.iter().any(|&(lo, hi)| lo <= c && c <= hi) != *negated
        }
    };
    let (mut pi, mut ni) = (0usize, 0usize);
    let (mut star, mut mark) = (None, 0usize);
    while ni < n.len() {
        if pi < tokens.len() && one(&tokens[pi], n[ni]) {
            pi += 1;
            ni += 1;
        } else if pi < tokens.len() && tokens[pi] == GlobToken::Any {
            star = Some(pi);
            mark = ni;
            pi += 1;
        } else if let Some(s) = star {
            pi = s + 1;
            mark += 1;
            ni = mark;
        } else {
            return false;
        }
    }
    tokens[pi..].iter().all(|t| *t == GlobToken::Any)
}

/// Functions whose arguments must NOT be expanded before the call.
fn call_lazy(name: &str, raw: &str, ctx: &Ctx, expand: Expand) -> Option<Result<String, String>> {
    match name {
        "if" => Some(f_if(raw, ctx, expand)),
        "or" => Some(f_or_and(raw, ctx, expand, true)),
        "and" => Some(f_or_and(raw, ctx, expand, false)),
        "foreach" => Some(f_foreach(raw, ctx, expand)),
        "call" => Some(f_call(raw, ctx, expand)),
        "eval" => Some(f_eval(raw, ctx, expand)),
        _ => None,
    }
}

/// `$(eval text)` — expand `text`, then hand it to the reader to be read back
/// as makefile source.
///
/// Eager argument splitting would be wrong here: eval takes a single argument,
/// commas inside it are literal, and `split_args` would cut a `$(...)`
/// reference in half if a shell `case` pattern in a `define` body had already
/// driven its paren depth to the floor.
///
/// One `$` level is consumed, matching GNU: a template writes `$$(CC)` so that
/// the recipe it generates still says `$(CC)`. Expansion writes a literal `$`
/// as `$$`, so without this the generated recipe would keep both
/// dollars and reach the shell as a command substitution.
fn f_eval(raw: &str, ctx: &Ctx, expand: Expand) -> Result<String, String> {
    // `R = $(eval $(R))` recurses through `expand` without ever reaching
    // `$(call)` or `$(foreach)`, so eval needs the depth guard of its own.
    let _guard = ctx.state.enter()?;
    // Two statements deliberately: writing `queue(expand(..)?)` would hold the
    // queue's borrow across a nested eval and panic.
    let text = expand(raw, ctx)?;
    ctx.state.queue(text.replace("$$", "$"))?;
    Ok(String::new())
}

fn f_if(raw: &str, ctx: &Ctx, expand: Expand) -> Result<String, String> {
    // `A = $(if 1,$(A))` recurses through `expand` just as `$(call)` does; the
    // guard was added to three of the six lazy functions and missed here.
    let _guard = ctx.state.enter()?;
    let args = split_args(raw);
    let condition = expand(args.first().map(String::as_str).unwrap_or(""), ctx)?;
    let branch = if condition.trim().is_empty() { 2 } else { 1 };
    match args.get(branch) {
        Some(text) => expand(text, ctx),
        None => Ok(String::new()),
    }
}

/// `$(or ...)` returns the first non-empty argument; `$(and ...)` returns the
/// last, or empty if any is empty. Both short-circuit.
fn f_or_and(raw: &str, ctx: &Ctx, expand: Expand, is_or: bool) -> Result<String, String> {
    let _guard = ctx.state.enter()?;
    let mut last = String::new();
    for arg in split_args(raw) {
        let value = expand(&arg, ctx)?;
        let empty = value.trim().is_empty();
        if is_or && !empty {
            return Ok(value);
        }
        if !is_or && empty {
            return Ok(String::new());
        }
        last = value;
    }
    Ok(if is_or { String::new() } else { last })
}

fn f_foreach(raw: &str, ctx: &Ctx, expand: Expand) -> Result<String, String> {
    let args = split_args(raw);
    if args.len() < 3 {
        return Err("foreach: needs three arguments".to_string());
    }
    let _guard = ctx.state.enter()?;
    let var = expand(&args[0], ctx)?.trim().to_string();
    let list = expand(&args[1], ctx)?;
    let mut results = Vec::new();
    for word in words(&list) {
        let locals = Locals {
            bindings: vec![(var.clone(), word.to_string())],
            outer: ctx.locals,
        };
        results.push(expand(&args[2], &ctx.with(&locals))?);
    }
    Ok(join(results))
}

/// `$(call name,arg...)` expands `name`'s body with `$(1)`..`$(N)` bound.
fn f_call(raw: &str, ctx: &Ctx, expand: Expand) -> Result<String, String> {
    let args = split_args(raw);
    let Some(name) = args.first() else {
        return Err("call: needs a macro name".to_string());
    };
    let _guard = ctx.state.enter()?;
    let name = expand(name, ctx)?.trim().to_string();
    // Looked up as a reference would be: a function defined only in the
    // environment is as callable as any other, and `$(F)` already finds it.
    let Some(Found {
        value: body,
        flavour,
        ..
    }) = ctx.lookup(&name)
    else {
        return Ok(String::new());
    };
    // An immediate-expansion macro was expanded when it was defined; there
    // is nothing in it for the arguments to fill in.
    if flavour == Flavour::Simple {
        return Ok(body.into_owned());
    }
    let mut bindings = vec![("0".to_string(), name)];
    for (i, arg) in args.iter().skip(1).enumerate() {
        bindings.push(((i + 1).to_string(), expand(arg, ctx)?));
    }
    let locals = Locals {
        bindings,
        outer: ctx.locals,
    };
    expand(&body, &ctx.with(&locals))
}

/// Invoke a function. `raw` is the unexpanded argument text.
pub(crate) fn call(name: &str, raw: &str, ctx: &Ctx, expand: Expand) -> Result<String, String> {
    if let Some(result) = call_lazy(name, raw, ctx, expand) {
        return result;
    }

    let args: Vec<String> = split_args(raw)
        .iter()
        .map(|a| expand(a, ctx))
        .collect::<Result<_, _>>()?;
    call_eager(name, &args)
}

/// Apply a function that takes its arguments expanded to them.
///
/// Apart from [`call`] so that the frame of this match, large in a debug
/// build, is not on the stack while arguments and lazy functions expand:
/// a level of nesting passes through `call`.
fn call_eager(name: &str, args: &[String]) -> Result<String, String> {
    let arg = |i: usize| -> &str { args.get(i).map(String::as_str).unwrap_or("") };
    // Expanded text writes a literal `$` as `$$`. Text leaving make -- a
    // command for the shell, a pattern or a name for the file system, a
    // message -- has the `$` itself, and what the shell or the file system
    // gives back is data, each `$` in it a `$`.
    let all = || unescape_dollars(&args.join(","));
    let data = |text: String| escape_dollars(&text).into_owned();

    let out = match name {
        "subst" => arg(2).replace(arg(0), arg(1)),
        "patsubst" => f_patsubst(arg(0).trim(), arg(1).trim(), arg(2)),
        "strip" => join(words(arg(0)).into_iter().map(String::from)),
        "findstring" => {
            if arg(1).contains(arg(0)) {
                arg(0).to_string()
            } else {
                String::new()
            }
        }
        "filter" => f_filter(arg(0), arg(1), true),
        "filter-out" => f_filter(arg(0), arg(1), false),
        "sort" => f_sort(arg(0)),
        "dir" => join(words(arg(0)).into_iter().map(dir_of)),
        "notdir" => join(words(arg(0)).into_iter().map(notdir_of)),
        "suffix" => join(
            words(arg(0))
                .into_iter()
                .map(suffix_of)
                .filter(|s| !s.is_empty()),
        ),
        "basename" => join(words(arg(0)).into_iter().map(basename_of)),
        "addsuffix" => join(words(arg(1)).into_iter().map(|w| format!("{w}{}", arg(0)))),
        "addprefix" => join(words(arg(1)).into_iter().map(|w| format!("{}{w}", arg(0)))),
        "join" => f_join(arg(0), arg(1)),
        "word" => f_word(arg(0), arg(1))?,
        "words" => words(arg(0)).len().to_string(),
        "wordlist" => f_wordlist(arg(0), arg(1), arg(2))?,
        "firstword" => words(arg(0))
            .first()
            .map(|s| s.to_string())
            .unwrap_or_default(),
        "lastword" => words(arg(0))
            .last()
            .map(|s| s.to_string())
            .unwrap_or_default(),
        "wildcard" => data(f_wildcard(&unescape_dollars(arg(0)))),
        "realpath" => data(f_realpath(&unescape_dollars(arg(0)))),
        "shell" => data(f_shell(&all())?),
        "error" => return Err(all()),
        "warning" | "info" => {
            eprintln!("{}", all());
            String::new()
        }
        other => return Err(format!("unknown function '{other}'")),
    };
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ctx_table() -> MacroTable {
        MacroTable::default()
    }

    fn plain(text: &str, _ctx: &Ctx) -> Result<String, String> {
        Ok(text.to_string())
    }

    fn run(name: &str, raw: &str) -> String {
        let table = ctx_table();
        let state = Expansion::new();
        let ctx = Ctx {
            table: &table,
            env_wins: false,
            state: &state,
            locals: None,
            automatic: None,
        };
        call(name, raw, &ctx, &plain).expect("function should succeed")
    }

    #[test]
    fn splits_on_top_level_commas_only() {
        assert_eq!(split_args("a,b,c"), vec!["a", "b", "c"]);
        assert_eq!(split_args("$(x,y),b"), vec!["$(x,y)", "b"]);
    }

    #[test]
    fn text_functions() {
        assert_eq!(run("subst", "ee,EE,feet street"), "fEEt strEEt");
        assert_eq!(run("patsubst", "%.c,%.o,a.c b.c"), "a.o b.o");
        assert_eq!(run("strip", "  a   b  "), "a b");
        assert_eq!(run("findstring", "a,a b c"), "a");
        assert_eq!(run("findstring", "z,a b c"), "");
        assert_eq!(run("filter", "%.c %.s,a.c b.s c.o"), "a.c b.s");
        assert_eq!(run("filter-out", "%.c,a.c b.o"), "b.o");
        assert_eq!(run("sort", "b a c a"), "a b c");
    }

    #[test]
    fn path_functions() {
        assert_eq!(run("dir", "src/foo.c bar.c"), "src/ ./");
        assert_eq!(run("notdir", "src/foo.c bar.c"), "foo.c bar.c");
        assert_eq!(run("suffix", "src/foo.c bar"), ".c");
        assert_eq!(run("basename", "src/foo.c bar"), "src/foo bar");
        assert_eq!(run("addprefix", "obj/,a.o b.o"), "obj/a.o obj/b.o");
        assert_eq!(run("addsuffix", ".o,a b"), "a.o b.o");
    }

    #[test]
    fn word_functions() {
        assert_eq!(run("words", "a b c"), "3");
        assert_eq!(run("word", "2,a b c"), "b");
        assert_eq!(run("word", "9,a b c"), "");
        assert_eq!(run("wordlist", "2,3,a b c d"), "b c");
        assert_eq!(run("firstword", "a b c"), "a");
        assert_eq!(run("lastword", "a b c"), "c");
        assert_eq!(run("join", "a b,1 2"), "a1 b2");
    }

    #[test]
    fn shell_trims_like_the_bang_operator() {
        assert_eq!(run("shell", "printf 'one\\ntwo\\n'"), "one two");
    }

    #[test]
    fn glob_matching() {
        let glob_matches = |pattern: &str, name: &str| tokens_match(&glob_tokens(pattern), name);
        assert!(glob_matches("*.c", "foo.c"));
        assert!(!glob_matches("*.c", "foo.o"));
        assert!(glob_matches("f?o.c", "foo.c"));
        assert!(glob_matches("*", "anything"));
        assert!(glob_matches("a*b*c", "axxbyyc"));
        assert!(glob_matches("[ab].c", "b.c"));
        assert!(!glob_matches("[!ab].c", "b.c"));
        assert!(glob_matches("[^ab].c", "c.c"));
        assert!(glob_matches("[a-c]", "b"));
        assert!(glob_matches("[]x]", "]"));
        assert!(glob_matches("[a-]", "-"));
        assert!(glob_matches("a[b", "a[b"));
        assert!(glob_matches("\\*.c", "*.c"));
        assert!(!glob_matches("\\*.c", "x.c"));
        // `*`, `?` and `[...]` never match a leading `.`.
        assert!(!glob_matches("*", ".hidden"));
        assert!(!glob_matches("?h", ".h"));
        assert!(!glob_matches("[.]h", ".h"));
        assert!(glob_matches(".*", ".hidden"));
    }

    #[test]
    fn pattern_helpers() {
        assert_eq!(pattern_stem("%.c", "foo.c"), Some("foo"));
        assert_eq!(pattern_stem("%.c", "foo.o"), None);
        assert_eq!(pattern_apply("%.o", "foo"), "foo.o");
        assert_eq!(pattern_apply("fixed", "foo"), "fixed");
    }

    #[test]
    fn error_function_reports() {
        let table = ctx_table();
        let state = Expansion::new();
        let ctx = Ctx {
            table: &table,
            env_wins: false,
            state: &state,
            locals: None,
            automatic: None,
        };
        assert!(call("error", "boom", &ctx, &plain).is_err());
    }
}
