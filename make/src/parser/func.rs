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

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

/// How deep the expansion cycle may recurse before we call it non-terminating.
///
/// `substitute` -> `func::call` -> `expand` -> `substitute` is a real recursion,
/// and `MAX_EXPANSION_ROUNDS` bounds only the rounds *within* one frame. A
/// self-referential `$(call ...)` therefore used to exhaust the stack and abort
/// the process rather than report anything.
const MAX_EXPANSION_DEPTH: usize = 200;

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
    pending: Option<RefCell<Vec<String>>>,
}

impl Expansion {
    /// State for a context that can accept `$(eval ...)`.
    pub(crate) fn new() -> Self {
        Expansion {
            depth: Cell::new(0),
            pending: Some(RefCell::new(Vec::new())),
        }
    }

    /// State for a context that cannot -- a command line.
    pub(crate) fn without_eval() -> Self {
        Expansion {
            depth: Cell::new(0),
            pending: None,
        }
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
        Ok(DepthGuard(self))
    }
}

/// Restores the expansion depth when dropped.
struct DepthGuard<'a>(&'a Expansion);

impl Drop for DepthGuard<'_> {
    fn drop(&mut self) {
        self.0.depth.set(self.0.depth.get() - 1);
    }
}

/// What a function needs to expand text of its own.
pub(crate) struct Ctx<'a> {
    pub table: &'a HashMap<String, String>,
    pub env_wins: bool,
    /// Shared across every nested `Ctx`, so recursion is bounded end to end.
    pub state: &'a Expansion,
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

fn f_shell(command: &str) -> Result<String, String> {
    let output = std::process::Command::new("sh")
        .args(["-c", command])
        .output()
        .map_err(|e| format!("shell: {e}"))?;
    let text = String::from_utf8_lossy(&output.stdout);
    // Same convention as the `!=` operator: trailing newline dropped, the rest
    // turned into spaces.
    let text = text.strip_suffix('\n').unwrap_or(&text);
    Ok(text.replace('\n', " "))
}

fn f_wildcard(patterns: &str) -> String {
    let mut hits: Vec<String> = Vec::new();
    for pattern in words(patterns) {
        hits.extend(glob(pattern));
    }
    join(hits)
}

/// Minimal filename globbing: `*` and `?` within a single directory, which is
/// what `$(wildcard ...)` is used for in practice.
fn glob(pattern: &str) -> Vec<String> {
    if !pattern.contains(['*', '?']) {
        return if std::path::Path::new(pattern).exists() {
            vec![pattern.to_string()]
        } else {
            Vec::new()
        };
    }
    let (dir, name) = match pattern.rfind('/') {
        Some(i) => (&pattern[..i], &pattern[i + 1..]),
        None => (".", pattern),
    };
    let Ok(entries) = std::fs::read_dir(if dir.is_empty() { "/" } else { dir }) else {
        return Vec::new();
    };
    let mut hits: Vec<String> = entries
        .flatten()
        .map(|e| e.file_name().to_string_lossy().to_string())
        .filter(|f| glob_matches(name, f))
        .map(|f| {
            if pattern.contains('/') {
                format!("{dir}/{f}")
            } else {
                f
            }
        })
        .collect();
    hits.sort_unstable();
    hits
}

/// Shell-style `*`/`?` match over a single name component.
fn glob_matches(pattern: &str, name: &str) -> bool {
    let (p, n): (Vec<char>, Vec<char>) = (pattern.chars().collect(), name.chars().collect());
    let (mut pi, mut ni) = (0usize, 0usize);
    let (mut star, mut mark) = (None, 0usize);
    while ni < n.len() {
        if pi < p.len() && (p[pi] == '?' || p[pi] == n[ni]) {
            pi += 1;
            ni += 1;
        } else if pi < p.len() && p[pi] == '*' {
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
    p[pi..].iter().all(|&c| c == '*')
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
/// the recipe it generates still says `$(CC)`. Our `substitute` passes `$$`
/// through untouched, so without this the generated recipe would keep both
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
        let mut table = ctx.table.clone();
        table.insert(var.clone(), word.to_string());
        let inner = Ctx {
            table: &table,
            env_wins: ctx.env_wins,
            state: ctx.state,
        };
        results.push(expand(&args[2], &inner)?);
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
    let Some(body) = ctx.table.get(&name).cloned() else {
        return Ok(String::new());
    };
    let mut table = ctx.table.clone();
    table.insert("0".to_string(), name);
    for (i, arg) in args.iter().skip(1).enumerate() {
        table.insert((i + 1).to_string(), expand(arg, ctx)?);
    }
    let inner = Ctx {
        table: &table,
        env_wins: ctx.env_wins,
        state: ctx.state,
    };
    expand(&body, &inner)
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
    let arg = |i: usize| -> &str { args.get(i).map(String::as_str).unwrap_or("") };
    let all = args.join(",");

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
        "wildcard" => f_wildcard(arg(0)),
        "shell" => f_shell(&all)?,
        "error" => return Err(all),
        "warning" | "info" => {
            eprintln!("{all}");
            String::new()
        }
        other => return Err(format!("unknown function '{other}'")),
    };
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ctx_table() -> HashMap<String, String> {
        HashMap::new()
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
        assert!(glob_matches("*.c", "foo.c"));
        assert!(!glob_matches("*.c", "foo.o"));
        assert!(glob_matches("f?o.c", "foo.c"));
        assert!(glob_matches("*", "anything"));
        assert!(glob_matches("a*b*c", "axxbyyc"));
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
        };
        assert!(call("error", "boom", &ctx, &plain).is_err());
    }
}
