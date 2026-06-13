//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The roff front-end: the low-level request/escape interpreter that mandoc
//! calls `roff`. It runs over the raw page *before* mdoc/man detection and
//! parsing, executing roff programmability — number/string registers
//! (`.nr`/`.ds`/`.as`), conditionals (`.if`/`.ie`/`.el`), user macro definitions
//! (`.de`/`.am`), source inclusion (`.so`), ignore blocks (`.ig`), and
//! register/string interpolation (`\nX`, `\*X`, `\w'…'`) — and emits a
//! normalized stream of macro and text lines for the language parser.
//!
//! It deliberately passes through the mdoc/man macro set and the typographic
//! escapes (`\f`, `\(xx`, `\[..]`) untouched; those are handled downstream by
//! the formatter's escape layer. A page that uses no roff programmability is
//! emitted essentially unchanged.

mod expr;

use std::collections::HashMap;

use expr::eval_numeric;

/// Maximum number of lines the interpreter will emit before assuming a runaway
/// macro recursion and bailing out. Real pages are far below this.
const MAX_OUTPUT_LINES: usize = 2_000_000;

/// Maximum pending-line queue depth, a second runaway guard.
const MAX_QUEUE: usize = 4_000_000;

/// A user-defined macro body (the lines between `.de NAME` and `..`).
type MacroBody = Vec<String>;

/// Resolver for `.so` include targets (`None` disables inclusion).
type SoLoader = Box<dyn Fn(&str) -> Option<String>>;

/// An active output diversion (`.di`/`.da`): emitted lines are captured into
/// `buf` instead of the output, and stored as macro `name` when the diversion
/// ends.
struct Diversion {
    name: String,
    append: bool,
    buf: Vec<String>,
}

/// roff interpreter state.
#[derive(Default)]
pub struct Roff {
    /// Number registers.
    nr: HashMap<String, i64>,
    /// String registers / user strings.
    ds: HashMap<String, String>,
    /// User-defined macros (`.de`/`.am`).
    de: HashMap<String, MacroBody>,
    /// Result of the most recent `.if`/`.ie`, consumed by `.el`.
    last_cond: bool,
    /// Emitted output lines.
    out: Vec<String>,
    /// The active output diversion, if any (`.di`/`.da`).
    divert: Option<Diversion>,
    /// Resolves `.so` targets to file contents (None disables `.so`).
    loader: Option<SoLoader>,
}

/// Preprocess `input`, resolving `.so` targets through `loader`.
pub fn preprocess_with_loader<F>(input: &str, loader: F) -> String
where
    F: Fn(&str) -> Option<String> + 'static,
{
    let roff = Roff {
        loader: Some(Box::new(loader)),
        ..Roff::default()
    };
    roff.run(input)
}

impl Roff {
    /// Run the interpreter over `input` and return the normalized text.
    pub fn run(mut self, input: &str) -> String {
        let mut queue: Vec<String> = input.lines().rev().map(|l| l.to_string()).collect();
        let mut steps = 0usize;

        while let Some(line) = queue.pop() {
            steps += 1;
            if steps > MAX_OUTPUT_LINES || queue.len() > MAX_QUEUE {
                break;
            }
            self.process_line(line, &mut queue);
        }

        let mut text = self.out.join("\n");
        text.push('\n');
        text
    }

    /// Push `lines` so they are read next, in order.
    fn push_front(queue: &mut Vec<String>, lines: Vec<String>) {
        for l in lines.into_iter().rev() {
            queue.push(l);
        }
    }

    fn process_line(&mut self, line: String, queue: &mut Vec<String>) {
        // A control line begins with the control char `.` or the no-break `'`.
        let control = line.starts_with('.') || line.starts_with('\'');
        if !control {
            let line = self.strip_comment(&line);
            self.emit_text(line);
            return;
        }

        let body = line[1..].trim_start();
        let (name, args) = split_name(body);

        // Full-line comment.
        if name.starts_with("\\\"") || name.starts_with("\\#") || name.is_empty() {
            return;
        }

        match name {
            "de" | "de1" | "dei" => self.define_macro(args, queue, false),
            "am" | "am1" | "ami" => self.define_macro(args, queue, true),
            "ds" | "ds1" => self.do_ds(args, false),
            "as" | "as1" => self.do_ds(args, true),
            "nr" => self.do_nr(args),
            "rr" => {
                for reg in args.split_whitespace() {
                    self.nr.remove(reg);
                }
            }
            "rm" => {
                for n in args.split_whitespace() {
                    self.de.remove(n);
                    self.ds.remove(n);
                }
            }
            "rn" => self.do_rn(args),
            "als" => self.do_als(args),
            "if" => self.do_if(args, queue, false),
            "ie" => self.do_if(args, queue, true),
            "el" => self.do_el(args, queue),
            "ig" => self.do_ig(args, queue),
            "di" => self.do_di(args, false),
            "da" => self.do_di(args, true),
            "while" => self.do_while(args, queue),
            // Traps and environments are pure typesetting-position constructs:
            // on a terminal device (where the pager handles paging) they have no
            // observable effect, so they are recognized and dropped rather than
            // leaked to the language parser.
            "wh" | "ch" | "dt" | "ev" | "evc" => {}
            "TS" => self.do_tbl(queue),
            "EQ" => self.do_eqn(queue),
            "so" | "mso" => self.do_so(args, queue),
            "nop" => {
                if !args.is_empty() {
                    Self::push_front(queue, vec![args.to_string()]);
                }
            }
            // A call to a user-defined macro: expand it.
            _ if self.de.contains_key(name) => {
                let expanded = self.expand_macro(name, args);
                Self::push_front(queue, expanded);
            }
            // A recognized mdoc/man macro or an unknown request: pass the control
            // line through (with register/string interpolation applied), for the
            // downstream language parser.
            _ => {
                let line = self.strip_comment(&line);
                let line = self.interpolate(&line);
                self.emit_line(line);
            }
        }
    }

    fn emit_text(&mut self, line: String) {
        let line = self.interpolate(&line);
        self.emit_line(line);
    }

    /// Route an emitted line to the active diversion, or to the output.
    fn emit_line(&mut self, line: String) {
        match &mut self.divert {
            Some(d) => d.buf.push(line),
            None => self.out.push(line),
        }
    }

    // ── Macro definition & expansion ──────────────────────────────────────

    fn define_macro(&mut self, args: &str, queue: &mut Vec<String>, append: bool) {
        let parts = tokenize(args);
        let name = match parts.first() {
            Some(n) => n.clone(),
            None => return,
        };
        // Optional custom end macro (`.de NAME END`); default terminator is `..`.
        let end = parts.get(1).cloned();

        let mut body = Vec::new();
        while let Some(line) = queue.pop() {
            let trimmed = line.trim_start();
            let is_end = match &end {
                Some(e) => {
                    let b = trimmed
                        .strip_prefix('.')
                        .or_else(|| trimmed.strip_prefix('\''));
                    b.map(|b| split_name(b.trim_start()).0 == e)
                        .unwrap_or(false)
                }
                None => trimmed == ".." || trimmed == "'.",
            };
            if is_end {
                break;
            }
            body.push(line);
        }

        if append {
            self.de.entry(name).or_default().extend(body);
        } else {
            self.de.insert(name, body);
        }
    }

    fn expand_macro(&self, name: &str, args: &str) -> Vec<String> {
        let body = match self.de.get(name) {
            Some(b) => b,
            None => return Vec::new(),
        };
        let argv = tokenize(args);
        body.iter()
            .map(|line| substitute_args(line, name, &argv))
            .collect()
    }

    // ── Strings & registers ───────────────────────────────────────────────

    fn do_ds(&mut self, args: &str, append: bool) {
        let (name, rest) = split_name(args);
        if name.is_empty() {
            return;
        }
        // The value is the remainder, with a single leading space removed and an
        // optional leading double quote stripped (roff `.ds` quoting).
        let mut value = rest.to_string();
        if let Some(stripped) = value.strip_prefix('"') {
            value = stripped.to_string();
        }
        if append {
            self.ds
                .entry(name.to_string())
                .or_default()
                .push_str(&value);
        } else {
            self.ds.insert(name.to_string(), value);
        }
    }

    fn do_nr(&mut self, args: &str) {
        let (name, rest) = split_name(args);
        if name.is_empty() {
            return;
        }
        let rest = self.interpolate(rest);
        let expr = rest.split_whitespace().next().unwrap_or("");
        let cur = *self.nr.get(name).unwrap_or(&0);
        // A leading `+`/`-` increments/decrements the existing value by the
        // operand; otherwise the register is set to the absolute value.
        let new = if let Some(e) = expr.strip_prefix('+') {
            eval_numeric(e).map(|v| cur + v)
        } else if let Some(e) = expr.strip_prefix('-') {
            eval_numeric(e).map(|v| cur - v)
        } else {
            eval_numeric(expr)
        };
        if let Some(v) = new {
            self.nr.insert(name.to_string(), v);
        }
    }

    fn do_rn(&mut self, args: &str) {
        let parts = tokenize(args);
        if let (Some(from), Some(to)) = (parts.first(), parts.get(1)) {
            if let Some(b) = self.de.remove(from) {
                self.de.insert(to.clone(), b);
            }
            if let Some(s) = self.ds.remove(from) {
                self.ds.insert(to.clone(), s);
            }
        }
    }

    fn do_als(&mut self, args: &str) {
        let parts = tokenize(args);
        if let (Some(new), Some(old)) = (parts.first(), parts.get(1)) {
            if let Some(b) = self.de.get(old).cloned() {
                self.de.insert(new.clone(), b);
            }
        }
    }

    // ── Conditionals ──────────────────────────────────────────────────────

    fn do_if(&mut self, args: &str, queue: &mut Vec<String>, is_ie: bool) {
        let (cond, rest) = self.eval_condition(args);
        if is_ie {
            self.last_cond = cond;
        }
        self.run_conditional_body(cond, rest, queue);
    }

    fn do_el(&mut self, args: &str, queue: &mut Vec<String>) {
        let cond = !self.last_cond;
        self.run_conditional_body(cond, args.to_string(), queue);
    }

    /// Execute (or skip) a conditional body.
    fn run_conditional_body(&mut self, cond: bool, rest: String, queue: &mut Vec<String>) {
        let lines = Self::collect_body(&rest, queue);
        if cond {
            Self::push_front(queue, lines);
        }
    }

    /// Collect a request/conditional body: either the remainder of the line, or a
    /// `\{ … \}` block that may span lines pulled from `queue`.
    fn collect_body(rest: &str, queue: &mut Vec<String>) -> Vec<String> {
        let rest = rest.trim_start();
        let mut lines: Vec<String> = Vec::new();

        if let Some(after) = rest.strip_prefix("\\{") {
            // Block form: collect until the matching `\}`.
            let mut buf = after.to_string();
            let mut depth = 1i32;
            loop {
                // A trailing backslash is line continuation; drop it.
                if let Some(b) = buf.strip_suffix('\\') {
                    buf = b.to_string();
                }
                // Count brace escapes on the current buffer.
                depth += brace_delta(&buf);
                if depth <= 0 {
                    // Trim a trailing `\}` and keep anything before it.
                    if let Some(pos) = buf.rfind("\\}") {
                        let head = buf[..pos].to_string();
                        if !head.trim().is_empty() {
                            lines.push(head);
                        }
                    }
                    break;
                }
                if !buf.trim().is_empty() {
                    lines.push(buf.clone());
                }
                match queue.pop() {
                    Some(l) => buf = l,
                    None => break,
                }
            }
        } else if !rest.is_empty() {
            // Single-line body: the remainder is one request/text line.
            lines.push(rest.to_string());
        }

        lines
    }

    /// `.while COND body` — repeatedly run `body` while `COND` holds. The body is
    /// captured once; the condition is re-evaluated each iteration against the
    /// (mutating) register state. A hard iteration cap guards against runaway
    /// loops.
    fn do_while(&mut self, args: &str, queue: &mut Vec<String>) {
        let (_, rest) = self.eval_condition(args);
        let body = Self::collect_body(&rest, queue);

        let mut iters = 0;
        while self.eval_condition(args).0 {
            iters += 1;
            if iters > 100_000 || body.is_empty() {
                break;
            }
            let mut subq: Vec<String> = body.iter().rev().cloned().collect();
            let mut steps = 0;
            while let Some(l) = subq.pop() {
                steps += 1;
                if steps > MAX_OUTPUT_LINES {
                    break;
                }
                self.process_line(l, &mut subq);
            }
        }
    }

    /// `.di NAME` / `.da NAME` start (or, with no name, end) an output diversion.
    /// Captured lines become macro `NAME`, so `.NAME` re-emits the diverted text.
    fn do_di(&mut self, args: &str, append: bool) {
        // Close any diversion already in progress.
        if let Some(d) = self.divert.take() {
            if d.append {
                self.de.entry(d.name).or_default().extend(d.buf);
            } else {
                self.de.insert(d.name, d.buf);
            }
        }
        let name = split_name(args).0;
        if !name.is_empty() {
            self.divert = Some(Diversion {
                name: name.to_string(),
                append,
                buf: Vec::new(),
            });
        }
    }

    /// Parse and evaluate a condition at the start of `s`, returning the truth
    /// value and the remaining body text.
    fn eval_condition(&self, s: &str) -> (bool, String) {
        let s = s.trim_start();
        let mut neg = false;
        let mut rest = s;
        while let Some(r) = rest.strip_prefix('!') {
            neg = !neg;
            rest = r.trim_start();
        }

        let (cond, body) = self.parse_condition(rest);
        (cond ^ neg, body)
    }

    fn parse_condition(&self, s: &str) -> (bool, String) {
        let first = match s.chars().next() {
            Some(c) => c,
            None => return (false, String::new()),
        };

        // Built-in single-letter conditions whose body follows a space.
        match first {
            'n' | 'o' => {
                if let Some(rest) = s.strip_prefix(first) {
                    return (true, rest.trim_start().to_string());
                }
            }
            't' | 'e' => {
                if let Some(rest) = s.strip_prefix(first) {
                    return (false, rest.trim_start().to_string());
                }
            }
            'd' | 'r' | 'c' => {
                // `.if dNAME body` / `.if rREG body` / `.if cCHAR body`
                let after = &s[1..];
                let (name, rest) = split_name(after);
                let defined = match first {
                    'd' => {
                        self.de.contains_key(name)
                            || self.ds.contains_key(name)
                            || BUILTIN_REQUESTS.contains(&name)
                    }
                    'r' => self.nr.contains_key(name),
                    _ => true, // `c`: assume the glyph is available.
                };
                return (defined, rest.trim_start().to_string());
            }
            _ => {}
        }

        // String comparison: `'a'b'` (any non-alphanumeric delimiter).
        if !first.is_alphanumeric() && first != '(' && first != '-' && first != '+' && first != '\\'
        {
            let delim = first;
            let interp = self.interpolate(s);
            let parts: Vec<&str> = interp[delim.len_utf8()..].splitn(3, delim).collect();
            if parts.len() >= 2 {
                let body = parts
                    .get(2)
                    .map(|s| s.trim_start().to_string())
                    .unwrap_or_default();
                return (parts[0] == parts[1], body);
            }
        }

        // Numeric expression terminated by whitespace.
        let interp = self.interpolate(s);
        let expr = interp.split_whitespace().next().unwrap_or("");
        let body_start = interp
            .find(char::is_whitespace)
            .map(|i| &interp[i..])
            .unwrap_or("");
        let val = eval_numeric(expr).unwrap_or(0);
        (val > 0, body_start.trim_start().to_string())
    }

    // ── ig / so ───────────────────────────────────────────────────────────

    fn do_ig(&mut self, args: &str, queue: &mut Vec<String>) {
        let end = tokenize(args).into_iter().next();
        while let Some(line) = queue.pop() {
            let trimmed = line.trim_start();
            let is_end = match &end {
                Some(e) => {
                    let b = trimmed
                        .strip_prefix('.')
                        .or_else(|| trimmed.strip_prefix('\''));
                    b.map(|b| split_name(b.trim_start()).0 == e)
                        .unwrap_or(false)
                }
                None => trimmed == ".." || trimmed == ".end",
            };
            if is_end {
                break;
            }
        }
    }

    /// Capture a `.TS … .TE` table region, lay it out, and re-emit it as no-fill
    /// text so the renderer preserves the column alignment.
    fn do_tbl(&mut self, queue: &mut Vec<String>) {
        let body = Self::capture_until(queue, "TE");
        let table = crate::man_util::preproc::tbl::format(&body);
        let mut emit = vec![".nf".to_string()];
        emit.extend(table);
        emit.push(".fi".to_string());
        Self::push_front(queue, emit);
    }

    /// Capture a `.EQ … .EN` equation region and re-emit its linearized text.
    fn do_eqn(&mut self, queue: &mut Vec<String>) {
        let body = Self::capture_until(queue, "EN");
        let eq = crate::man_util::preproc::eqn::format(&body);
        Self::push_front(queue, eq);
    }

    /// Pop lines up to (and consuming) the `.END` control line.
    fn capture_until(queue: &mut Vec<String>, end: &str) -> Vec<String> {
        let mut body = Vec::new();
        while let Some(line) = queue.pop() {
            let trimmed = line.trim_start();
            let is_end = trimmed
                .strip_prefix('.')
                .or_else(|| trimmed.strip_prefix('\''))
                .map(|b| split_name(b.trim_start()).0 == end)
                .unwrap_or(false);
            if is_end {
                break;
            }
            body.push(line);
        }
        body
    }

    fn do_so(&mut self, args: &str, queue: &mut Vec<String>) {
        let target = self.interpolate(args);
        let target = target.trim();
        if let Some(loader) = &self.loader {
            if let Some(content) = loader(target) {
                let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
                Self::push_front(queue, lines);
            }
        }
    }

    // ── Interpolation & comments ──────────────────────────────────────────

    /// Strip a trailing roff comment (`\"` to end of line) that is not escaped.
    fn strip_comment(&self, line: &str) -> String {
        let bytes = line.as_bytes();
        let mut i = 0;
        let mut backslashes = 0;
        while i < bytes.len() {
            if bytes[i] == b'\\' {
                if i + 1 < bytes.len() && (bytes[i + 1] == b'"' || bytes[i + 1] == b'#') {
                    // `\"` or `\#` starts a comment when the backslash is real.
                    if backslashes % 2 == 0 {
                        return line[..i].trim_end().to_string();
                    }
                }
                backslashes += 1;
                i += 1;
                continue;
            }
            backslashes = 0;
            i += 1;
        }
        line.to_string()
    }

    /// Resolve `\nX`/`\n(xx`/`\n[name]` number registers, `\*X`/`\*(xx`/
    /// `\*[name]` string registers, and `\w'…'` width escapes. Other escapes are
    /// left for the downstream escape layer.
    fn interpolate(&self, input: &str) -> String {
        let mut out = String::with_capacity(input.len());
        let mut chars = input.char_indices().peekable();

        while let Some((_, c)) = chars.next() {
            if c != '\\' {
                out.push(c);
                continue;
            }
            match chars.peek().map(|&(_, c)| c) {
                Some('n') => {
                    chars.next();
                    let name = read_escape_name(&mut chars);
                    let val = self.nr.get(&name).copied().unwrap_or(0);
                    out.push_str(&val.to_string());
                }
                Some('*') => {
                    chars.next();
                    let name = read_escape_name(&mut chars);
                    if let Some(v) = self.ds.get(&name) {
                        // Resolve nested interpolations once.
                        out.push_str(&self.interpolate(v));
                    }
                }
                Some('w') => {
                    chars.next();
                    if let Some(width) = read_quoted(&mut chars) {
                        let resolved = self.interpolate(&width);
                        out.push_str(&display_cells(&resolved).to_string());
                    } else {
                        out.push_str("\\w");
                    }
                }
                _ => out.push('\\'),
            }
        }
        out
    }
}

/// Recognized built-in request names (used by the `d` condition).
const BUILTIN_REQUESTS: &[&str] = &[
    "de", "am", "ds", "as", "nr", "rr", "rm", "rn", "als", "if", "ie", "el", "ig", "so", "nop",
    "TH", "SH", "SS", "TP", "IP", "HP", "PP", "LP", "P", "br", "nf", "fi", "RS", "RE", "B", "I",
];

/// Split `s` into its first whitespace-delimited token (the request/string name)
/// and the remainder (leading whitespace trimmed).
fn split_name(s: &str) -> (&str, &str) {
    let s = s.trim_start();
    match s.find(|c: char| c.is_whitespace()) {
        Some(i) => (&s[..i], s[i..].trim_start()),
        None => (s, ""),
    }
}

/// Tokenize an argument string, honoring double quotes (so `"a b"` is one arg).
fn tokenize(s: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut cur = String::new();
    let mut in_quote = false;
    let mut has = false;
    for ch in s.chars() {
        match ch {
            '"' => {
                in_quote = !in_quote;
                has = true;
            }
            c if c.is_whitespace() && !in_quote => {
                if has {
                    args.push(std::mem::take(&mut cur));
                    has = false;
                }
            }
            c => {
                cur.push(c);
                has = true;
            }
        }
    }
    if has {
        args.push(cur);
    }
    args
}

/// Substitute `\\$1`…`\\$9`, `\\$0`, `\\$*`, `\\$@`, `\\$#` in a macro body line.
fn substitute_args(line: &str, name: &str, argv: &[String]) -> String {
    let mut out = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        if chars.peek() == Some(&'$') {
            chars.next();
            match chars.next() {
                Some('0') => out.push_str(name),
                Some('*') => out.push_str(&argv.join(" ")),
                Some('@') => {
                    let quoted: Vec<String> = argv.iter().map(|a| format!("\"{a}\"")).collect();
                    out.push_str(&quoted.join(" "));
                }
                Some('#') => out.push_str(&argv.len().to_string()),
                Some(d) if d.is_ascii_digit() => {
                    let idx = d.to_digit(10).unwrap() as usize;
                    if idx >= 1 {
                        if let Some(a) = argv.get(idx - 1) {
                            out.push_str(a);
                        }
                    }
                }
                Some(other) => {
                    out.push('\\');
                    out.push('$');
                    out.push(other);
                }
                None => out.push_str("\\$"),
            }
        } else {
            out.push('\\');
        }
    }
    out
}

/// Read an escape operand name after `\n`/`\*`: `(xx`, `[name]`, or a single char.
fn read_escape_name<I>(chars: &mut std::iter::Peekable<I>) -> String
where
    I: Iterator<Item = (usize, char)>,
{
    match chars.peek().map(|&(_, c)| c) {
        Some('(') => {
            chars.next();
            let mut s = String::new();
            for _ in 0..2 {
                if let Some((_, c)) = chars.next() {
                    s.push(c);
                }
            }
            s
        }
        Some('[') => {
            chars.next();
            let mut s = String::new();
            for (_, c) in chars.by_ref() {
                if c == ']' {
                    break;
                }
                s.push(c);
            }
            s
        }
        Some(_) => {
            let (_, c) = chars.next().unwrap();
            c.to_string()
        }
        None => String::new(),
    }
}

/// Read a `\w'…'`-style quoted operand (the delimiter is the next char).
fn read_quoted<I>(chars: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = (usize, char)>,
{
    let (_, delim) = *chars.peek()?;
    chars.next();
    let mut s = String::new();
    for (_, c) in chars.by_ref() {
        if c == delim {
            return Some(s);
        }
        s.push(c);
    }
    Some(s)
}

/// Net change in `\{`/`\}` brace-escape depth on a line.
fn brace_delta(s: &str) -> i32 {
    let opens = s.matches("\\{").count() as i32;
    let closes = s.matches("\\}").count() as i32;
    opens - closes
}

/// Approximate terminal-cell width of a string for `\w'…'` (ignores escapes).
fn display_cells(s: &str) -> usize {
    s.chars().filter(|c| !c.is_control()).count()
}

#[cfg(test)]
mod tests;
