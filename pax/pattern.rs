//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX pattern matching for pax pattern operands
//!
//! POSIX pax: a pattern "needs to be given in the name-generating notation of
//! the pattern matching notation in 2.14 Pattern Matching Notation, including
//! the filename expansion rules in 2.14.3". So, as `fnmatch(3)` with
//! `FNM_PATHNAME | FNM_PERIOD`:
//!
//! - `*` matches any string, `?` any single character, `[...]` a bracket
//!   expression (with `[:class:]`, `[=c=]`, `[.c.]`, ranges and `!`
//!   negation), and `\` makes the next character literal.
//! - A `/` is matched only by a `/` in the pattern -- never by `*`, `?` or a
//!   bracket expression.
//! - A `.` at the start of the name or just after a `/` is matched only by a
//!   literal `.`.
//! - A `[` that does not begin a complete bracket expression matches itself,
//!   and so does one whose expression would hold a `/` (XCU 2.14.3).
//! - A trailing `/` names a directory: the pattern matches only a directory
//!   member, by the rest of the pattern.
//!
//! Names and patterns are bytes. They are split into characters under the
//! current `LC_CTYPE`; a byte that does not begin a valid character is a
//! character of its own, so a name that is not valid text still matches `*`,
//! `?` and a literal of the same byte.
//!
//! Matching is iterative, with one backtracking point (the most recent `*`), so
//! it takes time proportional to name length times pattern length whatever the
//! pattern, and no stack.

/// A compiled pattern for matching
#[derive(Debug, Clone)]
pub struct Pattern {
    tokens: Vec<Token>,
    /// It ended in `/`, so it matches only a directory.
    dir_only: bool,
    /// The original pattern string, retained for "not found" diagnostics when a
    /// pattern operand matches no archive member.
    pub source: String,
}

#[derive(Debug, Clone)]
enum Token {
    /// A character that must appear as is: its bytes.
    Literal(Vec<u8>),
    /// `?`
    Any,
    /// `*`
    Star,
    /// `[...]`
    Bracket(Bracket),
}

#[derive(Debug, Clone)]
struct Bracket {
    negated: bool,
    items: Vec<BracketItem>,
}

#[derive(Debug, Clone)]
enum BracketItem {
    /// One character, by its bytes.
    Char(Vec<u8>),
    /// A range of characters, by code point.
    Range(char, char),
    /// `[:name:]`, under the current `LC_CTYPE`.
    Class(fn(char) -> bool),
    /// `[:name:]` naming no class: matches nothing.
    NoClass,
}

/// One character of a name: its bytes, and the character they encode when
/// they are valid text.
#[derive(Debug, Clone, Copy)]
struct Unit<'a> {
    bytes: &'a [u8],
    ch: Option<char>,
}

impl Unit<'_> {
    fn is(&self, byte: u8) -> bool {
        self.bytes == [byte]
    }
}

/// A name split into characters, ready to be matched against any number of
/// patterns.
pub struct Name<'a> {
    /// The characters, without the trailing `/`s a directory member is
    /// stored with (all but one, when that is all there is).
    units: Vec<Unit<'a>>,
    /// It names a directory.
    is_dir: bool,
}

impl<'a> Name<'a> {
    /// A name, which names a directory when it ends in `/`.
    pub fn new(bytes: &'a [u8]) -> Self {
        Self::member(bytes, false)
    }

    /// The name of a member, which is a directory when `is_dir` says so (a
    /// cpio directory has no trailing `/`) or the name ends in `/`.
    pub fn member(bytes: &'a [u8], is_dir: bool) -> Self {
        let mut units = split_units(bytes);
        let slash = units.last().is_some_and(|u| u.is(b'/'));
        while units.len() > 1 && units.last().is_some_and(|u| u.is(b'/')) {
            units.pop();
        }
        Name {
            units,
            is_dir: is_dir || slash,
        }
    }
}

/// How a pattern selects a member.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Selected {
    /// It matches the member's name.
    Itself,
    /// It matches the directory whose name is the first this many bytes of
    /// the member's, and so the member, which is in its hierarchy.
    Below(usize),
}

/// Split bytes into characters under the current `LC_CTYPE`, each with the
/// character it encodes there, if any.
fn split_units(bytes: &[u8]) -> Vec<Unit<'_>> {
    if bytes.is_ascii() {
        // Every locale this runs in encodes ASCII as itself, one byte each.
        return bytes
            .chunks(1)
            .map(|b| Unit {
                bytes: b,
                ch: Some(b[0] as char),
            })
            .collect();
    }
    plib::locale::mb_char_slices(bytes)
        .into_iter()
        .map(|b| Unit {
            bytes: b,
            ch: match plib::locale::MbDecoder::new().decode(b).as_slice() {
                [Some(c)] => Some(*c),
                _ => None,
            },
        })
        .collect()
}

impl Pattern {
    /// Compile a pattern. Every byte string is a valid pattern: a `[` that
    /// begins no bracket expression is an ordinary character.
    pub fn new(pattern: impl AsRef<[u8]>) -> Self {
        let pattern = pattern.as_ref();
        // A name, so its trailing slashes say "a directory" and nothing more.
        let name = Name::new(pattern);
        Pattern {
            tokens: parse_pattern(&name.units),
            // `/` alone is the root, not a name with a trailing slash.
            dir_only: name.is_dir && !(name.units.len() == 1 && name.units[0].is(b'/')),
            source: String::from_utf8_lossy(pattern).into_owned(),
        }
    }

    /// Whether the whole of `name` matches.
    pub fn matches(&self, name: &[u8]) -> bool {
        self.matches_name(&Name::new(name))
    }

    /// Whether the whole of `name` matches; a directory member stored as
    /// `dir/` is matched as `dir`.
    fn matches_name(&self, name: &Name) -> bool {
        self.matches_run(&name.units, name.is_dir, Periods::Explicit)
    }

    /// Whether the characters `text`, naming a directory if `is_dir`, match.
    fn matches_run(&self, text: &[Unit], is_dir: bool, periods: Periods) -> bool {
        (is_dir || !self.dir_only) && match_tokens(&self.tokens, text, periods)
    }

    /// How this pattern selects `name`, if it does: by matching the name
    /// itself or -- when `expand_subtree` is set -- a directory above it.
    ///
    /// Per POSIX, a pattern that selects a directory member also selects the
    /// entire file hierarchy rooted at that directory; `-d`
    /// (`expand_subtree == false`) restricts the match to the directory itself.
    /// The directory is the topmost the pattern matches, so its hierarchy
    /// holds every other. An absolute name's leading `/` is the directory `/`.
    pub fn selects(&self, name: &Name, expand_subtree: bool) -> Option<Selected> {
        if self.matches_name(name) {
            return Some(Selected::Itself);
        }
        if !expand_subtree {
            return None;
        }
        let units = &name.units;
        (0..units.len())
            .filter(|&i| units[i].is(b'/'))
            .map(|i| &units[..i.max(1)])
            .find(|dir| self.matches_run(dir, true, Periods::Explicit))
            .map(|dir| Selected::Below(dir.iter().map(|u| u.bytes.len()).sum()))
    }
}

/// Whether a leading `.` must be matched by a literal `.`, as in pax
/// patterns, or may be matched by a wildcard, as in tar's exclusions.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Periods {
    Explicit,
    Wild,
}

/// Parse a pattern into tokens.
fn parse_pattern(units: &[Unit]) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < units.len() {
        let u = units[i];
        i += 1;
        let token = if u.is(b'*') {
            Token::Star
        } else if u.is(b'?') {
            Token::Any
        } else if u.is(b'[') {
            match parse_bracket(&units[i..]) {
                Some((bracket, used)) => {
                    i += used;
                    Token::Bracket(bracket)
                }
                // No matching ']': the '[' matches itself (XCU 2.14.1).
                None => Token::Literal(b"[".to_vec()),
            }
        } else if u.is(b'\\') && i < units.len() {
            i += 1;
            Token::Literal(units[i - 1].bytes.to_vec())
        } else {
            Token::Literal(u.bytes.to_vec())
        };
        tokens.push(token);
    }
    tokens
}

/// Parse the bracket expression that follows a `[`, returning it and the
/// number of units it used, closing `]` included. `None` when there is no
/// closing `]`.
fn parse_bracket(units: &[Unit]) -> Option<(Bracket, usize)> {
    let mut i = 0;
    let negated = units.first().is_some_and(|u| u.is(b'!') || u.is(b'^'));
    if negated {
        i += 1;
    }
    let mut items = Vec::new();
    let mut first = true;
    loop {
        let u = *units.get(i)?;
        // XCU 2.14.3: "If a <slash> character is found following an unescaped
        // <left-square-bracket> character before a corresponding
        // <right-square-bracket> is found, the open bracket shall be treated as
        // an ordinary character."
        if u.is(b'/') {
            return None;
        }
        if u.is(b']') && !first {
            return Some((Bracket { negated, items }, i + 1));
        }
        first = false;

        // [:class:], [=c=], [.c.]
        if u.is(b'[') {
            if let Some((item, used)) = parse_bracket_term(&units[i + 1..]) {
                i += 1 + used;
                items.push(item);
                continue;
            }
        }

        let (start, used) = bracket_char(&units[i..])?;
        i += used;

        // A range, unless the '-' is the last thing before the ']'.
        let is_range = units.get(i).is_some_and(|u| u.is(b'-'))
            && units.get(i + 1).is_some_and(|u| !u.is(b']'));
        if is_range {
            let (end, used) = bracket_char(&units[i + 1..])?;
            i += 1 + used;
            match (start.ch, end.ch) {
                (Some(lo), Some(hi)) => items.push(BracketItem::Range(lo, hi)),
                // A range over bytes that are not characters names only its
                // endpoints.
                _ => {
                    items.push(BracketItem::Char(start.bytes.to_vec()));
                    items.push(BracketItem::Char(end.bytes.to_vec()));
                }
            }
        } else {
            items.push(BracketItem::Char(start.bytes.to_vec()));
        }
    }
}

/// One character of a bracket expression, after an optional escaping `\`.
fn bracket_char<'a>(units: &[Unit<'a>]) -> Option<(Unit<'a>, usize)> {
    let u = *units.first()?;
    if u.is(b'\\') {
        // An escaped '/' is still a '/': the '[' is ordinary.
        return units
            .get(1)
            .filter(|next| !next.is(b'/'))
            .map(|&next| (next, 2));
    }
    Some((u, 1))
}

/// Parse `:name:]`, `=c=]` or `.c.]` after a `[` inside a bracket expression,
/// returning the item and the units used.
fn parse_bracket_term(units: &[Unit]) -> Option<(BracketItem, usize)> {
    let delim = units.first()?;
    let kind = delim
        .bytes
        .first()
        .copied()
        .filter(|_| delim.bytes.len() == 1)?;
    if !matches!(kind, b':' | b'=' | b'.') {
        return None;
    }
    let close =
        (1..units.len().saturating_sub(1)).find(|&j| units[j].is(kind) && units[j + 1].is(b']'))?;
    if units[1..close].iter().any(|u| u.is(b'/')) {
        return None;
    }
    let body: Vec<u8> = units[1..close]
        .iter()
        .flat_map(|u| u.bytes.iter().copied())
        .collect();
    let item = if kind == b':' {
        char_class(&body).map_or(BracketItem::NoClass, BracketItem::Class)
    } else if close == 2 {
        // An equivalence class or collating symbol of one character stands
        // for that character.
        BracketItem::Char(body)
    } else {
        // Multi-character collating elements are not supported.
        return None;
    };
    Some((item, close + 2))
}

/// The predicate for a `[:name:]` character class.
fn char_class(name: &[u8]) -> Option<fn(char) -> bool> {
    use plib::locale;
    let f: fn(char) -> bool = match name {
        b"alnum" => locale::isalnum,
        b"alpha" => locale::isalpha,
        b"blank" => locale::isblank,
        b"cntrl" => locale::iscntrl,
        b"digit" => locale::isdigit,
        b"graph" => locale::isgraph,
        b"lower" => locale::islower,
        b"print" => locale::isprint,
        b"punct" => locale::ispunct,
        b"space" => locale::isspace,
        b"upper" => locale::isupper,
        b"xdigit" => locale::isxdigit,
        _ => return None,
    };
    Some(f)
}

impl Bracket {
    fn matches(&self, u: &Unit) -> bool {
        let found = self.items.iter().any(|item| match item {
            BracketItem::Char(bytes) => u.bytes == bytes.as_slice(),
            BracketItem::Range(lo, hi) => u.ch.is_some_and(|c| *lo <= c && c <= *hi),
            BracketItem::Class(f) => u.ch.is_some_and(f),
            BracketItem::NoClass => false,
        });
        found != self.negated
    }
}

/// Whether the character at `pos` is a '.' in a leading position -- at the
/// start of the name or immediately after a '/'. Such a '.' must be matched by
/// an explicit literal, never by `*`, `?`, or a bracket expression.
fn is_leading_period(text: &[Unit], pos: usize) -> bool {
    text[pos].is(b'.') && (pos == 0 || text[pos - 1].is(b'/'))
}

/// Whether a single-character token matches the character at `pos`.
fn token_matches(token: &Token, text: &[Unit], pos: usize, periods: Periods) -> bool {
    let u = &text[pos];
    // A wildcard never matches a '/', nor -- in a pax pattern -- a leading '.'.
    let explicit_period = periods == Periods::Explicit && is_leading_period(text, pos);
    let wild_ok = || !u.is(b'/') && !explicit_period;
    match token {
        Token::Literal(bytes) => u.bytes == bytes.as_slice(),
        Token::Any => wild_ok(),
        Token::Bracket(b) => wild_ok() && b.matches(u),
        Token::Star => unreachable!("handled by match_tokens"),
    }
}

/// Match a whole name against the tokens.
///
/// A `*` cannot match a `/`, so on a mismatch only the most recent `*` ever
/// needs to take one more character: an earlier one is confined to its own
/// pathname component, which the text after it has already fixed. That one
/// backtracking point keeps this linear in each `*` instead of exponential.
fn match_tokens(tokens: &[Token], text: &[Unit], periods: Periods) -> bool {
    let (mut p, mut s) = (0, 0);
    // Where to resume after the most recent `*`: the token after it, and the
    // text position it will next try to start from.
    let mut backtrack: Option<(usize, usize)> = None;

    loop {
        if p < tokens.len() {
            if let Token::Star = tokens[p] {
                while p < tokens.len() && matches!(tokens[p], Token::Star) {
                    p += 1;
                }
                if periods == Periods::Explicit && s < text.len() && is_leading_period(text, s) {
                    // Not even an empty `*` may stand before a leading '.'.
                    return false;
                }
                backtrack = Some((p, s));
                continue;
            }
            if s < text.len() && token_matches(&tokens[p], text, s, periods) {
                p += 1;
                s += 1;
                continue;
            }
        } else if s == text.len() {
            return true;
        }

        // Mismatch: let the last `*` take one more character, if it may.
        match backtrack {
            Some((bp, bs)) if bs < text.len() && !text[bs].is(b'/') => {
                backtrack = Some((bp, bs + 1));
                p = bp;
                s = bs + 1;
            }
            _ => return false,
        }
    }
}

/// Check whether `path` is excluded by any of `patterns` (tar's `--exclude`).
///
/// As in GNU tar, exclusion patterns are *unanchored*, match a leading `.`
/// with a wildcard, and take a directory's contents with it: a pattern is
/// tried against every run of whole components, so `--exclude=build` drops
/// `src/build` and everything below it, `--exclude=*.o` drops `src/obj/x.o`,
/// and `--exclude=*` drops `.profile`. An empty list excludes nothing.
pub fn matches_excluded(patterns: &[Pattern], path: &[u8]) -> bool {
    if patterns.is_empty() {
        return false;
    }
    let name = Name::new(path);
    let units = &name.units;
    let slashes = || (0..units.len()).filter(|&i| units[i].is(b'/'));
    let mut starts = std::iter::once(0).chain(slashes().map(|i| i + 1));
    starts.any(|start| {
        // Every run that ends before a '/' is a directory.
        let ends = slashes()
            .map(|end| (end, true))
            .chain(std::iter::once((units.len(), name.is_dir)));
        ends.filter(|&(end, _)| end > start).any(|(end, is_dir)| {
            patterns
                .iter()
                .any(|p| p.matches_run(&units[start..end], is_dir, Periods::Wild))
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn m(pattern: &str, name: &str) -> bool {
        Pattern::new(pattern).matches(name.as_bytes())
    }

    #[test]
    fn test_matches_excluded_is_unanchored() {
        let pats = vec![Pattern::new("build")];
        // The pattern is tried against the whole name and every suffix that
        // starts after a slash, so a bare component matches at any depth.
        assert!(matches_excluded(&pats, b"build"));
        assert!(matches_excluded(&pats, b"src/build"));
        assert!(matches_excluded(&pats, b"a/b/build"));
        // ...but only on a whole component boundary.
        assert!(!matches_excluded(&pats, b"rebuild"));
        assert!(!matches_excluded(&pats, b"src/rebuild"));
        assert!(!matches_excluded(&pats, b"builder/x"));
        // A directory takes its contents with it.
        assert!(matches_excluded(&pats, b"build/x"));
        assert!(matches_excluded(&pats, b"src/build/x/y"));

        // A directory member arrives with a trailing slash and still matches.
        assert!(matches_excluded(&pats, b"src/build/"));

        // An empty list excludes nothing.
        assert!(!matches_excluded(&[], b"anything"));
    }

    #[test]
    fn test_matches_excluded_glob() {
        let pats = vec![Pattern::new("*.o")];
        assert!(matches_excluded(&pats, b"x.o"));
        assert!(matches_excluded(&pats, b"src/obj/x.o"));
        assert!(!matches_excluded(&pats, b"x.c"));

        // A wildcard matches a leading '.' here, as in GNU tar.
        assert!(matches_excluded(&[Pattern::new("*")], b".profile"));
        assert!(matches_excluded(&[Pattern::new("?h")], b"e/.h/q"));
        // A trailing '/' names only a directory.
        let dirs = [Pattern::new("x/")];
        assert!(matches_excluded(&dirs, b"x/"));
        assert!(matches_excluded(&dirs, b"x/f"));
        assert!(!matches_excluded(&dirs, b"a/x"));
    }

    #[test]
    fn test_literal() {
        assert!(m("hello", "hello"));
        assert!(!m("hello", "hello2"));
        assert!(!m("hello", "hell"));
    }

    #[test]
    fn test_star() {
        assert!(m("*.txt", "file.txt"));
        assert!(m("*.txt", "long_filename.txt"));
        assert!(!m("*.txt", "file.txt.bak"));
        // A leading '.' is matched only by a literal '.': not even an empty
        // '*' may stand in front of it.
        assert!(!m("*.txt", ".txt"));
    }

    #[test]
    fn test_question() {
        assert!(m("file?.txt", "file1.txt"));
        assert!(m("file?.txt", "fileA.txt"));
        assert!(!m("file?.txt", "file.txt"));
        assert!(!m("file?.txt", "file12.txt"));
    }

    #[test]
    fn test_char_class() {
        assert!(m("file[123].txt", "file1.txt"));
        assert!(m("file[123].txt", "file3.txt"));
        assert!(!m("file[123].txt", "file4.txt"));
        assert!(m("file[a-z].txt", "filem.txt"));
        assert!(!m("file[a-z].txt", "fileA.txt"));
        assert!(m("file[!0-9].txt", "filea.txt"));
        assert!(!m("file[!0-9].txt", "file1.txt"));
    }

    #[test]
    fn test_bracket_forms() {
        assert!(m("a[[:digit:]]", "a1"));
        assert!(!m("a[[:digit:]]", "ab"));
        assert!(m("a[![:alpha:]]", "a1"));
        assert!(m("[]]", "]"));
        assert!(m("[!]]", "x"));
        assert!(m("[a-]", "-"));
        assert!(m("[[.-.]]", "-"));
        assert!(m("[[=e=]]", "e"));
        assert!(m(r"[\]]", "]"));
        // An unknown class matches nothing.
        assert!(!m("[[:nosuch:]]", "a"));
        // A bracket expression never matches '/': one that would hold a '/'
        // is no bracket expression, and its '[' is an ordinary character.
        assert!(!m("a[!x]b", "a/b"));
        assert!(!m("a[/]b", "a/b"));
        assert!(m("a[/]b", "a[/]b"));
        assert!(m(r"a[\/]b", "a[/]b"));
        assert!(m("a[[:x/y:]]", "a[[:x/y:]]"));
    }

    #[test]
    fn test_unterminated_bracket_is_literal() {
        assert!(m("x[y", "x[y"));
        assert!(m("[", "["));
        assert!(m("*[", "ab["));
        assert!(!m("x[y", "xy"));
    }

    #[test]
    fn test_star_middle() {
        assert!(m("src/*.rs", "src/main.rs"));
        assert!(!m("src/*.rs", "src/sub/mod.rs"));
        assert!(m("*/*", "a/b"));
        assert!(!m("*/*", "file.txt"));
        assert!(m("a*b*c", "aXbYbZc"));
        assert!(!m("a*b*c", "aXb/c"));
    }

    #[test]
    fn test_many_stars_are_linear() {
        let name = "a".repeat(200);
        let pattern = format!("{}b", "*a".repeat(100));
        assert!(!m(&pattern, &name));
        assert!(!m(&format!("{}b", "*".repeat(5000)), &name));
    }

    #[test]
    fn test_escape() {
        assert!(m(r"file\*.txt", "file*.txt"));
        assert!(!m(r"file\*.txt", "file1.txt"));
    }

    #[test]
    fn test_bytes_that_are_not_text() {
        let p = Pattern::new(b"caf\xe9");
        assert!(p.matches(b"caf\xe9"));
        assert!(!p.matches(b"caf"));
        assert!(Pattern::new("caf?").matches(b"caf\xe9"));
        assert!(Pattern::new("*").matches(b"\xff\xfe"));
    }

    #[test]
    fn test_leading_period_not_matched_by_wildcards() {
        // A leading '.' must be matched explicitly, never by * ? or [...].
        assert!(!m("*", ".hidden"));
        assert!(!m("?hidden", ".hidden"));
        assert!(!m("[.]hidden", ".hidden"));
        // An explicit leading dot does match.
        assert!(m(".*", ".hidden"));
        assert!(m(".hidden", ".hidden"));
        // The same rule applies just after a '/'.
        assert!(!m("dir/*", "dir/.hidden"));
        assert!(m("dir/.*", "dir/.hidden"));
        // A non-leading dot is matched normally.
        assert!(m("a*", "a.b"));
    }

    #[test]
    fn test_selects_subtree() {
        let p = Pattern::new("dir");
        let sel = |name: &str, expand| p.selects(&Name::new(name.as_bytes()), expand);

        // Without expansion, only the directory itself matches (stored "dir/").
        assert_eq!(sel("dir/", false), Some(Selected::Itself));
        assert_eq!(sel("dir/sub/f", false), None);
        // With expansion, the whole subtree matches via an ancestor.
        assert_eq!(sel("dir/sub/f", true), Some(Selected::Below(3)));
        // An unrelated sibling never matches.
        assert_eq!(sel("dirfoo", true), None);
    }

    #[test]
    fn test_selects_by_directory_names() {
        let sel = |pattern: &str, name: &str| {
            Pattern::new(pattern).selects(&Name::new(name.as_bytes()), true)
        };
        // A trailing '/' names a directory, and so its hierarchy.
        assert_eq!(sel("d/", "d/"), Some(Selected::Itself));
        assert_eq!(sel("d/", "d/x"), Some(Selected::Below(1)));
        assert_eq!(sel("f/", "f"), None);
        assert_eq!(
            Pattern::new("d/").selects(&Name::member(b"d", true), true),
            Some(Selected::Itself)
        );
        // 'a/*' is what is in a, not a itself.
        assert_eq!(sel("a/*", "a/"), None);
        assert_eq!(sel("a/*", "a/f"), Some(Selected::Itself));
        // The topmost directory matched is the hierarchy's root.
        assert_eq!(sel("a*", "a/ab/c"), Some(Selected::Below(1)));
        // An absolute name's leading '/' is the directory '/', which neither
        // '*' nor '' names.
        assert_eq!(sel("*", "/abs/k"), None);
        assert_eq!(sel("", "/abs/k"), None);
        assert_eq!(sel("/", "/abs/k"), Some(Selected::Below(1)));
        assert_eq!(sel("/abs", "/abs/k"), Some(Selected::Below(4)));
    }
}
