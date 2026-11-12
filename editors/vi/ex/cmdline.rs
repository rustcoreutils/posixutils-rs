//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The ex command line (POSIX 94542-94646): where one command ends and the
//! next begins, what a command's name is, and the count and flags that may
//! follow it.

use super::address::{parse_address_range, scan_pattern};

/// Every POSIX ex command name with the length of its shortest spelling
/// (the bracketed synopses: `d[elete]` is 1, `ma[rk]` is 2).
const POSIX_NAMES: &[(&str, usize)] = &[
    ("abbreviate", 2),
    ("append", 1),
    ("args", 2),
    ("change", 1),
    ("chdir", 3),
    ("copy", 2),
    ("delete", 1),
    ("edit", 1),
    ("file", 1),
    ("global", 1),
    ("insert", 1),
    ("join", 1),
    ("k", 1),
    ("list", 1),
    ("map", 3),
    ("mark", 2),
    ("move", 1),
    ("next", 1),
    ("number", 2),
    ("open", 1),
    ("preserve", 3),
    ("print", 1),
    ("put", 2),
    ("quit", 1),
    ("read", 1),
    ("recover", 3),
    ("rewind", 3),
    ("set", 2),
    ("shell", 2),
    ("source", 2),
    ("stop", 2),
    ("substitute", 1),
    ("suspend", 2),
    ("t", 1),
    ("tag", 2),
    ("unabbreviate", 3),
    ("undo", 1),
    ("unmap", 3),
    ("v", 1),
    ("version", 2),
    ("visual", 2),
    ("write", 1),
    ("xit", 1),
    ("yank", 2),
    ("z", 1),
];

/// Names this implementation adds.  POSIX 94598-94600: they are tried only
/// once no POSIX name matched, and only as spelled here.
const EXTENSIONS: &[(&str, &str)] = &[
    ("cd", "chdir"),
    ("help", "help"),
    ("h", "help"),
    ("pop", "pop"),
    ("po", "pop"),
    ("prev", "previous"),
    ("previous", "previous"),
    ("pwd", "pwd"),
    ("red", "redo"),
    ("redo", "redo"),
    ("sus", "suspend"),
    ("tags", "tags"),
    ("vglobal", "v"),
    ("wq", "wq"),
    ("y", "yank"),
];

/// The command a typed name names (POSIX 94588-94597): a POSIX command whose
/// name it is a prefix of, no shorter than that name's shortest spelling.
/// Where several qualify, the one with the longest shortest spelling wins,
/// which is the standard's table of ambiguous prefixes (`su` is suspend,
/// `ma` is mark, `pr` is print, `re` is read, `un` is undo).
pub fn resolve_name(typed: &str) -> Option<&'static str> {
    POSIX_NAMES
        .iter()
        .filter(|(full, min)| typed.len() >= *min && full.starts_with(typed))
        .max_by_key(|(_, min)| *min)
        .map(|(full, _)| *full)
        .or_else(|| {
            EXTENSIONS
                .iter()
                .find(|(spelled, _)| *spelled == typed)
                .map(|(_, canon)| *canon)
        })
}

/// A command name and what came after it.
pub struct Name<'a> {
    /// The canonical name: a POSIX name in full, or an extension's.
    pub canon: &'static str,
    /// Characters POSIX 94575-94587 split off the typed name and hands to the
    /// command as arguments: the mark of `ka`, the flags of `dp`, the options
    /// of `sg`.
    pub split_off: &'a str,
    /// Whether `split_off` holds print flags rather than ordinary arguments
    /// (the `dp` case: `p` there is the flag, not a buffer named `p`).
    pub split_flags: bool,
    /// The input after the name.
    pub rest: &'a str,
}

/// The command name at the start of `input` (POSIX 94572-94597).
pub fn command_name(input: &str) -> Option<Name<'_>> {
    let first = input.chars().next()?;
    if !first.is_ascii_alphabetic() {
        let canon = match first {
            '!' => "!",
            '<' => "<",
            '>' => ">",
            '=' => "=",
            '@' => "@",
            '*' => "*",
            '&' => "&",
            '~' => "~",
            '#' => "#",
            _ => return None,
        };
        return Some(Name {
            canon,
            split_off: "",
            split_flags: false,
            rest: &input[first.len_utf8()..],
        });
    }
    let len = input
        .find(|c: char| !c.is_ascii_alphabetic())
        .unwrap_or(input.len());
    let typed = &input[..len];
    let rest = &input[len..];
    // Names are matched without regard to case, as this editor always has
    // (`:W`, `:Q`); the characters split off below keep theirs, since a mark
    // name is case-sensitive.
    let lower = typed.to_ascii_lowercase();
    let name = |canon, split_off, split_flags| {
        Some(Name {
            canon,
            split_off,
            split_flags,
            rest,
        })
    };
    if let Some(canon) = resolve_name(&lower) {
        return name(canon, "", false);
    }
    // 94581-94583: `k` and a mark name is `mark` with that name.
    if typed.len() == 2 && typed.starts_with('k') {
        return name("mark", &typed[1..], false);
    }
    // 94575-94580: a prefix of `delete` followed by any of l, p (the other
    // flag characters end the alphabetic name by themselves) is `delete`
    // with those flags; the longest prefix of `delete` is the name.
    let del = typed
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(typed.len()))
        .filter(|&i| i > 0 && "delete".starts_with(&typed[..i]))
        .max();
    if let Some(i) = del {
        if typed[i..].chars().all(|c| matches!(c, 'l' | 'p')) {
            return name("delete", &typed[i..], true);
        }
    }
    // 94584-94587: `s` followed by characters that are options to `s` is
    // `s` with those options and no pattern.
    if let Some(opts) = typed.strip_prefix('s') {
        if opts
            .chars()
            .all(|c| matches!(c, 'c' | 'g' | 'r' | 'l' | 'p'))
        {
            return name("substitute", opts, false);
        }
    }
    None
}

/// The print flags and line offset that may follow a command (POSIX
/// `[flags]`: any of `#`, `l`, `p`, `+`, `-`).
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct ExFlags {
    /// `#`: write the line number.
    pub number: bool,
    /// `l`: write the line unambiguously.
    pub list: bool,
    /// `p`: write the line.
    pub print: bool,
    /// The sum of the `+` and `-` flags: how far to move the current line
    /// before writing it.
    pub offset: i64,
}

impl ExFlags {
    /// Whether any flag was given.
    pub fn any(&self) -> bool {
        self.number || self.list || self.print || self.offset != 0
    }

    /// Add flag character `c`; false if `c` is not one.
    fn add(&mut self, c: char) -> bool {
        match c {
            '#' => self.number = true,
            'l' => self.list = true,
            'p' => self.print = true,
            '+' => self.offset += 1,
            '-' => self.offset -= 1,
            _ => return false,
        }
        true
    }

    /// Flags from `s`, which must be nothing but flag characters and blanks.
    pub fn parse(s: &str) -> Option<ExFlags> {
        let mut flags = ExFlags::default();
        for c in s.chars() {
            if !c.is_whitespace() && !flags.add(c) {
                return None;
            }
        }
        Some(flags)
    }
}

/// `[count][flags]` at the start of `args`: the count, the flags, and what
/// follows.  `d22p` is not ambiguous (94650), so no blank is needed between
/// them.
pub fn count_and_flags(args: &str) -> (Option<usize>, ExFlags, &str) {
    let args = args.trim_start();
    let digits = args.len() - args.trim_start_matches(|c: char| c.is_ascii_digit()).len();
    // A count that does not fit is left unconsumed, for the caller to report
    // as the junk it is rather than to compute with.
    let count: Option<usize> = (digits > 0).then(|| args[..digits].parse().ok()).flatten();
    let consumed = if count.is_some() { digits } else { 0 };
    let mut flags = ExFlags::default();
    let mut rest = &args[consumed..];
    loop {
        let trimmed = rest.trim_start();
        match trimmed.chars().next() {
            Some(c) if flags.add(c) => rest = &trimmed[c.len_utf8()..],
            _ => break,
        }
    }
    (count, flags, rest.trim())
}

/// One command of a command line.
#[derive(Debug, Default, PartialEq)]
pub struct Segment {
    /// The command, escapes that only served to separate it from the next
    /// (`\|`) removed.
    pub command: String,
    /// For `a`, `c` and `i` ended by `|`: the rest of the line, as their
    /// text (94640-94642).
    pub text: Option<String>,
    /// Whether a `|` ended this command.
    pub ended_by_bar: bool,
    /// Whether this was a comment line (`"` first): there is no command.
    pub comment: bool,
}

/// Split a command line into its commands (POSIX 94545-94646).
///
/// Where a command ends depends on what it is: `!`, `r !`, `w !`, `g` and
/// `v` take the rest of the line; `s`, `g`, `v` and `open` first take their
/// delimited pattern (and `s` its replacement) whole; `map`, `unmap`, `ab`
/// and `una` end at a `|` or `"` not escaped by `^V`; anything else ends at
/// a `|` or `"` not escaped by `\`.  A `"` starts a comment to the end of
/// the line.
pub fn split_line(line: &str) -> Vec<Segment> {
    let mut out = Vec::new();
    let mut s = line;
    loop {
        let (seg, next) = next_command(s);
        let more = seg.ended_by_bar && seg.text.is_none();
        out.push(seg);
        match next {
            Some(n) if more => s = n,
            _ => break,
        }
    }
    out
}

/// The first command of `line` and, if a `|` ended it, the rest.
fn next_command(line: &str) -> (Segment, Option<&str>) {
    // Steps 1-2: leading colons and blanks.
    let s = line.trim_start_matches([':', ' ', '\t']);
    // Step 3: a comment line.
    if s.starts_with('"') {
        return (
            Segment {
                comment: true,
                ..Default::default()
            },
            None,
        );
    }
    // Step 4: the addresses.
    let (_, after_addr) = parse_address_range(s);
    let r = after_addr.trim_start();
    let head = &s[..s.len() - r.len()];

    // Step 6: no command.
    if r.is_empty() || r.starts_with('|') || r.starts_with('"') {
        let seg = Segment {
            command: head.trim_end().to_string(),
            text: None,
            ended_by_bar: r.starts_with('|'),
            comment: false,
        };
        let next = r.strip_prefix('|');
        return (seg, next);
    }

    let Some(name) = command_name(r) else {
        // Not a command: let the parser report it, the whole line at once.
        return (
            Segment {
                command: s.to_string(),
                ..Default::default()
            },
            None,
        );
    };
    let name_end = r.len() - name.rest.len();
    let body = name.rest;
    let whole = |s: &str| {
        (
            Segment {
                command: s.to_string(),
                ..Default::default()
            },
            None,
        )
    };

    match name.canon {
        // Step 9: the rest of the line.
        "!" | "global" | "v" => return whole(s),
        "read" if body.trim_start().starts_with('!') => return whole(s),
        "write" if body.starts_with([' ', '\t']) && body.trim_start().starts_with('!') => {
            return whole(s)
        }
        _ => {}
    }

    // Step 11: the delimited RE (and `s` replacement) is taken whole, so a
    // `|` or `"` inside it ends nothing.
    let mut consumed = head.len() + name_end;
    if matches!(name.canon, "substitute" | "open") && name.split_off.is_empty() {
        let lead = body.len() - body.trim_start().len();
        let b = &body[lead..];
        if let Some(delim) = b.chars().next() {
            if !delim.is_alphanumeric()
                && !matches!(delim, '"' | '|' | '\\')
                && !delim.is_whitespace()
            {
                let after = &b[delim.len_utf8()..];
                let (_, mut tail) = scan_pattern(after, delim);
                if name.canon == "substitute" && !tail.is_empty() {
                    tail = skip_replacement(tail, delim);
                }
                consumed += lead + (b.len() - tail.len());
            }
        }
    }

    // Step 12: to the first unescaped `|` or `"`.
    let map_like = matches!(name.canon, "map" | "unmap" | "abbreviate" | "unabbreviate");
    let mut command = s[..consumed].to_string();
    let rest = &s[consumed..];
    let mut chars = rest.char_indices();
    let mut end = None;
    while let Some((i, c)) = chars.next() {
        match c {
            '\x16' if map_like => {
                command.push(c);
                if let Some((_, q)) = chars.next() {
                    command.push(q);
                }
            }
            '\\' if !map_like => match chars.next() {
                // The backslash only kept the `|` or `"` from ending the
                // command; it has done that and goes.
                Some((_, q @ ('|' | '"'))) => command.push(q),
                Some((_, q)) => {
                    command.push('\\');
                    command.push(q);
                }
                None => command.push('\\'),
            },
            '|' => {
                end = Some((i, true));
                break;
            }
            '"' => {
                end = Some((i, false));
                break;
            }
            c => command.push(c),
        }
    }

    let Some((i, bar)) = end else {
        return whole(&command);
    };
    let next = &rest[i + 1..];
    if !bar {
        // A comment: the rest of the line is discarded.
        return (
            Segment {
                command,
                ..Default::default()
            },
            None,
        );
    }
    // 94640-94642: `a`, `c` and `i` take the rest of the line as their text.
    if matches!(name.canon, "append" | "change" | "insert") {
        return (
            Segment {
                command,
                text: Some(next.to_string()),
                ended_by_bar: true,
                comment: false,
            },
            None,
        );
    }
    (
        Segment {
            command,
            text: None,
            ended_by_bar: true,
            comment: false,
        },
        Some(next),
    )
}

/// Past an `s` replacement that starts at `input` (just after the pattern's
/// closing delimiter): what follows its closing delimiter, or nothing.
fn skip_replacement(input: &str, delim: char) -> &str {
    let mut chars = input.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == '\\' {
            chars.next();
        } else if c == delim {
            return &input[i + c.len_utf8()..];
        }
    }
    ""
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_name_prefixes_and_ambiguities() {
        for (typed, want) in [
            ("d", "delete"),
            ("del", "delete"),
            ("dele", "delete"),
            ("pri", "print"),
            ("su", "suspend"),
            ("s", "substitute"),
            ("subst", "substitute"),
            ("ma", "mark"),
            ("m", "move"),
            ("map", "map"),
            ("ch", "change"),
            ("chd", "chdir"),
            ("pr", "print"),
            ("pre", "preserve"),
            ("re", "read"),
            ("rec", "recover"),
            ("un", "undo"),
            ("una", "unabbreviate"),
            ("a", "append"),
            ("ab", "abbreviate"),
            ("n", "next"),
            ("nu", "number"),
            ("v", "v"),
            ("vi", "visual"),
            ("w", "write"),
            ("x", "xit"),
            ("co", "copy"),
            ("t", "t"),
            ("y", "yank"),
            ("wq", "wq"),
        ] {
            assert_eq!(resolve_name(typed), Some(want), "{typed}");
        }
        assert_eq!(resolve_name("frob"), None);
    }

    #[test]
    fn test_command_name_exceptions() {
        let n = command_name("ka").unwrap();
        assert_eq!((n.canon, n.split_off), ("mark", "a"));
        let n = command_name("dp").unwrap();
        assert_eq!((n.canon, n.split_off, n.split_flags), ("delete", "p", true));
        let n = command_name("dell").unwrap();
        assert_eq!((n.canon, n.split_off), ("delete", "l"));
        let n = command_name("sg").unwrap();
        assert_eq!((n.canon, n.split_off), ("substitute", "g"));
    }

    #[test]
    fn test_count_and_flags() {
        assert_eq!(count_and_flags("22p").0, Some(22));
        let (_, f, rest) = count_and_flags(" 2 #l+");
        assert!(f.number && f.list && f.offset == 1 && rest.is_empty());
        assert_eq!(count_and_flags("x").2, "x");
    }

    #[test]
    fn test_split_line() {
        let cmds =
            |l: &str| -> Vec<String> { split_line(l).into_iter().map(|s| s.command).collect() };
        assert_eq!(cmds("1p|2p"), ["1p", "2p"]);
        assert_eq!(cmds("s/a|b/c|d/g|p"), ["s/a|b/c|d/g", "p"]);
        assert_eq!(cmds("g/x/s/a/b/|p"), ["g/x/s/a/b/|p"]);
        assert_eq!(cmds("!echo a|tr a b"), ["!echo a|tr a b"]);
        assert_eq!(cmds("r !ls|wc"), ["r !ls|wc"]);
        assert_eq!(cmds("w a\\|b|q"), ["w a|b", "q"]);
        assert_eq!(cmds("map Q 1p\x16|2p|q"), ["map Q 1p\x16|2p", "q"]);
        assert_eq!(cmds("1p \" comment"), ["1p "]);
        assert_eq!(cmds("\" whole line"), [""]);
        assert_eq!(cmds(":::2p"), ["2p"]);
        let segs = split_line("1a|text|more");
        assert_eq!(segs[0].text.as_deref(), Some("text|more"));
    }
}
