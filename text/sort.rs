//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cmp::Ordering;
use std::ffi::CStr;
use std::{
    fs::File,
    io::{self, BufRead, BufWriter, Write},
    path::PathBuf,
};

use plib::io::input_stream_dashed;
use plib::locale;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

/// sort - sort, merge, or sequence check text files
#[derive(Parser)]
#[command(version, about = gettext("sort - sort, merge, or sequence check text files"))]
struct Args {
    #[arg(short = 'c', help = gettext("Check that the single input file is ordered as specified"))]
    check_order: bool,

    #[arg(short = 'C', help = gettext("Same as -c, but without warning message for disorder or duplicate keys"))]
    check_order_silent: bool,

    #[arg(short = 'm', help = gettext("Merge only; the input files shall be assumed to be already sorted"))]
    merge_only: bool,

    #[arg(short = 'o', help = gettext("Specify the name of an output file to be used instead of the standard output"))]
    output_file: Option<PathBuf>,

    #[arg(short = 'u', help = gettext("Unique: suppress all but one in each set of lines having equal keys"))]
    unique: bool,

    #[arg(short = 'd', help = gettext("Dictionary order: only blank and alphanumeric characters are significant"))]
    dictionary_order: bool,

    #[arg(short = 'f', help = gettext("Fold lowercase to uppercase for comparison"))]
    fold_case: bool,

    #[arg(short = 'i', help = gettext("Ignore all characters that are non-printable"))]
    ignore_nonprintable: bool,

    #[arg(short = 'n', help = gettext("Restrict the sort key to an initial numeric string"))]
    numeric_sort: bool,

    #[arg(short = 'r', help = gettext("Reverse the sense of comparisons"))]
    reverse: bool,

    #[arg(short = 'b', help = gettext("Ignore leading blank characters in sort keys"))]
    ignore_leading_blanks: bool,

    #[arg(short = 't', help = gettext("Specify the field separator character"))]
    field_separator: Option<char>,

    #[arg(short = 'k', help = gettext("Specify the key definition for sorting"))]
    key_definition: Vec<String>,

    #[arg(help = gettext("Input files"))]
    filenames: Vec<PathBuf>,
}

impl Args {
    fn validate_args(&self) -> Result<(), String> {
        // POSIX: -c and -C cannot be combined; -c/-C cannot be combined with -m.
        if self.check_order && self.check_order_silent {
            return Err("options '-c' and '-C' cannot be used together".to_string());
        }
        if (self.check_order || self.check_order_silent) && self.merge_only {
            return Err("options '-c'/'-C' and '-m' cannot be used together".to_string());
        }
        Ok(())
    }
}

/// One parsed `-k` key (or the implicit whole-line key).
///
/// Field and character positions are 1-based, matching the `-k F.C` notation.
#[derive(Clone)]
struct KeySpec {
    start_field: usize,
    start_char: usize,
    /// `None` means "to end of line" (no end position given).
    end_field: Option<usize>,
    /// `None` means "to end of field". Only meaningful when `end_field` is set.
    end_char: Option<usize>,
    numeric: bool,
    reverse: bool,
    fold_case: bool,
    dictionary: bool,
    ignore_nonprintable: bool,
    /// Skip leading blanks at the start position (`b` on the start key, or global `-b`).
    start_blanks: bool,
    /// Skip leading blanks at the end position (`b` on the end key, or global `-b`).
    end_blanks: bool,
}

/// A precomputed numeric value, retained with arbitrary precision so values
/// beyond `2^53` compare exactly (unlike an `f64`).
#[derive(Clone)]
struct ParsedNum {
    negative: bool,
    /// Integer digits with leading zeros stripped.
    int: Vec<char>,
    /// Fraction digits with trailing zeros stripped.
    frac: Vec<char>,
    is_zero: bool,
}

/// A precomputed comparison value for one key.
#[derive(Clone)]
enum KeyVal {
    Num(ParsedNum),
    Text(String),
}

/// A line plus its precomputed per-key comparison values.
struct Record {
    line: String,
    keys: Vec<KeyVal>,
}

/// Locale-driven numeric formatting: (decimal point, optional thousands separator).
fn numeric_conv() -> (char, Option<char>) {
    // SAFETY: localeconv returns a pointer to a static, locale-owned struct.
    unsafe {
        let lc = libc::localeconv();
        if lc.is_null() {
            return ('.', None);
        }
        let decimal = {
            let dp = (*lc).decimal_point;
            if dp.is_null() {
                '.'
            } else {
                CStr::from_ptr(dp)
                    .to_str()
                    .ok()
                    .and_then(|s| s.chars().next())
                    .unwrap_or('.')
            }
        };
        let thousands = {
            let ts = (*lc).thousands_sep;
            if ts.is_null() {
                None
            } else {
                CStr::from_ptr(ts)
                    .to_str()
                    .ok()
                    .and_then(|s| s.chars().next())
            }
        };
        (decimal, thousands)
    }
}

fn is_blank(c: char) -> bool {
    locale::isblank(c)
}

/// Offset (in chars) of the start of field `field` (1-based), including any
/// leading blanks belonging to the field (default separator) or following the
/// preceding separator (explicit `-t`).
fn field_start(chars: &[char], field: usize, sep: Option<char>) -> usize {
    let len = chars.len();
    let mut i = 0;
    match sep {
        Some(t) => {
            for _ in 1..field {
                while i < len && chars[i] != t {
                    i += 1;
                }
                if i < len {
                    i += 1;
                }
            }
        }
        None => {
            for _ in 1..field {
                while i < len && is_blank(chars[i]) {
                    i += 1;
                }
                while i < len && !is_blank(chars[i]) {
                    i += 1;
                }
            }
        }
    }
    i
}

/// Offset (in chars) just past the non-blank content of field `field`.
fn content_end(chars: &[char], field: usize, sep: Option<char>) -> usize {
    let len = chars.len();
    let mut i = field_start(chars, field, sep);
    match sep {
        Some(t) => {
            while i < len && chars[i] != t {
                i += 1;
            }
        }
        None => {
            while i < len && is_blank(chars[i]) {
                i += 1;
            }
            while i < len && !is_blank(chars[i]) {
                i += 1;
            }
        }
    }
    i
}

/// Extract the raw key substring described by `key` from `chars`.
fn extract_key(chars: &[char], key: &KeySpec, sep: Option<char>) -> String {
    let len = chars.len();

    let mut start = field_start(chars, key.start_field, sep);
    if key.start_blanks {
        while start < len && is_blank(chars[start]) {
            start += 1;
        }
    }
    start = (start + key.start_char - 1).min(len);

    let end = match key.end_field {
        None => len,
        Some(ef) => match key.end_char {
            None => content_end(chars, ef, sep),
            Some(ec) => {
                let mut base = field_start(chars, ef, sep);
                if key.end_blanks {
                    while base < len && is_blank(chars[base]) {
                        base += 1;
                    }
                }
                (base + ec).min(len)
            }
        },
    };

    if end <= start {
        String::new()
    } else {
        chars[start..end].iter().collect()
    }
}

/// Apply dictionary (`-d`), ignore-nonprintable (`-i`), and fold (`-f`)
/// transforms, per `LC_CTYPE`.
fn transform_text(s: &str, key: &KeySpec) -> String {
    if !key.dictionary && !key.ignore_nonprintable && !key.fold_case {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if key.dictionary && !(is_blank(c) || locale::isalnum(c)) {
            continue;
        }
        if key.ignore_nonprintable && !locale::isprint(c) {
            continue;
        }
        if key.fold_case {
            out.push(locale::to_upper(c));
        } else {
            out.push(c);
        }
    }
    out
}

/// Parse a numeric value from the start of `s` (POSIX `-n` semantics):
/// optional leading blanks, optional sign, integer digits, an optional radix
/// point, and fraction digits. Non-numeric / empty input parses as 0.
fn parse_num(s: &str, conv: (char, Option<char>)) -> ParsedNum {
    let (decimal, thousands) = conv;
    let mut it = s.chars().peekable();

    while let Some(&c) = it.peek() {
        if is_blank(c) {
            it.next();
        } else {
            break;
        }
    }

    let mut negative = false;
    match it.peek() {
        Some('-') => {
            negative = true;
            it.next();
        }
        Some('+') => {
            it.next();
        }
        _ => {}
    }

    let mut int = Vec::new();
    while let Some(&c) = it.peek() {
        if c.is_ascii_digit() {
            int.push(c);
            it.next();
        } else if Some(c) == thousands {
            it.next();
        } else {
            break;
        }
    }

    let mut frac = Vec::new();
    if it.peek() == Some(&decimal) {
        it.next();
        while let Some(&c) = it.peek() {
            if c.is_ascii_digit() {
                frac.push(c);
                it.next();
            } else {
                break;
            }
        }
    }

    // Normalize: drop leading integer zeros and trailing fraction zeros.
    let first_nonzero = int.iter().position(|&c| c != '0').unwrap_or(int.len());
    int.drain(..first_nonzero);
    while frac.last() == Some(&'0') {
        frac.pop();
    }

    let is_zero = int.is_empty() && frac.is_empty();
    if is_zero {
        negative = false;
    }
    ParsedNum {
        negative,
        int,
        frac,
        is_zero,
    }
}

fn cmp_frac(a: &[char], b: &[char]) -> Ordering {
    let n = a.len().max(b.len());
    for i in 0..n {
        let da = a.get(i).copied().unwrap_or('0');
        let db = b.get(i).copied().unwrap_or('0');
        match da.cmp(&db) {
            Ordering::Equal => continue,
            o => return o,
        }
    }
    Ordering::Equal
}

fn cmp_magnitude(a: &ParsedNum, b: &ParsedNum) -> Ordering {
    match a.int.len().cmp(&b.int.len()) {
        Ordering::Equal => match a.int.cmp(&b.int) {
            Ordering::Equal => cmp_frac(&a.frac, &b.frac),
            o => o,
        },
        o => o,
    }
}

fn cmp_num(a: &ParsedNum, b: &ParsedNum) -> Ordering {
    if a.is_zero && b.is_zero {
        return Ordering::Equal;
    }
    match (a.negative, b.negative) {
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        (false, false) => cmp_magnitude(a, b),
        (true, true) => cmp_magnitude(a, b).reverse(),
    }
}

fn compare_keyval(a: &KeyVal, b: &KeyVal) -> Ordering {
    match (a, b) {
        (KeyVal::Num(x), KeyVal::Num(y)) => cmp_num(x, y),
        (KeyVal::Text(x), KeyVal::Text(y)) => locale::strcoll(x, y),
        _ => Ordering::Equal,
    }
}

/// Compare two records by the key list. When `last_resort` is true and all keys
/// compare equal, fall back to a byte-wise whole-line comparison (reversed by
/// the global `-r`).
fn compare_records(
    a: &Record,
    b: &Record,
    keys: &[KeySpec],
    reverse_global: bool,
    last_resort: bool,
) -> Ordering {
    for (i, k) in keys.iter().enumerate() {
        let mut o = compare_keyval(&a.keys[i], &b.keys[i]);
        if k.reverse {
            o = o.reverse();
        }
        if o != Ordering::Equal {
            return o;
        }
    }
    if last_resort {
        // The whole-line last resort compares the entire lines using the
        // default (locale collation) comparison, falling back to byte order
        // only to break collation ties so the result is a total order. The
        // whole thing is reversed by the global `-r`.
        let mut o = locale::strcoll(&a.line, &b.line);
        if o == Ordering::Equal {
            o = a.line.as_bytes().cmp(b.line.as_bytes());
        }
        if reverse_global {
            o.reverse()
        } else {
            o
        }
    } else {
        Ordering::Equal
    }
}

/// Split a key spec field ("F[.C][mods]") into (field, optional char, modifiers).
fn parse_field_spec(s: &str) -> Result<(usize, Option<usize>, String), String> {
    // The numeric portion is the leading run of digits and '.'; the remainder
    // are single-letter modifiers.
    let split = s
        .char_indices()
        .find(|(_, c)| c.is_ascii_alphabetic())
        .map(|(i, _)| i)
        .unwrap_or(s.len());
    let (numpart, mods) = s.split_at(split);

    for c in mods.chars() {
        if !"bdfinr".contains(c) {
            return Err(format!("invalid modifier '{c}' in key specification"));
        }
    }

    let mut np = numpart.split('.');
    let field: usize = np.next().unwrap().parse().map_err(|e| format!("{e}"))?;
    let char_pos = match np.next() {
        Some(cs) => Some(cs.parse::<usize>().map_err(|e| format!("{e}"))?),
        None => None,
    };
    Ok((field, char_pos, mods.to_string()))
}

/// Parse one `-k` definition string into a `KeySpec`, OR-ing in global options.
fn parse_one_key(kdef: &str, args: &Args) -> Result<KeySpec, String> {
    if kdef.is_empty() {
        return Err("key must be non-empty".to_string());
    }
    let mut parts = kdef.splitn(2, ',');
    let start_str = parts.next().unwrap();
    let end_str = parts.next();

    let (sf, sc, smods) = parse_field_spec(start_str)?;
    if sf == 0 {
        return Err("the key can't be zero.".to_string());
    }
    let start_char = match sc {
        Some(0) | None => 1,
        Some(n) => n,
    };

    let (ef, ec, emods) = match end_str {
        Some(es) => {
            let (f, c, m) = parse_field_spec(es)?;
            if f == 0 {
                return Err("the key can't be zero.".to_string());
            }
            (Some(f), c, m)
        }
        None => (None, None, String::new()),
    };
    // An end char of 0 (or absent) means "to end of field".
    let end_char = ec.filter(|&c| c != 0);

    // POSIX: if a key carries ANY modifier letter (including `b`), the global
    // ordering options are ignored entirely for that key; otherwise the key
    // inherits all of them.
    let has_mods = !smods.is_empty() || !emods.is_empty();
    let has = |c: char| smods.contains(c) || emods.contains(c);
    let key = if has_mods {
        KeySpec {
            start_field: sf,
            start_char,
            end_field: ef,
            end_char,
            numeric: has('n'),
            reverse: has('r'),
            fold_case: has('f'),
            dictionary: has('d'),
            ignore_nonprintable: has('i'),
            start_blanks: smods.contains('b'),
            end_blanks: emods.contains('b'),
        }
    } else {
        KeySpec {
            start_field: sf,
            start_char,
            end_field: ef,
            end_char,
            numeric: args.numeric_sort,
            reverse: args.reverse,
            fold_case: args.fold_case,
            dictionary: args.dictionary_order,
            ignore_nonprintable: args.ignore_nonprintable,
            start_blanks: args.ignore_leading_blanks,
            end_blanks: args.ignore_leading_blanks,
        }
    };

    if let Some(ef) = ef {
        let before = ef < sf || (ef == sf && end_char.is_some_and(|e| e < start_char));
        if before {
            return Err("keys fields with end position before start!".to_string());
        }
    }

    Ok(key)
}

/// Build the key list. With no `-k`, a single implicit whole-line key carries
/// the global ordering options (including a global `-b`).
fn build_keys(args: &Args) -> Result<Vec<KeySpec>, String> {
    if args.key_definition.is_empty() {
        return Ok(vec![KeySpec {
            start_field: 1,
            start_char: 1,
            end_field: None,
            end_char: None,
            numeric: args.numeric_sort,
            reverse: args.reverse,
            fold_case: args.fold_case,
            dictionary: args.dictionary_order,
            ignore_nonprintable: args.ignore_nonprintable,
            start_blanks: args.ignore_leading_blanks,
            end_blanks: args.ignore_leading_blanks,
        }]);
    }
    args.key_definition
        .iter()
        .map(|k| parse_one_key(k, args))
        .collect()
}

fn make_record(
    line: String,
    keys: &[KeySpec],
    conv: (char, Option<char>),
    sep: Option<char>,
) -> Record {
    let chars: Vec<char> = line.chars().collect();
    let mut kvs = Vec::with_capacity(keys.len());
    for k in keys {
        let s = extract_key(&chars, k, sep);
        if k.numeric {
            kvs.push(KeyVal::Num(parse_num(&s, conv)));
        } else {
            kvs.push(KeyVal::Text(transform_text(&s, k)));
        }
    }
    Record { line, keys: kvs }
}

/// Read every input fully into memory before any output is produced (so an
/// output file that is also an input is not truncated before being read).
fn read_inputs(args: &Args) -> Result<Vec<(String, Vec<String>)>, String> {
    let files: Vec<PathBuf> = if args.filenames.is_empty() {
        vec![PathBuf::new()]
    } else {
        args.filenames.clone()
    };

    let mut out = Vec::with_capacity(files.len());
    for f in files {
        let s = f.as_os_str();
        let name = if s.is_empty() || s == "-" {
            "-".to_string()
        } else {
            f.display().to_string()
        };
        let reader = input_stream_dashed(&f).map_err(|e| format!("cannot read: {name}: {e}"))?;
        let br = io::BufReader::new(reader);
        let mut lines = Vec::new();
        for line in br.lines() {
            lines.push(line.map_err(|e| format!("read error: {name}: {e}"))?);
        }
        out.push((name, lines));
    }
    Ok(out)
}

fn write_output(records: &[Record], args: &Args) -> Result<(), String> {
    if let Some(path) = &args.output_file {
        let file =
            File::create(path).map_err(|e| format!("open failed: {}: {e}", path.display()))?;
        let mut w = BufWriter::new(file);
        for r in records {
            writeln!(w, "{}", r.line).map_err(|e| format!("write error: {e}"))?;
        }
        w.flush().map_err(|e| format!("write error: {e}"))?;
    } else {
        let stdout = io::stdout();
        let mut w = BufWriter::new(stdout.lock());
        for r in records {
            writeln!(w, "{}", r.line).map_err(|e| format!("write error: {e}"))?;
        }
        w.flush().map_err(|e| format!("write error: {e}"))?;
    }
    Ok(())
}

/// Sequentially scan the inputs for the first out-of-order pair (`-c`/`-C`).
/// Returns the exit status (0 ordered, 1 disordered). `-c` prints a diagnostic
/// matching GNU's `sort: FILE:LINE: disorder: TEXT`; `-C` is silent.
fn check_order(
    args: &Args,
    keys: &[KeySpec],
    conv: (char, Option<char>),
    sep: Option<char>,
    inputs: &[(String, Vec<String>)],
) -> i32 {
    let last_resort = !args.unique;
    let mut prev: Option<Record> = None;

    for (name, lines) in inputs {
        for (i, line) in lines.iter().enumerate() {
            let cur = make_record(line.clone(), keys, conv, sep);
            if let Some(p) = &prev {
                let o = compare_records(p, &cur, keys, args.reverse, last_resort);
                let bad = if args.unique {
                    o != Ordering::Less
                } else {
                    o == Ordering::Greater
                };
                if bad {
                    if args.check_order {
                        eprintln!("sort: {}:{}: disorder: {}", name, i + 1, cur.line);
                    }
                    return 1;
                }
            }
            prev = Some(cur);
        }
    }
    0
}

fn run(args: &Args) -> Result<i32, String> {
    args.validate_args()?;

    let keys = build_keys(args)?;
    let conv = numeric_conv();
    let sep = args.field_separator;
    let inputs = read_inputs(args)?;

    if args.check_order || args.check_order_silent {
        return Ok(check_order(args, &keys, conv, sep, &inputs));
    }

    // With -u the sort omits the last-resort whole-line tiebreak so equal-key
    // lines keep input order (a stable sort), and -u then keeps the first.
    let last_resort = !args.unique;

    let mut records: Vec<Record> = if args.merge_only {
        // Merge each (already sorted) input as a stream.
        let files: Vec<Vec<Record>> = inputs
            .iter()
            .map(|(_, lines)| {
                lines
                    .iter()
                    .map(|l| make_record(l.clone(), &keys, conv, sep))
                    .collect()
            })
            .collect();
        merge_records(files, &keys, args.reverse, last_resort)
    } else {
        let mut recs: Vec<Record> = inputs
            .iter()
            .flat_map(|(_, lines)| {
                lines
                    .iter()
                    .map(|l| make_record(l.clone(), &keys, conv, sep))
            })
            .collect();
        recs.sort_by(|a, b| compare_records(a, b, &keys, args.reverse, last_resort));
        recs
    };

    if args.unique {
        records
            .dedup_by(|a, b| compare_records(a, b, &keys, args.reverse, false) == Ordering::Equal);
    }

    write_output(&records, args)?;
    Ok(0)
}

/// Consume per-file sorted record lists and produce a single merged list via a
/// stable k-way merge.
fn merge_records(
    mut files: Vec<Vec<Record>>,
    keys: &[KeySpec],
    reverse: bool,
    last_resort: bool,
) -> Vec<Record> {
    let mut idx = vec![0usize; files.len()];
    let total: usize = files.iter().map(|f| f.len()).sum();
    let mut out: Vec<Record> = Vec::with_capacity(total);
    loop {
        let mut best: Option<usize> = None;
        for f in 0..files.len() {
            if idx[f] >= files[f].len() {
                continue;
            }
            match best {
                None => best = Some(f),
                Some(b) => {
                    if compare_records(
                        &files[f][idx[f]],
                        &files[b][idx[b]],
                        keys,
                        reverse,
                        last_resort,
                    ) == Ordering::Less
                    {
                        best = Some(f);
                    }
                }
            }
        }
        match best {
            None => break,
            Some(f) => {
                let i = idx[f];
                let rec = std::mem::replace(
                    &mut files[f][i],
                    Record {
                        line: String::new(),
                        keys: Vec::new(),
                    },
                );
                out.push(rec);
                idx[f] += 1;
            }
        }
    }
    out
}

fn main() {
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    let code = match run(&args) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("sort: {e}");
            2
        }
    };
    std::process::exit(code);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeric_large_ints_exact() {
        let conv = ('.', None);
        let a = parse_num("99999999999999999999", conv);
        let b = parse_num("100000000000000000000", conv);
        assert_eq!(cmp_num(&a, &b), Ordering::Less);
    }

    #[test]
    fn numeric_negatives_and_zero() {
        let conv = ('.', None);
        let neg = parse_num("-5", conv);
        let zero = parse_num("0", conv);
        let pos = parse_num("3", conv);
        assert_eq!(cmp_num(&neg, &zero), Ordering::Less);
        assert_eq!(cmp_num(&zero, &pos), Ordering::Less);
        assert_eq!(cmp_num(&neg, &pos), Ordering::Less);
        // -0 == 0
        assert_eq!(cmp_num(&parse_num("-0", conv), &zero), Ordering::Equal);
    }
}
