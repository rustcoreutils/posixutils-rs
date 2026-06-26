//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use plib::diag;
use plib::io::input_stream_dashed;
use plib::locale::strcoll;
use std::cmp::Ordering;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// join - relational database operator
#[derive(Parser)]
#[command(version, about = gettext("join - relational database operator"))]
struct Args {
    /// Print unpairable lines from file FILENUM (1 or 2), in addition to joined output.
    #[arg(short = 'a', value_name = "FILENUM", action = clap::ArgAction::Append)]
    additional: Vec<String>,

    /// Replace empty output fields with the given string.
    #[arg(short = 'e', value_name = "STRING", action = clap::ArgAction::Append)]
    empty: Vec<String>,

    /// Output fields according to the given list of field specifiers.
    #[arg(short = 'o', value_name = "LIST", action = clap::ArgAction::Append)]
    order: Vec<String>,

    /// Use CHAR as the input and output field separator.
    #[arg(short = 't', value_name = "CHAR")]
    separator: Option<String>,

    /// Print only unpairable lines from file FILENUM (1 or 2).
    #[arg(short = 'v', value_name = "FILENUM", action = clap::ArgAction::Append)]
    unpairable: Vec<String>,

    /// Join on the given field of file 1 (1-based).
    #[arg(short = '1', value_name = "FIELD")]
    field1: Option<String>,

    /// Join on the given field of file 2 (1-based).
    #[arg(short = '2', value_name = "FIELD")]
    field2: Option<String>,

    /// File 1 (use '-' for standard input).
    file1: PathBuf,

    /// File 2 (use '-' for standard input).
    file2: PathBuf,
}

/// A field specifier from an `-o` list.
enum OutSpec {
    /// `0`: the join field.
    JoinField,
    /// `filenum.fieldnum`: a specific field of file 1 or 2 (field is 0-based).
    Field { file: u8, field: usize },
}

/// A parsed input line, split into its fields.
struct Line {
    fields: Vec<String>,
}

/// Buffered reader over one input file's lines.
struct Reader {
    lines: Vec<String>,
    pos: usize,
}

/// Mutable processing state shared across the merge.
struct State {
    jf: [usize; 2],          // 0-based join field per file
    names: [String; 2],      // operand display names
    sep: Option<char>,       // None => default <blank> separator
    out_sep: String,         // output field separator
    empty: Option<String>,   // -e replacement string
    outlist: Vec<OutSpec>,   // -o list (empty => default output format)
    print_pairables: bool,   // print joined lines
    print_unpair: [bool; 2], // -a / -v per file
    // Order-checking state (mirrors GNU join).
    seen_unpairable: bool,
    issued: [bool; 2],
    line_no: [usize; 2],
    prev_key: [Option<String>; 2],
    any_disorder: bool,
}

/// Error raised during argument parsing or processing; carries a diagnostic
/// message already suitable for `plib::diag::error`.
struct JoinError(String);

fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

/// Split a line into fields using the default <blank> separator rules: leading
/// blanks are ignored, runs of blanks collapse to a single separator, and a
/// non-empty line that ends with a blank run yields one trailing empty field.
/// An empty line, or a line consisting solely of blanks, yields no fields.
/// Mirrors GNU coreutils `join` 9.4 `xfields()` for the default separator.
fn split_default(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    if s.is_empty() {
        return out;
    }
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len();

    // Skip leading blanks; a line of only blanks has no fields.
    let mut ptr = 0usize;
    while ptr < n && is_blank(chars[ptr]) {
        ptr += 1;
    }
    if ptr == n {
        return out;
    }

    loop {
        let mut sep = ptr + 1;
        while sep < n && !is_blank(chars[sep]) {
            sep += 1;
        }
        out.push(chars[ptr..sep].iter().collect());
        if sep == n {
            return out;
        }
        // Skip the run of separating blanks.
        ptr = sep + 1;
        while ptr < n && is_blank(chars[ptr]) {
            ptr += 1;
        }
        if ptr == n {
            break;
        }
    }
    // The line ended with a blank run: emit the trailing empty field.
    out.push(String::new());
    out
}

/// Split a line into fields. With an explicit separator, an empty line yields
/// no fields and a non-empty line is split on the separator character
/// (adjacent separators produce empty fields).
fn split_fields(s: &str, sep: Option<char>) -> Vec<String> {
    match sep {
        Some(c) => {
            if s.is_empty() {
                Vec::new()
            } else {
                s.split(c).map(|f| f.to_string()).collect()
            }
        }
        None => split_default(s),
    }
}

fn key_of(fields: &[String], jf: usize) -> String {
    fields.get(jf).cloned().unwrap_or_default()
}

/// Compare two join keys using LC_COLLATE. An empty (or missing) key collates
/// less than any non-empty key; two empty keys are equal.
fn keycmp(a: &str, b: &str) -> Ordering {
    strcoll(a, b)
}

/// Read the next line from `reader`, updating order-checking state. Returns the
/// parsed line, or `None` at end of input.
fn get_line(reader: &mut Reader, which: usize, st: &mut State) -> Option<Line> {
    if reader.pos >= reader.lines.len() {
        return None;
    }
    let text = reader.lines[reader.pos].clone();
    reader.pos += 1;
    st.line_no[which] += 1;

    let fields = split_fields(&text, st.sep);
    let key = key_of(&fields, st.jf[which]);

    if let Some(prev) = &st.prev_key[which] {
        if st.seen_unpairable && !st.issued[which] && keycmp(prev, &key) == Ordering::Greater {
            diag::error(&format!(
                "{}:{}: is not sorted: {}",
                st.names[which], st.line_no[which], text
            ));
            st.issued[which] = true;
            st.any_disorder = true;
        }
    }
    st.prev_key[which] = Some(key);

    Some(Line { fields })
}

/// Append the next line of `reader` to `seq`. Returns false at end of input.
fn getseq(reader: &mut Reader, seq: &mut Vec<Line>, which: usize, st: &mut State) -> bool {
    match get_line(reader, which, st) {
        Some(l) => {
            seq.push(l);
            true
        }
        None => false,
    }
}

/// Read the next line of `reader` as the sole element of `seq`.
fn advance_first(reader: &mut Reader, seq: &mut Vec<Line>, which: usize, st: &mut State) {
    seq.clear();
    getseq(reader, seq, which, st);
}

/// Write field `idx` of `line` (or the empty-field replacement). `line` is
/// `None` for an absent (unpaired) side.
fn prfield<W: Write>(line: Option<&Line>, idx: usize, st: &State, out: &mut W) -> io::Result<()> {
    match line.and_then(|l| l.fields.get(idx)) {
        Some(s) if !s.is_empty() => out.write_all(s.as_bytes()),
        _ => {
            if let Some(e) = &st.empty {
                out.write_all(e.as_bytes())
            } else {
                Ok(())
            }
        }
    }
}

/// Write all fields of `line` other than its join field, each preceded by the
/// output separator. No-op for an absent side.
fn prfields<W: Write>(
    line: Option<&Line>,
    join_field: usize,
    st: &State,
    out: &mut W,
) -> io::Result<()> {
    if let Some(l) = line {
        let n = l.fields.len();
        for i in 0..join_field.min(n) {
            out.write_all(st.out_sep.as_bytes())?;
            prfield(Some(l), i, st, out)?;
        }
        for i in (join_field + 1)..n {
            out.write_all(st.out_sep.as_bytes())?;
            prfield(Some(l), i, st, out)?;
        }
    }
    Ok(())
}

/// Print one joined output record. Either side may be `None` (unpaired).
fn prjoin<W: Write>(
    line1: Option<&Line>,
    line2: Option<&Line>,
    st: &State,
    out: &mut W,
) -> io::Result<()> {
    if !st.outlist.is_empty() {
        for (i, o) in st.outlist.iter().enumerate() {
            if i > 0 {
                out.write_all(st.out_sep.as_bytes())?;
            }
            let (line, field) = match o {
                OutSpec::JoinField => {
                    if line1.is_none() {
                        (line2, st.jf[1])
                    } else {
                        (line1, st.jf[0])
                    }
                }
                OutSpec::Field { file, field } => (if *file == 1 { line1 } else { line2 }, *field),
            };
            prfield(line, field, st, out)?;
        }
    } else {
        let (line, field) = if line1.is_none() {
            (line2, st.jf[1])
        } else {
            (line1, st.jf[0])
        };
        prfield(line, field, st, out)?;
        prfields(line1, st.jf[0], st, out)?;
        prfields(line2, st.jf[1], st, out)?;
    }
    out.write_all(b"\n")
}

/// Core merge-join. Mirrors GNU join's `join()` so order-checking and
/// unpairable bookkeeping match byte-for-byte.
fn merge_join<W: Write>(
    r1: &mut Reader,
    r2: &mut Reader,
    st: &mut State,
    out: &mut W,
) -> io::Result<()> {
    let mut seq1: Vec<Line> = Vec::new();
    let mut seq2: Vec<Line> = Vec::new();

    getseq(r1, &mut seq1, 0, st);
    getseq(r2, &mut seq2, 1, st);

    while !seq1.is_empty() && !seq2.is_empty() {
        let diff = keycmp(
            &key_of(&seq1[0].fields, st.jf[0]),
            &key_of(&seq2[0].fields, st.jf[1]),
        );
        if diff == Ordering::Less {
            if st.print_unpair[0] {
                prjoin(Some(&seq1[0]), None, st, out)?;
            }
            advance_first(r1, &mut seq1, 0, st);
            st.seen_unpairable = true;
            continue;
        }
        if diff == Ordering::Greater {
            if st.print_unpair[1] {
                prjoin(None, Some(&seq2[0]), st, out)?;
            }
            advance_first(r2, &mut seq2, 1, st);
            st.seen_unpairable = true;
            continue;
        }

        // Matching keys: read the full group from each file (plus one overflow
        // line that begins the next group, unless EOF).
        let k2 = key_of(&seq2[0].fields, st.jf[1]);
        let mut eof1 = false;
        loop {
            if !getseq(r1, &mut seq1, 0, st) {
                eof1 = true;
                break;
            }
            let last = key_of(&seq1[seq1.len() - 1].fields, st.jf[0]);
            if keycmp(&last, &k2) != Ordering::Equal {
                break;
            }
        }

        let k1 = key_of(&seq1[0].fields, st.jf[0]);
        let mut eof2 = false;
        loop {
            if !getseq(r2, &mut seq2, 1, st) {
                eof2 = true;
                break;
            }
            let last = key_of(&seq2[seq2.len() - 1].fields, st.jf[1]);
            if keycmp(&k1, &last) != Ordering::Equal {
                break;
            }
        }

        let n1 = if eof1 { seq1.len() } else { seq1.len() - 1 };
        let n2 = if eof2 { seq2.len() } else { seq2.len() - 1 };
        if st.print_pairables {
            for l1 in &seq1[0..n1] {
                for l2 in &seq2[0..n2] {
                    prjoin(Some(l1), Some(l2), st, out)?;
                }
            }
        }

        if eof1 {
            seq1.clear();
        } else {
            let last = seq1.pop().unwrap();
            seq1.clear();
            seq1.push(last);
        }
        if eof2 {
            seq2.clear();
        } else {
            let last = seq2.pop().unwrap();
            seq2.clear();
            seq2.push(last);
        }
    }

    // Drain the tail of whichever file still has lines: emit any remaining
    // unpairable lines and finish order-checking.
    let checktail = !(st.issued[0] && st.issued[1]);

    if (st.print_unpair[0] || checktail) && !seq1.is_empty() {
        if st.print_unpair[0] {
            prjoin(Some(&seq1[0]), None, st, out)?;
        }
        if !seq2.is_empty() {
            st.seen_unpairable = true;
        }
        while let Some(line) = get_line(r1, 0, st) {
            if st.print_unpair[0] {
                prjoin(Some(&line), None, st, out)?;
            }
            if st.issued[0] && !st.print_unpair[0] {
                break;
            }
        }
    }

    if (st.print_unpair[1] || checktail) && !seq2.is_empty() {
        if st.print_unpair[1] {
            prjoin(None, Some(&seq2[0]), st, out)?;
        }
        if !seq1.is_empty() {
            st.seen_unpairable = true;
        }
        while let Some(line) = get_line(r2, 1, st) {
            if st.print_unpair[1] {
                prjoin(None, Some(&line), st, out)?;
            }
            if st.issued[1] && !st.print_unpair[1] {
                break;
            }
        }
    }

    Ok(())
}

fn parse_field_number(s: &str) -> Result<usize, JoinError> {
    match s.parse::<usize>() {
        Ok(v) if v >= 1 => Ok(v),
        _ => Err(JoinError(format!(
            "{} '{}'",
            gettext("invalid field number:"),
            s
        ))),
    }
}

/// Parse one `-o` field specifier into an `OutSpec`.
fn decode_field_spec(s: &str) -> Result<OutSpec, JoinError> {
    let mut chars = s.chars();
    match chars.next() {
        Some('0') => {
            if s.len() == 1 {
                Ok(OutSpec::JoinField)
            } else {
                Err(JoinError(format!(
                    "{} '{}'",
                    gettext("invalid field specifier:"),
                    s
                )))
            }
        }
        Some(c @ ('1' | '2')) => {
            let rest = &s[1..];
            if !rest.starts_with('.') {
                return Err(JoinError(format!(
                    "{} '{}'",
                    gettext("invalid field specifier:"),
                    s
                )));
            }
            let field = parse_field_number(&rest[1..])?;
            Ok(OutSpec::Field {
                file: (c as u8) - b'0',
                field: field - 1,
            })
        }
        _ => Err(JoinError(format!(
            "{} '{}'",
            gettext("invalid file number in field spec:"),
            s
        ))),
    }
}

/// Parse all `-o` arguments (comma- and/or blank-separated, possibly across
/// multiple `-o` options) into the output list.
fn parse_outlist(args: &[String]) -> Result<Vec<OutSpec>, JoinError> {
    let mut out = Vec::new();
    for arg in args {
        for spec in arg.split([',', ' ', '\t']) {
            if spec.is_empty() {
                continue;
            }
            out.push(decode_field_spec(spec)?);
        }
    }
    Ok(out)
}

/// Parse an `-a`/`-v` file-number argument (must be 1 or 2).
fn parse_filenum(s: &str) -> Result<usize, JoinError> {
    match s {
        "1" => Ok(0),
        "2" => Ok(1),
        _ => Err(JoinError(format!(
            "{} '{}'",
            gettext("invalid field number:"),
            s
        ))),
    }
}

fn is_stdin(p: &Path) -> bool {
    let s = p.as_os_str();
    s.is_empty() || s == "-"
}

fn read_lines(path: &Path) -> Result<Vec<String>, JoinError> {
    let mut buf = String::new();
    input_stream_dashed(path)
        .and_then(|mut r| r.read_to_string(&mut buf))
        .map_err(|e| JoinError(format!("{}: {}", path.display(), strip_os_error(&e))))?;
    if buf.is_empty() {
        return Ok(Vec::new());
    }
    let mut lines: Vec<String> = buf.split('\n').map(|s| s.to_string()).collect();
    if buf.ends_with('\n') {
        lines.pop();
    }
    Ok(lines)
}

/// Render an `io::Error` without the trailing "(os error N)" that Rust appends,
/// matching the diagnostic style of system utilities.
fn strip_os_error(e: &io::Error) -> String {
    let s = e.to_string();
    match s.find(" (os error ") {
        Some(idx) => s[..idx].to_string(),
        None => s,
    }
}

fn run(args: Args) -> Result<bool, JoinError> {
    // Separator.
    let sep: Option<char> = match &args.separator {
        Some(s) => {
            let mut it = s.chars();
            match (it.next(), it.next()) {
                (Some(c), None) => Some(c),
                _ => {
                    return Err(JoinError(format!(
                        "{} '{}'",
                        gettext("multi-character tab"),
                        s
                    )))
                }
            }
        }
        None => None,
    };
    let out_sep = match sep {
        Some(c) => c.to_string(),
        None => " ".to_string(),
    };

    // Join fields.
    let jf1 = match &args.field1 {
        Some(s) => parse_field_number(s)? - 1,
        None => 0,
    };
    let jf2 = match &args.field2 {
        Some(s) => parse_field_number(s)? - 1,
        None => 0,
    };

    // -e (conflicting strings are an error).
    let mut empty: Option<String> = None;
    for e in &args.empty {
        if let Some(prev) = &empty {
            if prev != e {
                return Err(JoinError(gettext(
                    "conflicting empty-field replacement strings",
                )));
            }
        }
        empty = Some(e.clone());
    }

    // -o list.
    let outlist = parse_outlist(&args.order)?;

    // -a / -v.
    let mut print_unpair = [false; 2];
    let mut print_pairables = true;
    for a in &args.additional {
        print_unpair[parse_filenum(a)?] = true;
    }
    for v in &args.unpairable {
        print_pairables = false;
        print_unpair[parse_filenum(v)?] = true;
    }

    // Both operands being standard input is an error.
    if is_stdin(&args.file1) && is_stdin(&args.file2) {
        return Err(JoinError(gettext("both files cannot be standard input")));
    }

    let name1 = display_name(&args.file1);
    let name2 = display_name(&args.file2);

    let mut r1 = Reader {
        lines: read_lines(&args.file1)?,
        pos: 0,
    };
    let mut r2 = Reader {
        lines: read_lines(&args.file2)?,
        pos: 0,
    };

    let mut st = State {
        jf: [jf1, jf2],
        names: [name1, name2],
        sep,
        out_sep,
        empty,
        outlist,
        print_pairables,
        print_unpair,
        seen_unpairable: false,
        issued: [false; 2],
        line_no: [0; 2],
        prev_key: [None, None],
        any_disorder: false,
    };

    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());
    merge_join(&mut r1, &mut r2, &mut st, &mut out).map_err(|e| JoinError(strip_os_error(&e)))?;
    out.flush().map_err(|e| JoinError(strip_os_error(&e)))?;

    Ok(st.any_disorder)
}

fn display_name(p: &Path) -> String {
    if is_stdin(p) {
        "-".to_string()
    } else {
        p.display().to_string()
    }
}

fn main() -> ExitCode {
    diag::init_locale("join");

    let args = Args::parse();

    match run(args) {
        Ok(false) => ExitCode::SUCCESS,
        Ok(true) => {
            // Disorder was detected and warned about during processing.
            diag::error(&gettext("input is not in sorted order"));
            ExitCode::FAILURE
        }
        Err(JoinError(msg)) => {
            diag::error(&msg);
            ExitCode::FAILURE
        }
    }
}
