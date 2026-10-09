//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cell::RefCell;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, BufRead, Write as IoWrite};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use gettextrs::gettext;
use plib::modestr;

/// Match `string` against a shell filename pattern using POSIX `fnmatch(3)`,
/// the matching notation mandated for `-name`/`-iname`/`-path` (XBD filename
/// pattern matching), rather than a regular expression. `fold` requests a
/// case-insensitive match (`FNM_CASEFOLD`, for `-iname`).
///
/// Neither `-name` nor `-path` sets `FNM_PATHNAME`: POSIX `-path` explicitly
/// does not treat a `<slash>` specially, and `-name` only ever sees a basename.
fn fnmatch(pattern: &str, string: &str, fold: bool) -> bool {
    use std::ffi::CString;
    let (Ok(p), Ok(s)) = (CString::new(pattern), CString::new(string)) else {
        return false;
    };
    let flags = if fold { libc::FNM_CASEFOLD } else { 0 };
    unsafe { libc::fnmatch(p.as_ptr(), s.as_ptr(), flags) == 0 }
}

/// Symlink following mode
#[derive(Clone, Copy, Debug, Default, PartialEq)]
enum SymlinkMode {
    /// Default: never follow symlinks
    #[default]
    Never,
    /// -H: follow symlinks on command line only
    CommandLineOnly,
    /// -L: always follow symlinks
    Always,
}

/// Numeric comparison mode for primaries like -mtime, -size, etc.
#[derive(Clone, Debug, PartialEq)]
enum NumericComparison {
    /// Exactly n
    Equal(i64),
    /// Less than n
    LessThan(i64),
    /// Greater than n
    GreaterThan(i64),
}

impl NumericComparison {
    fn parse(s: &str) -> Result<Self, String> {
        if let Some(n) = s.strip_prefix('+') {
            let val = n
                .parse::<i64>()
                .map_err(|_| format!("invalid number: {}", s))?;
            Ok(NumericComparison::GreaterThan(val))
        } else if let Some(n) = s.strip_prefix('-') {
            let val = n
                .parse::<i64>()
                .map_err(|_| format!("invalid number: {}", s))?;
            Ok(NumericComparison::LessThan(val))
        } else {
            let val = s
                .parse::<i64>()
                .map_err(|_| format!("invalid number: {}", s))?;
            Ok(NumericComparison::Equal(val))
        }
    }

    fn matches(&self, value: i64) -> bool {
        match self {
            NumericComparison::Equal(n) => value == *n,
            NumericComparison::LessThan(n) => value < *n,
            NumericComparison::GreaterThan(n) => value > *n,
        }
    }
}

/// Permission matching mode for -perm
#[derive(Clone, Debug)]
enum PermMode {
    /// Exact match of permission bits
    Exact(u32),
    /// At least all these bits are set (prefixed with -)
    AtLeast(u32),
    /// Any of these bits is set, or no bits are given (prefixed with /): GNU
    /// extension, forced by debhelper (dh_shlibdeps `-perm /111`)
    Any(u32),
}

/// File types for -type primary
#[derive(Clone, Debug, PartialEq)]
enum FileTypeMatch {
    BlockDevice,
    CharDevice,
    Directory,
    Symlink,
    Fifo,
    Regular,
    Socket,
}

impl FileTypeMatch {
    fn from_char(c: char) -> Option<Self> {
        match c {
            'b' => Some(FileTypeMatch::BlockDevice),
            'c' => Some(FileTypeMatch::CharDevice),
            'd' => Some(FileTypeMatch::Directory),
            'l' => Some(FileTypeMatch::Symlink),
            'p' => Some(FileTypeMatch::Fifo),
            'f' => Some(FileTypeMatch::Regular),
            's' => Some(FileTypeMatch::Socket),
            _ => None,
        }
    }

    fn matches(&self, ft: &ftw::FileType) -> bool {
        match self {
            FileTypeMatch::BlockDevice => ft.is_block_device(),
            FileTypeMatch::CharDevice => ft.is_char_device(),
            FileTypeMatch::Directory => ft.is_dir(),
            FileTypeMatch::Symlink => ft.is_symlink(),
            FileTypeMatch::Fifo => ft.is_fifo(),
            FileTypeMatch::Regular => ft.is_file(),
            FileTypeMatch::Socket => ft.is_socket(),
        }
    }
}

/// Exec mode for -exec primary
#[derive(Clone, Debug)]
enum ExecMode {
    /// -exec ... ; - run once per file, {} replaced with pathname
    Single { utility: String, args: Vec<String> },
    /// -exec ... {} + - batch files, {} replaced with list of pathnames
    Batch {
        /// Index into `FindState::exec_batches`, stamped by
        /// [`register_exec_batches`] after parsing. Unique per `-exec ... {} +`
        /// primary: POSIX aggregates pathnames into sets *per primary*, so two
        /// textually identical primaries must not share a set.
        id: usize,
        utility: String,
        args_before: Vec<String>,
    },
}

/// One `-exec ... {} +` primary's aggregation set.
struct ExecBatch {
    utility: String,
    args_before: Vec<String>,
    files: Vec<PathBuf>,
}

/// A primary (test or action) in the find expression
#[derive(Clone, Debug)]
enum Primary {
    // Tests
    Name {
        pattern: String,
        fold: bool,
    },
    Path {
        pattern: String,
        fold: bool,
    },
    Type(FileTypeMatch),
    Perm(PermMode),
    Links(NumericComparison),
    // Resolved once at parse time; None means the name resolved to no id (never
    // matches), preserving the "unknown user/group → no match" behavior.
    User(Option<u32>),
    Group(Option<u32>),
    /// `-size`: the file size in `unit`-byte units, rounded up
    Size {
        cmp: NumericComparison,
        unit: u64,
    },
    ATime(NumericComparison),
    CTime(NumericComparison),
    MTime(NumericComparison),
    Newer(SystemTime),
    NoUser,
    NoGroup,
    /// `-true` / `-false`: GNU extensions, forced by debhelper (dh_fixperms,
    /// dh_compress)
    Const(bool),
    /// `-regex`: GNU extension, forced by debhelper (dh_md5sums, dh_fixperms,
    /// `-X`). Compiled from the Emacs syntax by [`emacs_regex_to_ere`].
    Regex(plib::regex::Regex),
    /// `-empty`: GNU extension, forced by debhelper (dh_install,
    /// dh_installdocs)
    Empty,
    /// `-executable`: GNU extension, forced by debhelper (dh_movelibkdeinit)
    Executable,

    // Actions
    Print,
    Print0,
    Printf(Vec<PrintfItem>),
    Prune,
    /// `-delete`: GNU extension, forced by debhelper (dh_doxygen,
    /// dh_autotools-dev_restoreconfig)
    Delete,
    Exec(ExecMode),
    Ok {
        utility: String,
        args: Vec<String>,
    },

    // Global options (always true, affect traversal)
    Depth,
    XDev,
    Mount,
    MinDepth(usize),
    MaxDepth(usize),
}

/// One piece of a `-printf` format. GNU extension, forced by debhelper; only
/// the directives its callers use are implemented.
#[derive(Clone, Debug)]
enum PrintfItem {
    /// Literal bytes, escapes already resolved
    Literal(Vec<u8>),
    /// `%p`: the pathname
    Path,
    /// `%P`: the pathname with its starting point removed
    RelativePath,
    /// `%s`: the size in bytes
    Size,
    /// `%T@`: the modification time in seconds since the Epoch
    MTimeEpoch,
}

/// Expression AST node
#[derive(Clone, Debug)]
enum Expr {
    Primary(Primary),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

/// Context for evaluating an expression against a file
struct EvalContext<'a, 'e> {
    /// Full path to the file. Printed, matched and passed to `-exec`, never
    /// resolved: every file-system operation goes through `entry`.
    path: &'a Path,
    /// The starting point (path operand) the file was found under
    root: &'a Path,
    /// The file as the walk found it: the descriptor of the directory that
    /// holds it, and its name in that directory
    entry: &'a ftw::Entry<'e>,
    /// Metadata (the symlink's own, or its target's under -H/-L)
    metadata: &'a ftw::Metadata,
    /// Whether the entry itself is a symbolic link
    is_symlink: bool,
    /// Initialization time (for -atime, -mtime, -ctime)
    init_time: SystemTime,
}

/// Result of evaluating an expression
struct EvalResult {
    /// Whether the expression evaluated to true
    matched: bool,
    /// Whether to prune this directory (not descend)
    prune: bool,
    /// Files to pass to batched -exec, each tagged with the id of the primary
    /// that matched it.
    exec_batch_files: Vec<(usize, PathBuf)>,
}

impl EvalResult {
    fn new(matched: bool) -> Self {
        Self {
            matched,
            prune: false,
            exec_batch_files: Vec::new(),
        }
    }
}

/// State for the find operation
struct FindState {
    /// Whether any error occurred
    had_error: bool,
    /// Whether a write to standard output failed (reported once)
    stdout_failed: bool,
    /// Whether -depth was specified anywhere in expression
    depth_first: bool,
    /// Whether -delete was specified anywhere in expression
    deletes: bool,
    /// Whether -xdev was specified anywhere in expression
    xdev: bool,
    /// Whether -mount was specified anywhere in expression
    mount: bool,
    /// `-mindepth`: entries shallower than this are walked but not evaluated
    min_depth: usize,
    /// `-maxdepth`: entries deeper than this are not walked
    max_depth: Option<usize>,
    /// -H / -L
    symlink_mode: SymlinkMode,
    /// Initialization time (for -atime, -mtime, -ctime)
    init_time: SystemTime,
    /// One entry per `-exec ... {} +` primary, in source order; index is the
    /// primary's stamped `id`.
    exec_batches: Vec<ExecBatch>,
}

impl FindState {
    fn new() -> Self {
        Self {
            had_error: false,
            stdout_failed: false,
            depth_first: false,
            deletes: false,
            xdev: false,
            mount: false,
            min_depth: 0,
            max_depth: None,
            symlink_mode: SymlinkMode::Never,
            init_time: SystemTime::now(),
            exec_batches: Vec::new(),
        }
    }
}

/// Parse command line arguments, returning (symlink_mode, paths, expression)
fn parse_args(args: &[String]) -> Result<(SymlinkMode, Vec<PathBuf>, Expr), String> {
    let mut symlink_mode = SymlinkMode::Never;
    let mut idx = 1; // skip program name

    // Parse options (-H, -L)
    while idx < args.len() {
        match args[idx].as_str() {
            "-H" => {
                symlink_mode = SymlinkMode::CommandLineOnly;
                idx += 1;
            }
            "-L" => {
                symlink_mode = SymlinkMode::Always;
                idx += 1;
            }
            _ => break,
        }
    }

    // Parse paths (until we hit an expression token)
    let mut paths = Vec::new();
    while idx < args.len() {
        let arg = &args[idx];
        // Expression starts with -, !, or (
        if arg.starts_with('-') || arg == "!" || arg == "(" {
            break;
        }
        paths.push(PathBuf::from(arg));
        idx += 1;
    }

    // Default path is current directory
    if paths.is_empty() {
        paths.push(PathBuf::from("."));
    }

    // Parse expression
    let expr_args: Vec<&str> = args[idx..].iter().map(|s| s.as_str()).collect();
    let expr = parse_expression(&expr_args)?;

    Ok((symlink_mode, paths, expr))
}

/// Parse an expression from arguments
fn parse_expression(args: &[&str]) -> Result<Expr, String> {
    if args.is_empty() {
        // Default expression is -print
        return Ok(Expr::Primary(Primary::Print));
    }

    let tokens = args.to_vec();
    let mut idx = 0;
    parse_or_expr(&tokens, &mut idx)
}

/// Is `tok` the OR operator? `-or` is GNU's spelling of `-o`, forced by
/// debhelper (dh_install, dh_installdocs, dh_shlibdeps, `-X` exclusions).
fn is_or(tok: &str) -> bool {
    tok == "-o" || tok == "-or"
}

/// Is `tok` the AND operator? `-and` is GNU's spelling of `-a`, forced by
/// debhelper (dh_install, dh_installdocs, dh_installexamples).
fn is_and(tok: &str) -> bool {
    tok == "-a" || tok == "-and"
}

/// Fail unless an operand follows operator `op`, whose operand would start
/// at `tokens[idx]`. The wording is GNU find's.
fn expect_operand(tokens: &[&str], idx: usize, op: &str) -> Result<(), String> {
    match tokens.get(idx) {
        None => Err(format!("expected an expression after '{op}'")),
        Some(&")") => Err(format!("expected an expression between '{op}' and ')'")),
        Some(&next) if is_or(next) || is_and(next) => Err(format!(
            "invalid expression; you have used a binary operator '{next}' with nothing before it."
        )),
        Some(_) => Ok(()),
    }
}

/// Parse OR expression (lowest precedence)
fn parse_or_expr(tokens: &[&str], idx: &mut usize) -> Result<Expr, String> {
    let mut left = parse_and_expr(tokens, idx)?;

    while *idx < tokens.len() && is_or(tokens[*idx]) {
        *idx += 1;
        expect_operand(tokens, *idx, tokens[*idx - 1])?;
        let right = parse_and_expr(tokens, idx)?;
        left = Expr::Or(Box::new(left), Box::new(right));
    }

    Ok(left)
}

/// Parse AND expression
fn parse_and_expr(tokens: &[&str], idx: &mut usize) -> Result<Expr, String> {
    let mut left = parse_unary_expr(tokens, idx)?;

    while *idx < tokens.len() {
        let tok = tokens[*idx];
        if is_or(tok) || tok == ")" {
            break;
        }
        if is_and(tok) {
            *idx += 1;
            expect_operand(tokens, *idx, tok)?;
        }
        // Implicit AND by juxtaposition
        if *idx >= tokens.len() || is_or(tokens[*idx]) || tokens[*idx] == ")" {
            break;
        }
        let right = parse_unary_expr(tokens, idx)?;
        left = Expr::And(Box::new(left), Box::new(right));
    }

    Ok(left)
}

/// Parse unary expression (NOT or primary)
fn parse_unary_expr(tokens: &[&str], idx: &mut usize) -> Result<Expr, String> {
    if *idx >= tokens.len() {
        return Err("unexpected end of expression".to_string());
    }

    if tokens[*idx] == "!" {
        *idx += 1;
        expect_operand(tokens, *idx, "!")?;
        let expr = parse_unary_expr(tokens, idx)?;
        return Ok(Expr::Not(Box::new(expr)));
    }

    if tokens[*idx] == "(" {
        *idx += 1;
        let expr = parse_or_expr(tokens, idx)?;
        if *idx >= tokens.len() || tokens[*idx] != ")" {
            return Err("missing closing parenthesis".to_string());
        }
        *idx += 1;
        return Ok(expr);
    }

    parse_primary(tokens, idx)
}

/// Parse a primary
fn parse_primary(tokens: &[&str], idx: &mut usize) -> Result<Expr, String> {
    if *idx >= tokens.len() {
        return Err("unexpected end of expression".to_string());
    }

    let tok = tokens[*idx];
    *idx += 1;

    match tok {
        "-name" => {
            let pattern = get_arg(tokens, idx, "-name")?;
            Ok(Expr::Primary(Primary::Name {
                pattern: pattern.to_string(),
                fold: false,
            }))
        }
        "-iname" => {
            let pattern = get_arg(tokens, idx, "-iname")?;
            Ok(Expr::Primary(Primary::Name {
                pattern: pattern.to_string(),
                fold: true,
            }))
        }
        "-path" => {
            let pattern = get_arg(tokens, idx, "-path")?;
            Ok(Expr::Primary(Primary::Path {
                pattern: pattern.to_string(),
                fold: false,
            }))
        }
        "-ipath" => {
            let pattern = get_arg(tokens, idx, "-ipath")?;
            Ok(Expr::Primary(Primary::Path {
                pattern: pattern.to_string(),
                fold: true,
            }))
        }
        "-type" => {
            let type_char = get_arg(tokens, idx, "-type")?;
            if type_char.len() != 1 {
                return Err(format!("invalid argument to -type: {}", type_char));
            }
            let ft = FileTypeMatch::from_char(type_char.chars().next().unwrap())
                .ok_or_else(|| format!("invalid argument to -type: {}", type_char))?;
            Ok(Expr::Primary(Primary::Type(ft)))
        }
        "-perm" => {
            let mode_str = get_arg(tokens, idx, "-perm")?;
            let perm = parse_perm_mode(mode_str)?;
            Ok(Expr::Primary(Primary::Perm(perm)))
        }
        "-links" => {
            let n = get_arg(tokens, idx, "-links")?;
            let cmp = NumericComparison::parse(n)?;
            Ok(Expr::Primary(Primary::Links(cmp)))
        }
        "-user" => {
            // POSIX: the argument is evaluated only once, so resolve it now.
            let uname = get_arg(tokens, idx, "-user")?;
            Ok(Expr::Primary(Primary::User(resolve_user(uname).ok())))
        }
        "-group" => {
            let gname = get_arg(tokens, idx, "-group")?;
            Ok(Expr::Primary(Primary::Group(resolve_group(gname).ok())))
        }
        "-size" => {
            let size_str = get_arg(tokens, idx, "-size")?;
            let (cmp, unit) = parse_size(size_str)?;
            Ok(Expr::Primary(Primary::Size { cmp, unit }))
        }
        "-atime" => {
            let n = get_arg(tokens, idx, "-atime")?;
            let cmp = NumericComparison::parse(n)?;
            Ok(Expr::Primary(Primary::ATime(cmp)))
        }
        "-ctime" => {
            let n = get_arg(tokens, idx, "-ctime")?;
            let cmp = NumericComparison::parse(n)?;
            Ok(Expr::Primary(Primary::CTime(cmp)))
        }
        "-mtime" => {
            let n = get_arg(tokens, idx, "-mtime")?;
            let cmp = NumericComparison::parse(n)?;
            Ok(Expr::Primary(Primary::MTime(cmp)))
        }
        "-newer" => {
            let file = get_arg(tokens, idx, "-newer")?;
            // Follow the link to the target; if it is a dangling symlink, fall
            // back to the link's own timestamp (POSIX: use the link's mtime
            // when the referenced file does not exist).
            let metadata = fs::metadata(file)
                .or_else(|_| fs::symlink_metadata(file))
                .map_err(|e| {
                    format!(
                        "cannot access '{}': {}",
                        file,
                        plib::diag::io_error_text(&e)
                    )
                })?;
            let mtime = metadata.modified().map_err(|e| {
                format!(
                    "cannot get mtime of '{}': {}",
                    file,
                    plib::diag::io_error_text(&e)
                )
            })?;
            Ok(Expr::Primary(Primary::Newer(mtime)))
        }
        "-newermt" => {
            let date = get_arg(tokens, idx, "-newermt")?;
            Ok(Expr::Primary(Primary::Newer(parse_newermt_date(date)?)))
        }
        // GNU's other -newerXY forms compare access, change or birth times, or take the time
        // from a file; none is used by what this find has to build.
        t if t.len() == "-newerXY".len() && t.starts_with("-newer") => Err(format!(
            "{t}: only -newermt is supported of the -newerXY forms"
        )),
        "-nouser" => Ok(Expr::Primary(Primary::NoUser)),
        "-true" => Ok(Expr::Primary(Primary::Const(true))),
        "-false" => Ok(Expr::Primary(Primary::Const(false))),
        "-empty" => Ok(Expr::Primary(Primary::Empty)),
        "-executable" => Ok(Expr::Primary(Primary::Executable)),
        "-regex" => {
            let pattern = get_arg(tokens, idx, "-regex")?;
            let ere = emacs_regex_to_ere(pattern)?;
            let re = plib::regex::Regex::ere(&ere).map_err(|e| format!("-regex: {e}"))?;
            Ok(Expr::Primary(Primary::Regex(re)))
        }
        "-nogroup" => Ok(Expr::Primary(Primary::NoGroup)),
        "-print" => Ok(Expr::Primary(Primary::Print)),
        "-print0" => Ok(Expr::Primary(Primary::Print0)),
        "-printf" => {
            let format = get_arg(tokens, idx, "-printf")?;
            Ok(Expr::Primary(Primary::Printf(parse_printf_format(format)?)))
        }
        "-mindepth" | "-maxdepth" => {
            let n = get_arg(tokens, idx, tok)?;
            let n = n
                .parse::<usize>()
                .map_err(|_| format!("invalid argument to {}: {}", tok, n))?;
            Ok(Expr::Primary(if tok == "-mindepth" {
                Primary::MinDepth(n)
            } else {
                Primary::MaxDepth(n)
            }))
        }
        "-prune" => Ok(Expr::Primary(Primary::Prune)),
        "-delete" => Ok(Expr::Primary(Primary::Delete)),
        "-depth" => Ok(Expr::Primary(Primary::Depth)),
        "-xdev" => Ok(Expr::Primary(Primary::XDev)),
        "-mount" => Ok(Expr::Primary(Primary::Mount)),
        "-exec" => {
            let exec_mode = parse_exec(tokens, idx)?;
            Ok(Expr::Primary(Primary::Exec(exec_mode)))
        }
        "-ok" => {
            let (utility, args) = parse_ok(tokens, idx)?;
            Ok(Expr::Primary(Primary::Ok { utility, args }))
        }
        _ => Err(format!("unknown primary: {}", tok)),
    }
}

/// Translate a `-regex` pattern from the Emacs syntax that is GNU find's
/// default into a POSIX ERE that must match the whole pathname.
///
/// In the Emacs syntax `\(`, `\)` and `\|` group and alternate while a bare
/// `(`, `)`, `|`, `{` and `}` are literals; `*`, `+` and `?` are operators
/// except where nothing precedes them (the start, or after `^`, `\(` or
/// `\|`); `^` and `$` anchor only at the start and end of the pattern or of a
/// group or alternative; and a backslash before punctuation quotes it.
/// Emacs-only escapes (`\w`, `\b`, `\<`, backreferences, ...) and character
/// classes such as `[[:alpha:]]`, which the Emacs syntax does not have, are
/// an error rather than a silently different match.
fn emacs_regex_to_ere(pattern: &str) -> Result<String, String> {
    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::from("^(");
    // True where an operator would have nothing to repeat.
    let mut operand_start = true;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        let at_start = std::mem::replace(&mut operand_start, false);
        i += 1;
        match c {
            '*' | '+' | '?' if at_start => {
                out.push('\\');
                out.push(c);
            }
            '*' | '+' | '?' => {
                // A run of stacked operators repeats one operand: the same
                // operator twice is itself, any other mix matches as `*`.
                // POSIX leaves adjacent duplication operators undefined
                // and macOS's regcomp refuses them, so emit just one.
                let mut op = c;
                while let Some(&next @ ('*' | '+' | '?')) = chars.get(i) {
                    if next != op {
                        op = '*';
                    }
                    i += 1;
                }
                out.push(op);
            }
            '.' => out.push(c),
            '^' if at_start => {
                out.push('^');
                operand_start = true;
            }
            '$' if matches!(&chars[i..], [] | ['\\', ')' | '|', ..]) => out.push('$'),
            '^' | '$' | '(' | ')' | '|' | '{' | '}' => {
                out.push('\\');
                out.push(c);
            }
            '[' => i = copy_bracket(&chars, i, pattern, &mut out)?,
            '\\' => match chars.get(i) {
                None => return Err(format!("-regex: trailing backslash in {pattern}")),
                Some(&e) => {
                    i += 1;
                    match e {
                        '(' | '|' => {
                            out.push(e);
                            operand_start = true;
                        }
                        ')' => out.push(')'),
                        e if e.is_ascii_alphanumeric() || "`'<>=_".contains(e) => {
                            return Err(format!("-regex: unsupported escape \\{e}"))
                        }
                        e => {
                            out.push('\\');
                            out.push(e);
                        }
                    }
                }
            },
            c => out.push(c),
        }
    }
    out.push_str(")$");
    Ok(out)
}

/// Copy the bracket expression whose `[` precedes `chars[start]` to `out`,
/// returning the index after its closing `]`. A backslash in it is a literal
/// in both syntaxes; `[:`, `[=` and `[.` are POSIX-only and refused.
fn copy_bracket(
    chars: &[char],
    start: usize,
    pattern: &str,
    out: &mut String,
) -> Result<usize, String> {
    let mut i = start;
    if chars.get(i) == Some(&'^') {
        i += 1;
    }
    if chars.get(i) == Some(&']') {
        i += 1;
    }
    while i < chars.len() && chars[i] != ']' {
        if chars[i] == '[' && matches!(chars.get(i + 1), Some(':' | '=' | '.')) {
            return Err(format!(
                "-regex: unsupported bracket expression in {pattern}"
            ));
        }
        i += 1;
    }
    if i == chars.len() {
        return Err(format!(
            "-regex: unterminated bracket expression in {pattern}"
        ));
    }
    out.push('[');
    out.extend(&chars[start..=i]);
    Ok(i + 1)
}

/// Parse a `-printf` format into literal runs and directives. Unsupported
/// directives and escapes are an error, never silently wrong output.
fn parse_printf_format(format: &str) -> Result<Vec<PrintfItem>, String> {
    let mut items = Vec::new();
    let mut literal = Vec::new();
    let bytes = format.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'%' => {
                let directive = match &bytes[i + 1..] {
                    [b'%', ..] => None,
                    [b'p', ..] => Some((PrintfItem::Path, 2)),
                    [b'P', ..] => Some((PrintfItem::RelativePath, 2)),
                    [b's', ..] => Some((PrintfItem::Size, 2)),
                    [b'T', b'@', ..] => Some((PrintfItem::MTimeEpoch, 3)),
                    rest => {
                        let shown = rest
                            .first()
                            .map_or(String::new(), |b| (*b as char).to_string());
                        return Err(format!("-printf: unsupported directive %{}", shown));
                    }
                };
                match directive {
                    None => {
                        literal.push(b'%');
                        i += 2;
                    }
                    Some((item, len)) => {
                        if !literal.is_empty() {
                            items.push(PrintfItem::Literal(std::mem::take(&mut literal)));
                        }
                        items.push(item);
                        i += len;
                    }
                }
            }
            b'\\' => {
                i += 1;
                match bytes.get(i) {
                    Some(b'n') => literal.push(b'\n'),
                    Some(b'\\') => literal.push(b'\\'),
                    Some(b'0'..=b'7') => {
                        // `\NNN`: up to three octal digits (`\0` is NUL).
                        let mut value = 0u32;
                        let start = i;
                        while i < bytes.len() && i < start + 3 && (b'0'..=b'7').contains(&bytes[i])
                        {
                            value = value * 8 + u32::from(bytes[i] - b'0');
                            i += 1;
                        }
                        literal.push(value as u8);
                        continue;
                    }
                    other => {
                        let shown = other.map_or(String::new(), |b| (*b as char).to_string());
                        return Err(format!("-printf: unsupported escape \\{}", shown));
                    }
                }
                i += 1;
            }
            b => {
                literal.push(b);
                i += 1;
            }
        }
    }
    if !literal.is_empty() {
        items.push(PrintfItem::Literal(literal));
    }
    Ok(items)
}

/// Get the next argument or return an error
/// The instant `-newermt DATE` names, read as `touch -d` and `date -d` read a date (a date
/// without a zone is local time).
fn parse_newermt_date(date: &str) -> Result<SystemTime, String> {
    let (secs, nanos) = plib::date_arg::parse(date, plib::date_arg::Zoneless::Local)?;
    let since_epoch = Duration::new(secs.unsigned_abs(), nanos);
    let instant = if secs >= 0 {
        UNIX_EPOCH.checked_add(since_epoch)
    } else {
        UNIX_EPOCH.checked_sub(since_epoch)
    };
    instant.ok_or_else(|| format!("invalid date format: '{date}'"))
}

fn get_arg<'a>(tokens: &[&'a str], idx: &mut usize, primary: &str) -> Result<&'a str, String> {
    if *idx >= tokens.len() {
        return Err(format!("{} requires an argument", primary));
    }
    let arg = tokens[*idx];
    *idx += 1;
    Ok(arg)
}

/// Parse -perm argument
fn parse_perm_mode(mode_str: &str) -> Result<PermMode, String> {
    // Check for - prefix (at least all bits)
    if let Some(rest) = mode_str.strip_prefix('-') {
        let mode = parse_mode_value(rest)?;
        Ok(PermMode::AtLeast(mode))
    } else if let Some(rest) = mode_str.strip_prefix('/') {
        Ok(PermMode::Any(parse_mode_value(rest)?))
    } else {
        let mode = parse_mode_value(mode_str)?;
        Ok(PermMode::Exact(mode))
    }
}

/// Parse a mode value (octal or symbolic)
fn parse_mode_value(mode_str: &str) -> Result<u32, String> {
    // Try octal first, but reject if it starts with a sign
    if !mode_str.starts_with('+') && !mode_str.starts_with('-') {
        if let Ok(m) = u32::from_str_radix(mode_str, 8) {
            if m > 0o7777 {
                return Err(format!("invalid mode: {}", mode_str));
            }
            return Ok(m);
        }
    }

    // Parse as symbolic mode starting from 0
    let chmod_mode = modestr::parse(mode_str).map_err(|_| format!("invalid mode: {}", mode_str))?;

    match chmod_mode {
        modestr::ChmodMode::Absolute(m, _) => Ok(m),
        modestr::ChmodMode::Symbolic(sym) => {
            // Start from 0 and apply symbolic changes
            Ok(modestr::mutate(0, false, &sym))
        }
    }
}

/// Parse a `-size` argument into the comparison and its unit in bytes:
/// 512-byte blocks by default, bytes with POSIX `c`, and KiB with GNU `k`
/// (forced by debhelper's dh_compress `-size +4k`; no other GNU unit is).
fn parse_size(s: &str) -> Result<(NumericComparison, u64), String> {
    let (num_str, unit) = if let Some(n) = s.strip_suffix('c') {
        (n, 1)
    } else if let Some(n) = s.strip_suffix('k') {
        (n, 1024)
    } else {
        (s, 512)
    };

    let cmp = NumericComparison::parse(num_str)?;
    Ok((cmp, unit))
}

/// Resolve username to UID
fn resolve_user(name: &str) -> Result<u32, String> {
    // Try by name first
    if let Some(user) = plib::user::get_by_name(name) {
        return Ok(user.uid());
    }
    // Try as numeric UID
    name.parse::<u32>()
        .map_err(|_| format!("unknown user: {}", name))
}

/// Resolve group name to GID
fn resolve_group(name: &str) -> Result<u32, String> {
    // Try by name first
    if let Some(group) = plib::group::get_by_name(name) {
        return Ok(group.gid());
    }
    // Try as numeric GID
    name.parse::<u32>()
        .map_err(|_| format!("unknown group: {}", name))
}

/// Parse -exec primary arguments
fn parse_exec(tokens: &[&str], idx: &mut usize) -> Result<ExecMode, String> {
    if *idx >= tokens.len() {
        return Err("-exec requires an argument".to_string());
    }

    let utility = tokens[*idx].to_string();
    *idx += 1;

    let mut args = Vec::new();
    let mut has_placeholder = false;

    while *idx < tokens.len() {
        let tok = tokens[*idx];
        *idx += 1;

        if tok == ";" {
            // Single mode: -exec utility [args...] ;
            return Ok(ExecMode::Single { utility, args });
        }

        if tok == "+" && has_placeholder && args.last().map(|s: &String| s.as_str()) == Some("{}") {
            // Batch mode: -exec utility [args...] {} +
            args.pop(); // Remove the {}
            return Ok(ExecMode::Batch {
                // Placeholder; stamped by register_exec_batches() once the
                // whole expression has been parsed.
                id: 0,
                utility,
                args_before: args,
            });
        }

        if tok == "{}" {
            has_placeholder = true;
        }
        args.push(tok.to_string());
    }

    Err("-exec not terminated by ; or {} +".to_string())
}

/// Parse -ok primary arguments
fn parse_ok(tokens: &[&str], idx: &mut usize) -> Result<(String, Vec<String>), String> {
    if *idx >= tokens.len() {
        return Err("-ok requires an argument".to_string());
    }

    let utility = tokens[*idx].to_string();
    *idx += 1;

    let mut args = Vec::new();

    while *idx < tokens.len() {
        let tok = tokens[*idx];
        *idx += 1;

        if tok == ";" {
            return Ok((utility, args));
        }
        args.push(tok.to_string());
    }

    Err("-ok not terminated by ;".to_string())
}

/// Check if expression contains any action
fn has_action(expr: &Expr) -> bool {
    has_primary(expr, |p| {
        matches!(
            p,
            Primary::Print
                | Primary::Print0
                | Primary::Printf(_)
                | Primary::Exec(_)
                | Primary::Ok { .. }
                | Primary::Delete
        )
    })
}

/// Give every `-exec ... {} +` primary its own aggregation set, stamping each
/// with the index of its batch.
///
/// POSIX (find, ll. 98284-98298) aggregates pathnames into sets *per primary*,
/// so `-name a -exec u {} + -o -name b -exec u {} +` is two sets and two
/// invocations even though the utility is identical. Identity therefore cannot
/// be derived from the utility and its arguments; it has to be positional. The
/// in-order walk below matches source order, since the trees built by
/// `parse_or_expr`/`parse_and_expr` are left-associative.
fn register_exec_batches(expr: &mut Expr, batches: &mut Vec<ExecBatch>) {
    match expr {
        Expr::Primary(Primary::Exec(ExecMode::Batch {
            id,
            utility,
            args_before,
        })) => {
            *id = batches.len();
            batches.push(ExecBatch {
                utility: utility.clone(),
                args_before: args_before.clone(),
                files: Vec::new(),
            });
        }
        Expr::Not(e) => register_exec_batches(e, batches),
        Expr::And(l, r) | Expr::Or(l, r) => {
            register_exec_batches(l, batches);
            register_exec_batches(r, batches);
        }
        _ => {}
    }
}

/// Does the expression contain a primary for which `pred` holds?
fn has_primary(expr: &Expr, pred: fn(&Primary) -> bool) -> bool {
    match expr {
        Expr::Primary(p) => pred(p),
        Expr::Not(e) => has_primary(e, pred),
        Expr::And(l, r) | Expr::Or(l, r) => has_primary(l, pred) || has_primary(r, pred),
    }
}

/// Apply `-mindepth` / `-maxdepth`. As in GNU find they are global options:
/// wherever they appear they limit the whole walk, and the last one wins.
fn set_depth_limits(expr: &Expr, state: &mut FindState) {
    match expr {
        Expr::Primary(Primary::MinDepth(n)) => state.min_depth = *n,
        Expr::Primary(Primary::MaxDepth(n)) => state.max_depth = Some(*n),
        Expr::Not(e) => set_depth_limits(e, state),
        Expr::And(l, r) | Expr::Or(l, r) => {
            set_depth_limits(l, state);
            set_depth_limits(r, state);
        }
        _ => {}
    }
}

/// Evaluate expression against a file
fn evaluate(expr: &Expr, ctx: &EvalContext, state: &mut FindState) -> EvalResult {
    match expr {
        Expr::Primary(p) => evaluate_primary(p, ctx, state),
        Expr::Not(e) => {
            let mut result = evaluate(e, ctx, state);
            result.matched = !result.matched;
            result
        }
        Expr::And(l, r) => {
            let left_result = evaluate(l, ctx, state);
            if !left_result.matched {
                // Short-circuit: left is false, whole AND is false
                return left_result;
            }
            let mut right_result = evaluate(r, ctx, state);
            right_result.prune = left_result.prune || right_result.prune;
            right_result
                .exec_batch_files
                .extend(left_result.exec_batch_files);
            right_result
        }
        Expr::Or(l, r) => {
            let left_result = evaluate(l, ctx, state);
            if left_result.matched {
                // Short-circuit: left is true, whole OR is true
                return left_result;
            }
            let mut right_result = evaluate(r, ctx, state);
            right_result.prune = left_result.prune || right_result.prune;
            right_result
                .exec_batch_files
                .extend(left_result.exec_batch_files);
            right_result
        }
    }
}

/// Evaluate a single primary
fn evaluate_primary(primary: &Primary, ctx: &EvalContext, state: &mut FindState) -> EvalResult {
    match primary {
        Primary::Name { pattern, fold } => {
            let name = ctx.path.file_name().unwrap_or(OsStr::new(""));
            let name_str = name.to_string_lossy();
            EvalResult::new(fnmatch(pattern, &name_str, *fold))
        }
        Primary::Path { pattern, fold } => {
            let path_str = ctx.path.to_string_lossy();
            EvalResult::new(fnmatch(pattern, &path_str, *fold))
        }
        // POSIX -H/-L: a symbolic link that is followed has the type of the
        // file it references, so `-type l` matches only a link that could
        // not be followed (the walk then hands over the link's own metadata).
        Primary::Type(ft) => EvalResult::new(ft.matches(&ctx.metadata.file_type())),
        Primary::Perm(mode) => {
            let file_mode = ctx.metadata.mode() & 0o7777;
            let matched = match mode {
                PermMode::Exact(m) => file_mode == *m,
                PermMode::AtLeast(m) => (file_mode & m) == *m,
                PermMode::Any(m) => *m == 0 || file_mode & m != 0,
            };
            EvalResult::new(matched)
        }
        Primary::Links(cmp) => {
            let nlinks = ctx.metadata.nlink() as i64;
            EvalResult::new(cmp.matches(nlinks))
        }
        Primary::User(uid) => EvalResult::new(*uid == Some(ctx.metadata.uid())),
        Primary::Group(gid) => EvalResult::new(*gid == Some(ctx.metadata.gid())),
        Primary::Size { cmp, unit } => {
            let size = ctx.metadata.size().div_ceil(*unit) as i64;
            EvalResult::new(cmp.matches(size))
        }
        Primary::ATime(cmp) => {
            let atime = SystemTime::UNIX_EPOCH
                + std::time::Duration::from_secs(ctx.metadata.atime() as u64);
            let days = time_diff_days(ctx.init_time, atime);
            EvalResult::new(cmp.matches(days))
        }
        Primary::CTime(cmp) => {
            let ctime = SystemTime::UNIX_EPOCH
                + std::time::Duration::from_secs(ctx.metadata.ctime() as u64);
            let days = time_diff_days(ctx.init_time, ctime);
            EvalResult::new(cmp.matches(days))
        }
        Primary::MTime(cmp) => {
            let mtime = SystemTime::UNIX_EPOCH
                + std::time::Duration::from_secs(ctx.metadata.mtime() as u64);
            let days = time_diff_days(ctx.init_time, mtime);
            EvalResult::new(cmp.matches(days))
        }
        Primary::Newer(ref_time) => EvalResult::new(modified(ctx.metadata) > *ref_time),
        Primary::Const(value) => EvalResult::new(*value),
        Primary::Empty => EvalResult::new(is_empty(ctx, state)),
        Primary::Executable => {
            // access(2) as GNU find asks it -- real user, final symlink
            // followed -- but on the name in the directory the walk holds.
            let (dir_fd, name) = (ctx.entry.dir_fd(), ctx.entry.file_name());
            EvalResult::new(ftw::is_executable_at(dir_fd, name))
        }
        Primary::Delete => EvalResult::new(delete_entry(ctx, state)),
        Primary::Regex(re) => EvalResult::new(re.is_match_bytes(ctx.path.as_os_str().as_bytes())),
        Primary::NoUser => {
            let uid = ctx.metadata.uid();
            EvalResult::new(plib::user::get_by_uid(uid).is_none())
        }
        Primary::NoGroup => {
            let gid = ctx.metadata.gid();
            EvalResult::new(plib::group::get_by_gid(gid).is_none())
        }
        Primary::Print => {
            let mut line = ctx.path.as_os_str().as_bytes().to_vec();
            line.push(b'\n');
            write_stdout(state, &line);
            EvalResult::new(true)
        }
        Primary::Print0 => {
            let mut name = ctx.path.as_os_str().as_bytes().to_vec();
            name.push(b'\0');
            write_stdout(state, &name);
            EvalResult::new(true)
        }
        Primary::Prune => {
            let mut result = EvalResult::new(true);
            if !state.depth_first {
                result.prune = true;
            }
            result
        }
        Primary::Printf(items) => {
            write_stdout(state, &format_printf(items, ctx));
            EvalResult::new(true)
        }
        Primary::Depth => {
            // Always true, affects traversal order (handled globally)
            EvalResult::new(true)
        }
        Primary::XDev | Primary::Mount | Primary::MinDepth(_) | Primary::MaxDepth(_) => {
            // Always true, affects traversal (handled globally)
            EvalResult::new(true)
        }
        Primary::Exec(mode) => {
            match mode {
                ExecMode::Single { utility, args } => {
                    // Replace {} with pathname
                    let expanded_args: Vec<String> = args
                        .iter()
                        .map(|a| {
                            if a == "{}" {
                                ctx.path.to_string_lossy().to_string()
                            } else {
                                a.clone()
                            }
                        })
                        .collect();

                    flush_stdout();
                    match Command::new(utility).args(&expanded_args).status() {
                        Ok(status) => EvalResult::new(status.success()),
                        Err(e) => {
                            eprintln!("find: '{}': {}", utility, plib::diag::io_error_text(&e));
                            state.had_error = true;
                            EvalResult::new(false)
                        }
                    }
                }
                ExecMode::Batch { id, .. } => {
                    // Batch mode: accumulate the file into *this* primary's
                    // set, always return true
                    let mut result = EvalResult::new(true);
                    result.exec_batch_files.push((*id, ctx.path.to_path_buf()));
                    result
                }
            }
        }
        Primary::Ok { utility, args } => {
            // Prompt user for confirmation
            eprint!("< {} ... {} > ? ", utility, ctx.path.display());
            let _ = io::stderr().flush();

            let mut response = String::new();
            if io::stdin().lock().read_line(&mut response).is_err() {
                return EvalResult::new(false);
            }

            if !is_affirmative(response.trim_end_matches('\n')) {
                return EvalResult::new(false);
            }

            // Replace {} with pathname and execute
            let expanded_args: Vec<String> = args
                .iter()
                .map(|a| {
                    if a == "{}" {
                        ctx.path.to_string_lossy().to_string()
                    } else {
                        a.clone()
                    }
                })
                .collect();

            flush_stdout();
            match Command::new(utility).args(&expanded_args).status() {
                Ok(status) => EvalResult::new(status.success()),
                Err(e) => {
                    eprintln!("find: '{}': {}", utility, plib::diag::io_error_text(&e));
                    state.had_error = true;
                    EvalResult::new(false)
                }
            }
        }
    }
}

/// `-empty`: a regular file of size zero, or a directory with no entries.
/// A directory that cannot be read is reported and is not empty.
///
/// The directory is opened through the descriptor of the directory holding
/// it, refusing a symbolic link the walk did not follow, and checked to be
/// the directory the walk stat'ed (see `ftw::Entry::is_empty_dir`).
fn is_empty(ctx: &EvalContext, state: &mut FindState) -> bool {
    let ft = ctx.metadata.file_type();
    if ft.is_file() {
        return ctx.metadata.size() == 0;
    }
    if !ft.is_dir() {
        return false;
    }
    match ctx.entry.is_empty_dir() {
        Ok(empty) => empty,
        Err(e) => {
            eprintln!(
                "find: '{}': {}",
                ctx.path.display(),
                plib::diag::io_error_text(&e)
            );
            state.had_error = true;
            false
        }
    }
}

/// `-delete`: remove the entry itself (a symlink, never its target), a
/// directory as `rmdir` does. As in GNU find, a starting point with no final
/// name component such as `.` is left alone. A failure is reported and
/// makes the primary false.
///
/// The removal is `unlinkat` of the entry's name in the directory the walk
/// holds open, never a pathname: a directory above it swapped for a symbolic
/// link cannot redirect it. If the name itself was replaced since the walk
/// stat'ed it, the removal acts on the replacement, which is in the same
/// directory and so still inside the tree; `AT_REMOVEDIR` on something that
/// is no longer a directory fails with `ENOTDIR`.
fn delete_entry(ctx: &EvalContext, state: &mut FindState) -> bool {
    if ctx.path.file_name().is_none() {
        return true;
    }
    let flags = if !ctx.is_symlink && ctx.metadata.is_dir() {
        libc::AT_REMOVEDIR
    } else {
        0
    };
    match ctx.entry.unlink(flags) {
        Ok(()) => true,
        Err(e) => {
            eprintln!(
                "find: cannot delete '{}': {}",
                ctx.path.display(),
                plib::diag::io_error_text(&e)
            );
            state.had_error = true;
            false
        }
    }
}

/// Expand a parsed `-printf` format for one file.
fn format_printf(items: &[PrintfItem], ctx: &EvalContext) -> Vec<u8> {
    let mut out = Vec::new();
    for item in items {
        match item {
            PrintfItem::Literal(bytes) => out.extend_from_slice(bytes),
            PrintfItem::Path => out.extend_from_slice(ctx.path.as_os_str().as_bytes()),
            PrintfItem::RelativePath => {
                let rel = ctx.path.strip_prefix(ctx.root).unwrap_or(Path::new(""));
                out.extend_from_slice(rel.as_os_str().as_bytes());
            }
            PrintfItem::Size => out.extend_from_slice(ctx.metadata.size().to_string().as_bytes()),
            PrintfItem::MTimeEpoch => {
                // GNU find 4.9 prints nanoseconds and a tenth digit.
                let t = format!("{}.{:09}0", ctx.metadata.mtime(), ctx.metadata.mtime_nsec());
                out.extend_from_slice(t.as_bytes());
            }
        }
    }
    out
}

/// Does `response` match the locale's affirmative pattern (`YESEXPR`)? Used by
/// `-ok` so the accepted answers follow `LC_MESSAGES` rather than hardcoded
/// English. Falls back to `^[yY]` if the locale pattern is unavailable.
fn is_affirmative(response: &str) -> bool {
    use std::ffi::CStr;
    let pattern = unsafe {
        let p = libc::nl_langinfo(libc::YESEXPR);
        if p.is_null() {
            None
        } else {
            CStr::from_ptr(p).to_str().ok().map(str::to_owned)
        }
    };
    let pattern = pattern.filter(|s| !s.is_empty());
    match pattern {
        Some(pat) => match plib::regex::Regex::ere(&pat) {
            Ok(re) => re.is_match(response),
            Err(_) => response.starts_with(['y', 'Y']),
        },
        None => response.starts_with(['y', 'Y']),
    }
}

/// Calculate time difference in days, (init_time - file_time) / 86400 with the
/// remainder discarded. A file timestamp in the future yields a negative value
/// (not clamped to 0), so comparisons like `-mtime -1` behave correctly.
fn time_diff_days(init_time: SystemTime, file_time: SystemTime) -> i64 {
    let secs = match init_time.duration_since(file_time) {
        Ok(d) => d.as_secs() as i64,
        Err(e) => -(e.duration().as_secs() as i64),
    };
    secs / 86400
}

/// The modification time of `md`, to the nanosecond.
fn modified(md: &ftw::Metadata) -> SystemTime {
    let nsec = Duration::from_nanos(md.mtime_nsec() as u64);
    let secs = Duration::from_secs(md.mtime().unsigned_abs());
    if md.mtime() >= 0 {
        SystemTime::UNIX_EPOCH + secs + nsec
    } else {
        SystemTime::UNIX_EPOCH - secs + nsec
    }
}

/// One walk over a starting point, driven by `ftw::traverse_directory`.
///
/// ftw owns every file-system access of the descent: it opens each directory
/// with `openat(parent_fd, name, O_DIRECTORY | O_NOFOLLOW)` (without
/// `O_NOFOLLOW` only where -H/-L follow the link), checks that the directory
/// it opened has the `(dev, ino)` it stat'ed, stats entries with `fstatat` on
/// that descriptor, and hands each entry over as (parent descriptor, name).
/// Nothing here resolves a pathname: the path is only printed and matched.
struct Walk<'a> {
    expr: &'a Expr,
    /// The starting point, as given
    root: &'a Path,
    /// Components in `root`; an entry's depth is how many more its path has
    root_components: usize,
    /// Device of the starting point, for -xdev / -mount
    root_dev: u64,
    /// The directories being descended, outermost first: `(dev, ino)` and
    /// path of each, for file-system loop detection. Index `i` is depth `i`.
    ancestors: Vec<((u64, u64), PathBuf)>,
    state: &'a mut FindState,
}

impl Walk<'_> {
    fn depth_of(&self, path: &Path) -> usize {
        path.components()
            .count()
            .saturating_sub(self.root_components)
    }

    /// ftw's `file_handler`: act on `entry` now unless -depth puts it after
    /// its contents, and say whether to descend into it.
    fn enter(&mut self, entry: &ftw::Entry) -> bool {
        let path = entry.path().as_inner().to_path_buf();
        let depth = self.depth_of(&path);
        // Normally a no-op: `leave` pops each level. ftw skips `leave` for a
        // directory whose (descriptor-conserving) parent it cannot reopen, so
        // the depth, which never goes stale, decides.
        self.ancestors.truncate(depth);
        let Some(md) = entry.metadata() else {
            return false;
        };
        if depth == 0 {
            self.root_dev = md.dev();
        }
        // `link/` names the directory the link points to. With -delete, refuse it before
        // descending, as rm -r does, rather than delete through the link: a directory operand
        // swapped for a symlink would otherwise redirect the deletion.
        if self.state.deletes && entry.reached_through_symlink() {
            eprintln!(
                "find: cannot delete '{}': {}",
                path.display(),
                plib::diag::io_error_text(&io::Error::from_raw_os_error(libc::ENOTDIR))
            );
            self.state.had_error = true;
            return false;
        }

        let key = (md.dev(), md.ino());
        if md.is_dir() {
            if let Some((_, ancestor)) = self.ancestors.iter().find(|(k, _)| *k == key) {
                eprintln!(
                    "find: File system loop detected; '{}' is part of the same file system loop as '{}'.",
                    path.display(),
                    ancestor.display()
                );
                self.state.had_error = true;
                return false;
            }
        }

        // Device-crossing handling for -xdev / -mount. When a directory on a
        // different device is encountered (not a command-line path operand):
        //   -mount: do not act on it and do not descend (excludes the mount point).
        //   -xdev:  act on it but do not descend (includes the mount point).
        let on_other_dev = depth > 0 && md.dev() != self.root_dev;
        if on_other_dev && self.state.mount {
            return false;
        }
        let descend = md.is_dir()
            && !(on_other_dev && self.state.xdev)
            && self.state.max_depth.is_none_or(|max| depth < max);

        // With -depth a directory is acted on in `leave`, after its contents.
        let prune = !(self.state.depth_first && descend) && self.visit(entry, &path, md, depth);
        if descend && !prune {
            self.ancestors.push((key, path));
        }
        descend && !prune
    }

    /// ftw's `postprocess_dir`: done with a directory `enter` descended into,
    /// whether or not ftw managed to open it. `entry` is relative to the
    /// parent's descriptor, as in `enter`.
    fn leave(&mut self, entry: &ftw::Entry) {
        let path = entry.path().as_inner().to_path_buf();
        let depth = self.depth_of(&path);
        self.ancestors.truncate(depth);
        if self.state.depth_first {
            if let Some(md) = entry.metadata() {
                self.visit(entry, &path, md, depth);
            }
        }
    }

    /// Evaluate the expression for one entry, unless it is above -mindepth.
    /// Returns whether it asked to prune.
    fn visit(&mut self, entry: &ftw::Entry, path: &Path, md: &ftw::Metadata, depth: usize) -> bool {
        if depth < self.state.min_depth {
            return false;
        }
        let ctx = EvalContext {
            path,
            root: self.root,
            entry,
            metadata: md,
            is_symlink: entry.is_symlink().unwrap_or(false),
            init_time: self.state.init_time,
        };
        let result = evaluate(self.expr, &ctx, self.state);

        // Handle batched exec files. Every id was assigned by
        // register_exec_batches() over this same expression, so it indexes a
        // registered batch.
        for (id, batch_path) in result.exec_batch_files {
            if let Some(batch) = self.state.exec_batches.get_mut(id) {
                batch.files.push(batch_path);
            }
        }
        result.prune
    }

    /// ftw's `err_reporter`.
    fn report(&mut self, entry: &ftw::Entry, error: ftw::Error) {
        let path = entry.path();
        let kind = error.kind();
        let text = plib::diag::io_error_text(&error.inner());
        if kind == ftw::ErrorKind::ReadDir {
            eprintln!("find: error reading directory '{}': {}", path, text);
        } else {
            eprintln!("find: '{}': {}", path, text);
        }
        self.state.had_error = true;
    }
}

/// Walk the tree under the starting point `root`, evaluating the expression
/// for each file.
fn walk_operand(root: &Path, expr: &Expr, state: &mut FindState) {
    let opts = ftw::TraverseDirectoryOpts {
        follow_symlinks_on_args: state.symlink_mode == SymlinkMode::CommandLineOnly,
        follow_symlinks: state.symlink_mode == SymlinkMode::Always,
        ..Default::default()
    };
    let walk = RefCell::new(Walk {
        expr,
        root,
        root_components: root.components().count(),
        root_dev: 0,
        ancestors: Vec::new(),
        state,
    });
    // The result only summarizes: every error has reached `report`, and a
    // starting point that is not a directory also counts as `false`.
    ftw::traverse_directory(
        root,
        |entry| Ok(walk.borrow_mut().enter(&entry)),
        |entry, _| {
            walk.borrow_mut().leave(&entry);
            Ok(())
        },
        |entry, error| walk.borrow_mut().report(&entry, error),
        opts,
    );
}

/// Flush what find has written so far, so that it reaches standard output
/// before anything a child utility writes there. A failure is left in the
/// buffer, and reported by the flush at the end of the run.
fn flush_stdout() {
    let _ = io::stdout().flush();
}

/// Write `bytes` to standard output. A write error makes find's exit status
/// nonzero and is reported once, not once per pathname.
fn write_stdout(state: &mut FindState, bytes: &[u8]) {
    if let Err(e) = io::stdout().lock().write_all(bytes) {
        state.had_error = true;
        if !state.stdout_failed {
            state.stdout_failed = true;
            plib::diag::error(&format!(
                "{}: {}",
                gettext("write error"),
                plib::diag::io_error_text(&e)
            ));
        }
    }
}

/// Run one `-exec ... {} +` invocation over a chunk of files. Returns whether
/// it exited successfully.
fn run_exec_command(utility: &str, args_before: &[String], files: &[PathBuf]) -> bool {
    flush_stdout();
    let mut cmd = Command::new(utility);
    cmd.args(args_before);
    cmd.args(files);
    match cmd.status() {
        Ok(status) => status.success(),
        Err(e) => {
            eprintln!("find: '{}': {}", utility, plib::diag::io_error_text(&e));
            false
        }
    }
}

/// Execute all pending batched -exec commands, splitting the file list into
/// invocations whose argument size stays under `ARG_MAX`.
fn execute_batches(state: &mut FindState) {
    let ptr = std::mem::size_of::<usize>();
    let arg_max = match unsafe { libc::sysconf(libc::_SC_ARG_MAX) } {
        n if n > 0 => n as usize,
        _ => 1 << 17, // 128 KiB fallback
    };
    // Reserve room for the environment, the utility/args_before, and overhead.
    let env_size: usize = std::env::vars_os()
        .map(|(k, v)| k.len() + v.len() + 2 + ptr)
        .sum();

    for batch in state.exec_batches.drain(..) {
        // Batches are registered eagerly, one per primary, so a primary that
        // never matched has an empty set and must not be invoked at all.
        if batch.files.is_empty() {
            continue;
        }
        let ExecBatch {
            utility,
            args_before,
            files,
        } = batch;

        let fixed: usize =
            utility.len() + 1 + ptr + args_before.iter().map(|a| a.len() + 1 + ptr).sum::<usize>();
        let budget = arg_max.saturating_sub(env_size + fixed + 2048);

        let mut chunk: Vec<PathBuf> = Vec::new();
        let mut chunk_size = 0usize;
        for f in files {
            let cost = f.as_os_str().len() + 1 + ptr;
            if !chunk.is_empty() && chunk_size + cost > budget {
                if !run_exec_command(&utility, &args_before, &chunk) {
                    state.had_error = true;
                }
                chunk.clear();
                chunk_size = 0;
            }
            chunk_size += cost;
            chunk.push(f);
        }
        if !chunk.is_empty() && !run_exec_command(&utility, &args_before, &chunk) {
            state.had_error = true;
        }
    }
}

/// Main find function
fn find(args: Vec<String>) -> Result<i32, String> {
    let (symlink_mode, paths, mut expr) = parse_args(&args)?;

    // If no action, wrap with implicit -print per POSIX
    if !has_action(&expr) {
        expr = Expr::And(Box::new(expr), Box::new(Expr::Primary(Primary::Print)));
    }

    // Set up state
    let mut state = FindState::new();
    let depth = has_primary(&expr, |p| matches!(p, Primary::Depth));
    // GNU: -delete implies -depth, so a -prune next to it would do nothing.
    let delete = has_primary(&expr, |p| matches!(p, Primary::Delete));
    if delete && !depth && has_primary(&expr, |p| matches!(p, Primary::Prune)) {
        return Err(
            "-delete implies -depth, which makes -prune do nothing; give -depth explicitly to go ahead"
                .to_string(),
        );
    }
    state.depth_first = depth || delete;
    state.deletes = delete;
    state.xdev = has_primary(&expr, |p| matches!(p, Primary::XDev));
    state.mount = has_primary(&expr, |p| matches!(p, Primary::Mount));
    state.symlink_mode = symlink_mode;
    set_depth_limits(&expr, &mut state);

    // Give every `-exec ... {} +` primary its own aggregation set, in source
    // order, before the walk begins.
    register_exec_batches(&mut expr, &mut state.exec_batches);

    for path in paths {
        walk_operand(&path, &expr, &mut state);
    }

    // Execute any pending batched commands
    execute_batches(&mut state);

    // A final partial line (`-print0`, `-printf` without `\n`) is still in the
    // line buffer; the runtime's flush at exit would discard its error.
    if !state.stdout_failed && !plib::diag::flush_stdout() {
        state.had_error = true;
    }

    if state.had_error {
        Ok(1)
    } else {
        Ok(0)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("find");

    let args: Vec<String> = std::env::args().collect();

    match find(args) {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("find: {}", e);
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::emacs_regex_to_ere;

    /// Stacked postfix operators. The Emacs syntax allows them (`c++`
    /// matches as `c+`); a POSIX ERE leaves adjacent duplication operators
    /// undefined, and macOS's regcomp refuses them ("repetition-operator
    /// operand invalid"). The translation collapses a run into the one
    /// operator with the same match: the same operator twice is itself, any
    /// other mix is `*`.
    #[test]
    fn stacked_operators_collapse_to_one() {
        for (emacs, ere) in [
            (".*/c++", "^(.*/c+)$"),
            ("a**", "^(a*)$"),
            ("a??", "^(a?)$"),
            ("a+*", "^(a*)$"),
            ("a*+", "^(a*)$"),
            ("a?+", "^(a*)$"),
            ("a+?", "^(a*)$"),
            ("a+++", "^(a+)$"),
            ("a+?+", "^(a*)$"),
            ("\\(ab\\)+*", "^((ab)*)$"),
            ("[xy]?*", "^([xy]*)$"),
        ] {
            assert_eq!(emacs_regex_to_ere(emacs).as_deref(), Ok(ere), "{emacs}");
        }
        // An operator with nothing before it is still a literal, and one
        // after it is an ordinary operator on that literal.
        assert_eq!(emacs_regex_to_ere("+a").as_deref(), Ok("^(\\+a)$"));
        assert_eq!(emacs_regex_to_ere("*+").as_deref(), Ok("^(\\*+)$"));
    }
}
