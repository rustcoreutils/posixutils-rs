//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs::{self, Metadata};
use std::io::{self, BufRead, Write as IoWrite};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

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

    fn matches(&self, ft: &std::fs::FileType) -> bool {
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
    Size {
        cmp: NumericComparison,
        in_bytes: bool,
    },
    ATime(NumericComparison),
    CTime(NumericComparison),
    MTime(NumericComparison),
    Newer(SystemTime),
    NoUser,
    NoGroup,

    // Actions
    Print,
    Print0,
    Printf(Vec<PrintfItem>),
    Prune,
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
struct EvalContext<'a> {
    /// Full path to the file
    path: &'a Path,
    /// The starting point (path operand) the file was found under
    root: &'a Path,
    /// Metadata (may be symlink or target depending on -H/-L)
    metadata: &'a Metadata,
    /// Raw symlink metadata (for -type l checks with -L)
    link_metadata: Option<&'a Metadata>,
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
    /// Whether -depth was specified anywhere in expression
    depth_first: bool,
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
    /// (dev, ino) of the directories on the current descent path (for
    /// file-system loop detection)
    visited_inodes: HashSet<(u64, u64)>,
    /// Path first seen at each visited inode, for the loop diagnostic
    visited_paths: std::collections::HashMap<(u64, u64), PathBuf>,
    /// One entry per `-exec ... {} +` primary, in source order; index is the
    /// primary's stamped `id`.
    exec_batches: Vec<ExecBatch>,
}

impl FindState {
    fn new() -> Self {
        Self {
            had_error: false,
            depth_first: false,
            xdev: false,
            mount: false,
            min_depth: 0,
            max_depth: None,
            symlink_mode: SymlinkMode::Never,
            init_time: SystemTime::now(),
            visited_inodes: HashSet::new(),
            visited_paths: std::collections::HashMap::new(),
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

/// Parse OR expression (lowest precedence)
fn parse_or_expr(tokens: &[&str], idx: &mut usize) -> Result<Expr, String> {
    let mut left = parse_and_expr(tokens, idx)?;

    while *idx < tokens.len() && tokens[*idx] == "-o" {
        *idx += 1;
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
        if tok == "-o" || tok == ")" {
            break;
        }
        if tok == "-a" {
            *idx += 1;
        }
        // Implicit AND by juxtaposition
        if *idx >= tokens.len() || tokens[*idx] == "-o" || tokens[*idx] == ")" {
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
            let (cmp, in_bytes) = parse_size(size_str)?;
            Ok(Expr::Primary(Primary::Size { cmp, in_bytes }))
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
        "-nouser" => Ok(Expr::Primary(Primary::NoUser)),
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

/// Parse -size argument
fn parse_size(s: &str) -> Result<(NumericComparison, bool), String> {
    let (num_str, in_bytes) = if let Some(n) = s.strip_suffix('c') {
        (n, true)
    } else {
        (s, false)
    };

    let cmp = NumericComparison::parse(num_str)?;
    Ok((cmp, in_bytes))
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
    match expr {
        Expr::Primary(p) => matches!(
            p,
            Primary::Print
                | Primary::Print0
                | Primary::Printf(_)
                | Primary::Exec(_)
                | Primary::Ok { .. }
        ),
        Expr::Not(e) => has_action(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_action(l) || has_action(r),
    }
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

/// Check if expression contains -depth
fn has_depth(expr: &Expr) -> bool {
    match expr {
        Expr::Primary(Primary::Depth) => true,
        Expr::Not(e) => has_depth(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_depth(l) || has_depth(r),
        _ => false,
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

/// Check if expression contains -xdev
fn has_xdev(expr: &Expr) -> bool {
    match expr {
        Expr::Primary(Primary::XDev) => true,
        Expr::Not(e) => has_xdev(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_xdev(l) || has_xdev(r),
        _ => false,
    }
}

/// Check if expression contains -mount
fn has_mount(expr: &Expr) -> bool {
    match expr {
        Expr::Primary(Primary::Mount) => true,
        Expr::Not(e) => has_mount(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_mount(l) || has_mount(r),
        _ => false,
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
        Primary::Type(ft) => {
            // When -L is used and checking for symlink, use link_metadata
            if *ft == FileTypeMatch::Symlink {
                if let Some(lm) = ctx.link_metadata {
                    return EvalResult::new(lm.file_type().is_symlink());
                }
            }
            EvalResult::new(ft.matches(&ctx.metadata.file_type()))
        }
        Primary::Perm(mode) => {
            let file_mode = ctx.metadata.permissions().mode() & 0o7777;
            let matched = match mode {
                PermMode::Exact(m) => file_mode == *m,
                PermMode::AtLeast(m) => (file_mode & m) == *m,
            };
            EvalResult::new(matched)
        }
        Primary::Links(cmp) => {
            let nlinks = ctx.metadata.nlink() as i64;
            EvalResult::new(cmp.matches(nlinks))
        }
        Primary::User(uid) => EvalResult::new(*uid == Some(ctx.metadata.uid())),
        Primary::Group(gid) => EvalResult::new(*gid == Some(ctx.metadata.gid())),
        Primary::Size { cmp, in_bytes } => {
            let size = if *in_bytes {
                ctx.metadata.len() as i64
            } else {
                // Size in 512-byte blocks, rounded up
                ctx.metadata.len().div_ceil(512) as i64
            };
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
        Primary::Newer(ref_time) => {
            if let Ok(mtime) = ctx.metadata.modified() {
                EvalResult::new(mtime > *ref_time)
            } else {
                EvalResult::new(false)
            }
        }
        Primary::NoUser => {
            let uid = ctx.metadata.uid();
            EvalResult::new(plib::user::get_by_uid(uid).is_none())
        }
        Primary::NoGroup => {
            let gid = ctx.metadata.gid();
            EvalResult::new(plib::group::get_by_gid(gid).is_none())
        }
        Primary::Print => {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            let _ = writeln!(handle, "{}", ctx.path.display());
            EvalResult::new(true)
        }
        Primary::Print0 => {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            let _ = handle.write_all(ctx.path.as_os_str().as_bytes());
            let _ = handle.write_all(b"\0");
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
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            let _ = handle.write_all(&format_printf(items, ctx));
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
            PrintfItem::Size => out.extend_from_slice(ctx.metadata.len().to_string().as_bytes()),
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

/// Get metadata for a path, following symlinks according to mode
fn get_metadata(
    path: &Path,
    symlink_mode: SymlinkMode,
    is_cmdline: bool,
) -> io::Result<(Metadata, Option<Metadata>)> {
    let follow = match symlink_mode {
        SymlinkMode::Never => false,
        SymlinkMode::CommandLineOnly => is_cmdline,
        SymlinkMode::Always => true,
    };

    if follow {
        // Try to get target metadata
        match fs::metadata(path) {
            Ok(m) => {
                // Also get symlink metadata for -type l check
                let link_meta = fs::symlink_metadata(path).ok();
                Ok((m, link_meta))
            }
            Err(_) => {
                // Target doesn't exist, use symlink metadata
                let m = fs::symlink_metadata(path)?;
                Ok((m.clone(), Some(m)))
            }
        }
    } else {
        let m = fs::symlink_metadata(path)?;
        Ok((m, None))
    }
}

/// Walk a directory tree and evaluate expression for each file. `depth` is 0
/// for the starting point `root` (a path operand).
fn walk_tree(
    path: &Path,
    expr: &Expr,
    root: &Path,
    root_dev: u64,
    depth: usize,
    state: &mut FindState,
) {
    let is_cmdline = depth == 0;
    // Get metadata
    let (metadata, link_metadata) = match get_metadata(path, state.symlink_mode, is_cmdline) {
        Ok(m) => m,
        Err(e) => {
            eprintln!(
                "find: '{}': {}",
                path.display(),
                plib::diag::io_error_text(&e)
            );
            state.had_error = true;
            return;
        }
    };

    // Check for cycles (infinite loop detection)
    let inode_key = (metadata.dev(), metadata.ino());
    if metadata.is_dir() && state.visited_inodes.contains(&inode_key) {
        let ancestor = state
            .visited_paths
            .get(&inode_key)
            .cloned()
            .unwrap_or_else(|| path.to_path_buf());
        eprintln!(
            "find: File system loop detected; '{}' is part of the same file system loop as '{}'.",
            path.display(),
            ancestor.display()
        );
        state.had_error = true;
        return;
    }

    // Device-crossing handling for -xdev / -mount. When a directory on a
    // different device is encountered (not a command-line path operand):
    //   -mount: do not act on it and do not descend (excludes the mount point).
    //   -xdev:  act on it but do not descend (includes the mount point).
    let on_other_dev = !is_cmdline && metadata.dev() != root_dev;
    if on_other_dev && state.mount {
        return;
    }
    let block_descend = on_other_dev && state.xdev;

    let ctx = EvalContext {
        path,
        root,
        metadata: &metadata,
        link_metadata: link_metadata.as_ref(),
        init_time: state.init_time,
    };
    let descend =
        metadata.is_dir() && !block_descend && state.max_depth.is_none_or(|max| depth < max);

    // If depth-first, process children before this entry
    if state.depth_first && descend {
        state.visited_inodes.insert(inode_key);
        state.visited_paths.insert(inode_key, path.to_path_buf());
        process_children(path, expr, root, root_dev, depth + 1, state);
        state.visited_inodes.remove(&inode_key);
        state.visited_paths.remove(&inode_key);
    }

    // Evaluate expression for this entry, unless it is above -mindepth
    let result = if depth >= state.min_depth {
        evaluate(expr, &ctx, state)
    } else {
        EvalResult::new(true)
    };

    // Handle batched exec files. Every id was assigned by
    // register_exec_batches() over this same expression, so it indexes a
    // registered batch.
    for (id, batch_path) in result.exec_batch_files {
        if let Some(batch) = state.exec_batches.get_mut(id) {
            batch.files.push(batch_path);
        }
    }

    // If not depth-first and is directory, process children
    if !state.depth_first && descend && !result.prune {
        state.visited_inodes.insert(inode_key);
        state.visited_paths.insert(inode_key, path.to_path_buf());
        process_children(path, expr, root, root_dev, depth + 1, state);
        state.visited_inodes.remove(&inode_key);
        state.visited_paths.remove(&inode_key);
    }
}

/// Process children of a directory
fn process_children(
    dir: &Path,
    expr: &Expr,
    root: &Path,
    root_dev: u64,
    depth: usize,
    state: &mut FindState,
) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("find: '{}': {}", dir.display(), e);
            state.had_error = true;
            return;
        }
    };

    for entry in entries {
        match entry {
            Ok(e) => {
                walk_tree(&e.path(), expr, root, root_dev, depth, state);
            }
            Err(e) => {
                eprintln!("find: error reading directory '{}': {}", dir.display(), e);
                state.had_error = true;
            }
        }
    }
}

/// Run one `-exec ... {} +` invocation over a chunk of files. Returns whether
/// it exited successfully.
fn run_exec_command(utility: &str, args_before: &[String], files: &[PathBuf]) -> bool {
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
    state.depth_first = has_depth(&expr);
    state.xdev = has_xdev(&expr);
    state.mount = has_mount(&expr);
    state.symlink_mode = symlink_mode;
    set_depth_limits(&expr, &mut state);

    // Give every `-exec ... {} +` primary its own aggregation set, in source
    // order, before the walk begins.
    register_exec_batches(&mut expr, &mut state.exec_batches);

    // Process each path
    for path in paths {
        // Get root device for -xdev
        let root_dev = match fs::metadata(&path) {
            Ok(m) => m.dev(),
            Err(e) => {
                eprintln!(
                    "find: '{}': {}",
                    path.display(),
                    plib::diag::io_error_text(&e)
                );
                state.had_error = true;
                continue;
            }
        };

        walk_tree(&path, &expr, &path, root_dev, 0, &mut state);
    }

    // Execute any pending batched commands
    execute_batches(&mut state);

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
