//
// Copyright (c) 2024 Hemi Labs, Inc.
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

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::modestr;
use regex::Regex;

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
        utility: String,
        args_before: Vec<String>,
    },
}

/// A primary (test or action) in the find expression
#[derive(Clone, Debug)]
enum Primary {
    // Tests
    Name(Regex),
    Path(Regex),
    Type(FileTypeMatch),
    Perm(PermMode),
    Links(NumericComparison),
    User(String),
    Group(String),
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
    Prune,
    Exec(ExecMode),
    Ok {
        utility: String,
        args: Vec<String>,
    },

    // Global options (always true, affect traversal)
    Depth,
    XDev,
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
    /// Files to pass to batched -exec
    exec_batch_files: Vec<PathBuf>,
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
    /// Device IDs of visited directories (for cycle detection)
    visited_inodes: HashSet<(u64, u64)>,
    /// Whether expression contains any action (-print, -exec, -ok)
    has_action: bool,
    /// Files accumulated for batched -exec
    exec_batches: Vec<(ExecMode, Vec<PathBuf>)>,
}

impl FindState {
    fn new() -> Self {
        Self {
            had_error: false,
            depth_first: false,
            xdev: false,
            visited_inodes: HashSet::new(),
            has_action: false,
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
            let regex = pattern_to_regex(pattern)?;
            Ok(Expr::Primary(Primary::Name(regex)))
        }
        "-path" => {
            let pattern = get_arg(tokens, idx, "-path")?;
            let regex = pattern_to_regex(pattern)?;
            Ok(Expr::Primary(Primary::Path(regex)))
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
            let uname = get_arg(tokens, idx, "-user")?;
            Ok(Expr::Primary(Primary::User(uname.to_string())))
        }
        "-group" => {
            let gname = get_arg(tokens, idx, "-group")?;
            Ok(Expr::Primary(Primary::Group(gname.to_string())))
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
            let metadata =
                fs::metadata(file).map_err(|e| format!("cannot access '{}': {}", file, e))?;
            let mtime = metadata
                .modified()
                .map_err(|e| format!("cannot get mtime of '{}': {}", file, e))?;
            Ok(Expr::Primary(Primary::Newer(mtime)))
        }
        "-nouser" => Ok(Expr::Primary(Primary::NoUser)),
        "-nogroup" => Ok(Expr::Primary(Primary::NoGroup)),
        "-print" => Ok(Expr::Primary(Primary::Print)),
        "-print0" => Ok(Expr::Primary(Primary::Print0)),
        "-prune" => Ok(Expr::Primary(Primary::Prune)),
        "-depth" => Ok(Expr::Primary(Primary::Depth)),
        "-xdev" => Ok(Expr::Primary(Primary::XDev)),
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
            Primary::Print | Primary::Print0 | Primary::Exec(_) | Primary::Ok { .. }
        ),
        Expr::Not(e) => has_action(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_action(l) || has_action(r),
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

/// Check if expression contains -xdev
fn has_xdev(expr: &Expr) -> bool {
    match expr {
        Expr::Primary(Primary::XDev) => true,
        Expr::Not(e) => has_xdev(e),
        Expr::And(l, r) | Expr::Or(l, r) => has_xdev(l) || has_xdev(r),
        _ => false,
    }
}

/// Convert shell glob pattern to regex
fn pattern_to_regex(pattern: &str) -> Result<Regex, String> {
    let mut regex_str = String::from("^");

    let mut chars = pattern.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '*' => regex_str.push_str(".*"),
            '?' => regex_str.push('.'),
            '[' => {
                regex_str.push('[');
                // Handle character class
                if chars.peek() == Some(&'!') {
                    chars.next();
                    regex_str.push('^');
                }
                // Copy until ]
                let mut first = true;
                while let Some(&cc) = chars.peek() {
                    chars.next();
                    if cc == ']' && !first {
                        regex_str.push(']');
                        break;
                    }
                    // Escape regex special chars inside class
                    if cc == '\\' || cc == '^' || cc == '-' {
                        regex_str.push('\\');
                    }
                    regex_str.push(cc);
                    first = false;
                }
            }
            '.' | '+' | '^' | '$' | '(' | ')' | '{' | '}' | '|' | '\\' => {
                regex_str.push('\\');
                regex_str.push(c);
            }
            _ => regex_str.push(c),
        }
    }

    regex_str.push('$');
    Regex::new(&regex_str).map_err(|e| format!("invalid pattern: {}", e))
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
        Primary::Name(regex) => {
            let name = ctx.path.file_name().unwrap_or(OsStr::new(""));
            let name_str = name.to_string_lossy();
            EvalResult::new(regex.is_match(&name_str))
        }
        Primary::Path(regex) => {
            let path_str = ctx.path.to_string_lossy();
            EvalResult::new(regex.is_match(&path_str))
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
        Primary::User(uname) => {
            // Resolve at evaluation time - return false if user doesn't exist
            let target_uid = match resolve_user(uname) {
                Ok(uid) => uid,
                Err(_) => return EvalResult::new(false),
            };
            EvalResult::new(ctx.metadata.uid() == target_uid)
        }
        Primary::Group(gname) => {
            // Resolve at evaluation time - return false if group doesn't exist
            let target_gid = match resolve_group(gname) {
                Ok(gid) => gid,
                Err(_) => return EvalResult::new(false),
            };
            EvalResult::new(ctx.metadata.gid() == target_gid)
        }
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
        Primary::Depth => {
            // Always true, affects traversal order (handled globally)
            EvalResult::new(true)
        }
        Primary::XDev => {
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
                        Err(_) => {
                            state.had_error = true;
                            EvalResult::new(false)
                        }
                    }
                }
                ExecMode::Batch { .. } => {
                    // Batch mode: accumulate files, always return true
                    let mut result = EvalResult::new(true);
                    result.exec_batch_files.push(ctx.path.to_path_buf());
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

            let response = response.trim().to_lowercase();
            if response != "y" && response != "yes" {
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
                Err(_) => {
                    state.had_error = true;
                    EvalResult::new(false)
                }
            }
        }
    }
}

/// Calculate time difference in days (init_time - file_time) / 86400
fn time_diff_days(init_time: SystemTime, file_time: SystemTime) -> i64 {
    match init_time.duration_since(file_time) {
        Ok(d) => (d.as_secs() / 86400) as i64,
        Err(_) => 0, // File is in the future
    }
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

/// Walk a directory tree and evaluate expression for each file
fn walk_tree(
    path: &Path,
    expr: &Expr,
    symlink_mode: SymlinkMode,
    root_dev: u64,
    init_time: SystemTime,
    state: &mut FindState,
    is_cmdline: bool,
) {
    // Get metadata
    let (metadata, link_metadata) = match get_metadata(path, symlink_mode, is_cmdline) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("find: '{}': {}", path.display(), e);
            state.had_error = true;
            return;
        }
    };

    // Check for cycles (infinite loop detection)
    let inode_key = (metadata.dev(), metadata.ino());
    if metadata.is_dir() && state.visited_inodes.contains(&inode_key) {
        eprintln!(
            "find: File system loop detected; '{}' is part of the same file system loop as '{}'.",
            path.display(),
            path.display()
        );
        state.had_error = true;
        return;
    }

    // Check xdev
    if state.xdev && metadata.dev() != root_dev && !is_cmdline {
        return;
    }

    let ctx = EvalContext {
        path,
        metadata: &metadata,
        link_metadata: link_metadata.as_ref(),
        init_time,
    };

    // If depth-first, process children before this entry
    if state.depth_first && metadata.is_dir() {
        state.visited_inodes.insert(inode_key);
        process_children(path, expr, symlink_mode, root_dev, init_time, state);
        state.visited_inodes.remove(&inode_key);
    }

    // Evaluate expression for this entry
    let result = evaluate(expr, &ctx, state);

    // Handle batched exec files
    for batch_path in result.exec_batch_files {
        // Find matching batch or create new one
        let batch_idx = find_or_create_batch(expr, state);
        if let Some(idx) = batch_idx {
            state.exec_batches[idx].1.push(batch_path);
        }
    }

    // If not depth-first and is directory, process children
    if !state.depth_first && metadata.is_dir() && !result.prune {
        state.visited_inodes.insert(inode_key);
        process_children(path, expr, symlink_mode, root_dev, init_time, state);
        state.visited_inodes.remove(&inode_key);
    }
}

/// Process children of a directory
fn process_children(
    dir: &Path,
    expr: &Expr,
    symlink_mode: SymlinkMode,
    root_dev: u64,
    init_time: SystemTime,
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
                walk_tree(
                    &e.path(),
                    expr,
                    symlink_mode,
                    root_dev,
                    init_time,
                    state,
                    false,
                );
            }
            Err(e) => {
                eprintln!("find: error reading directory '{}': {}", dir.display(), e);
                state.had_error = true;
            }
        }
    }
}

/// Find or create a batch for -exec {} +
fn find_or_create_batch(expr: &Expr, state: &mut FindState) -> Option<usize> {
    // Find first batch exec in expression
    fn find_batch_exec(expr: &Expr) -> Option<ExecMode> {
        match expr {
            Expr::Primary(Primary::Exec(mode @ ExecMode::Batch { .. })) => Some(mode.clone()),
            Expr::Not(e) => find_batch_exec(e),
            Expr::And(l, r) | Expr::Or(l, r) => find_batch_exec(l).or_else(|| find_batch_exec(r)),
            _ => None,
        }
    }

    if let Some(mode) = find_batch_exec(expr) {
        // Check if we already have this batch
        for (i, (m, _)) in state.exec_batches.iter().enumerate() {
            if matches!((m, &mode), (ExecMode::Batch { utility: u1, .. }, ExecMode::Batch { utility: u2, .. }) if u1 == u2)
            {
                return Some(i);
            }
        }
        // Create new batch
        state.exec_batches.push((mode, Vec::new()));
        Some(state.exec_batches.len() - 1)
    } else {
        None
    }
}

/// Execute all pending batched -exec commands
fn execute_batches(state: &mut FindState) {
    for (mode, files) in state.exec_batches.drain(..) {
        if files.is_empty() {
            continue;
        }

        if let ExecMode::Batch {
            utility,
            args_before,
        } = mode
        {
            // Build command with all files
            let mut cmd = Command::new(&utility);
            cmd.args(&args_before);
            for f in &files {
                cmd.arg(f);
            }

            match cmd.status() {
                Ok(status) => {
                    if !status.success() {
                        state.had_error = true;
                    }
                }
                Err(e) => {
                    eprintln!("find: '{}': {}", utility, e);
                    state.had_error = true;
                }
            }
        }
    }
}

/// Main find function
fn find(args: Vec<String>) -> Result<i32, String> {
    let (symlink_mode, paths, mut expr) = parse_args(&args)?;

    // Check if expression has any action
    let expr_has_action = has_action(&expr);

    // If no action, wrap with implicit -print per POSIX
    if !expr_has_action {
        expr = Expr::And(Box::new(expr), Box::new(Expr::Primary(Primary::Print)));
    }

    // Set up state
    let mut state = FindState::new();
    state.depth_first = has_depth(&expr);
    state.xdev = has_xdev(&expr);
    state.has_action = expr_has_action;

    let init_time = SystemTime::now();

    // Process each path
    for path in paths {
        // Get root device for -xdev
        let root_dev = match fs::metadata(&path) {
            Ok(m) => m.dev(),
            Err(e) => {
                eprintln!("find: '{}': {}", path.display(), e);
                state.had_error = true;
                continue;
            }
        };

        walk_tree(
            &path,
            &expr,
            symlink_mode,
            root_dev,
            init_time,
            &mut state,
            true,
        );
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args: Vec<String> = std::env::args().collect();

    match find(args) {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            eprintln!("find: {}", e);
            std::process::exit(1);
        }
    }
}
