//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::VecDeque;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::mem;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::os::unix::process::ExitStatusExt;
use std::process::{Command, ExitStatus, Stdio};

use clap::Parser;
use gettextrs::gettext;
use plib::{diag, BUFSZ};

const FALLBACK_ARG_MAX: usize = 131072;
// POSIX requires at least 255 bytes for -I constructed arguments
// We use a higher limit for better usability
const INSERT_ARG_MAX: usize = 4096;

fn get_arg_max() -> usize {
    let result = unsafe { libc::sysconf(libc::_SC_ARG_MAX) };
    if result <= 0 {
        FALLBACK_ARG_MAX
    } else {
        result as usize
    }
}

fn get_max_args_bytes() -> usize {
    get_arg_max().saturating_sub(2048)
}

#[derive(Parser)]
#[command(
    version,
    about = gettext("xargs - construct argument lists and invoke utility"),
    trailing_var_arg = true
)]
struct Args {
    #[arg(
        short = 'L',
        long,
        allow_hyphen_values = true,
        conflicts_with_all = ["maxnum", "replstr"],
        help = gettext(
            "The utility shall be executed for each non-empty number lines of arguments from standard input"
        )
    )]
    lines: Option<usize>,

    #[arg(
        short = 'n',
        long,
        allow_hyphen_values = true,
        conflicts_with_all = ["lines", "replstr"],
        help = gettext(
            "Invoke utility using as many standard input arguments as possible, up to number"
        )
    )]
    maxnum: Option<usize>,

    #[arg(
        short = 's',
        long,
        allow_hyphen_values = true,
        help = gettext(
            "Invoke utility using as many standard input arguments as possible yielding a command line length less than size"
        )
    )]
    maxsize: Option<usize>,

    #[arg(
        short = 'E',
        long,
        allow_hyphen_values = true,
        default_value = "",
        help = gettext("Use eofstr as the logical end-of-file string")
    )]
    eofstr: OsString,

    #[arg(
        short = 'I',
        long,
        allow_hyphen_values = true,
        conflicts_with_all = ["lines", "maxnum"],
        help = gettext("Insert mode: execute utility for each line, replacing replstr with input")
    )]
    replstr: Option<OsString>,

    #[arg(short, long, help = gettext("Prompt mode: ask before executing each command"))]
    prompt: bool,

    #[arg(
        short = 'r',
        long = "no-run-if-empty",
        help = gettext("Do not run the utility if standard input yields no arguments")
    )]
    no_run_if_empty: bool,

    #[arg(short, long, help = gettext("Trace mode: print each command before execution"))]
    trace: bool,

    #[arg(short = '0', long = "null", help = gettext("Use null character as input delimiter"))]
    null_mode: bool,

    #[arg(
        short = 'x',
        long,
        help = gettext(
            "Terminate if a constructed command line will not fit in the implied or specified size"
        )
    )]
    exit: bool,

    // XBD 12.2 Guideline 9: xargs's options all precede the utility, so the
    // utility name and every word after it are one trailing operand list.
    // Two separate positionals let clap go on parsing options after the
    // utility name: `xargs touch -t STAMP` traced the command instead of
    // passing `-t` to touch.
    #[arg(
        value_name = "UTILITY",
        trailing_var_arg = true,
        help = gettext("Utility to invoke (default: echo) and its arguments")
    )]
    command: Vec<OsString>,

    #[arg(skip)]
    util: OsString,

    #[arg(skip)]
    util_args: Vec<OsString>,
}

impl Args {
    /// Parse the command line and split the operand list into the utility
    /// and its arguments.
    fn parse_command_line() -> Self {
        let mut args = plib::optarg::parse::<Args>();
        let mut command = std::mem::take(&mut args.command).into_iter();
        args.util = command.next().unwrap_or_else(|| OsString::from("echo"));
        args.util_args = command.collect();
        args
    }
}

/// Result of executing a utility
#[derive(Debug)]
enum ExecResult {
    /// Command executed and returned this exit code
    Exited(i32),
    /// Command was not found (exit 127)
    NotFound,
    /// Command found but could not be invoked (exit 126)
    CannotInvoke,
    /// User declined to execute (for -p mode)
    Skipped,
}

/// The command line `util util_args...` as the bytes that -t and -p write,
/// the words separated by single spaces.
fn command_line_bytes(util: &OsStr, util_args: &[OsString]) -> Vec<u8> {
    let mut line = util.as_bytes().to_vec();
    line.push(b' ');
    for (i, arg) in util_args.iter().enumerate() {
        if i > 0 {
            line.push(b' ');
        }
        line.extend_from_slice(arg.as_bytes());
    }
    line
}

/// Prompt user for confirmation. Returns true if user confirms.
fn prompt_confirm(util: &OsStr, util_args: &[OsString]) -> io::Result<bool> {
    // Write command and prompt to stderr.
    let mut stderr = io::stderr().lock();
    stderr.write_all(&command_line_bytes(util, util_args))?;
    stderr.write_all(b"?...")?;
    stderr.flush()?;
    drop(stderr);

    // Read response from /dev/tty (not stdin, which carries the argument list).
    let tty = File::open("/dev/tty")?;
    let mut reader = BufReader::new(tty);
    let mut response = String::new();
    reader.read_line(&mut response)?;

    // Affirmative per the locale's LC_MESSAGES yesexpr (falls back to y/Y).
    Ok(plib::locale::is_affirmative(
        response.trim_end_matches(['\r', '\n']),
    ))
}

/// Execute the utility with the given arguments
fn exec_util(
    util: &OsStr,
    util_args: Vec<OsString>,
    trace: bool,
    prompt: bool,
) -> io::Result<ExecResult> {
    // If prompting, ask user for confirmation
    if prompt {
        match prompt_confirm(util, &util_args) {
            Ok(true) => {} // proceed
            Ok(false) => return Ok(ExecResult::Skipped),
            Err(_) => return Ok(ExecResult::Skipped), // if can't read tty, skip
        }
    } else if trace {
        // If tracing (and not prompting, since prompt implies trace output),
        // write command to stderr
        let mut line = command_line_bytes(util, &util_args);
        line.push(b'\n');
        io::stderr().write_all(&line)?;
    }

    match Command::new(util)
        .args(util_args)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
    {
        Ok(output) => {
            let code = exit_code_from_status(output.status);
            Ok(ExecResult::Exited(code))
        }
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                diag::error(&format!(
                    "{}: {}",
                    util.to_string_lossy(),
                    gettext("No such file or directory")
                ));
                Ok(ExecResult::NotFound)
            } else {
                diag::error(&format!("{}: {}", util.to_string_lossy(), e));
                Ok(ExecResult::CannotInvoke)
            }
        }
    }
}

/// Convert ExitStatus to exit code
fn exit_code_from_status(status: ExitStatus) -> i32 {
    if let Some(code) = status.code() {
        code
    } else if let Some(sig) = status.signal() {
        // Killed by signal: return 128 + signal number
        128 + sig
    } else {
        1
    }
}

/// Input is parsed as bytes: an argument is a byte string, as a pathname is,
/// and is handed to the utility exactly as read.  The delimiters (<blank>,
/// <newline>, quotes, backslash, NUL) are all single ASCII bytes, which never
/// occur inside a multibyte UTF-8 character.
struct ParseState {
    // cmdline-related state
    util_size: usize,

    // input state
    tmp_arg: Vec<u8>,
    in_arg: bool,
    in_quote: bool,
    in_escape: bool,
    quote_char: u8,
    skip_remainder: bool,
    null_slop: Vec<u8>,
    // Set when a <newline> appears inside a quoted string (an error per POSIX:
    // a quoted string is "non-<quote> non-<newline> characters") or a quote is
    // left unterminated at end of input.
    unmatched_quote: bool,

    // line mode state
    line_count: usize, // number of complete non-empty lines seen
    max_lines: Option<usize>,
    line_continues: bool,   // trailing blank means continuation
    line_has_content: bool, // current line has at least one arg

    // output state
    max_bytes: usize,
    max_args: Option<usize>,
    exit_on_overflow: bool,

    // parsed args, ready for exec
    args: VecDeque<Vec<u8>>,
    // Bytes of `args`, each counted with its terminating NUL.
    args_size: usize,
}

/// True for a <blank> byte.
fn is_blank(byte: u8) -> bool {
    byte == b' ' || byte == b'\t'
}

/// `line` without its leading <blank> bytes.
fn trim_leading_blanks(line: &[u8]) -> &[u8] {
    let start = line
        .iter()
        .position(|&b| !is_blank(b))
        .unwrap_or(line.len());
    &line[start..]
}

/// `word` with every occurrence of `from` replaced by `to`.
fn replace_bytes(word: &[u8], from: &[u8], to: &[u8]) -> Vec<u8> {
    if from.is_empty() {
        return word.to_vec();
    }
    let mut out = Vec::with_capacity(word.len());
    let mut rest = word;
    while let Some(pos) = rest.windows(from.len()).position(|w| w == from) {
        out.extend_from_slice(&rest[..pos]);
        out.extend_from_slice(to);
        rest = &rest[pos + from.len()..];
    }
    out.extend_from_slice(rest);
    out
}

impl ParseState {
    fn new(args: &Args) -> ParseState {
        let mut total = args.util.len() + 1; // +1 for null terminator
        for arg in &args.util_args {
            total += arg.len() + 1; // +1 for null terminator
        }

        // -I implies -x
        let exit_on_overflow = args.exit || args.replstr.is_some();

        ParseState {
            util_size: total,
            tmp_arg: Vec::new(),
            in_arg: false,
            in_quote: false,
            in_escape: false,
            quote_char: b'"',
            skip_remainder: false,
            null_slop: Vec::new(),
            unmatched_quote: false,
            line_count: 0,
            max_lines: args.lines,
            line_continues: false,
            line_has_content: false,
            max_bytes: args.maxsize.unwrap_or_else(get_max_args_bytes),
            max_args: args.maxnum,
            exit_on_overflow,
            args: VecDeque::new(),
            args_size: 0,
        }
    }

    /// Queue a parsed argument for the next command.
    fn push_arg(&mut self, arg: Vec<u8>) {
        self.args_size += arg.len() + 1; // +1 for null terminator
        self.args.push_back(arg);
    }

    /// Take the oldest queued argument.
    fn pop_arg(&mut self) -> Option<Vec<u8>> {
        let arg = self.args.pop_front()?;
        self.args_size -= arg.len() + 1;
        Some(arg)
    }

    /// Queue the argument being accumulated.
    fn push_tmp_arg(&mut self) {
        let arg = mem::take(&mut self.tmp_arg);
        self.push_arg(arg);
    }

    fn current_cmd_size(&self) -> usize {
        self.util_size + self.args_size
    }

    fn full(&self) -> bool {
        if self.current_cmd_size() > self.max_bytes {
            return true;
        }

        if let Some(max_args) = self.max_args {
            // POSIX: -n limits stdin arguments only, not utility command-line args
            if self.args.len() >= max_args {
                return true;
            }
        }

        if let Some(max_lines) = self.max_lines {
            if self.line_count >= max_lines {
                return true;
            }
        }

        false
    }

    /// Check if a single argument is too large to fit
    fn arg_too_large(&self, arg: &[u8]) -> bool {
        self.util_size + arg.len() + 1 > self.max_bytes
    }

    fn remove_args(&mut self) -> Vec<OsString> {
        let mut total = self.util_size;
        let mut ret = Vec::new();

        while let Some(front) = self.args.front() {
            // stop if adding the next arg would exceed the max size
            if (total + front.len() + 1) > self.max_bytes {
                break;
            }

            // add the next arg
            let arg = self.pop_arg().unwrap();
            total += arg.len() + 1; // +1 for null terminator
            ret.push(OsString::from_vec(arg));

            // stop if we have reached the max number of args
            // POSIX: -n limits stdin arguments only, not utility command-line args
            if let Some(max_args) = self.max_args {
                if ret.len() >= max_args {
                    break;
                }
            }
        }

        // In line mode, reset line count after removing args
        if self.max_lines.is_some() {
            self.line_count = 0;
            self.line_has_content = false;
        }

        ret
    }

    // args are null-separated, without any further processing.
    // if the input data crosses a null boundary, the remainder is
    // stored as state for the next call to parse_buf_null.
    fn parse_buf_null(&mut self, in_buf: &[u8]) {
        if self.skip_remainder {
            return;
        }

        let mut rest = in_buf;
        while let Some(pos) = rest.iter().position(|&b| b == 0) {
            let mut arg = mem::take(&mut self.null_slop);
            arg.extend_from_slice(&rest[..pos]);
            self.push_arg(arg);
            rest = &rest[pos + 1..];
        }

        // remember remainder, if any, for next call
        self.null_slop.extend_from_slice(rest);
    }

    fn parse_buf(&mut self, text: &[u8]) {
        if self.skip_remainder {
            return;
        }

        let mut prev_was_blank = false;

        for &ch in text {
            if self.in_quote {
                if ch == self.quote_char {
                    self.in_quote = false;
                    self.in_arg = false;
                    self.push_tmp_arg();
                    self.line_has_content = true;
                } else if ch == b'\n' {
                    // A <newline> inside a quoted string is not permitted.
                    self.unmatched_quote = true;
                    return;
                } else {
                    self.tmp_arg.push(ch);
                }
                prev_was_blank = false;
            } else if self.in_escape {
                self.in_escape = false;
                if ch == b'\n' {
                    // Escaped newline: in -L mode, this continues the line
                    // but doesn't add anything to the argument
                } else {
                    self.tmp_arg.push(ch);
                }
                prev_was_blank = false;
            } else if ch == b'\n' {
                // End of line
                if self.in_arg {
                    self.in_arg = false;
                    self.push_tmp_arg();
                    self.line_has_content = true;
                }

                // In -L mode: count non-empty lines
                if self.max_lines.is_some() {
                    if prev_was_blank && self.line_has_content {
                        // Trailing blank means continuation to next line
                        self.line_continues = true;
                    } else if self.line_continues {
                        // Was continuing from previous line
                        if !prev_was_blank {
                            // No more continuation, count this as a line
                            self.line_continues = false;
                            if self.line_has_content {
                                self.line_count += 1;
                                self.line_has_content = false;
                            }
                        }
                        // If still has trailing blank, continue to next line
                    } else if self.line_has_content {
                        // Normal non-empty line
                        self.line_count += 1;
                        self.line_has_content = false;
                    }
                    // Empty lines don't count
                }
                prev_was_blank = false;
            } else if self.in_arg && is_blank(ch) {
                self.in_arg = false;
                self.push_tmp_arg();
                self.line_has_content = true;
                prev_was_blank = true;
            } else if ch == b'\'' || ch == b'"' {
                self.in_arg = true;
                self.in_quote = true;
                self.quote_char = ch;
                prev_was_blank = false;
            } else if ch == b'\\' {
                self.in_escape = true;
                prev_was_blank = false;
            } else if is_blank(ch) {
                // ignore leading/inter-arg whitespace
                prev_was_blank = true;
            } else {
                self.in_arg = true;
                self.tmp_arg.push(ch);
                prev_was_blank = false;
            }
        }
    }

    /// Queue the line accumulated in insert mode, without its leading
    /// <blank> characters, unless that leaves it empty.
    fn push_insert_line(&mut self) {
        let line = mem::take(&mut self.tmp_arg);
        let trimmed = trim_leading_blanks(&line);
        if !trimmed.is_empty() {
            let arg = trimmed.to_vec();
            self.push_arg(arg);
        }
    }

    /// Parse text for -I insert mode: lines are separated only by newlines,
    /// blanks are preserved within arguments
    fn parse_buf_insert(&mut self, text: &[u8]) {
        if self.skip_remainder {
            return;
        }

        for &ch in text {
            if self.in_escape {
                self.in_escape = false;
                if ch == b'\n' {
                    // Escaped newline: continue line, don't add newline
                } else {
                    self.tmp_arg.push(ch);
                }
            } else if ch == b'\n' {
                // End of line - this is our argument (trim leading blanks)
                self.push_insert_line();
            } else if ch == b'\\' {
                self.in_escape = true;
            } else {
                self.tmp_arg.push(ch);
            }
        }
    }

    fn parse_finalize(&mut self) {
        if self.in_quote {
            // Input ended with an open quote.
            self.unmatched_quote = true;
        }

        if self.in_arg {
            self.in_arg = false;
            self.push_tmp_arg();
            self.line_has_content = true;
        } else if !self.tmp_arg.is_empty() {
            // For insert mode: finalize any remaining content
            self.push_insert_line();
        }

        if !self.null_slop.is_empty() {
            let arg = mem::take(&mut self.null_slop);
            self.push_arg(arg);
        }

        // Count final partial line if it had content
        if self.max_lines.is_some() && self.line_has_content {
            self.line_count += 1;
        }
    }

    fn postprocess(&mut self, args: &Args) {
        let eofstr = args.eofstr.as_bytes();
        if !eofstr.is_empty() {
            if let Some(pos) = self.args.iter().position(|s| s == eofstr) {
                while self.args.len() > pos {
                    let arg = self.args.pop_back().unwrap();
                    self.args_size -= arg.len() + 1;
                }
                self.skip_remainder = true;
            }
        }
    }
}

/// Execute for insert mode (-I): one invocation per input line,
/// replacing replstr in utility args with the input
fn exec_insert_mode(
    args: &Args,
    replstr: &OsStr,
    input_arg: &[u8],
    trace: bool,
    prompt: bool,
) -> io::Result<ExecResult> {
    // Replace replstr with input_arg in each utility argument
    let util_args: Vec<OsString> = args
        .util_args
        .iter()
        .map(|arg| OsString::from_vec(replace_bytes(arg.as_bytes(), replstr.as_bytes(), input_arg)))
        .collect();

    // POSIX: Check that constructed arguments don't exceed the limit
    // POSIX requires at least 255 bytes, we use INSERT_ARG_MAX (4096).
    // Message has no "xargs: " prefix; the caller routes it through diag.
    if let Some(arg) = util_args.iter().find(|arg| arg.len() > INSERT_ARG_MAX) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "constructed argument of {} bytes exceeds {} byte limit in insert mode",
                arg.len(),
                INSERT_ARG_MAX
            ),
        ));
    }

    exec_util(&args.util, util_args, trace, prompt)
}

/// Helper macro to handle exec result
macro_rules! handle_exec_result {
    ($result:expr, $any_failed:expr) => {
        match $result {
            ExecResult::Exited(255) => {
                return Ok(1);
            }
            ExecResult::Exited(code) if code != 0 => {
                $any_failed = true;
            }
            ExecResult::NotFound => {
                return Ok(127);
            }
            ExecResult::CannotInvoke => {
                return Ok(126);
            }
            ExecResult::Skipped | ExecResult::Exited(0) => {}
            ExecResult::Exited(_) => {}
        }
    };
}

/// Emit the "argument line too long" diagnostic.
fn err_arg_too_long() {
    diag::error(&gettext("argument line too long"));
}

/// If parsing flagged an unterminated/invalid quote, report and return true.
fn check_quote_error(state: &ParseState) -> bool {
    if state.unmatched_quote {
        diag::error(&gettext("unterminated quote"));
        true
    } else {
        false
    }
}

fn read_and_spawn(args: &Args) -> io::Result<i32> {
    let mut state = ParseState::new(args);
    let mut any_failed = false;
    let mut invoked = false;
    let trace = args.trace || args.prompt; // -p implies -t
    let insert_mode = args.replstr.is_some();
    let line_mode = args.lines.is_some();

    // For line mode, read line-by-line to properly batch
    if line_mode {
        let stdin = io::stdin();
        let mut reader = BufReader::new(stdin.lock());
        let mut line = Vec::new();

        loop {
            line.clear();
            if reader.read_until(b'\n', &mut line)? == 0 {
                break;
            }
            // A last line without its <newline> still ends there.
            if line.last() != Some(&b'\n') {
                line.push(b'\n');
            }
            state.parse_buf(&line);
            if check_quote_error(&state) {
                return Ok(1);
            }
            state.postprocess(args);

            // Check if we have enough lines to execute
            while state.full() && !state.args.is_empty() {
                if state.exit_on_overflow && state.arg_too_large(&state.args[0]) {
                    err_arg_too_long();
                    return Ok(1);
                }

                let mut util_args = args.util_args.clone();
                let batch = state.remove_args();
                if batch.is_empty() {
                    break;
                }
                util_args.extend(batch);

                invoked = true;
                let result = exec_util(&args.util, util_args, trace, args.prompt)?;
                handle_exec_result!(result, any_failed);
            }
        }
    } else {
        // Non-line mode: use buffered reading
        let mut buffer = [0; BUFSZ];

        loop {
            let n_read = io::stdin().read(&mut buffer)?;
            if n_read == 0 {
                break;
            }

            if args.null_mode {
                state.parse_buf_null(&buffer[..n_read]);
            } else if insert_mode {
                state.parse_buf_insert(&buffer[..n_read]);
                state.postprocess(args);
            } else {
                state.parse_buf(&buffer[..n_read]);
                if check_quote_error(&state) {
                    return Ok(1);
                }
                state.postprocess(args);
            }

            // For insert mode: execute once per input line
            if insert_mode {
                let replstr = args.replstr.as_ref().unwrap();
                while !state.args.is_empty() {
                    let input_arg = state.pop_arg().unwrap();

                    if state.exit_on_overflow && state.arg_too_large(&input_arg) {
                        err_arg_too_long();
                        return Ok(1);
                    }

                    invoked = true;
                    let result = exec_insert_mode(args, replstr, &input_arg, trace, args.prompt)?;
                    handle_exec_result!(result, any_failed);
                }
            } else {
                // Normal mode: batch arguments
                while state.full() {
                    if state.exit_on_overflow
                        && !state.args.is_empty()
                        && state.arg_too_large(&state.args[0])
                    {
                        err_arg_too_long();
                        return Ok(1);
                    }

                    let mut util_args = args.util_args.clone();
                    let batch = state.remove_args();
                    if batch.is_empty() && state.exit_on_overflow {
                        err_arg_too_long();
                        return Ok(1);
                    }
                    util_args.extend(batch);

                    invoked = true;
                    let result = exec_util(&args.util, util_args, trace, args.prompt)?;
                    handle_exec_result!(result, any_failed);
                }
            }
        }
    }

    // finalize parsing
    state.parse_finalize();
    if check_quote_error(&state) {
        return Ok(1);
    }
    if !line_mode {
        state.postprocess(args);
    }

    // Handle remaining arguments
    if insert_mode {
        let replstr = args.replstr.as_ref().unwrap();
        while !state.args.is_empty() {
            let input_arg = state.pop_arg().unwrap();

            if state.exit_on_overflow && state.arg_too_large(&input_arg) {
                err_arg_too_long();
                return Ok(1);
            }

            invoked = true;
            let result = exec_insert_mode(args, replstr, &input_arg, trace, args.prompt)?;
            handle_exec_result!(result, any_failed);
        }
    } else if !state.args.is_empty() {
        let mut util_args = args.util_args.clone();
        util_args.extend(state.remove_args());

        invoked = true;
        let result = exec_util(&args.util, util_args, trace, args.prompt)?;
        handle_exec_result!(result, any_failed);
    }

    // POSIX: if standard input yields no arguments, the utility shall be
    // executed exactly once unless -r (--no-run-if-empty) was given. (Insert
    // mode substitutes per input line, so an empty input means zero runs.)
    if !invoked && !insert_mode && !args.no_run_if_empty {
        let result = exec_util(&args.util, args.util_args.clone(), trace, args.prompt)?;
        handle_exec_result!(result, any_failed);
    }

    Ok(if any_failed { 1 } else { 0 })
}

fn main() {
    diag::init_locale("xargs");

    let args = Args::parse_command_line();

    let exit_code = match read_and_spawn(&args) {
        Ok(code) => code,
        Err(e) => {
            diag::error(&e.to_string());
            1
        }
    };

    std::process::exit(exit_code);
}
