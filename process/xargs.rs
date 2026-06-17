//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::mem;
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
        conflicts_with_all = ["maxnum", "replstr"],
        help = gettext(
            "The utility shall be executed for each non-empty number lines of arguments from standard input"
        )
    )]
    lines: Option<usize>,

    #[arg(
        short = 'n',
        long,
        conflicts_with_all = ["lines", "replstr"],
        help = gettext(
            "Invoke utility using as many standard input arguments as possible, up to number"
        )
    )]
    maxnum: Option<usize>,

    #[arg(
        short = 's',
        long,
        help = gettext(
            "Invoke utility using as many standard input arguments as possible yielding a command line length less than size"
        )
    )]
    maxsize: Option<usize>,

    #[arg(
        short = 'E',
        long,
        default_value = "",
        help = gettext("Use eofstr as the logical end-of-file string")
    )]
    eofstr: String,

    #[arg(
        short = 'I',
        long,
        conflicts_with_all = ["lines", "maxnum"],
        help = gettext("Insert mode: execute utility for each line, replacing replstr with input")
    )]
    replstr: Option<String>,

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

    #[arg(default_value = "echo", help = gettext("Utility to invoke (default: echo)"))]
    util: String,

    #[arg(allow_hyphen_values = true, help = gettext("Utility arguments"))]
    util_args: Vec<String>,
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

/// Prompt user for confirmation. Returns true if user confirms.
fn prompt_confirm(util: &str, util_args: &[String]) -> io::Result<bool> {
    // Write command and prompt to stderr.
    eprint!("{} {}?...", util, util_args.join(" "));
    io::stderr().flush()?;

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
    util: &str,
    util_args: Vec<String>,
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
        eprintln!("{} {}", util, util_args.join(" "));
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
                    util,
                    gettext("No such file or directory")
                ));
                Ok(ExecResult::NotFound)
            } else {
                diag::error(&format!("{}: {}", util, e));
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

struct ParseState {
    // cmdline-related state
    util_size: usize,

    // input state
    tmp_arg: String,
    in_arg: bool,
    in_quote: bool,
    in_escape: bool,
    quote_char: char,
    skip_remainder: bool,
    null_slop: Vec<u8>,
    // Incomplete trailing UTF-8 bytes carried across read() boundaries.
    pending: Vec<u8>,
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
    args: VecDeque<String>,
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
            tmp_arg: String::new(),
            in_arg: false,
            in_quote: false,
            in_escape: false,
            quote_char: '"',
            skip_remainder: false,
            null_slop: Vec::new(),
            pending: Vec::new(),
            unmatched_quote: false,
            line_count: 0,
            max_lines: args.lines,
            line_continues: false,
            line_has_content: false,
            max_bytes: args.maxsize.unwrap_or_else(get_max_args_bytes),
            max_args: args.maxnum,
            exit_on_overflow,
            args: VecDeque::new(),
        }
    }

    fn current_cmd_size(&self) -> usize {
        let mut total = self.util_size;
        for arg in &self.args {
            total += arg.len() + 1; // +1 for null terminator
        }
        total
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
    fn arg_too_large(&self, arg: &str) -> bool {
        self.util_size + arg.len() + 1 > self.max_bytes
    }

    fn remove_args(&mut self) -> Vec<String> {
        let mut total = self.util_size;
        let mut ret = Vec::new();

        while let Some(front) = self.args.front() {
            // stop if adding the next arg would exceed the max size
            if (total + front.len() + 1) > self.max_bytes {
                break;
            }

            // add the next arg
            let arg = self.args.pop_front().unwrap();
            total += arg.len() + 1; // +1 for null terminator
            ret.push(arg);

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

    /// Decode a freshly-read byte chunk into a `String`, prepending any
    /// incomplete UTF-8 sequence carried over from the previous read and
    /// stashing a new incomplete trailing sequence for the next read. An
    /// invalid byte sequence in the middle is replaced with U+FFFD. This keeps
    /// multibyte characters intact instead of mangling each byte via `as char`.
    fn decode_chunk(&mut self, buf: &[u8]) -> String {
        let mut bytes = mem::take(&mut self.pending);
        bytes.extend_from_slice(buf);

        let mut out = String::new();
        let mut i = 0;
        while i < bytes.len() {
            match std::str::from_utf8(&bytes[i..]) {
                Ok(s) => {
                    out.push_str(s);
                    i = bytes.len();
                }
                Err(e) => {
                    let valid = e.valid_up_to();
                    if valid > 0 {
                        out.push_str(unsafe {
                            std::str::from_utf8_unchecked(&bytes[i..i + valid])
                        });
                        i += valid;
                    }
                    match e.error_len() {
                        Some(n) => {
                            out.push('\u{FFFD}');
                            i += n;
                        }
                        None => {
                            // Incomplete trailing sequence: keep for next read.
                            self.pending = bytes[i..].to_vec();
                            break;
                        }
                    }
                }
            }
        }
        out
    }

    /// Decode any leftover pending bytes at end-of-input, lossily (there is no
    /// more data to complete a truncated sequence).
    fn take_pending_lossy(&mut self) -> String {
        if self.pending.is_empty() {
            String::new()
        } else {
            String::from_utf8_lossy(&mem::take(&mut self.pending)).to_string()
        }
    }

    // args are null-separated, without any further processing.
    // if the input data crosses a null boundary, the remainder is
    // stored as state for the next call to parse_buf_null.
    fn parse_buf_null(&mut self, in_buf: &[u8]) {
        if self.skip_remainder {
            return;
        }

        // pull prior state into current buffer
        let mut buf = Vec::with_capacity(self.null_slop.len() + in_buf.len());
        buf.extend_from_slice(&self.null_slop);
        buf.extend_from_slice(in_buf);
        self.null_slop.clear();

        // divide buffer into null-terminated strings, with remainder
        let mut start = 0;
        let mut end = 0;
        while end < buf.len() {
            if buf[end] == 0 {
                let s = String::from_utf8_lossy(&buf[start..end]).to_string();
                self.args.push_back(s);
                start = end + 1;
            }
            end += 1;
        }

        // remember remainder, if any, for next call
        if start < buf.len() {
            self.null_slop.extend_from_slice(&buf[start..]);
        }
    }

    fn parse_buf(&mut self, text: &str) {
        if self.skip_remainder {
            return;
        }

        let mut prev_was_blank = false;

        for ch in text.chars() {
            if self.in_quote {
                if ch == self.quote_char {
                    self.in_quote = false;
                    self.in_arg = false;
                    self.args.push_back(mem::take(&mut self.tmp_arg));
                    self.line_has_content = true;
                } else if ch == '\n' {
                    // A <newline> inside a quoted string is not permitted.
                    self.unmatched_quote = true;
                    return;
                } else {
                    self.tmp_arg.push(ch);
                }
                prev_was_blank = false;
            } else if self.in_escape {
                self.in_escape = false;
                if ch == '\n' {
                    // Escaped newline: in -L mode, this continues the line
                    // but doesn't add anything to the argument
                } else {
                    self.tmp_arg.push(ch);
                }
                prev_was_blank = false;
            } else if ch == '\n' {
                // End of line
                if self.in_arg {
                    self.in_arg = false;
                    self.args.push_back(mem::take(&mut self.tmp_arg));
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
            } else if self.in_arg && (ch == ' ' || ch == '\t') {
                self.in_arg = false;
                self.args.push_back(mem::take(&mut self.tmp_arg));
                self.line_has_content = true;
                prev_was_blank = true;
            } else if ch == '\'' || ch == '"' {
                self.in_arg = true;
                self.in_quote = true;
                self.quote_char = ch;
                prev_was_blank = false;
            } else if ch == '\\' {
                self.in_escape = true;
                prev_was_blank = false;
            } else if ch == ' ' || ch == '\t' {
                // ignore leading/inter-arg whitespace
                prev_was_blank = true;
            } else {
                self.in_arg = true;
                self.tmp_arg.push(ch);
                prev_was_blank = false;
            }
        }
    }

    /// Parse text for -I insert mode: lines are separated only by newlines,
    /// blanks are preserved within arguments
    fn parse_buf_insert(&mut self, text: &str) {
        if self.skip_remainder {
            return;
        }

        for ch in text.chars() {
            if self.in_escape {
                self.in_escape = false;
                if ch == '\n' {
                    // Escaped newline: continue line, don't add newline
                } else {
                    self.tmp_arg.push(ch);
                }
            } else if ch == '\n' {
                // End of line - this is our argument (trim leading blanks)
                let trimmed = self.tmp_arg.trim_start().to_string();
                if !trimmed.is_empty() {
                    self.args.push_back(trimmed);
                }
                self.tmp_arg.clear();
            } else if ch == '\\' {
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
            self.args.push_back(mem::take(&mut self.tmp_arg));
            self.line_has_content = true;
        } else if !self.tmp_arg.is_empty() {
            // For insert mode: finalize any remaining content
            let trimmed = self.tmp_arg.trim_start().to_string();
            if !trimmed.is_empty() {
                self.args.push_back(trimmed);
            }
            self.tmp_arg.clear();
        }

        if !self.null_slop.is_empty() {
            let s = String::from_utf8_lossy(&self.null_slop).to_string();
            self.args.push_back(s);
            self.null_slop.clear();
        }

        // Count final partial line if it had content
        if self.max_lines.is_some() && self.line_has_content {
            self.line_count += 1;
        }
    }

    fn postprocess(&mut self, args: &Args) {
        if !args.eofstr.is_empty() {
            if let Some(pos) = self.args.iter().position(|s| s == &args.eofstr) {
                self.args.truncate(pos);
                self.skip_remainder = true;
            }
        }
    }
}

/// Execute for insert mode (-I): one invocation per input line,
/// replacing replstr in utility args with the input
fn exec_insert_mode(
    args: &Args,
    replstr: &str,
    input_arg: &str,
    trace: bool,
    prompt: bool,
) -> io::Result<ExecResult> {
    // Replace replstr with input_arg in each utility argument
    let util_args: Vec<String> = args
        .util_args
        .iter()
        .map(|arg| arg.replace(replstr, input_arg))
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
        let reader = BufReader::new(stdin.lock());

        for line_result in reader.lines() {
            let line = line_result?;

            // Parse the line (add newline since BufReader strips it)
            let line_with_nl = format!("{}\n", line);
            state.parse_buf(&line_with_nl);
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
                let text = state.decode_chunk(&buffer[..n_read]);
                state.parse_buf_insert(&text);
                state.postprocess(args);
            } else {
                let text = state.decode_chunk(&buffer[..n_read]);
                state.parse_buf(&text);
                if check_quote_error(&state) {
                    return Ok(1);
                }
                state.postprocess(args);
            }

            // For insert mode: execute once per input line
            if insert_mode {
                let replstr = args.replstr.as_ref().unwrap();
                while !state.args.is_empty() {
                    let input_arg = state.args.pop_front().unwrap();

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

        // Flush any incomplete trailing bytes (lossily) before finalizing.
        let leftover = state.take_pending_lossy();
        if !leftover.is_empty() {
            if insert_mode {
                state.parse_buf_insert(&leftover);
            } else {
                state.parse_buf(&leftover);
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
            let input_arg = state.args.pop_front().unwrap();

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

    let args = Args::parse();

    let exit_code = match read_and_spawn(&args) {
        Ok(code) => code,
        Err(e) => {
            diag::error(&e.to_string());
            1
        }
    };

    std::process::exit(exit_code);
}
