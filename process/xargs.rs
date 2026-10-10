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
use std::process::{Child, Command, ExitStatus, Stdio};

use clap::{CommandFactory, FromArgMatches, Parser};
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

/// The default command line length when -s is not given: GNU xargs's,
/// comfortably above the {LINE_MAX} POSIX requires as a minimum.
const DEFAULT_SIZE: usize = 128 * 1024;

/// Bytes the environment takes in exec's combined argument and environment
/// lists: each string with its NUL, and its pointer.
fn environment_size() -> usize {
    std::env::vars_os()
        .map(|(name, value)| name.len() + value.len() + 2 + mem::size_of::<*const u8>())
        .sum()
}

/// The largest command line length allowed: POSIX bounds the combined
/// argument and environment lists by {ARG_MAX}-2048 bytes.
fn get_max_args_bytes() -> usize {
    get_arg_max()
        .saturating_sub(2048)
        .saturating_sub(environment_size())
}

/// The command line length to build up to: -s's size, or the default, but
/// never beyond what exec accepts.
fn command_size_limit(maxsize: Option<usize>) -> usize {
    maxsize.unwrap_or(DEFAULT_SIZE).min(get_max_args_bytes())
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
        help = gettext(
            "The utility shall be executed for each non-empty number lines of arguments from standard input"
        )
    )]
    lines: Option<usize>,

    #[arg(
        short = 'n',
        long,
        allow_hyphen_values = true,
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

    #[arg(
        short = 'P',
        allow_hyphen_values = true,
        value_name = "MAXPROCS",
        default_value_t = 1,
        help = gettext("Run up to maxprocs invocations of the utility at once (0: no limit)")
    )]
    max_procs: usize,

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
        let matches = Args::command().get_matches_from(plib::optarg::args_os::<Args>());
        let mut args = Args::from_arg_matches(&matches).unwrap_or_else(|e| e.exit());
        args.keep_last_batching_option(&matches);
        let mut command = std::mem::take(&mut args.command).into_iter();
        args.util = command.next().unwrap_or_else(|| OsString::from("echo"));
        args.util_args = command.collect();
        args
    }

    /// -I, -L and -n are mutually exclusive: the last one specified takes
    /// effect, as POSIX permits, and each one it cancels is warned about.
    /// `-n 1` after -I leaves -I in effect, silently, as GNU xargs does: each
    /// -I command takes one line anyway, and util-linux's test runner passes
    /// `-I '{}' ... -n 1`.
    fn keep_last_batching_option(&mut self, matches: &clap::ArgMatches) {
        let mut given: Vec<(usize, char)> = [("replstr", 'I'), ("lines", 'L'), ("maxnum", 'n')]
            .into_iter()
            .filter_map(|(id, opt)| Some((matches.index_of(id)?, opt)))
            .collect();
        given.sort();
        let mut in_effect: Option<char> = None;
        for (_, opt) in given {
            if in_effect == Some('I') && opt == 'n' && self.maxnum == Some(1) {
                self.maxnum = None;
                continue;
            }
            if let Some(earlier) = in_effect {
                diag::warning(
                    &gettext("options -{} and -{} are mutually exclusive; ignoring -{}")
                        .replacen("{}", &earlier.to_string(), 1)
                        .replacen("{}", &opt.to_string(), 1)
                        .replacen("{}", &earlier.to_string(), 1),
                );
                match earlier {
                    'I' => self.replstr = None,
                    'L' => self.lines = None,
                    _ => self.maxnum = None,
                }
            }
            in_effect = Some(opt);
        }
    }
}

/// Result of executing a utility
#[derive(Debug)]
enum ExecResult {
    /// Command executed and returned this exit code
    Exited(i32),
    /// Command was terminated by this signal
    Signaled(i32),
    /// Command was not found (exit 127)
    NotFound,
    /// Command found but could not be invoked (exit 126)
    CannotInvoke,
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

/// Runs the invocations of the utility, up to `max_procs` at once (0: no
/// limit), and gathers what their results mean for xargs (`stop_after`).
struct Runner<'a> {
    util: &'a OsStr,
    trace: bool,
    prompt: bool,
    max_procs: usize,
    running: Vec<Child>,
    any_failed: bool,
    /// The exit status once xargs must stop launching invocations.
    stop: Option<i32>,
}

impl<'a> Runner<'a> {
    fn new(args: &'a Args) -> Self {
        Runner {
            util: &args.util,
            trace: args.trace || args.prompt, // -p implies -t
            prompt: args.prompt,
            max_procs: args.max_procs,
            running: Vec::new(),
            any_failed: false,
            stop: None,
        }
    }

    /// Invoke the utility with `util_args`, first waiting for one running
    /// invocation if `max_procs` are. False once xargs must stop: then
    /// nothing more is launched, and `finish` gives its exit status.
    fn run(&mut self, util_args: Vec<OsString>) -> io::Result<bool> {
        if self.max_procs != 0 && self.running.len() >= self.max_procs {
            self.reap_one()?;
        }
        if self.stop.is_some() {
            return Ok(false);
        }
        match self.spawn(util_args)? {
            Some(child) => self.running.push(child),
            None => return Ok(self.stop.is_none()),
        }
        // One at a time: the invocation ends before the next input is read.
        if self.max_procs == 1 {
            self.reap_one()?;
        }
        Ok(self.stop.is_none())
    }

    /// Start the utility after -p's prompt or -t's trace; `None` when -p is
    /// declined or the utility cannot be started (recorded).
    fn spawn(&mut self, util_args: Vec<OsString>) -> io::Result<Option<Child>> {
        let util = self.util;
        if self.prompt {
            // A tty that cannot be read declines.
            if !prompt_confirm(util, &util_args).unwrap_or(false) {
                return Ok(None);
            }
        } else if self.trace {
            // If tracing (and not prompting, since prompt implies trace
            // output), write command to stderr
            let mut line = command_line_bytes(util, &util_args);
            line.push(b'\n');
            io::stderr().write_all(&line)?;
        }

        match Command::new(util)
            .args(util_args)
            .stdin(Stdio::null())
            .spawn()
        {
            Ok(child) => Ok(Some(child)),
            Err(e) => {
                let result = if e.kind() == io::ErrorKind::NotFound {
                    diag::error(&format!(
                        "{}: {}",
                        util.to_string_lossy(),
                        gettext("No such file or directory")
                    ));
                    ExecResult::NotFound
                } else {
                    diag::error(&format!("{}: {}", util.to_string_lossy(), e));
                    ExecResult::CannotInvoke
                };
                self.record(result);
                Ok(None)
            }
        }
    }

    /// Wait for one running invocation to end, and record its result.
    fn reap_one(&mut self) -> io::Result<()> {
        let status = if self.running.len() == 1 {
            self.running.pop().expect("one running").wait()?
        } else {
            // Any of them: xargs has no children but these.
            loop {
                let mut status = 0;
                let pid = unsafe { libc::waitpid(-1, &mut status, 0) };
                if pid < 0 {
                    let e = io::Error::last_os_error();
                    if e.kind() == io::ErrorKind::Interrupted {
                        continue;
                    }
                    return Err(e);
                }
                if let Some(i) = self.running.iter().position(|c| c.id() == pid as u32) {
                    // Reaped by the waitpid above.
                    #[allow(clippy::zombie_processes)]
                    self.running.swap_remove(i);
                    break ExitStatus::from_raw(status);
                }
            }
        };
        self.record(match status.signal() {
            Some(sig) => ExecResult::Signaled(sig),
            None => ExecResult::Exited(status.code().unwrap_or(1)),
        });
        Ok(())
    }

    fn record(&mut self, result: ExecResult) {
        if let Some(code) = stop_after(result, self.util, &mut self.any_failed) {
            self.stop.get_or_insert(code);
        }
    }

    /// Wait for every running invocation, and give xargs's exit status:
    /// `status` when xargs itself failed, else what the invocations gave.
    fn finish(&mut self, status: i32) -> i32 {
        while !self.running.is_empty() {
            if let Err(e) = self.reap_one() {
                diag::error(&e.to_string());
                self.running.clear();
                self.stop.get_or_insert(1);
            }
        }
        match self.stop {
            Some(code) => code,
            None if status != 0 => status,
            None => i32::from(self.any_failed),
        }
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
            max_bytes: command_size_limit(args.maxsize),
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

/// The utility's arguments for one input line in insert mode (-I), each
/// replstr replaced with the line.
fn insert_args(args: &Args, replstr: &OsStr, input_arg: &[u8]) -> io::Result<Vec<OsString>> {
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

    Ok(util_args)
}

/// What one invocation's result means for xargs: `Some` exit status when it
/// must stop, without processing any remaining input; a nonzero exit is
/// recorded in `any_failed`.
fn stop_after(result: ExecResult, util: &OsStr, any_failed: &mut bool) -> Option<i32> {
    match result {
        // POSIX: an invocation that exits 255 or is terminated by a signal
        // makes xargs write a diagnostic and stop.
        ExecResult::Exited(255) => {
            diag::error(
                &gettext("{}: exited with status 255; aborting")
                    .replace("{}", &util.to_string_lossy()),
            );
            Some(1)
        }
        ExecResult::Signaled(sig) => {
            diag::error(
                &gettext("{}: terminated by signal {}")
                    .replacen("{}", &util.to_string_lossy(), 1)
                    .replacen("{}", &sig.to_string(), 1),
            );
            Some(1)
        }
        ExecResult::NotFound => Some(127),
        ExecResult::CannotInvoke => Some(126),
        ExecResult::Exited(0) => None,
        ExecResult::Exited(_) => {
            *any_failed = true;
            None
        }
    }
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

/// Read the input and invoke the utility through `runner`: the status of
/// xargs's own failure, 0 when there was none (and when the runner stopped,
/// which holds the status then).
fn read_and_spawn(args: &Args, runner: &mut Runner) -> io::Result<i32> {
    let mut state = ParseState::new(args);
    let mut invoked = false;
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
                    // The first argument does not fit even alone.
                    err_arg_too_long();
                    return Ok(1);
                }
                util_args.extend(batch);

                invoked = true;
                if !runner.run(util_args)? {
                    return Ok(0);
                }
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
                    if !runner.run(insert_args(args, replstr, &input_arg)?)? {
                        return Ok(0);
                    }
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
                    if batch.is_empty() {
                        // The first argument does not fit even alone.
                        err_arg_too_long();
                        return Ok(1);
                    }
                    util_args.extend(batch);

                    invoked = true;
                    if !runner.run(util_args)? {
                        return Ok(0);
                    }
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
            if !runner.run(insert_args(args, replstr, &input_arg)?)? {
                return Ok(0);
            }
        }
    } else {
        // The last argument read can overflow the batch, so what remains may
        // need more than one command.
        while !state.args.is_empty() {
            let batch = state.remove_args();
            if batch.is_empty() {
                // The first argument does not fit even alone.
                err_arg_too_long();
                return Ok(1);
            }
            let mut util_args = args.util_args.clone();
            util_args.extend(batch);

            invoked = true;
            if !runner.run(util_args)? {
                return Ok(0);
            }
        }
    }

    // POSIX: if standard input yields no arguments, the utility shall be
    // executed exactly once unless -r (--no-run-if-empty) was given. (Insert
    // mode substitutes per input line, so an empty input means zero runs.)
    if !invoked && !insert_mode && !args.no_run_if_empty {
        // Whether it stops xargs or not, nothing follows it.
        runner.run(args.util_args.clone())?;
    }

    Ok(0)
}

fn main() {
    diag::init_locale("xargs");

    let args = Args::parse_command_line();

    let mut runner = Runner::new(&args);
    let status = match read_and_spawn(&args, &mut runner) {
        Ok(code) => code,
        Err(e) => {
            diag::error(&e.to_string());
            1
        }
    };
    let exit_code = runner.finish(status);

    std::process::exit(exit_code);
}
