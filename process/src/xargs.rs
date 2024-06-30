//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - prompt mode (-p)
// - insert mode (-I)
// - split by lines (-L)
// - exit feature (-x)
// - write tests
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::io::{self, Read};
use std::process::{Command, Stdio};

const ARG_MAX: i32 = 131072; // arbitrary.  todo: discover actual value
const MAX_ARGS_BYTES: usize = ARG_MAX as usize - 2048;

/// xargs - construct argument lists and invoke utility
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// The utility shall be executed for each non-empty number lines of arguments from standard input.
    #[arg(short = 'L', long)]
    lines: Option<usize>,

    /// Invoke utility using as many standard input arguments as possible, up to number
    #[arg(short = 'n', long)]
    maxnum: Option<usize>,

    /// Invoke utility using as many standard input arguments as possible yielding a command line length less than size
    #[arg(short = 's', long)]
    maxsize: Option<usize>,

    /// Use eofstr as the logical end-of-file string.
    #[arg(short = 'E', long, default_value = "")]
    eofstr: String,

    /// Insert mode.
    #[arg(short = 'I', long)]
    replstr: Option<String>,

    /// prompt mode
    #[arg(short, long)]
    prompt: bool,

    /// trace mode
    #[arg(short, long)]
    trace: bool,

    /// null-based processing
    #[arg(short = '0', long = "null")]
    null_mode: bool,

    /// Terminate if a constructed command line will not fit in the implied or specified size
    #[arg(short = 'x', long)]
    exit: bool,

    /// utility to invoke
    util: String,

    /// utility arguments
    util_args: Vec<String>,
}

// find a string in a vector of strings
fn find_str(needle: &str, haystack: &[String]) -> Option<usize> {
    haystack.iter().position(|s| s == needle)
}

// execute the utility
fn exec_util(util: &str, util_args: Vec<String>, trace: bool) -> io::Result<()> {
    // if tracing, Each generated command line shall be written to
    // standard error just prior to invocation.
    if trace {
        eprintln!("{} {}", util, util_args.join(" "));
    }

    let _output = Command::new(util)
        .args(util_args)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()?;

    Ok(())
}

struct ParseState {
    // cmdline-related state
    util_size: usize,
    util_n_args: usize,

    // input state
    tmp_arg: String,
    in_arg: bool,
    in_quote: bool,
    in_escape: bool,
    quote_char: char,
    skip_remainder: bool,
    null_slop: Vec<u8>,

    // output state
    max_bytes: usize,
    max_args: Option<usize>,

    // parsed args, ready for exec
    args: Vec<String>,
}

impl ParseState {
    fn new(args: &Args) -> ParseState {
        let mut total = args.util.len();
        for arg in &args.util_args {
            total += arg.len() + 1; // +1 for space
        }

        ParseState {
            util_size: total,
            util_n_args: args.util_args.len(),
            tmp_arg: String::new(),
            in_arg: false,
            in_quote: false,
            in_escape: false,
            quote_char: '"',
            skip_remainder: false,
            null_slop: Vec::new(),
            max_bytes: args.maxsize.unwrap_or(MAX_ARGS_BYTES),
            max_args: args.maxnum,
            args: Vec::new(),
        }
    }

    fn full(&self) -> bool {
        let mut total = self.util_size;
        for arg in &self.args {
            total += arg.len() + 1; // +1 for space
        }

        if total > self.max_bytes {
            true
        } else if let Some(max_args) = self.max_args {
            (self.util_n_args + self.args.len()) >= max_args
        } else {
            false
        }
    }

    fn remove_args(&mut self) -> Vec<String> {
        let mut total = self.util_size;
        let mut ret = Vec::new();
        while !self.args.is_empty() {
            // stop if adding the next arg would exceed the max size
            if (total + self.args[0].len() + 1) > self.max_bytes {
                break;
            }

            // add the next arg
            let arg = self.args.remove(0);
            total += arg.len() + 1; // +1 for space
            ret.push(arg);

            // stop if we have reached the max number of args
            if let Some(max_args) = self.max_args {
                if (ret.len() + self.util_n_args) == max_args {
                    break;
                }
            }
        }

        ret
    }

    // args are null-separated, without any further processing.
    // if the input data crosses a null boundary, the remainder is
    // stored as state for the next call to parse_buf_null.
    fn parse_buf_null(&mut self, in_buf: &[u8]) -> io::Result<()> {
        if self.skip_remainder {
            return Ok(());
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
                self.args.push(s);
                start = end + 1;
            }
            end += 1;
        }

        // remember remainder, if any, for next call
        if start < buf.len() {
            self.null_slop.extend_from_slice(&buf[start..]);
        }

        Ok(())
    }

    fn parse_buf(&mut self, buf: &[u8]) -> io::Result<()> {
        if self.skip_remainder {
            return Ok(());
        }

        for &c8 in buf {
            let ch = c8 as char;

            if self.in_quote {
                if ch == self.quote_char {
                    self.in_quote = false;
                    self.in_arg = false;
                    self.args.push(self.tmp_arg.clone());
                    self.tmp_arg.clear();
                } else {
                    self.tmp_arg.push(ch);
                }
            } else if self.in_escape {
                self.in_escape = false;
                self.tmp_arg.push(ch);
            } else if self.in_arg && ch.is_whitespace() {
                self.in_arg = false;
                self.args.push(self.tmp_arg.clone());
                self.tmp_arg.clear();
            } else if ch == '\'' || ch == '"' {
                self.in_arg = true;
                self.in_quote = true;
                self.quote_char = ch;
            } else if ch == '\\' {
                self.in_escape = true;
            } else if ch.is_whitespace() {
                // ignore whitespace
            } else {
                self.in_arg = true;
                self.tmp_arg.push(ch);
            }
        }

        Ok(())
    }

    fn parse_finalize(&mut self) {
        if self.in_arg {
            self.in_arg = false;
            self.args.push(self.tmp_arg.clone());
            self.tmp_arg.clear();
        }

        if !self.null_slop.is_empty() {
            let s = String::from_utf8_lossy(&self.null_slop).to_string();
            self.args.push(s);
            self.null_slop.clear();
        }
    }

    fn postprocess(&mut self, args: &Args) -> io::Result<()> {
        if !args.eofstr.is_empty() {
            if let Some(pos) = find_str(&args.eofstr, &self.args) {
                self.args.truncate(pos);
                self.skip_remainder = true;
            }
        }

        Ok(())
    }
}

fn read_and_spawn(args: &Args) -> io::Result<()> {
    let mut state = ParseState::new(args);

    let mut buffer = [0; plib::BUFSZ];

    // read stdin until EOF
    loop {
        // read a chunk of input
        let n_read = io::stdin().read(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        if args.null_mode {
            state.parse_buf_null(&buffer[..n_read])?;
        } else {
            // parse the line
            state.parse_buf(&buffer[..n_read])?;

            // handle eofstr and other details
            state.postprocess(args)?;
        }

        // if enough args, spawn the utility
        while state.full() {
            let mut util_args = args.util_args.clone();
            util_args.append(&mut state.remove_args());
            exec_util(&args.util, util_args, args.trace)?;
        }
    }

    // finalize parsing
    state.parse_finalize();

    // if there are any remaining args, spawn the utility
    if !state.args.is_empty() {
        let mut util_args = args.util_args.clone();
        util_args.append(&mut state.remove_args());
        exec_util(&args.util, util_args, args.trace)?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    read_and_spawn(&args)?;

    Ok(())
}
