//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - err on line num == 0
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use regex::Regex;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, Error, ErrorKind, Read, Write};

/// csplit - split files based on context
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Name the created files prefix 00, prefix 01, ..., prefixn.
    #[arg(short = 'f', long, default_value = "xx")]
    prefix: String,

    /// Leave previously created files intact. By default, csplit shall remove created files if an error occurs.
    #[arg(short, long, default_value_t = false)]
    keep: bool,

    /// Use number decimal digits to form filenames for the file pieces.
    #[arg(short, long, default_value_t = 2)]
    num: u32,

    /// Suppress the output of file size messages.
    #[arg(short, long)]
    suppress: bool,

    /// File to read as input.
    filename: String,

    /// Operands defining context on which to split.
    operands: Vec<String>,
}

#[derive(Debug)]
enum Operand {
    Rx(Regex, isize, bool),
    LineNum(usize),
    Repeat(usize),
}

#[derive(Debug)]
struct SplitOps {
    ops: Vec<Operand>,
}

fn inc_char(ch: char) -> char {
    ((ch as u8) + 1) as char
}

struct OutputState {
    prefix: String,
    in_line_no: usize,

    suffix: String,
    suffix_len: u32,

    outf: Option<File>,
}

impl OutputState {
    fn new(prefix: &str, suffix_len: u32) -> OutputState {
        OutputState {
            prefix: String::from(prefix),
            in_line_no: 0,
            suffix: String::new(),
            suffix_len,
            outf: None,
        }
    }

    fn incr_suffix(&mut self) -> Result<(), &'static str> {
        assert!(self.suffix_len > 1);

        if self.suffix.is_empty() {
            self.suffix = String::from("a".repeat(self.suffix_len as usize));
            return Ok(());
        }

        assert!(self.suffix.len() > 1);
        let mut i = self.suffix.len() - 1;
        loop {
            let ch = self.suffix.chars().nth(i).unwrap();
            if ch != 'z' {
                self.suffix
                    .replace_range(i..i + 1, inc_char(ch).to_string().as_str());
                return Ok(());
            }

            self.suffix
                .replace_range(i..i + 1, 'a'.to_string().as_str());

            if i == 0 {
                break;
            }
            i = i - 1;
        }

        Err("maximum suffix reached")
    }

    fn open_output(&mut self) -> io::Result<()> {
        if self.outf.is_some() {
            return Ok(());
        }

        let inc_res = self.incr_suffix();
        if let Err(e) = inc_res {
            return Err(Error::new(ErrorKind::Other, e));
        }

        let out_fn = format!("{}{}", self.prefix, self.suffix);
        let f = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&out_fn)?;
        self.outf = Some(f);

        Ok(())
    }

    fn close_output(&mut self) {
        if self.outf.is_some() {
            self.outf = None;
        }
    }
}

fn output_line(_ctx: &SplitOps, _state: &mut OutputState, _line: &str) -> io::Result<()> {
    Ok(())
}

fn csplit_file(args: &Args, ctx: SplitOps) -> io::Result<()> {
    // open file, or stdin
    let file: Box<dyn Read> = {
        if args.filename == "-" {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(&args.filename)?)
        }
    };
    let mut state = OutputState::new(&args.prefix, args.num);
    let mut reader = io::BufReader::new(file);

    loop {
        let mut line = String::new();
        let n_read = reader.read_line(&mut line)?;
        if n_read == 0 {
            break;
        }

        output_line(&ctx, &mut state, &line)?;

        state.in_line_no += 1;
    }

    Ok(())
}

fn escaped_end_pos(s: &str, delim: char) -> Option<usize> {
    let mut first = true;
    let mut escaped = false;
    for (i, ch) in s.chars().enumerate() {
        if first {
            assert_eq!(ch, delim);
            first = false;
        } else if escaped {
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == delim {
            return Some(i);
        }
    }

    return None;
}

fn parse_op_rx(opstr: &str, delim: char) -> io::Result<Operand> {
    // delimiter indicates skip-mode
    let is_skip = delim == '%';

    // find where regex string ends, and (optionally) offset begins
    let res = escaped_end_pos(opstr, delim);
    if res.is_none() {
        return Err(Error::new(ErrorKind::Other, "invalid regex str"));
    }

    // parse string sandwiched between two delimiter chars
    let end_pos = res.unwrap();
    let re_str = &opstr[1..end_pos];
    let res = Regex::new(re_str);
    if res.is_err() {
        return Err(Error::new(ErrorKind::Other, "invalid regex"));
    }
    let re = res.unwrap();

    // reference offset string
    let mut offset_str = &opstr[end_pos + 1..];

    // if empty, we are done
    if offset_str.len() == 0 {
        return Ok(Operand::Rx(re, 0, is_skip));
    }

    // skip optional leading '+'
    if offset_str.starts_with("+") {
        offset_str = &opstr[end_pos + 2..];
    }

    // parse offset number, positive or negative
    match offset_str.parse::<isize>() {
        Ok(n) => Ok(Operand::Rx(re, n, is_skip)),
        Err(_e) => Err(Error::new(ErrorKind::Other, "invalid regex offset")),
    }
}

fn parse_op_repeat(opstr: &str) -> io::Result<Operand> {
    // a regex fully describes what must be parsed
    let re = Regex::new(r"^\{(\d+)}$").unwrap();

    // grab and parse capture #1, if matched
    match re.captures(opstr) {
        None => {}
        Some(caps) => {
            let numstr = caps.get(1).unwrap().as_str();
            match numstr.parse::<usize>() {
                Ok(n) => return Ok(Operand::Repeat(n)),
                Err(_e) => {}
            }
        }
    }

    // error cases fall through to here
    Err(Error::new(ErrorKind::Other, "invalid repeating operand"))
}

fn parse_op_linenum(opstr: &str) -> io::Result<Operand> {
    // parse simple positive integer
    match opstr.parse::<usize>() {
        Ok(n) => Ok(Operand::LineNum(n)),
        Err(e) => {
            let msg = format!("{}", e);
            Err(Error::new(ErrorKind::Other, msg))
        }
    }
}

fn parse_operands(args: &Args) -> io::Result<SplitOps> {
    let mut ops = Vec::new();

    for opstr in &args.operands {
        let first_ch = opstr.chars().nth(0).unwrap();

        let op = {
            match first_ch {
                '/' => parse_op_rx(opstr, '/')?,
                '%' => parse_op_rx(opstr, '%')?,
                '{' => parse_op_repeat(opstr)?,
                '1'..='9' => parse_op_linenum(opstr)?,
                _ => return Err(Error::new(ErrorKind::Other, "invalid operand")),
            }
        };

        ops.push(op);
    }

    Ok(SplitOps { ops })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let ctx = parse_operands(&args)?;

    let mut exit_code = 0;

    match csplit_file(&args, ctx) {
        Ok(()) => {}
        Err(e) => {
            exit_code = 1;
            eprintln!("{}: {}", args.filename, e);
        }
    }

    std::process::exit(exit_code)
}
