//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cmp;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, Error, Read, Write};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::{input_reader, input_stream};
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("split - split a file into pieces"))]
struct Args {
    #[arg(
        short = 'a',
        long,
        default_value_t = 2,
        value_parser = clap::value_parser!(u32).range(1..),
        help = gettext(
            "Use suffix_length letters to form the suffix portion of the filenames of the split file"
        )
    )]
    suffix_len: u32,

    #[arg(
        short,
        long,
        group = "mode",
        value_parser = clap::value_parser!(u64).range(1..),
        help = gettext(
            "Use suffix_length letters to form the suffix portion of the filenames of the split file"
        )
    )]
    lines: Option<u64>,

    #[arg(
        short,
        long,
        group = "mode",
        help = gettext("Split a file into pieces n bytes in size")
    )]
    bytes: Option<String>,

    #[arg(default_value = "", help = gettext("File to be split"))]
    file: PathBuf,

    #[arg(default_value = "x", help = gettext("Prefix of output files"))]
    prefix: String,
}

pub struct Suffix {
    suffix: String,
}

impl Suffix {
    pub fn new(len: usize) -> Self {
        debug_assert!(len > 0);
        Self {
            suffix: "a".repeat(len),
        }
    }

    fn inc_char(ch: char) -> char {
        debug_assert!(('a'..='y').contains(&ch));
        ((ch as u8) + 1) as char
    }
}

impl Iterator for Suffix {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.suffix.clone();

        let mut i = self.suffix.len() - 1;
        loop {
            let ch = self.suffix.chars().nth(i).unwrap();
            if ch != 'z' {
                self.suffix
                    .replace_range(i..i + 1, Self::inc_char(ch).to_string().as_str());
                return Some(current);
            }

            self.suffix
                .replace_range(i..i + 1, 'a'.to_string().as_str());

            if i == 0 {
                break;
            }
            i -= 1;
        }
        None
    }
}

struct OutputState {
    prefix: String,
    boundary: u64,

    suffix: Suffix,
    count: u64,
    outf: Option<File>,
}

impl OutputState {
    fn new(prefix: &str, boundary: u64, suffix_len: u32) -> OutputState {
        OutputState {
            prefix: String::from(prefix),
            boundary,
            suffix: Suffix::new(suffix_len as usize),
            count: 0,
            outf: None,
        }
    }

    fn open_output(&mut self) -> io::Result<()> {
        if self.outf.is_some() {
            return Ok(());
        }

        let suffix = match self.suffix.next() {
            Some(s) => s,
            None => {
                return Err(Error::other("maximum suffix reached"));
            }
        };

        let out_fn = format!("{}{}", self.prefix, suffix);
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
            self.count = 0;
        }
    }

    fn incr_output(&mut self, n: u64) {
        self.count += n;
        assert!(self.count <= self.boundary);

        if self.count == self.boundary {
            self.close_output();
        }
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<()> {
        match &mut self.outf {
            Some(f) => f.write_all(buf),
            // TODO:
            None => panic!("unreachable"),
        }
    }

    fn output_bytes(&mut self, buf: &[u8]) -> io::Result<()> {
        let mut consumed: usize = 0;
        while consumed < buf.len() {
            self.open_output()?;

            let remainder = buf.len() - consumed;
            let dist = self.boundary - self.count;
            let wlen = cmp::min(dist as usize, remainder);
            let slice = &buf[consumed..consumed + wlen];
            self.write(slice)?;

            consumed += wlen;

            self.incr_output(wlen as u64);
        }

        Ok(())
    }
}

fn split_by_bytes(args: &Args, bytesplit: String) -> io::Result<()> {
    let mul: u64 = {
        if bytesplit.ends_with("k") {
            1024
        } else if bytesplit.ends_with("m") {
            1024 * 1024
        } else if bytesplit.ends_with("g") {
            1024 * 1024 * 1024
        } else {
            1
        }
    };
    let bytestr = match mul {
        1 => &bytesplit[..],
        _ => &bytesplit[0..bytesplit.len() - 1],
    };
    let boundary: u64 = match bytestr.parse::<u64>() {
        Ok(n) => n * mul,
        Err(e) => {
            eprintln!("{}", e);
            return Err(Error::other("invalid byte spec"));
        }
    };

    // open file, or stdin
    let mut file = input_stream(&args.file, false)?;
    let mut raw_buffer = [0; BUFSZ];
    let mut state = OutputState::new(&args.prefix, boundary, args.suffix_len);

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];

        state.output_bytes(buf)?;
    }

    Ok(())
}

fn split_by_lines(args: &Args, linesplit: u64) -> io::Result<()> {
    assert!(linesplit > 0);

    // open file, or stdin
    let mut reader = input_reader(&args.file, false)?;
    let mut state = OutputState::new(&args.prefix, linesplit, args.suffix_len);

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        state.open_output()?;

        state.write(buffer.as_ref())?;

        state.incr_output(1);
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    if args.lines.is_none() && args.bytes.is_none() {
        args.lines = Some(1000);
    }

    if args.lines.is_some() {
        split_by_lines(&args, args.lines.unwrap())?;
    } else {
        split_by_bytes(&args, args.bytes.clone().unwrap())?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_suffix_inc_char() {
        assert_eq!(Suffix::inc_char('a'), 'b');
        assert_eq!(Suffix::inc_char('b'), 'c');
        assert_eq!(Suffix::inc_char('y'), 'z');
    }

    #[ignore]
    #[test]
    fn test_suffix_iterable() {
        let suffix = Suffix::new(1);
        assert_eq!(suffix.count(), 26);
    }
}
