//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;

use clap::{Parser, ValueEnum};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use object::{Object, ObjectSection};

#[derive(Clone, Copy, ValueEnum)]
enum OffsetFormat {
    #[value(name = "d", help = "decimal")]
    Decimal,
    #[value(name = "o", help = "octal")]
    Octal,
    #[value(name = "x", help = "hexadecimal")]
    Hex,
}

#[derive(clap::Args, Clone, Copy)]
struct OutputOptions {
    #[arg(short = 'a', help = gettext("Scan the input files in their entirety"))]
    scan_all: bool,

    #[arg(short = 't', help = gettext("Byte offset format"))]
    format: Option<OffsetFormat>,

    #[arg(short = 'n', default_value_t = 4, help = gettext("Minimum string length"))]
    minimum_string_length: usize,
}

/// strings - find printable strings in files
#[derive(Parser)]
#[command(version, about = gettext("strings - find printable strings in files"))]
struct Args {
    #[command(flatten)]
    output_options: OutputOptions,

    input_files: Vec<OsString>,
}

type StringsResult = Result<(), Box<dyn std::error::Error>>;

enum CharacterSet {
    Ascii,
    Utf8,
}

impl CharacterSet {
    fn from_locale(var: String) -> Self {
        if var.contains("UTF-8") {
            CharacterSet::Utf8
        } else {
            // if UTF-8 is not explicitly specified, we assume ASCII.
            // This behavior is consistent with the GNU implementation
            // of strings.
            CharacterSet::Ascii
        }
    }

    fn from_env() -> Self {
        // the precedence is specified at:
        // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08_02
        if let Ok(locale) = std::env::var("LC_ALL") {
            CharacterSet::from_locale(locale)
        } else if let Ok(locale) = std::env::var("LC_CTYPE") {
            CharacterSet::from_locale(locale)
        } else if let Ok(locale) = std::env::var("LANG") {
            CharacterSet::from_locale(locale)
        } else {
            // we chose to default to ASCII if no locale is set
            // to match the behavior of the GNU implementation
            CharacterSet::Ascii
        }
    }
}

fn read_printable_char_utf8(bytes: &[u8]) -> Option<char> {
    // we limit the number of bytes to check to 4
    // because that is the maximum number of bytes
    // in a valid UTF-8 sequence.
    let max_utf8_sequence_len = bytes.len().min(4);
    let s = match std::str::from_utf8(&bytes[..max_utf8_sequence_len]) {
        Ok(s) => s,
        Err(e) => {
            let max = e.valid_up_to();
            if max > 0 {
                // we know the string is valid UTF-8 so unwrap is safe
                std::str::from_utf8(&bytes[..max]).unwrap()
            } else {
                return None;
            }
        }
    };
    // we know the string isn't empty so unwrap is safe
    let c = s.chars().next().unwrap();
    if !c.is_control() || c.is_whitespace() {
        Some(c)
    } else {
        None
    }
}

fn read_printable_ascii_char(bytes: &[u8]) -> Option<char> {
    let c = bytes[0] as char;
    if c.is_ascii_graphic() || c.is_whitespace() {
        Some(c)
    } else {
        None
    }
}

fn print_string(s: &str, starting_offset: usize, format: Option<OffsetFormat>) {
    // the width of the byte offset was chosen to match the
    // behavior of the GNU strings implementation.
    match format {
        Some(OffsetFormat::Decimal) => {
            println!("{:7} {}", starting_offset, s);
        }
        Some(OffsetFormat::Octal) => {
            println!("{:7o} {}", starting_offset, s);
        }
        Some(OffsetFormat::Hex) => {
            println!("{:7x} {}", starting_offset, s);
        }
        _ => println!("{}", s),
    }
}

fn print_strings<F>(bytes: &[u8], options: OutputOptions, read_char: F)
where
    F: Fn(&[u8]) -> Option<char>,
{
    let mut offset = 0;
    let mut print_buffer = String::new();

    while offset < bytes.len() {
        if let Some(c) = read_char(&bytes[offset..]) {
            print_buffer.push(c);
            offset += c.len_utf8();
        } else {
            if print_buffer.len() >= options.minimum_string_length {
                print_string(&print_buffer, offset - print_buffer.len(), options.format);
            }
            print_buffer.clear();
            offset += 1;
        }
    }
    if print_buffer.len() >= options.minimum_string_length {
        print_string(&print_buffer, offset - print_buffer.len(), options.format);
    }
}

fn print_file<F>(path: OsString, output_options: OutputOptions, read_char: F) -> StringsResult
where
    F: Fn(&[u8]) -> Option<char> + Copy,
{
    let bytes = std::fs::read(path)?;

    if output_options.scan_all {
        print_strings(&bytes, output_options, read_char);
        return Ok(());
    }

    if let Ok(parsed_object) = object::read::File::parse(&*bytes) {
        for section in parsed_object.sections() {
            // skip empty sections
            if !section.kind().is_bss() {
                print_strings(section.data()?, output_options, read_char);
            }
        }
    } else {
        print_strings(&bytes, output_options, read_char);
    }

    Ok(())
}

fn main() -> StringsResult {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    match CharacterSet::from_env() {
        CharacterSet::Utf8 => {
            for file in args.input_files {
                print_file(file, args.output_options, read_printable_char_utf8)?;
            }
        }
        CharacterSet::Ascii => {
            for file in args.input_files {
                print_file(file, args.output_options, read_printable_ascii_char)?;
            }
        }
    }
    Ok(())
}
