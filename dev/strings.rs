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
use gettextrs::gettext;
use object::{Object, ObjectSection};
use plib::{diag, locale};

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

/// Decode one printable character from the head of `bytes`.
///
/// Attempts a UTF-8 decode of up to 4 leading bytes. Returns `Some(c)` iff
/// a complete UTF-8 character was decoded *and* it is printable in the
/// current `LC_CTYPE` locale (`plib::locale::isprint`). Returns `None` for
/// invalid UTF-8 lead bytes — the caller advances by one byte and treats
/// the position as a string-terminator, matching the historical behaviour
/// of `read_printable_char_utf8`.
///
/// Per POSIX 115860-115861: `<newline>` and NUL terminate a printable string,
/// so they (and every other control character) yield `None` here, which the
/// outer loop interprets as a string boundary.
///
/// When `Some(c)` is returned, `c.len_utf8()` equals the number of input
/// bytes consumed (UTF-8 is byte-deterministic), so the caller can safely
/// advance the byte offset by that amount.
fn read_printable_char(bytes: &[u8]) -> Option<char> {
    if bytes.is_empty() {
        return None;
    }
    let max_seq = bytes.len().min(4);
    let s = match std::str::from_utf8(&bytes[..max_seq]) {
        Ok(s) => s,
        Err(e) => {
            let valid = e.valid_up_to();
            if valid == 0 {
                // Invalid UTF-8 lead byte; treat as non-printable terminator.
                return None;
            }
            // We have a valid prefix; decode that.
            std::str::from_utf8(&bytes[..valid]).unwrap()
        }
    };
    let c = s.chars().next().unwrap();
    if locale::isprint(c) {
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

fn print_strings(bytes: &[u8], options: OutputOptions) {
    let mut offset = 0;
    let mut print_buffer = String::new();

    while offset < bytes.len() {
        if let Some(c) = read_printable_char(&bytes[offset..]) {
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

fn print_file(path: &OsString, output_options: OutputOptions) -> StringsResult {
    let bytes = std::fs::read(path)?;

    if output_options.scan_all {
        print_strings(&bytes, output_options);
        return Ok(());
    }

    if let Ok(parsed_object) = object::read::File::parse(&*bytes) {
        for section in parsed_object.sections() {
            // skip empty sections
            if !section.kind().is_bss() {
                print_strings(section.data()?, output_options);
            }
        }
    } else {
        print_strings(&bytes, output_options);
    }

    Ok(())
}

fn process_files(files: &[OsString], opts: OutputOptions) {
    for file in files {
        if let Err(err) = print_file(file, opts) {
            // Log and continue with subsequent files; exit status will
            // reflect the error via plib::diag::exit_status().
            diag::error(&format!("{}: {}", file.to_string_lossy(), err));
        }
    }
}

fn main() {
    diag::init_locale("strings");
    let args = Args::parse();
    process_files(&args.input_files, args.output_options);
    std::process::exit(diag::exit_status());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_char_rejects_invalid_utf8_lead_byte() {
        // 0xC0 is a UTF-8 lead byte but cannot start a valid sequence
        // (it would encode a 2-byte representation of a 1-byte codepoint).
        // We must return None so the outer loop advances by exactly 1
        // input byte and does NOT swallow the following 'A'.
        let r = read_printable_char(&[0xC0, b'A', b'B']);
        assert!(r.is_none(), "expected None for invalid UTF-8 lead byte");
    }

    #[test]
    fn read_char_returns_consumed_matching_input() {
        // After init_locale runs (in tests, separately for each call), the
        // C.UTF-8 locale is in effect from cargo test. For valid UTF-8,
        // c.len_utf8() must equal the number of input bytes consumed.
        // ASCII 'A' = 1 input byte, len_utf8 = 1.
        let c = read_printable_char(b"A").unwrap();
        assert_eq!(c.len_utf8(), 1);
    }
}
