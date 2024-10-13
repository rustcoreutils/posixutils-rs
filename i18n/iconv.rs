//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use iconv_lib::{
    ascii,
    utf_16::{self, UTF16Variant},
    utf_32::{self, UTF32Variant},
    utf_8,
};
use plib::PROJECT_NAME;
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{self, BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    process::exit,
    str::FromStr,
};
use strum::IntoEnumIterator;
use strum_macros::{Display, EnumIter, EnumString};

mod iconv_lib;

/// iconv — codeset conversion
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Omit invalid characters of the input file from the output
    #[arg(short = 'c')]
    omit_invalid: bool,

    /// Suppress messages about invalid characters
    #[arg(short = 's')]
    suppress_messages: bool,

    /// Identify the codeset of the input file
    #[arg(short = 'f')]
    from_codeset: Option<String>,

    /// List all supported codeset values
    #[arg(short = 'l')]
    list_codesets: bool,

    /// Identify the codeset for the output file
    #[arg(short = 't')]
    to_codeset: Option<String>,

    /// Input files (reads from stdin if not provided)
    files: Option<Vec<PathBuf>>,
}

#[derive(EnumString, EnumIter, Debug, PartialEq, Display)]
#[strum(serialize_all = "SCREAMING-KEBAB-CASE")]
#[allow(non_camel_case_types)]
enum Encodings {
    ASCII,
    UTF_8,
    UTF_16,
    UTF_16LE,
    UTF_16BE,
    UTF_32,
    UTF_32LE,
    UTF_32BE,
}

impl Encodings {
    fn parse(encoding: &str) -> Self {
        let cleaned_encoding = encoding.trim_matches('"');
        match Encodings::from_str(cleaned_encoding) {
            Ok(encoding) => encoding,
            Err(_) => {
                eprintln!("Error: Unknown encoding: {}", cleaned_encoding);
                exit(1);
            }
        }
    }
}

fn list_encodings() {
    for encoding in Encodings::iter() {
        println!("{:?}", encoding);
    }
}

#[derive(Debug, Default)]
struct CharmapHeader {
    code_set_name: Option<String>,
    mb_cur_max: usize,
    mb_cur_min: usize,
    escape_char: char,
    comment_char: char,
}

#[derive(Debug)]
struct CharmapEntry {
    symbolic_name: String,
    encoding: Vec<u8>,
    _comments: Option<String>,
}

#[derive(Debug)]
struct Charmap {
    header: CharmapHeader,
    entries: HashMap<String, CharmapEntry>,
    width_entries: HashMap<String, usize>,
    width_default: usize,
}

fn parse_encoding(
    encoding: &str,
    escape_char: char,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut bytes = Vec::new();
    let mut chars = encoding.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c == escape_char {
            chars.next(); // consume escape char
            match chars.next() {
                Some('d') => {
                    let num: String = chars.by_ref().take(3).collect();
                    bytes.push(num.parse::<u8>()?);
                }
                Some('x') => {
                    let num: String = chars.by_ref().take(2).collect();
                    bytes.push(u8::from_str_radix(&num, 16)?);
                }
                Some(c) if c.is_digit(8) => {
                    let num: String = std::iter::once(c).chain(chars.by_ref().take(2)).collect();
                    bytes.push(u8::from_str_radix(&num, 8)?);
                }
                _ => return Err("Invalid encoding format".into()),
            }
        } else {
            chars.next(); // consume char
        }
    }

    Ok(bytes)
}

fn parse_charmap(path: &Path) -> Result<Charmap, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut charmap = Charmap {
        header: CharmapHeader::default(),
        entries: HashMap::new(),
        width_entries: HashMap::new(),
        width_default: 1,
    };

    let mut in_charmap_section = false;
    let mut in_width_section = false;

    for line in reader.lines() {
        let line = line?;
        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed.starts_with(charmap.header.comment_char) {
            continue;
        }

        if !in_charmap_section && !in_width_section {
            if trimmed.starts_with("<code_set_name>") {
                charmap.header.code_set_name =
                    Some(trimmed.split_whitespace().nth(1).unwrap().to_string());
            } else if trimmed.starts_with("<mb_cur_max>") {
                charmap.header.mb_cur_max = trimmed.split_whitespace().nth(1).unwrap().parse()?;
            } else if trimmed.starts_with("<mb_cur_min>") {
                charmap.header.mb_cur_min = trimmed.split_whitespace().nth(1).unwrap().parse()?;
            } else if trimmed.starts_with("<escape_char>") {
                charmap.header.escape_char = trimmed
                    .split_whitespace()
                    .nth(1)
                    .unwrap()
                    .chars()
                    .next()
                    .expect("Escape char is missing in charmap file")
            } else if trimmed.starts_with("<comment_char>") {
                charmap.header.comment_char = trimmed
                    .split_whitespace()
                    .nth(1)
                    .unwrap()
                    .chars()
                    .next()
                    .expect("Comment char is missing in charmap file")
            } else if trimmed == "CHARMAP" {
                in_charmap_section = true;
            }
        } else if in_charmap_section {
            if trimmed == "END CHARMAP" {
                in_charmap_section = false;
            } else {
                let mut parts = Vec::new();
                let mut remaining = trimmed;

                for _ in 0..3 {
                    if remaining.is_empty() {
                        break;
                    }
                    let part = remaining
                        .trim_start()
                        .split_once(' ')
                        .map_or((remaining, ""), |(a, b)| (a, b));
                    parts.push(part.0);
                    remaining = part.1;
                }

                if parts.len() >= 2 {
                    let symbolic_name = parts[0].trim_matches(|c| c == '<' || c == '>').to_string();
                    let encoding = parse_encoding(parts[1], charmap.header.escape_char)?;
                    let _comments = parts.get(2).map(|&s| s.to_string());
                    charmap.entries.insert(
                        symbolic_name.clone(),
                        CharmapEntry {
                            symbolic_name,
                            encoding,
                            _comments,
                        },
                    );
                }
            }
        } else if in_width_section {
            if trimmed == "END WIDTH" {
                in_width_section = false;
            } else {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if parts.len() >= 2 {
                    let symbolic_name = parts[0].trim_matches(|c| c == '<' || c == '>').to_string();
                    let width = parts[1].parse()?;
                    charmap.width_entries.insert(symbolic_name, width);
                }
            }
        } else if trimmed == "WIDTH" {
            in_width_section = true;
        } else if trimmed.starts_with("WIDTH_DEFAULT") {
            charmap.width_default = trimmed.split_whitespace().nth(1).unwrap().parse()?;
        }
    }

    Ok(charmap)
}

#[derive(Debug)]
enum CodesetType {
    Encoding(Encodings),
    Charmap(Charmap),
}

fn parse_codeset(codeset: &str) -> Result<CodesetType, Box<dyn std::error::Error>> {
    if codeset.contains('/') {
        Ok(CodesetType::Charmap(parse_charmap(Path::new(codeset))?))
    } else {
        Ok(CodesetType::Encoding(Encodings::parse(codeset)))
    }
}

fn encoding_conversion(
    from: &Encodings,
    to: &Encodings,
    input: &[u8],
    omit_invalid: bool,
    supress_error: bool,
) -> (Vec<u8>, u32) {
    let (input_exit_code, input) = match from {
        Encodings::UTF_8 => utf_8::to_ucs4(input, omit_invalid, supress_error),
        Encodings::UTF_16 => {
            utf_16::to_ucs4(input, omit_invalid, supress_error, UTF16Variant::UTF16)
        }
        Encodings::UTF_16LE => {
            utf_16::to_ucs4(input, omit_invalid, supress_error, UTF16Variant::UTF16LE)
        }
        Encodings::UTF_16BE => {
            utf_16::to_ucs4(input, omit_invalid, supress_error, UTF16Variant::UTF16BE)
        }
        Encodings::UTF_32 => {
            utf_32::to_ucs4(input, omit_invalid, supress_error, UTF32Variant::UTF32)
        }
        Encodings::UTF_32LE => {
            utf_32::to_ucs4(input, omit_invalid, supress_error, UTF32Variant::UTF32LE)
        }
        Encodings::UTF_32BE => {
            utf_32::to_ucs4(input, omit_invalid, supress_error, UTF32Variant::UTF32BE)
        }
        Encodings::ASCII => ascii::to_ucs4(input, omit_invalid, supress_error),
    };

    let (output_exit_code, output) = match to {
        Encodings::UTF_8 => utf_8::from_ucs4(input.as_slice(), omit_invalid, supress_error),
        Encodings::UTF_16 => utf_16::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF16Variant::UTF16,
        ),
        Encodings::UTF_16LE => utf_16::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF16Variant::UTF16LE,
        ),
        Encodings::UTF_16BE => utf_16::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF16Variant::UTF16BE,
        ),
        Encodings::UTF_32 => utf_32::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF32Variant::UTF32,
        ),
        Encodings::UTF_32LE => utf_32::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF32Variant::UTF32LE,
        ),
        Encodings::UTF_32BE => utf_32::from_ucs4(
            input.as_slice(),
            omit_invalid,
            supress_error,
            UTF32Variant::UTF32BE,
        ),
        Encodings::ASCII => ascii::from_ucs4(input.as_slice(), omit_invalid, supress_error),
    };

    let exit_code = input_exit_code.max(output_exit_code);

    (output, exit_code)
}
fn charmap_conversion(
    from: &Charmap,
    to: &Charmap,
    input: &[u8],
    omit_invalid: bool,
    suppress_error: bool,
) -> (Vec<u8>, u32) {
    let mut output = Vec::new();
    let mut error_count = 0;

    let mut i = 0;
    while i < input.len() {
        let mut found = false;
        for (_, entry) in &from.entries {
            if input[i..].starts_with(&entry.encoding) {
                if let Some(to_entry) = to
                    .entries
                    .values()
                    .find(|e| e.symbolic_name == entry.symbolic_name)
                {
                    output.extend_from_slice(&to_entry.encoding);
                    i += entry.encoding.len();
                    found = true;
                    break;
                }
            }
        }

        if !found {
            if !suppress_error {
                eprintln!("Error: Invalid or unmapped character at position {}", i);
            }
            error_count += 1;
            if omit_invalid {
                i += 1;
            } else {
                output.push(input[i]);
                i += 1;
            }
        }
    }

    let exit_code = if error_count > 0 { 1 } else { 0 };
    (output, exit_code)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.list_codesets {
        list_encodings();
        exit(0);
    }

    let from_codeset = args.from_codeset.unwrap_or_else(|| {
        env::var("LANG")
            .ok()
            .and_then(|lang| lang.split('.').nth(1).map(String::from))
            .unwrap_or_else(|| {
                eprintln!("Error: Could not find a codeset from your locale");
                exit(1);
            })
    });

    let to_codeset = args.to_codeset.unwrap_or_else(|| {
        env::var("LANG")
            .ok()
            .and_then(|lang| lang.split('.').nth(1).map(String::from))
            .unwrap_or_else(|| {
                eprintln!("Error: Could not find a codeset from your locale");
                exit(1);
            })
    });

    let from_codeset = parse_codeset(&from_codeset)?;
    let to_codeset = parse_codeset(&to_codeset)?;

    let inputs: Vec<Box<dyn Read>> = match args.files {
        Some(files) => files
            .into_iter()
            .map(|file| plib::io::input_stream(&file, true))
            .collect::<Result<Vec<_>, _>>()?,
        None => vec![Box::new(io::stdin().lock())],
    };

    for mut input in inputs {
        let mut inp_buf = Vec::new();
        input.read_to_end(&mut inp_buf)?;

        match (&from_codeset, &to_codeset) {
            (CodesetType::Encoding(from), CodesetType::Encoding(to)) => {
                let (output, exit_code) = encoding_conversion(
                    from,
                    to,
                    &inp_buf,
                    args.omit_invalid,
                    args.suppress_messages,
                );

                io::stdout().write_all(&output)?;
                exit(exit_code as i32);
            }

            (CodesetType::Charmap(from), CodesetType::Charmap(to)) => {
                let (output, exit_code) = charmap_conversion(
                    from,
                    to,
                    &inp_buf,
                    args.omit_invalid,
                    args.suppress_messages,
                );

                io::stdout().write_all(&output)?;
                exit(exit_code as i32);
            }
            _ => {
                eprintln!(
                    "Error: Both codesets must be of the same type (either Encoding or Charmap)"
                );
                exit(1);
            }
        }
    }

    Ok(())
}
