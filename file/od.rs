//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::File;
use std::io::{self, BufReader, Error, Read, Seek, SeekFrom};
use std::num::ParseIntError;
use std::path::PathBuf;
use std::slice::Chunks;
use std::str::FromStr;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

use crate::io::ErrorKind;

#[derive(Parser)]
#[command(version, about = gettext("od - dump files in octal and other formats"))]
struct Args {
    #[arg(
        short = 'A',
        help = gettext(
            "Address base (d for decimal, o for octal, x for hexadecimal, n for none)"
        )
    )]
    address_base: Option<char>,

    #[arg(short = 'j', help = gettext("Skip bytes from the beginning of the input"))]
    skip: Option<String>,

    #[arg(short = 'N', help = gettext("Read only the specified number of bytes"))]
    count: Option<String>,

    #[arg(short = 't', help = gettext("Select the output format"))]
    type_strings: Vec<String>,

    #[arg(
        short = 'b',
        help = gettext("Interpret bytes in octal")
    )]
    octal_bytes: bool,

    #[arg(
        short = 'd',
        help = gettext("Interpret words (two-byte units) in unsigned decimal")
    )]
    unsigned_decimal_words: bool,

    #[arg(
        short = 'o',
        help = gettext("Interpret words (two-byte units) in octal")
    )]
    octal_words: bool,

    #[arg(
        short = 'c',
        help = gettext("Interpret bytes as characters")
    )]
    bytes_char: bool,

    #[arg(
        short = 's',
        help = gettext("Interpret words (two-byte units) in signed decimal")
    )]
    signed_decimal_words: bool,

    #[arg(
        short = 'x',
        help = gettext("Interpret words (two-byte units) in hexadecimal")
    )]
    hex_words: bool,

    #[arg(short = 'v', help = gettext("Verbose output"))]
    verbose: bool,

    #[arg(help = gettext("Input files"))]
    files: Vec<PathBuf>,

    #[arg(skip)]
    /// Offset in the file where dumping is to commence, must start with "+"]
    offset: Option<String>,
}

impl Args {
    /// Validate the arguments for any conflicts or invalid combinations.
    fn validate_args(&mut self) -> Result<(), String> {
        // Check if conflicting options are used together

        for file in &self.files {
            let string = file.to_str().unwrap();
            if string.starts_with('+') {
                self.offset = Some(string.to_string());
            }
        }

        // '-A', '-j', '-N', '-t', '-v' should not be used with offset syntax [+]offset[.][b]
        if (self.address_base.is_some()
            || self.skip.is_some()
            || self.count.is_some()
            || !self.type_strings.is_empty()
            || self.verbose)
            && self.offset.is_some()
        {
            return Err("Options '-A', '-j', '-N', '-t', '-v' cannot be used together with offset syntax '[+]offset[.][b]'".to_string());
        }

        // '-b', '-c', '-d', '-o', '-s', '-x' should not be used with '-t' options
        if !self.type_strings.is_empty()
            && (self.octal_bytes
                || self.bytes_char
                || self.unsigned_decimal_words
                || self.octal_words
                || self.signed_decimal_words
                || self.hex_words)
        {
            return Err(
                "Options '-b', '-c', '-d', '-o', '-s', '-x' cannot be used together with '-t'"
                    .to_string(),
            );
        }

        if self.octal_bytes {
            self.type_strings = vec!["o1".to_string()];
        }
        if self.unsigned_decimal_words {
            self.type_strings = vec!["u2".to_string()];
        }
        if self.octal_words {
            self.type_strings = vec!["o2".to_string()];
        }
        if self.signed_decimal_words {
            self.type_strings = vec!["d2".to_string()];
        }
        if self.hex_words {
            self.type_strings = vec!["x2".to_string()];
        }

        // Check if multiple mutually exclusive options are used together
        let mut basic_types = 0;
        if self.octal_bytes {
            basic_types += 1;
        }
        if self.bytes_char {
            basic_types += 1;
        }
        if self.unsigned_decimal_words {
            basic_types += 1;
        }
        if self.octal_words {
            basic_types += 1;
        }
        if self.signed_decimal_words {
            basic_types += 1;
        }
        if self.hex_words {
            basic_types += 1;
        }

        if basic_types > 1 {
            return Err(
                "Options '-b', '-c', '-d', '-o', '-s', '-x' cannot be used together".to_string(),
            );
        }

        Ok(())
    }
}

/// Parses an offset string and converts it into a `u64` value.
///
/// # Parameters
///
/// - `offset: &str`: A string slice representing the offset. This string can be in hexadecimal
///   format prefixed with "0x" or "0X", octal format prefixed with "0", or decimal format. The
///   string may also end with 'b', 'k', or 'm' to indicate byte multipliers.
///
/// # Returns
///
/// - `Result<u64, Box<dyn std::error::Error>>`: This function returns a `Result` which is:
///   - `Ok(u64)`: On success, the parsed and multiplied offset as a `u64`.
///   - `Err(Box<dyn std::error::Error>)`: On failure, an error boxed as a `dyn std::error::Error`.
///
fn parse_skip(offset: &str) -> Result<u64, Box<dyn std::error::Error>> {
    let (number, multiplier) = if offset.starts_with("0x") || offset.starts_with("0X") {
        // For hexadecimal, 'b' should be part of the number if it is the last character
        (offset, 1)
    } else {
        let mut chars = offset.chars();

        match chars.next_back() {
            Some('b') => (chars.as_str(), 512),
            Some('k') => (chars.as_str(), 1024),
            Some('m') => (chars.as_str(), 1048576),
            _ => (offset, 1),
        }
    };

    let base_value = parse_count::<u64>(number)?;

    Ok(base_value * multiplier)
}
/// Parses a count string and converts it into a specified numeric type.
///
/// # Parameters
///
/// - `count: &str`: A string slice representing the count. This string can be in hexadecimal
///   format prefixed with "0x" or "0X", octal format prefixed with "0", or decimal format.
///
/// # Returns
///
/// - `Result<T, Box<dyn std::error::Error>>`: This function returns a `Result` which is:
///   - `Ok(T)`: On success, the parsed count as the specified type.
///   - `Err(Box<dyn std::error::Error>)`: On failure, an error boxed as a `dyn std::error::Error`.
///
fn parse_count<T: FromStr<Err = ParseIntError> + FromStrRadix>(
    count: &str,
) -> Result<T, Box<dyn std::error::Error>> {
    if count.starts_with("0x") || count.starts_with("0X") {
        T::from_str_radix(&count[2..], 16).map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
    } else if count.starts_with('0') && count.len() > 1 {
        T::from_str_radix(&count[1..], 8).map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
    } else {
        count
            .parse::<T>()
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
    }
}

trait FromStrRadix: Sized {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>;
}

impl FromStrRadix for usize {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> {
        usize::from_str_radix(src, radix)
    }
}

impl FromStrRadix for u64 {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> {
        u64::from_str_radix(src, radix)
    }
}

/// Parses an offset string and converts it into a `u64` value.
///
/// This function handles special suffixes and bases:
/// - A suffix of 'b' indicates the value is in 512-byte blocks.
/// - A suffix of '.' indicates the value is in base 10 (decimal).
/// - Otherwise, the value is assumed to be in base 8 (octal).
///
/// # Parameters
///
/// - `offset: &str`: A string slice representing the offset. This string can optionally end with
///   'b' for 512-byte blocks or '.' for decimal format. By default, the string is considered
///   to be in octal format.
///
/// # Returns
///
/// - `Result<u64, ParseIntError>>`: This function returns a `Result` which is:
///   - `Ok(u64)`: On success, the parsed and multiplied offset as a `u64`.
///   - `Err(ParseIntError)`: On failure, an error.
///
fn parse_offset(offset: &str) -> Result<u64, ParseIntError> {
    let mut base = 8;
    let mut multiplier = 1;

    // Handle special suffixes
    let offset = if let Some(offset) = offset.strip_suffix('b') {
        multiplier = 512;
        offset
    } else if let Some(offset) = offset.strip_suffix('.') {
        base = 10;
        offset
    } else {
        offset
    };

    let parsed_offset = u64::from_str_radix(offset, base)?;

    Ok(parsed_offset * multiplier)
}

/// Reads data from a reader and prints it based on the provided configuration.
///
/// # Parameters
///
/// - `reader`: A mutable reference to an object implementing the `Read` trait. This is the source from which data will be read.
/// - `config`: A reference to an `Args` struct that holds configuration options for printing the data.
///
/// # Returns
///
/// Returns a `Result` which is:
/// - `Ok(())` if the function completes successfully.
/// - `Err(Box<dyn std::error::Error>)` if there is an error during reading or processing data.
///
/// # Errors
///
/// This function can return an error if:
/// - There is an issue reading from the `reader`.
/// - There is an invalid type string specified in `config.type_strings`.
///
/// # Behavior
///
/// 1. Initializes the offset to 0 for printing addresses.
/// 2. Defines a buffer of 16 bytes to read data in chunks.
/// 3. Checks if a count limit is specified in the configuration and parses it.
/// 4. Enters a loop to read and process data until the reader is exhausted or the count limit is reached.
/// 5. Reads up to 16 bytes into the buffer. If fewer than 16 bytes are read, it attempts to read the remaining bytes.
/// 6. Exits the loop if no more bytes can be read.
/// 7. Truncates the buffer if a count limit is specified and reached.
/// 8. Prints the address in the specified base format (decimal, octal, hexadecimal, or none).
/// 9. Processes the buffer according to the specified format in the configuration:
///     - If `bytes_char` is true, it processes and prints bytes as characters.
///     - If no specific type strings are provided, it processes the buffer in chunks of 2 bytes.
///     - For each type string, processes and prints the buffer in the specified format (e.g., unsigned integer, float, etc.).
/// 10. Increments the offset by the number of bytes read and processed.
/// 11. Prints a newline after processing each line of bytes.
/// 12. Continues until all data is read or the count limit is reached.
/// 13. Prints the final address in the specified base format.
///
fn print_data<R: Read>(
    reader: &mut R,
    config: &Args,
    bytes_that_will_be_skipped: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    // The bytes have been skipped now. The offset will be > 0 if skipping was performed.
    let mut offset = bytes_that_will_be_skipped; // Initialize offset for printing addresses.

    let mut buffer = [0; 16]; // Buffer to read data in chunks of 16 bytes.
    let mut previous_offset_string = String::new();
    let mut previous_asterisk = false;

    // Parse count limit from config, if specified.
    let count = if let Some(count) = config.count.as_ref() {
        Some(parse_count::<usize>(count)?)
    } else {
        None
    };

    let mut run = true; // Flag to indicate if the reader should continue reading.

    while run {
        let mut bytes_read = reader.read(&mut buffer)?; // Read up to 16 bytes into the buffer.

        if bytes_read != 16 {
            // If fewer than 16 bytes are read, attempt to read the remaining bytes.
            let bytes_read_2 = reader.read(&mut buffer[bytes_read..])?;
            bytes_read += bytes_read_2;
        }
        if bytes_read == 0 {
            break; // Exit loop if no more bytes can be read.
        }

        let mut local_buf = &buffer[..bytes_read]; // Create a slice of the buffer up to the number of bytes read.

        // Truncate the buffer to the specified count, if provided.
        if let Some(count) = count {
            let all_bytes = offset + bytes_read;
            if count < all_bytes {
                local_buf = &buffer[..all_bytes - (all_bytes - count)];
                bytes_read = local_buf.len();
                run = false;
            }
        }

        let local_buf_len = local_buf.len();

        // Print the address in the specified base format.
        let offset_string = if let Some(base) = config.address_base {
            match base {
                'd' => format!("{:07}", offset),  // Decimal format
                'o' => format!("{:07o}", offset), // Octal format
                'x' => format!("{:06x}", offset), // Hexadecimal format (only six characters)
                'n' => String::new(),             // No address printed
                _ => format!("{:07o}", offset),   // Default to octal if invalid base
            }
        } else {
            format!("{:07o}", offset) // Default to octal if no base is specified.
        };

        // Process and print the buffer based on configuration.
        if config.bytes_char {
            // Print bytes as characters.

            let res = process_formatter(&BCFormatter, local_buf, local_buf_len);
            process_res_string(
                &offset_string,
                &mut previous_offset_string,
                &mut previous_asterisk,
                &res,
                config.verbose,
            );
        } else if config.type_strings.is_empty() {
            // Process the buffer in chunks of 2 bytes.
            let chunks = local_buf.chunks(2);
            let res = process_chunks_formatter(&OFormatter, chunks, 2, local_buf_len);
            process_res_string(
                &offset_string,
                &mut previous_offset_string,
                &mut previous_asterisk,
                &res,
                config.verbose,
            );
        } else {
            // Process the buffer according to specified type strings.
            for type_string in &config.type_strings {
                // Determine the number of bytes to read for this type.
                let mut chars = type_string.chars();
                let type_char = chars.next().unwrap();
                let num_bytes: usize = chars.as_str().parse().unwrap_or(match type_char {
                    'd' | 'u' | 'o' | 'x' => 2, // Default to 2 bytes for integers
                    'f' => 4,                   // Default to 4 bytes for floats
                    _ => 1,                     // Default to 1 byte for unknown types
                });

                let chunks = local_buf.chunks(num_bytes);
                match type_char {
                    'a' => {
                        let res = process_formatter(&AFormatter, local_buf, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'c' => {
                        let res = process_formatter(&CFormatter, local_buf, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'u' => {
                        // Check if the number of bytes is valid for unsigned integers.
                        if !matches!(num_bytes, 1 | 2 | 4 | 8) {
                            return Err(Box::new(Error::new(
                                ErrorKind::Other,
                                format!("invalid type string `u{}`", num_bytes),
                            )));
                        }
                        let res =
                            process_chunks_formatter(&UFormatter, chunks, num_bytes, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'd' => {
                        // Check if the number of bytes is valid for signed integers.
                        if !matches!(num_bytes, 1 | 2 | 4 | 8) {
                            return Err(Box::new(Error::new(
                                ErrorKind::Other,
                                format!("invalid type string `d{}`", num_bytes),
                            )));
                        }
                        let res =
                            process_chunks_formatter(&DFormatter, chunks, num_bytes, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'x' => {
                        // Check if the number of bytes is valid for hexadecimal format.
                        if !matches!(num_bytes, 1 | 2 | 4 | 8) {
                            return Err(Box::new(Error::new(
                                ErrorKind::Other,
                                format!("invalid type string `x{}`", num_bytes),
                            )));
                        }
                        let res =
                            process_chunks_formatter(&XFormatter, chunks, num_bytes, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'o' => {
                        // Check if the number of bytes is valid for octal format.
                        if !matches!(num_bytes, 1 | 2 | 4 | 8) {
                            return Err(Box::new(Error::new(
                                ErrorKind::Other,
                                format!("invalid type string `o{}`", num_bytes),
                            )));
                        }
                        let res =
                            process_chunks_formatter(&OFormatter, chunks, num_bytes, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    'f' => {
                        // Check if the number of bytes is valid for floats.
                        if !(matches!(num_bytes, 4 | 8)) {
                            return Err(Box::new(Error::new(
                                ErrorKind::Other,
                                format!("invalid type string `f{}`", num_bytes),
                            )));
                        }
                        let res =
                            process_chunks_formatter(&FFormatter, chunks, num_bytes, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                    _ => {
                        // Default formatter for unknown types.
                        let res = process_formatter(&DefaultFormatter, local_buf, local_buf_len);
                        process_res_string(
                            &offset_string,
                            &mut previous_offset_string,
                            &mut previous_asterisk,
                            &res,
                            config.verbose,
                        );
                    }
                }
            }
        }

        offset += bytes_read; // Move to the next line of bytes.
    }

    // Print the final address in the specified base format.
    if let Some(base) = config.address_base {
        match base {
            'd' => println!("{:07}", offset),  // Decimal format
            'o' => println!("{:07o}", offset), // Octal format
            'x' => println!("{:06x}", offset), // Hexadecimal format (only six characters)
            'n' => (),                         // No address printed
            _ => println!("{:07o}", offset),   // Default to octal if invalid base
        }
    } else {
        println!("{:07o}", offset); // Default to octal if no base is specified.
    }

    Ok(())
}

fn process_res_string(
    offset_string: &str,
    previous_offset_string: &mut String,
    previous_asterisk: &mut bool,
    res_string: &str,
    verbose: bool,
) {
    if !verbose && res_string == previous_offset_string {
        if !*previous_asterisk {
            print!("*");
            *previous_asterisk = true;
            println!(); // Print a newline after each line of bytes.
        }
    } else {
        print!("{offset_string}");
        print!("{res_string}");
        println!(); // Print a newline after each line of bytes.
        res_string.clone_into(previous_offset_string);
    }
}

trait FormatterChunks {
    fn format_value_from_chunk(&self, chunk: &[u8], num_bytes: usize) -> String;
}

struct UFormatter;

impl FormatterChunks for UFormatter {
    fn format_value_from_chunk(&self, chunk: &[u8], num_bytes: usize) -> String {
        let pad_to = match num_bytes {
            1 => {
                // u8::MAX is 255 (3 digits)
                3_usize
            }
            2 => {
                // u16::MAX is 65535 (5 digits)
                5_usize
            }
            4 => {
                // u32::MAX is 4294967295 (10 digits)
                10_usize
            }
            8 => {
                // u64::MAX is 18446744073709551615 (20 digits)
                20_usize
            }
            _ => {
                // TODO
                // Should be unreachable
                debug_assert!(false);

                0
            }
        };

        let value = match chunk.len() {
            1 => u8::from_be_bytes(chunk.try_into().unwrap()) as u64,
            2 => {
                let mut arr: [u8; 2] = chunk.try_into().unwrap();
                arr.reverse();
                u16::from_be_bytes(arr) as u64
            }
            3 => {
                let mut arr = [0u8; 4];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            4 => {
                let mut arr: [u8; 4] = chunk.try_into().unwrap();
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            5 => {
                let mut arr = [0u8; 8];
                arr[3..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            6 => {
                let mut arr = [0u8; 8];
                arr[2..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            7 => {
                let mut arr = [0u8; 8];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            8 => {
                let mut arr: [u8; 8] = chunk.try_into().unwrap();
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            _ => 0,
        };

        format!(" {value: >width$}", width = pad_to)
    }
}

struct DFormatter;

impl FormatterChunks for DFormatter {
    fn format_value_from_chunk(&self, chunk: &[u8], num_bytes: usize) -> String {
        let pad_to = match num_bytes {
            1 => {
                // i8::MIN is -128 (4 digits)
                4_usize
            }
            2 => {
                // i16::MIN is -32768 (6 digits)
                6_usize
            }
            4 => {
                // i32::MIN is -2147483648 (11 digits)
                11_usize
            }
            8 => {
                // i64::MIN is -9223372036854775808 (20 digits)
                20_usize
            }
            _ => {
                // TODO
                // Should be unreachable
                debug_assert!(false);

                0
            }
        };

        let value = match chunk.len() {
            1 => i8::from_be_bytes(chunk.try_into().unwrap()) as i64,
            2 => {
                let mut arr: [u8; 2] = chunk.try_into().unwrap();
                arr.reverse();
                i16::from_be_bytes(arr) as i64
            }
            3 => {
                let mut arr = [0u8; 4];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                i32::from_be_bytes(arr) as i64
            }
            4 => {
                let mut arr: [u8; 4] = chunk.try_into().unwrap();
                arr.reverse();
                i32::from_be_bytes(arr) as i64
            }
            5 => {
                let mut arr = [0u8; 8];
                arr[3..].copy_from_slice(chunk);
                arr.reverse();
                i64::from_be_bytes(arr)
            }
            6 => {
                let mut arr = [0u8; 8];
                arr[2..].copy_from_slice(chunk);
                arr.reverse();
                i64::from_be_bytes(arr)
            }
            7 => {
                let mut arr = [0u8; 8];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                i64::from_be_bytes(arr)
            }
            8 => {
                let mut arr: [u8; 8] = chunk.try_into().unwrap();
                arr.reverse();
                i64::from_be_bytes(arr)
            }
            _ => 0,
        };

        format!(" {value: >width$}", width = pad_to)
    }
}

struct XFormatter;

impl FormatterChunks for XFormatter {
    fn format_value_from_chunk(&self, chunk: &[u8], num_bytes: usize) -> String {
        let value = match chunk.len() {
            1 => u8::from_be_bytes(chunk.try_into().unwrap()) as u64,
            2 => {
                let mut arr: [u8; 2] = chunk.try_into().unwrap();
                arr.reverse();
                u16::from_be_bytes(arr) as u64
            }
            3 => {
                let mut arr = [0u8; 4];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            4 => {
                let mut arr: [u8; 4] = chunk.try_into().unwrap();
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            5 => {
                let mut arr = [0u8; 8];
                arr[3..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            6 => {
                let mut arr = [0u8; 8];
                arr[2..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            7 => {
                let mut arr = [0u8; 8];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            8 => {
                let mut arr: [u8; 8] = chunk.try_into().unwrap();
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            _ => 0,
        };

        // It takes exactly two hexadecimal digits to represent a byte (2^8 = 256 and 16^2 = 256)
        format!(" {value:0width$x}", width = num_bytes * 2)
    }
}

struct OFormatter;

impl FormatterChunks for OFormatter {
    fn format_value_from_chunk(&self, chunk: &[u8], num_bytes: usize) -> String {
        let value = match chunk.len() {
            1 => u8::from_be_bytes(chunk.try_into().unwrap()) as u64,
            2 => {
                let mut arr: [u8; 2] = chunk.try_into().unwrap();
                arr.reverse();
                u16::from_be_bytes(arr) as u64
            }
            3 => {
                let mut arr = [0u8; 4];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            4 => {
                let mut arr: [u8; 4] = chunk.try_into().unwrap();
                arr.reverse();
                u32::from_be_bytes(arr) as u64
            }
            5 => {
                let mut arr = [0u8; 8];
                arr[3..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            6 => {
                let mut arr = [0u8; 8];
                arr[2..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            7 => {
                let mut arr = [0u8; 8];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            8 => {
                let mut arr: [u8; 8] = chunk.try_into().unwrap();
                arr.reverse();
                u64::from_be_bytes(arr)
            }
            _ => 0,
        };

        // It takes three octal digits to represent a byte (2^8 = 256 and 8^3 = 512)
        format!(" {value:0width$o}", width = num_bytes * 3)
    }
}

struct FFormatter;

impl FormatterChunks for FFormatter {
    fn format_value_from_chunk(&self, chunk: &[u8], _num_bytes: usize) -> String {
        let value = match chunk.len() {
            4 => {
                let mut arr: [u8; 4] = chunk.try_into().unwrap();
                arr.reverse();
                f32::from_be_bytes(arr) as f64
            }
            5 => {
                let mut arr = [0u8; 8];
                arr[3..].copy_from_slice(chunk);
                arr.reverse();
                f64::from_be_bytes(arr)
            }
            6 => {
                let mut arr = [0u8; 8];
                arr[2..].copy_from_slice(chunk);
                arr.reverse();
                f64::from_be_bytes(arr)
            }
            7 => {
                let mut arr = [0u8; 8];
                arr[1..].copy_from_slice(chunk);
                arr.reverse();
                f64::from_be_bytes(arr)
            }
            8 => {
                let mut arr: [u8; 8] = chunk.try_into().unwrap();
                arr.reverse();
                f64::from_be_bytes(arr)
            }
            _ => 0.0,
        };
        format!(" {value:e}")
    }
}

fn process_chunks_formatter(
    formatter: &dyn FormatterChunks,
    chunks: Chunks<u8>,
    num_bytes: usize,
    local_buf_len: usize,
) -> String {
    let buffer_size = local_buf_len * 8;

    let mut result = String::with_capacity(buffer_size);

    for chunk in chunks {
        result.push_str(&formatter.format_value_from_chunk(chunk, num_bytes));
    }

    result
}

trait Formatter {
    fn format_value(&self, byte: u8) -> String;
}

struct AFormatter;
struct CFormatter;
struct BCFormatter;
struct DefaultFormatter;

impl Formatter for AFormatter {
    fn format_value(&self, byte: u8) -> String {
        if let Some(name) = get_named_char(byte) {
            format!(" {name: >3}")
        } else if byte.is_ascii_graphic() || byte.is_ascii_whitespace() {
            format!("   {}", byte as char)
        } else {
            format!(" {byte:03o}")
        }
    }
}

impl Formatter for CFormatter {
    fn format_value(&self, byte: u8) -> String {
        match byte {
            b'\\' => "  \\".to_string(),
            b'\x07' => "  \\a".to_string(),
            b'\x08' => "  \\b".to_string(),
            b'\x0C' => "  \\f".to_string(),
            b'\x0A' => "  \\n".to_string(),
            b'\x0D' => "  \\r".to_string(),
            b'\x09' => "  \\t".to_string(),
            b'\x0B' => "  \\v".to_string(),
            _ if byte.is_ascii_graphic() || byte.is_ascii_whitespace() => {
                format!("   {}", byte as char)
            }
            _ => format!(" {:03o}", byte),
        }
    }
}

impl Formatter for BCFormatter {
    fn format_value(&self, byte: u8) -> String {
        match byte {
            b'\0' => " NUL".to_string(),
            b'\x08' => "  BS".to_string(),
            b'\x0C' => "  FF".to_string(),
            b'\x0A' => "  NL".to_string(),
            b'\x0D' => "  CR".to_string(),
            b'\x09' => "  HT".to_string(),
            _ if byte.is_ascii_graphic() || byte.is_ascii_whitespace() => {
                format!("   {}", byte as char)
            }
            _ => format!(" {:03o}", byte),
        }
    }
}

impl Formatter for DefaultFormatter {
    fn format_value(&self, byte: u8) -> String {
        format!(" {:03o}", byte)
    }
}

fn process_formatter(formatter: &dyn Formatter, local_buf: &[u8], local_buf_len: usize) -> String {
    let buffer_size = local_buf_len * 8;

    let mut result = String::with_capacity(buffer_size);

    for byte in local_buf {
        result.push_str(&formatter.format_value(*byte));
    }

    result
}

fn get_named_char(byte: u8) -> Option<&'static str> {
    match byte {
        0x00 => Some("nul"),
        0x01 => Some("soh"),
        0x02 => Some("stx"),
        0x03 => Some("etx"),
        0x04 => Some("eot"),
        0x05 => Some("enq"),
        0x06 => Some("ack"),
        0x07 => Some("bel"),
        0x08 => Some("bs"),
        0x09 => Some("ht"),
        0x0A => Some("nl"),
        0x0B => Some("vt"),
        0x0C => Some("ff"),
        0x0D => Some("cr"),
        0x0E => Some("so"),
        0x0F => Some("si"),
        0x10 => Some("dle"),
        0x11 => Some("dc1"),
        0x12 => Some("dc2"),
        0x13 => Some("dc3"),
        0x14 => Some("dc4"),
        0x15 => Some("nak"),
        0x16 => Some("syn"),
        0x17 => Some("etb"),
        0x18 => Some("can"),
        0x19 => Some("em"),
        0x1A => Some("sub"),
        0x1B => Some("esc"),
        0x1C => Some("fs"),
        0x1D => Some("gs"),
        0x1E => Some("rs"),
        0x1F => Some("us"),
        0x7F => Some("del"),
        0x20 => Some("sp"),
        _ => None,
    }
}

/// Processes and prints data from one or more input sources according to the specified arguments.
///
/// # Parameters
///
/// - `args`: A reference to an `Args` struct that holds the configuration options for reading and printing data.
///
/// # Returns
///
/// Returns a `Result` which is:
/// - `Ok(())` if the function completes successfully.
/// - `Err(Box<dyn std::error::Error>)` if there is an error during reading or processing data.
///
/// # Errors
///
/// This function can return an error if:
/// - There is an issue opening or reading from the specified files.
/// - There is an error with the skip or offset options.
/// - There is an issue chaining multiple file readers.
///
/// # Behavior
///
/// 1. Initializes `bytes_to_skip` and `bytes_skipped` to manage byte skipping.
/// 2. Prepares a vector to hold all file readers.
/// 3. Parses and sets the number of bytes to skip based on the `-j` option or offset.
/// 4. Determines the source of the input:
///     - If there is one file and it is "-" (stdin) or no files, reads from stdin and skips the specified bytes.
///     - Otherwise, processes each specified file:
///         - If the cumulative bytes skipped are less than the bytes to skip, skips entire files or parts of them.
///         - Adds each file to the vector of readers.
/// 5. Combines multiple file readers into a single reader if necessary.
/// 6. Calls `print_data` to read and print the data from the combined reader according to the configuration in `args`.
///
fn od(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let mut bytes_to_skip = 0; // Initialize the number of bytes to skip.
    let mut bytes_skipped = 0; // Initialize the number of bytes already skipped.

    let mut all_files: Vec<Box<dyn Read>> = Vec::new(); // Vector to hold file readers.

    // Skip bytes if the -j option is specified.
    if let Some(skip) = &args.skip {
        bytes_to_skip = parse_skip(skip)?; // Parse the skip option.
    }

    // Override skip bytes with offset if specified.
    if let Some(offset) = &args.offset {
        bytes_to_skip = parse_offset(offset)?; // Parse the offset option.
    }

    let bytes_that_will_be_skipped = usize::try_from(bytes_to_skip)?;

    let mut reader: Box<dyn Read> = if (args.files.len() == 1
        && args.files[0] == PathBuf::from("-"))
        || args.files.is_empty()
    {
        // If there is one file and it is "-" (stdin) or no files, read from stdin.
        let mut stdin: Box<dyn Read> = Box::new(io::stdin().lock());

        // Buffer of size 1 byte for reading char by char to skip bytes.
        let mut empty_buffer = [0; 1];

        // Skip the specified number of bytes from stdin.
        while bytes_to_skip > 0 {
            stdin.read_exact(&mut empty_buffer)?;
            bytes_to_skip -= 1;
        }
        stdin // Use stdin as the reader.
    } else {
        // Otherwise, process each specified file.
        for file in &args.files {
            let mut file = File::open(file)?; // Open the file.

            if bytes_skipped < bytes_to_skip {
                // If the cumulative bytes skipped are less than the bytes to skip, process the file for skipping.
                let metadata = file.metadata()?; // Get file metadata.
                let file_size = metadata.len(); // Get file size.

                if bytes_skipped + file_size <= bytes_to_skip {
                    // Skip the entire file if it is within the range of bytes to skip.
                    bytes_skipped += file_size;
                    continue; // Move to the next file.
                } else {
                    // Skip part of the file if only a portion of it is within the range of bytes to skip.
                    let remaining_skip = bytes_to_skip - bytes_skipped;
                    file.seek(SeekFrom::Start(remaining_skip))?; // Seek to the remaining bytes.
                    bytes_skipped = bytes_to_skip; // Update the bytes skipped.
                }
            }

            // Add the file reader to the vector of readers.
            all_files.push(Box::new(BufReader::new(file)));
        }

        if all_files.len() > 1 {
            // Combine multiple file readers into a single reader.
            all_files
                .into_iter()
                .reduce(|acc, file| Box::new(acc.chain(file)) as Box<dyn Read>)
                .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "No files to chain"))?
        // Handle error if no files to chain.
        } else {
            // If only one file, use it as the reader.
            match all_files.pop() {
                None => return Ok(()), // Return Ok if no files.
                Some(f) => f,          // Use the single file as the reader.
            }
        }
    };

    // Print the data using the reader.
    print_data(&mut reader, args, bytes_that_will_be_skipped)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let mut args = Args::parse();

    args.validate_args()?;
    let mut exit_code = 0;

    if let Err(err) = od(&args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use std::num::IntErrorKind;

    use super::*;

    #[test]
    fn test_parse_offset() {
        assert_eq!(parse_offset("777b"), Ok(0o777 * 512));
        assert_eq!(parse_offset("777."), Ok(777));
        assert_eq!(parse_offset("777"), Ok(0o777));
    }

    #[test]
    fn test_parse_offset_invalid() {
        let result = parse_offset("7.7");
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.kind(), &IntErrorKind::InvalidDigit);
    }
}
