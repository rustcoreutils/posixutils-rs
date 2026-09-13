//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
use std::str::FromStr;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

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
        // The obsolescent offset operand "[+]offset[.][b]" is only valid in the
        // obsolescent synopsis form, which uses -b/-c/-d/-o/-s/-x (and -v) but
        // not -A/-j/-N/-t.
        let new_form = self.address_base.is_some()
            || self.skip.is_some()
            || self.count.is_some()
            || !self.type_strings.is_empty();

        if let Some(last) = self.files.last() {
            let s = last.to_str().unwrap_or("");
            let plus = s.starts_with('+');
            // A '+'-prefixed last operand is always an offset. A bare numeric
            // last operand is an offset only in the two-operand XSI form
            // (file offset), to avoid misreading a numeric filename.
            let bare_numeric = self.files.len() == 2 && s.starts_with(|c: char| c.is_ascii_digit());

            if (plus || bare_numeric) && s != "-" {
                if new_form {
                    // Mixing the offset operand with the new-form options is an
                    // error only for the explicit '+' form; a bare numeric
                    // operand stays a filename.
                    if plus {
                        return Err("Options '-A', '-j', '-N', '-t' cannot be used with the offset operand '[+]offset[.][b]'".to_string());
                    }
                } else {
                    self.offset = Some(s.strip_prefix('+').unwrap_or(s).to_string());
                    self.files.pop();
                }
            }
        }

        // The short type options -b/-c/-d/-o/-s/-x are shorthands for -t types
        // and may be combined (with each other and with -t); the requested
        // types accumulate. (-c ≡ -t c: C-style escapes, not named characters.)
        if self.octal_bytes {
            self.type_strings.push("o1".to_string());
        }
        if self.bytes_char {
            self.type_strings.push("c".to_string());
        }
        if self.unsigned_decimal_words {
            self.type_strings.push("u2".to_string());
        }
        if self.octal_words {
            self.type_strings.push("o2".to_string());
        }
        if self.signed_decimal_words {
            self.type_strings.push("d2".to_string());
        }
        if self.hex_words {
            self.type_strings.push("x2".to_string());
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
        let hex_part = &count[2..];
        // Reject if hex part contains a sign
        if hex_part.starts_with('+') || hex_part.starts_with('-') {
            return Err("invalid hexadecimal number".into());
        }
        T::from_str_radix(hex_part, 16).map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
    } else if count.starts_with('0') && count.len() > 1 {
        let oct_part = &count[1..];
        // Reject if octal part contains a sign
        if oct_part.starts_with('+') || oct_part.starts_with('-') {
            return Err("invalid octal number".into());
        }
        T::from_str_radix(oct_part, 8).map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
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

    // Reject if offset contains a sign (offsets should be unsigned)
    if offset.starts_with('+') || offset.starts_with('-') {
        return Err("invalid offset".parse::<u64>().unwrap_err());
    }

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
///
/// Type strings are resolved into `specs` by the caller, before any input is
/// opened, so an invalid one can no longer surface from here.
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
    specs: &[TypeSpec],
    bytes_that_will_be_skipped: u64,
) -> Result<(), Box<dyn std::error::Error>> {
    // The bytes have been skipped now. The offset will be > 0 if skipping was performed.
    let mut offset: u64 = bytes_that_will_be_skipped; // Initialize offset for printing addresses.

    let mut buffer = [0; 16]; // Buffer to read data in chunks of 16 bytes.
    let mut previous_offset_string = String::new();
    let mut previous_asterisk = false;

    // Parse count limit from config, if specified.
    let count = if let Some(count) = config.count.as_ref() {
        Some(parse_count::<u64>(count)?)
    } else {
        None
    };

    let mut run = true; // Flag to indicate if the reader should continue reading.
    let mut written: u64 = 0; // Bytes written so far, which is what -N limits.

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

        // Truncate to the `-N` count, if provided. The count is a *length*:
        // it limits the bytes written, and says nothing about where they
        // start. Comparing it against `offset` charged the `-j` skip against
        // it as well, so `-j 2 -N 9` wrote seven bytes -- and underflowed when
        // the skip was the larger of the two.
        if let Some(count) = count {
            if written + bytes_read as u64 > count {
                let remaining = (count - written) as usize;
                local_buf = &local_buf[..remaining];
                bytes_read = remaining;
                run = false;
            }
        }

        // A count reached exactly on a block boundary, or a zero count, leaves
        // nothing to write: stop rather than emitting an empty field line.
        if bytes_read == 0 {
            break;
        }
        written += bytes_read as u64;

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

        // Render every type's line for this block, then print them as one
        // group. POSIX 109196-109199 puts the input offset on "the first
        // output line produced for each input block" -- one per block, not one
        // per type -- and the three-type example at 109240-109245 shows the
        // continuation lines blank where the offset would be.
        let scale = column_scale(specs);
        let lines: Vec<String> = specs
            .iter()
            .map(|spec| render_line(spec, local_buf, scale))
            .collect();

        for (index, line) in lines.iter().enumerate() {
            let prefix = if index == 0 {
                offset_string.clone()
            } else {
                " ".repeat(offset_string.len())
            };
            process_res_string(
                &prefix,
                &mut previous_offset_string,
                &mut previous_asterisk,
                line,
                config.verbose,
            );
        }

        offset += bytes_read as u64; // Move to the next line of bytes.
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

/// Lay a chunk out as `num_bytes` bytes of memory, extending a short final
/// chunk with null bytes.
///
/// POSIX 109193-109195: "If, as a result of the specification of the -N option
/// or end-of-file being reached on the last input file, input data only
/// partially satisfies an output type, the input shall be extended
/// sufficiently with null bytes to write the last byte of the input."
///
/// The nulls are appended *after* the input in memory, and that is what makes
/// one implementation right on either byte order: on a little-endian host they
/// become the high-order bytes of the value, on a big-endian host the
/// low-order ones. The POSIX example at 109240-109245 is from a big-endian
/// system and shows the padding at the opposite end for exactly this reason.
///
/// The callers then read the result with `from_ne_bytes`, as 109149-109151
/// requires -- the byte order "shall correspond to the order in which a
/// constant of the corresponding type is stored in memory on the system".
fn extend_chunk(chunk: &[u8], num_bytes: usize) -> [u8; 8] {
    let mut buf = [0_u8; 8];
    let n = chunk.len().min(num_bytes).min(buf.len());
    buf[..n].copy_from_slice(&chunk[..n]);
    buf
}

/// Read an extended chunk as an unsigned value of the *declared* width.
///
/// Reading at the chunk's own length instead is what made a short tail wrong:
/// the value came out of a narrower type and then had to be widened, which is
/// a different number.
fn chunk_to_u64(chunk: &[u8], num_bytes: usize) -> u64 {
    let buf = extend_chunk(chunk, num_bytes);
    match num_bytes {
        1 => buf[0] as u64,
        2 => u16::from_ne_bytes(buf[..2].try_into().unwrap()) as u64,
        4 => u32::from_ne_bytes(buf[..4].try_into().unwrap()) as u64,
        8 => u64::from_ne_bytes(buf),
        // `parse_type_string` admits no other width.
        _ => unreachable!("unsupported integer width {num_bytes}"),
    }
}

/// Read an extended chunk as a signed value of the *declared* width.
///
/// The width decides the sign, not the bytes present: a one-byte tail of 0xc7
/// under `-t d4` is a positive `i32` on a little-endian host, where the three
/// null bytes it is extended with are the high-order ones. Read as an `i8` it
/// was -57.
fn chunk_to_i64(chunk: &[u8], num_bytes: usize) -> i64 {
    let buf = extend_chunk(chunk, num_bytes);
    match num_bytes {
        1 => buf[0] as i8 as i64,
        2 => i16::from_ne_bytes(buf[..2].try_into().unwrap()) as i64,
        4 => i32::from_ne_bytes(buf[..4].try_into().unwrap()) as i64,
        8 => i64::from_ne_bytes(buf),
        _ => unreachable!("unsupported integer width {num_bytes}"),
    }
}

/// The column width a float field occupies, the separating blank included.
fn float_field(num_bytes: usize) -> usize {
    match num_bytes {
        4 => 16,
        8 => 25,
        _ => unreachable!("unsupported float width {num_bytes}"),
    }
}

/// Render the shortest round-trip decimal of a float by C's `%g` rules.
///
/// `exp_form` is Rust's `{:e}`, which is already the shortest decimal that
/// reads back as the same value -- `"3.4028235e38"`, `"1e-1"`, `"-0e0"`. What
/// it is not is `%g`: it always uses the exponent form, writes no sign on a
/// positive exponent, and pads it to no width. od wants positional notation
/// unless the exponent is below -4 or has reached the number of significant
/// digits, and a signed, two-digit-minimum exponent when it does use one.
fn render_g(exp_form: &str) -> String {
    let (mantissa, exponent) = match exp_form.split_once('e') {
        Some(parts) => parts,
        // `{:e}` always writes an exponent; nothing to rescue if it did not.
        None => return exp_form.to_string(),
    };
    let exponent: i32 = exponent.parse().unwrap_or(0);

    let negative = mantissa.starts_with('-');
    let digits: String = mantissa.chars().filter(|c| c.is_ascii_digit()).collect();
    let sig = digits.len() as i32;

    // C's %g rule, with the precision being the significant digits present.
    let body = if exponent < -4 || exponent >= sig {
        let mantissa = if digits.len() == 1 {
            digits
        } else {
            format!("{}.{}", &digits[..1], &digits[1..])
        };
        format!(
            "{}e{}{:02}",
            mantissa,
            if exponent < 0 { '-' } else { '+' },
            exponent.abs()
        )
    } else if exponent < 0 {
        // 0.00…digits -- exponent is at least -4 here.
        format!("0.{}{}", "0".repeat((-exponent - 1) as usize), digits)
    } else {
        let whole = (exponent + 1) as usize;
        if digits.len() > whole {
            format!("{}.{}", &digits[..whole], &digits[whole..])
        } else {
            format!("{}{}", digits, "0".repeat(whole - digits.len()))
        }
    };

    if negative {
        format!("-{body}")
    } else {
        body
    }
}

/// Read an extended chunk as a float of the *declared* width, and render it.
///
/// The width comes from the type, never from what is left of the input: a
/// four-byte tail of a `-t f8` run is the first four bytes of a double, not a
/// float, and decoding it as a float gives an unrelated number rather than a
/// rounded one.
///
/// The value is rendered at the width it was read at, not widened to `f64`
/// first: the shortest decimal that round-trips an `f32` is shorter than the
/// one that round-trips the `f64` holding the same number, so widening would
/// print a float with a double's worth of digits.
fn chunk_to_float_text(chunk: &[u8], num_bytes: usize) -> String {
    let buf = extend_chunk(chunk, num_bytes);
    match num_bytes {
        4 => {
            let v = f32::from_ne_bytes(buf[..4].try_into().unwrap());
            if v.is_nan() {
                "nan".to_string()
            } else if v.is_infinite() {
                if v.is_sign_negative() { "-inf" } else { "inf" }.to_string()
            } else {
                render_g(&format!("{v:e}"))
            }
        }
        8 => {
            let v = f64::from_ne_bytes(buf);
            if v.is_nan() {
                "nan".to_string()
            } else if v.is_infinite() {
                if v.is_sign_negative() { "-inf" } else { "inf" }.to_string()
            } else {
                render_g(&format!("{v:e}"))
            }
        }
        // `parse_type_string` admits no other width.
        _ => unreachable!("unsupported float width {num_bytes}"),
    }
}

/// One field's text, before it is padded into its column.
///
/// The padding that is part of the *conversion* -- the leading zeroes of a hex
/// or octal field -- is applied here, because it belongs to the value. The
/// padding that positions the field in its column is not, because that depends
/// on the other types sharing the line and is applied by [`render_line`].
fn field_text(spec: &TypeSpec, chunk: &[u8]) -> String {
    match spec.type_char {
        'a' => {
            // Named-character output uses only the least significant seven bits.
            let byte = chunk[0] & 0x7F;
            if let Some(name) = get_named_char(byte) {
                name.to_string()
            } else if byte.is_ascii_graphic() || byte.is_ascii_whitespace() {
                (byte as char).to_string()
            } else {
                format!("{byte:03o}")
            }
        }
        'c' => match chunk[0] {
            b'\0' => "\\0".to_string(),
            // POSIX (l. 109177): a <backslash> is exempt from the escape table
            // and "shall be written as a single <backslash>" -- NOT as `\\`.
            // Keeping the arm explicit (rather than letting the graphic
            // character case below handle it) documents that omitting the
            // escape is deliberate.
            b'\\' => "\\".to_string(),
            b'\x07' => "\\a".to_string(),
            b'\x08' => "\\b".to_string(),
            b'\x0C' => "\\f".to_string(),
            b'\x0A' => "\\n".to_string(),
            b'\x0D' => "\\r".to_string(),
            b'\x09' => "\\t".to_string(),
            b'\x0B' => "\\v".to_string(),
            byte if byte.is_ascii_graphic() || byte.is_ascii_whitespace() => {
                (byte as char).to_string()
            }
            byte => format!("{byte:03o}"),
        },
        'u' => chunk_to_u64(chunk, spec.num_bytes).to_string(),
        'd' => chunk_to_i64(chunk, spec.num_bytes).to_string(),
        'x' => format!(
            "{:0width$x}",
            chunk_to_u64(chunk, spec.num_bytes),
            width = spec.num_bytes * 2
        ),
        'o' => format!(
            "{:0width$o}",
            chunk_to_u64(chunk, spec.num_bytes),
            // As many digits as the widest value of this width needs, which is
            // ceil(bits/3). Three digits *per byte* is a bound on a byte, not
            // on the number the bytes compose, and is tight only up to two of
            // them: u32::MAX is 37777777777, eleven digits, not twelve.
            width = (spec.num_bytes * 8).div_ceil(3)
        ),
        'f' => chunk_to_float_text(chunk, spec.num_bytes),
        // `parse_type_string` admits no other type character.
        _ => unreachable!("unsupported type character {}", spec.type_char),
    }
}

/// The width one field of this type occupies on its own, the blank that
/// separates it from the previous field included.
fn natural_field(spec: &TypeSpec) -> usize {
    match spec.type_char {
        // A named character, a C escape, or three octal digits.
        'a' | 'c' => 4,
        'u' => {
            1 + match spec.num_bytes {
                1 => 3,
                2 => 5,
                4 => 10,
                8 => 20,
                _ => unreachable!("unsupported integer width {}", spec.num_bytes),
            }
        }
        'd' => {
            1 + match spec.num_bytes {
                1 => 4,
                2 => 6,
                4 => 11,
                8 => 20,
                _ => unreachable!("unsupported integer width {}", spec.num_bytes),
            }
        }
        'x' => 1 + spec.num_bytes * 2,
        'o' => 1 + (spec.num_bytes * 8).div_ceil(3),
        'f' => float_field(spec.num_bytes),
        _ => unreachable!("unsupported type character {}", spec.type_char),
    }
}

/// The column width one input byte is given, shared by every type on the
/// block, as the fraction `natural / bytes` of whichever type needs the most
/// room per byte.
///
/// Sharing it is what lines the types up: a type converting more bytes per
/// field gets a proportionally wider field, so the same input byte sits in the
/// same column on every line. POSIX 109189-109192 asks only that fields be
/// "separated by one or more <blank> characters", so the alignment is a
/// courtesy rather than a requirement -- but an unaligned multi-type dump is
/// most of the reason to ask for one.
fn column_scale(specs: &[TypeSpec]) -> (usize, usize) {
    // Zero is the identity for the maximum below, so the first spec always
    // wins outright: seeding with a real width instead would impose it as a
    // floor, widening every type that needs less room per byte than it does.
    let mut scale = (0, 1);
    for spec in specs {
        let (num, den) = (natural_field(spec), spec.num_bytes);
        // num/den > scale.0/scale.1, cross-multiplied to stay in integers.
        if num * scale.1 > scale.0 * den {
            scale = (num, den);
        }
    }
    scale
}

/// The column at which field `index` of a `bytes`-wide type ends.
///
/// Rounded to the nearest column rather than truncated, so that a scale which
/// is not a whole number -- seven columns per two bytes, say -- spreads its
/// remainder across the fields (4, 3, 4, 3, ...) instead of letting them drift
/// out of alignment with the wider type's.
fn field_end(index: usize, bytes: usize, scale: (usize, usize)) -> usize {
    let (num, den) = scale;
    (2 * index * bytes * num + den) / (2 * den)
}

/// Render one output line: every field of one type, across the whole block.
fn render_line(spec: &TypeSpec, local_buf: &[u8], scale: (usize, usize)) -> String {
    let mut out = String::new();
    let mut column = 0;

    for (index, chunk) in local_buf.chunks(spec.num_bytes).enumerate() {
        let text = field_text(spec, chunk);
        let end = field_end(index + 1, spec.num_bytes, scale);
        // Never narrower than the text: a rounded-down column would otherwise
        // run two fields together with no separating blank.
        let width = (end - column).max(text.len() + 1);
        column = end;
        out.push_str(&format!("{text:>width$}"));
    }

    out
}

/// One resolved output type: the type character and the number of input bytes
/// each conversion of it consumes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct TypeSpec {
    type_char: char,
    num_bytes: usize,
}

/// The byte count a size letter names, which depends on the type character.
///
/// POSIX 109071-2 gives `f` the letters `F`/`D`/`L` -- float, double, long
/// double -- while 109073-5 gives `d`/`o`/`u`/`x` the letters `C`/`S`/`I`/`L`
/// -- char, short, int, long. Sharing one table let `-t fD` match nothing and
/// silently take the float default, so a double printed as two floats.
fn size_letter(type_char: char, letter: char) -> Option<usize> {
    match type_char {
        'f' => match letter {
            'F' => Some(4),
            'D' => Some(8),
            'L' => Some(16),
            _ => None,
        },
        _ => match letter {
            'C' => Some(1),
            'S' => Some(2),
            'I' => Some(4),
            'L' => Some(8),
            _ => None,
        },
    }
}

/// The byte count a type character converts when no size is written.
///
/// POSIX 109141-3: for `d`, `o`, `u` and `x` the default is "the size of the
/// underlying implementation's basic integer type" -- `int`, 4 bytes.
/// 109152-4: for `f` it is "the number of bytes in the underlying
/// implementation's basic double precision floating-point data type" -- 8.
/// These were 2 and 4, so a bare `-t x` printed eight 2-byte fields where
/// POSIX and every other od print four 4-byte ones.
fn default_size(type_char: char) -> usize {
    match type_char {
        'd' | 'o' | 'u' | 'x' => 4,
        'f' => 8,
        // `a` and `c` convert one byte and take no size at all.
        _ => 1,
    }
}

/// Reject a size the conversion cannot perform, naming the whole type string.
///
/// POSIX 109143-7 requires `char`, `short`, `int` and `long` for the integer
/// conversions -- and 1, 2, 4 and 8 "even if it provides no C-language types
/// of those sizes" -- and 109155-7 requires `float`, `double` and `long
/// double` for `f`.
fn validate_size(type_char: char, num_bytes: usize, spec: &str) -> Result<(), String> {
    let ok = match type_char {
        'f' => matches!(num_bytes, 4 | 8),
        _ => matches!(num_bytes, 1 | 2 | 4 | 8),
    };
    if ok {
        return Ok(());
    }
    let kind = if type_char == 'f' {
        gettext("floating-point")
    } else {
        gettext("integer")
    };
    Err(format!(
        "{}: {}",
        gettext("invalid type string `{}`").replace("{}", spec),
        gettext("no {n}-byte {kind} type")
            .replace("{n}", &num_bytes.to_string())
            .replace("{kind}", &kind)
    ))
}

/// Split one `-t` type string into the types it names.
///
/// POSIX 109075-6: "Multiple types can be concatenated within the same
/// type_string", which is why this yields a vector -- the spec's own example
/// at 109237 is `-t o2x2x`, three types in one string. The old parser took
/// the first character as the type and *all* the rest as its size, so
/// `o2x2x` collapsed to a single type whose size failed to parse and silently
/// became the default.
///
/// A size is either one letter from the type's own table or a run of decimal
/// digits, never both: a digit may not follow a size letter, so `-t fL8` is
/// rejected rather than read as `fL` followed by a stray `8`.
fn parse_type_string(spec: &str) -> Result<Vec<TypeSpec>, String> {
    let chars: Vec<char> = spec.chars().collect();
    let mut specs = Vec::new();
    let mut i = 0;

    while i < chars.len() {
        let type_char = chars[i];
        i += 1;

        if !matches!(type_char, 'a' | 'c' | 'd' | 'f' | 'o' | 'u' | 'x') {
            return Err(gettext("invalid character '{c}' in type string `{s}`")
                .replace("{c}", &type_char.to_string())
                .replace("{s}", spec));
        }

        // `a` and `c` take no size: they always convert one byte.
        if matches!(type_char, 'a' | 'c') {
            specs.push(TypeSpec {
                type_char,
                num_bytes: 1,
            });
            continue;
        }

        let num_bytes = if i < chars.len() && chars[i].is_ascii_digit() {
            let start = i;
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
            }
            let digits: String = chars[start..i].iter().collect();
            // Only an overflowing run can fail here; `validate_size` rejects
            // every in-range count the conversions do not provide.
            digits
                .parse::<usize>()
                .map_err(|_| gettext("invalid type string `{}`").replace("{}", spec))?
        } else if let Some(bytes) = chars.get(i).and_then(|&c| size_letter(type_char, c)) {
            i += 1;
            // A digit after a size letter is not a new type, and not part of
            // the size either.
            if let Some(&next) = chars.get(i) {
                if next.is_ascii_digit() {
                    return Err(gettext("invalid character '{c}' in type string `{s}`")
                        .replace("{c}", &next.to_string())
                        .replace("{s}", spec));
                }
            }
            bytes
        } else {
            // No size written. Whatever follows, if anything, is the next
            // type character and is checked on the next turn of the loop.
            default_size(type_char)
        };

        validate_size(type_char, num_bytes, spec)?;
        specs.push(TypeSpec {
            type_char,
            num_bytes,
        });
    }

    Ok(specs)
}

/// Resolve every `-t` option into a flat list of output types, in order.
///
/// With no type requested at all, od dumps two-byte octal words -- the
/// historical default the XSI synopsis spells `-t o2`. Making it an ordinary
/// spec rather than a branch in the render loop means it goes through the same
/// field layout and duplicate-suppression as everything else.
fn parse_type_specs(type_strings: &[String]) -> Result<Vec<TypeSpec>, String> {
    let mut specs = Vec::new();
    for spec in type_strings {
        specs.extend(parse_type_string(spec)?);
    }
    if specs.is_empty() {
        specs.push(TypeSpec {
            type_char: 'o',
            num_bytes: 2,
        });
    }
    Ok(specs)
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
    // Resolve every `-t` before opening anything, so a malformed type string is
    // diagnosed instead of surfacing partway through the output -- or, for a
    // zero size, as a panic inside `chunks()`.
    let specs = parse_type_specs(&args.type_strings).map_err(Error::other)?;

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

    let bytes_that_will_be_skipped = bytes_to_skip;

    let mut reader: Box<dyn Read> = if (args.files.len() == 1 && args.files[0].as_os_str() == "-")
        || args.files.is_empty()
    {
        // If there is one file and it is "-" (stdin) or no files, read from stdin.
        let mut stdin: Box<dyn Read> = Box::new(io::stdin().lock());

        // Buffer of size 1 byte for reading char by char to skip bytes.
        let mut empty_buffer = [0; 1];

        // Skip the specified number of bytes from stdin.
        while bytes_to_skip > 0 {
            match stdin.read(&mut empty_buffer)? {
                0 => {
                    // Spec: skipping past the end of input is a diagnostic
                    // error with a non-zero exit status.
                    return Err(io::Error::other(gettext("cannot skip past end of input")).into());
                }
                _ => bytes_to_skip -= 1,
            }
        }
        stdin // Use stdin as the reader.
    } else {
        // Otherwise, process each specified file.
        for path in &args.files {
            // Named here: `?` on a bare io::Error loses the operand, and the
            // diagnostic then cannot say which file failed.
            let mut file = File::open(path).map_err(|e| {
                io::Error::other(format!(
                    "{}: {}",
                    path.display(),
                    plib::diag::io_error_text(&e)
                ))
            })?;

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

        // The requested skip extended past the end of all input.
        if bytes_skipped < bytes_to_skip {
            return Err(io::Error::other(gettext("cannot skip past end of input")).into());
        }

        if all_files.len() > 1 {
            // Combine multiple file readers into a single reader.
            all_files
                .into_iter()
                .reduce(|acc, file| Box::new(acc.chain(file)) as Box<dyn Read>)
                .ok_or_else(|| io::Error::other("No files to chain"))?
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
    print_data(&mut reader, args, &specs, bytes_that_will_be_skipped)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    args.validate_args()?;
    let mut exit_code = 0;

    if let Err(err) = od(&args) {
        exit_code = 1;
        // `eprintln!`, and prefixed: this was `eprint!("{}", err)`, so the
        // diagnostic carried no utility name and no newline, and ran into
        // whatever printed next.
        eprintln!("od: {}", err);
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

    // POSIX 109193-5 extends a short final chunk "with null bytes", appending
    // them after the input *in memory*. Asserted on the byte array rather than
    // on the integer it decodes to, because that is the one form of the claim
    // that says nothing about byte order -- and so is checkable here, on a
    // little-endian host, for the big-endian case as well.
    #[test]
    fn extend_chunk_appends_nulls_after_the_input() {
        assert_eq!(
            extend_chunk(&[0xaa, 0xbb, 0xcc], 8),
            [0xaa, 0xbb, 0xcc, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            extend_chunk(&[0xaa, 0xbb, 0xcc, 0xdd, 0xee], 8),
            [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0, 0, 0]
        );
        assert_eq!(extend_chunk(&[0xaa], 2), [0xaa, 0, 0, 0, 0, 0, 0, 0]);

        // A full chunk is copied through untouched.
        assert_eq!(
            extend_chunk(&[1, 2, 3, 4, 5, 6, 7, 8], 8),
            [1, 2, 3, 4, 5, 6, 7, 8]
        );

        // Never read beyond the declared width, even if the caller hands over
        // a longer slice.
        assert_eq!(extend_chunk(&[1, 2, 3, 4], 2), [1, 2, 0, 0, 0, 0, 0, 0]);
    }

    // The declared width decides the sign, not the bytes present.
    #[test]
    fn a_short_chunk_is_signed_at_the_declared_width() {
        // 0xc7 alone is negative as an i8 and positive as a little-endian i32.
        assert_eq!(chunk_to_i64(&[0xc7], 1), -57);
        let want = if cfg!(target_endian = "little") {
            199
        } else {
            -956301312
        };
        assert_eq!(chunk_to_i64(&[0xc7], 4), want);
        assert_eq!(chunk_to_u64(&[0xc7], 1), 199);
    }

    #[test]
    fn test_parse_offset_invalid() {
        let result = parse_offset("7.7");
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.kind(), &IntErrorKind::InvalidDigit);
    }
}
