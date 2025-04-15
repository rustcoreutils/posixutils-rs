//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cmp::Ordering;

use std::io::{ErrorKind, Read};
use std::{
    fs::File,
    io::{self, BufRead, BufWriter, Error, Write},
    path::PathBuf,
};

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};

/// Sort, merge, or sequence check text files
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Check that the single input file is ordered as specified
    #[arg(short = 'c')]
    check_order: bool,

    /// Same as -c, except that a warning message shall not be sent to standard error if disorder or, with -u, a duplicate key is detected.
    #[arg(short = 'C')]
    check_order_without_war_mess: bool,

    /// Merge only; the input file shall be assumed to be already sorted
    #[arg(short = 'm')]
    merge_only: bool,

    /// Specify the name of an output file to be used instead of the standard output
    #[arg(short = 'o')]
    output_file: Option<PathBuf>,

    /// Unique: suppress all but one in each set of lines having equal keys
    #[arg(short = 'u')]
    unique: bool,

    /// Specify that only <blank> characters and alphanumeric characters, according to the current setting of LC_CTYPE, shall be significant in comparisons. The behavior is undefined for a sort key to which -i or -n also applies.
    #[arg(short = 'd')]
    dictionary_order: bool,

    /// Consider all lowercase characters that have uppercase equivalents to be the uppercase equivalent for the purposes of comparison
    #[arg(short = 'f')]
    fold_case: bool,

    /// Ignore all characters that are non-printable
    #[arg(short = 'i')]
    ignore_nonprintable: bool,

    /// Restrict the sort key to an initial numeric string
    #[arg(short = 'n')]
    numeric_sort: bool,

    /// Reverse the sense of comparisons
    #[arg(short = 'r')]
    reverse: bool,

    /// Ignore leading <blank> characters when determining the starting and ending positions of a restricted sort key
    #[arg(short = 'b')]
    ignore_leading_blanks: bool,

    /// Specify the field separator character
    #[arg(short = 't')]
    field_separator: Option<char>,

    /// Specify the key definition for sorting
    #[arg(short = 'k')]
    key_definition: Vec<String>,

    /// Input files
    filenames: Vec<PathBuf>,
}

impl Args {
    fn validate_args(&self) -> Result<(), String> {
        // Check if conflicting options are used together
        if self.check_order && self.merge_only {
            return Err("Options '-c' and '-m' cannot be used together".to_string());
        }

        // Check if conflicting options are used together
        if self.check_order && self.check_order_without_war_mess {
            return Err("Options '-c' and '-C' cannot be used together".to_string());
        }

        // Check if conflicting options are used together
        if self.dictionary_order && self.ignore_nonprintable {
            return Err("Options '-d' and '-i' cannot be used together".to_string());
        }

        // Check if conflicting options are used together
        if self.dictionary_order && self.numeric_sort {
            return Err("Options '-d' and '-n' cannot be used together".to_string());
        }

        // Check if conflicting options are used together
        if self.numeric_sort && self.ignore_nonprintable {
            return Err("Options '-n' and '-i' cannot be used together".to_string());
        }

        // Check if conflicting options are used together
        if self.ignore_leading_blanks && self.key_definition.is_empty() {
            return Err("Options '-b' can be used together with '-k' ".to_string());
        }
        // Check if conflicting options are used together
        if self.field_separator.is_some() && self.key_definition.is_empty() {
            return Err("Options '-t' can be used together with '-k' ".to_string());
        }

        Ok(())
    }
}

/// A struct representing a range field with various sorting and comparison options.
#[derive(Clone, Default)]
struct RangeField {
    /// The number of the field to be considered in the range.
    field_number: usize,

    /// The position of the first character in the field to start comparison.
    first_character: usize,

    /// A boolean flag to indicate if the field should be sorted numerically.
    numeric_sort: bool,

    /// A boolean flag to indicate if leading blanks should be ignored during comparison.
    ignore_leading_blanks: bool,

    /// A boolean flag to indicate if the sort order should be reversed.
    reverse: bool,

    /// A boolean flag to indicate if non-printable characters should be ignored during comparison.
    ignore_nonprintable: bool,

    /// A boolean flag to indicate if the case should be folded (case-insensitive comparison).
    fold_case: bool,

    /// A boolean flag to indicate if the field should be compared in dictionary order
    /// (considers only alphanumeric characters and blanks).
    dictionary_order: bool,
}

/// Updates two RangeField objects based on their comparison options.
///
/// This function takes two RangeField objects, compares their fields, and updates the fields
/// according to the comparison options. If any of the comparison options (`dictionary_order`,
/// `fold_case`, `ignore_nonprintable`, or `numeric_sort`) is true in either of the objects,
/// it sets the same option to true in both objects.
///
/// # Arguments
///
/// * `field1` - The first RangeField object to compare and update.
/// * `field2` - The second RangeField object to compare and update.
///
/// # Returns
///
/// A tuple containing the updated RangeField objects.
///
fn update_range_field(mut field1: RangeField, mut field2: RangeField) -> (RangeField, RangeField) {
    if field1.dictionary_order || field2.dictionary_order {
        field1.dictionary_order = true;
        field2.dictionary_order = true;
    }

    if field1.fold_case || field2.fold_case {
        field1.fold_case = true;
        field2.fold_case = true;
    }

    if field1.ignore_nonprintable || field2.ignore_nonprintable {
        field1.ignore_nonprintable = true;
        field2.ignore_nonprintable = true;
    }

    if field1.numeric_sort || field2.numeric_sort {
        field1.numeric_sort = true;
        field2.numeric_sort = true;
    }

    (field1, field2)
}

/// Compares two RangeField objects based on their field numbers and first characters.
///
/// This function takes references to two RangeField objects and compares them based on their
/// field numbers and first characters. If the field number of the first object is less than
/// the field number of the second object, it returns true. If the field numbers are equal,
/// it compares the first characters, returning true if the first character of the first object
/// is less than or equal to the first character of the second object. Otherwise, it returns false.
///
/// # Arguments
///
/// * `field1` - A reference to the first RangeField object to compare.
/// * `field2` - A reference to the second RangeField object to compare.
///
/// # Returns
///
/// A boolean value indicating whether the first RangeField object is less than or equal to
/// the second RangeField object based on their field numbers and first characters.
///
fn compare_range_fields(field1: &RangeField, field2: &RangeField) -> bool {
    match field1.field_number.cmp(&field2.field_number) {
        Ordering::Less => true,
        Ordering::Equal => field1.first_character <= field2.first_character,
        Ordering::Greater => false,
    }
}

/// Trims and concatenates substrings from a vector of strings based on a specified range.
///
/// This function takes a vector of string slices and a key range, then extracts and concatenates
/// the substrings from the specified range. The range is defined by a tuple of `RangeField`
/// structs, where the first element defines the start of the range, and the optional second element
/// defines the end of the range.
///
/// # Arguments
///
/// * `line` - A vector of string slices representing the fields to be processed.
/// * `key_range` - A tuple containing two elements:
///     * The first `RangeField` specifies the starting field and character position.
///     * An optional `RangeField` that specifies the ending field and character position.
///
/// # Returns
///
/// A `String` that contains the concatenated result of the specified range of substrings.
///
fn cut_line_by_range(line: Vec<&str>, key_range: &(RangeField, Option<RangeField>)) -> String {
    let mut result = String::new();

    let start_field = key_range.0.field_number;
    let start_char = key_range.0.first_character;

    let end_field = match &key_range.1 {
        Some(range) => range.field_number,
        None => line.len() - 1,
    };
    let end_char = key_range.1.as_ref().map(|range| range.first_character);

    for (i, field) in line.iter().skip(start_field).enumerate() {
        let i = i + start_field;
        if i >= start_field && i <= end_field {
            let start = if i == start_field {
                if key_range.0.ignore_leading_blanks {
                    start_char + (field.len() - field.trim_start().len())
                } else {
                    start_char
                }
            } else {
                0
            };
            let mut end = if i == end_field {
                if let Some(char) = end_char {
                    if char == usize::MAX - 1 {
                        field.len() - 1
                    } else if key_range.clone().1.unwrap().ignore_leading_blanks {
                        char + (field.len() - field.trim_start().len())
                    } else {
                        char
                    }
                } else {
                    field.len() - 1
                }
            } else {
                field.len() - 1
            };
            if end >= field.len() {
                end = field.len() - 1;
            }

            result.push_str(&field[start..=end]);
        }
    }

    result
}

/// Extracts a number from a string, ignoring other characters.
///
/// This function processes an input string and extracts the first sequence of characters that can
/// be part of a number (including digits, a minus sign, a decimal point, and an asterisk). If no
/// such sequence is found, it returns `None`.
///
/// # Arguments
///
/// * `input` - A string slice (`&str`) containing the input to be processed.
///
/// # Returns
///
/// An `Option<String>` containing the extracted number if found, or `None` if no numeric sequence
/// is found.
///
fn numeric_sort_filter(input: &str) -> Option<String> {
    let mut result = String::new();
    let mut found_number = false;

    for c in input.chars() {
        if c.is_ascii_digit() || c == '-' || c == '.' || c == '*' {
            found_number = true;
            result.push(c);
        } else if found_number {
            return Some(result);
        }
    }

    if found_number { Some(result) } else { None }
}

/// Compares two strings numerically, extracting numbers and performing a numeric comparison.
///
/// This function extracts the first numeric sequence from each input string using
/// `numeric_sort_filter` and then compares these sequences as floating-point numbers (`f64`).
/// If no valid numeric sequence is found, it defaults to comparing the strings lexicographically.
///
/// # Arguments
///
/// * `line1` - A string slice (`&str`) representing the first input string.
/// * `line2` - A string slice (`&str`) representing the second input string.
///
/// # Returns
///
/// An `Ordering` value (`Ordering::Less`, `Ordering::Greater`, or `Ordering::Equal`) indicating
/// the result of the comparison:
/// * `Ordering::Less` if the first number is less than the second.
/// * `Ordering::Greater` if the first number is greater than the second.
/// * `Ordering::Equal` if the numbers are equal or if both strings lack numeric sequences and are
///   equal lexicographically.
///
fn compare_numeric(line1: &str, line2: &str) -> Ordering {
    let line1 = numeric_sort_filter(line1).unwrap_or("0".to_string());
    let line2 = numeric_sort_filter(line2).unwrap_or("0".to_string());
    let a_num = line1.parse::<f64>().ok();
    let b_num = line2.parse::<f64>().ok();

    a_num.partial_cmp(&b_num).unwrap_or(Ordering::Equal)
}

/// Filters a string to include only alphanumeric characters and whitespace.
///
/// This function processes an input string and retains only the alphanumeric characters
/// (`a-z`, `A-Z`, `0-9`) and whitespace characters, effectively removing all other characters.
/// It is useful for preparing strings for dictionary-order comparisons.
///
/// # Arguments
///
/// * `line` - A string slice (`&str`) representing the input string to be filtered.
///
/// # Returns
///
/// A `String` containing only the alphanumeric and whitespace characters from the input string.
///
fn dictionary_order_filter(line: &str) -> String {
    line.chars()
        .filter(|c| c.is_alphanumeric() || c.is_whitespace())
        .collect::<String>()
}

/// Filters a string to include only printable ASCII characters.
///
/// This function processes an input string and retains only the ASCII characters that are
/// considered printable and graphic (i.e., not control characters or whitespace).
///
/// # Arguments
///
/// * `line` - A string slice (`&str`) representing the input string to be filtered.
///
/// # Returns
///
/// A `String` containing only the printable ASCII characters from the input string.
///
fn ignore_nonprintable_filter(line: &str) -> String {
    line.chars()
        .filter(|&c| c.is_ascii() && c.is_ascii_graphic())
        .collect()
}

/// Generates a `RangeField` struct based on the specified key range and arguments.
///
/// This function constructs a `RangeField` struct based on the provided key range string
/// and the arguments specified in the `Args` struct. It parses the key range string to
/// determine various sorting and comparison options.
///
/// # Arguments
///
/// * `key_range` - A string slice (`&str`) representing the key range configuration.
/// * `args` - A reference to an `Args` struct containing additional configuration options.
/// * `first` - A boolean indicating whether this is the first part of a range.
///
/// # Returns
///
/// A `Result` containing the constructed `RangeField` if successful, or a `Box` containing
/// a dynamic `Error` trait object if parsing or construction fails.
///
fn generate_range(
    key_range: &str,
    args: &Args,
    first: bool,
) -> Result<RangeField, Box<dyn std::error::Error>> {
    let mut numeric_sort = args.numeric_sort;
    let mut ignore_leading_blanks = args.ignore_leading_blanks;
    let mut reverse = args.reverse;
    let mut ignore_nonprintable = args.ignore_nonprintable;
    let mut fold_case = args.fold_case;
    let mut dictionary_order = args.dictionary_order;

    if key_range.contains('n')
        || key_range.contains('b')
        || key_range.contains('r')
        || key_range.contains('i')
        || key_range.contains('f')
        || key_range.contains('d')
    {
        numeric_sort = false;
        ignore_leading_blanks = false;
        ignore_nonprintable = false;
        fold_case = false;
        dictionary_order = false;
    }

    let mut key_range = key_range.to_string();
    if key_range.contains('n') {
        key_range = key_range.replace('n', "");
        numeric_sort = true;
    }
    if key_range.contains('b') {
        key_range = key_range.replace('b', "");
        ignore_leading_blanks = true;
    }
    if key_range.contains('r') {
        key_range = key_range.replace('r', "");
        reverse = true;
    }
    if key_range.contains('i') {
        key_range = key_range.replace('i', "");
        ignore_nonprintable = true;
    }
    if key_range.contains('f') {
        key_range = key_range.replace('f', "");
        fold_case = true;
    }
    if key_range.contains('d') {
        key_range = key_range.replace('d', "");
        dictionary_order = true;
    }
    let mut parts = key_range.split('.');
    let start_1: usize = parts
        .next()
        .unwrap()
        .parse()
        .map_err(|err| Box::new(Error::new(ErrorKind::Other, err)))?;

    let start_2 = parts.next();
    let mut start_2: usize = match first {
        true => start_2
            .unwrap_or("1")
            .parse()
            .map_err(|err| Box::new(Error::new(ErrorKind::Other, err)))?,
        false => start_2
            .unwrap_or(&usize::MAX.to_string())
            .parse()
            .map_err(|err| Box::new(Error::new(ErrorKind::Other, err)))?,
    };

    if !first && start_2 == 0 {
        start_2 = usize::MAX;
    }

    Ok(RangeField {
        field_number: start_1 - 1,
        first_character: start_2 - 1,
        numeric_sort,
        ignore_leading_blanks,
        reverse,
        ignore_nonprintable,
        fold_case,
        dictionary_order,
    })
}

/// Cuts a line into fields based on the provided RangeField object(s) and field separator.
///
/// This function takes a line of text, a RangeField object or a tuple of two RangeField objects
/// representing the key range(s), and an optional field separator character. It then splits the line
/// into fields according to the provided separator or space character, and returns a string containing
/// the selected fields based on the key range(s).
///
/// If a field separator character is provided, the line is split using that separator.
/// If no separator is provided, the line is split using space (' ') as the separator.
///
/// # Arguments
///
/// * `line` - A reference to the input line of text to be cut into fields.
/// * `key_range` - A tuple containing one or two RangeField objects representing the key range(s).
///   The first RangeField object represents the primary key range, and the second one represents
///   the secondary key range. If only one key range is provided, the second element of the tuple
///   should be None.
/// * `field_separator` - An optional character used to separate fields in the line. If None, space
///   (' ') is used as the default separator.
///
/// # Returns
///
/// A string containing the selected fields based on the key range(s).
///
fn cut_line(
    line: &str,
    key_range: &(RangeField, Option<RangeField>),
    field_separator: Option<char>,
) -> String {
    if let Some(separator) = field_separator {
        let split = line.split(separator);
        cut_line_by_range(split.collect(), key_range)
    } else {
        let split: Vec<&str> = line.split(' ').collect();

        cut_line_by_range(
            merge_empty_lines(split)
                .iter()
                .map(|s| s.as_str())
                .collect(),
            key_range,
        )
    }
}

/// Compares two strings based on a specified key range and optional field separator.
///
/// This function compares two strings (`line1` and `line2`) based on a specified key range,
/// taking into account optional field separation if provided. It applies sorting and comparison
/// rules defined in the `RangeField` struct associated with the key range.
///
/// # Arguments
///
/// * `line1` - A string slice (`&str`) representing the first input string to be compared.
/// * `line2` - A string slice (`&str`) representing the second input string to be compared.
/// * `key_range` - A tuple containing two elements:
///     * The first `RangeField` specifies the key range configuration for both strings.
///     * An optional `RangeField` specifies the end of the key range if different from the start.
/// * `field_separator` - An optional character specifying the field separator for splitting
///   strings into fields before comparison. If `None`, whitespace is used as the separator.
///
/// # Returns
///
/// An `Ordering` value (`Ordering::Less`, `Ordering::Greater`, or `Ordering::Equal`) indicating
/// the result of the comparison:
/// * `Ordering::Less` if `line1` is less than `line2` according to the specified key range.
/// * `Ordering::Greater` if `line1` is greater than `line2` according to the specified key range.
/// * `Ordering::Equal` if `line1` and `line2` are equal within the specified key range.
///
fn compare_key(
    line1: &str,
    line2: &str,
    key_range: &(RangeField, Option<RangeField>),
    field_separator: Option<char>,
) -> Ordering {
    let mut line1 = cut_line(line1, key_range, field_separator);
    let mut line2 = cut_line(line2, key_range, field_separator);

    // Compare keys
    if key_range.0.numeric_sort {
        // If the keys are represented by numbers, compare them as numbers
        let mut result = compare_numeric(&line1, &line2);
        if key_range.0.reverse {
            match result {
                Ordering::Less => result = Ordering::Greater,
                Ordering::Greater => result = Ordering::Less,
                _ => (),
            }
        }
        return result;
    } else if key_range.0.dictionary_order {
        line1 = dictionary_order_filter(&line1);
        line2 = dictionary_order_filter(&line2);
    } else if key_range.0.ignore_nonprintable {
        line1 = ignore_nonprintable_filter(&line1);
        line2 = ignore_nonprintable_filter(&line2);
    }

    let result;

    if key_range.0.fold_case {
        let cmp = line1.to_uppercase().cmp(&line2.to_uppercase());
        if cmp == std::cmp::Ordering::Equal {
            result = line1.cmp(&line2);
        } else {
            result = cmp;
        }
    } else {
        result = line1.cmp(&line2);
    }
    if key_range.0.reverse {
        match result {
            Ordering::Less => Ordering::Greater,
            Ordering::Greater => Ordering::Less,
            _ => Ordering::Equal,
        }
    } else {
        result
    }
}

/// Compares two lines of text based on specified sorting and comparison options.
///
/// This function compares two lines of text (`line1` and `line2`) based on specified
/// sorting and comparison options. It supports options for numeric sorting, dictionary
/// ordering, case folding, and ignoring non-printable characters.
///
/// # Arguments
///
/// * `line1` - A string slice (`&str`) representing the first line of text to be compared.
/// * `line2` - A string slice (`&str`) representing the second line of text to be compared.
/// * `dictionary_order` - A boolean indicating whether to use dictionary ordering.
/// * `fold_case` - A boolean indicating whether to fold case during comparison.
/// * `ignore_nonprintable` - A boolean indicating whether to ignore non-printable characters.
/// * `numeric_sort` - A boolean indicating whether to perform numeric sorting.
///
/// # Returns
///
/// An `Ordering` value (`Ordering::Less`, `Ordering::Greater`, or `Ordering::Equal`)
/// indicating the result of the comparison:
/// * `Ordering::Less` if `line1` is less than `line2` according to the specified options.
/// * `Ordering::Greater` if `line1` is greater than `line2` according to the specified options.
/// * `Ordering::Equal` if `line1` and `line2` are equal within the specified options.
///
fn compare_lines(
    line1: &str,
    line2: &str,
    dictionary_order: bool,
    fold_case: bool,
    ignore_nonprintable: bool,
    numeric_sort: bool,
) -> Ordering {
    let mut line1 = line1.to_string();
    let mut line2 = line2.to_string();

    if numeric_sort {
        return compare_numeric(&line1, &line2);
    } else if dictionary_order {
        line1 = dictionary_order_filter(&line1);
        line2 = dictionary_order_filter(&line2);
    } else if ignore_nonprintable {
        line1 = ignore_nonprintable_filter(&line1);
        line2 = ignore_nonprintable_filter(&line2);
    }

    if fold_case {
        let cmp = line1.to_uppercase().cmp(&line2.to_uppercase());
        if cmp == std::cmp::Ordering::Equal {
            line1.cmp(&line2)
        } else {
            cmp
        }
    } else {
        line1.cmp(&line2)
    }
}

/// Finds the first differing line between two slices of strings.
///
/// This function iterates over two slices of strings (`lines_1` and `lines_2`)
/// and finds the index and content of the first line where they differ.
/// If the slices have different lengths, it returns the index and content
/// of the first line from `lines_1` that doesn't have a corresponding line
/// in `lines_2`.
///
/// # Arguments
///
/// * `lines_1` - A slice of strings (`&[String]`) representing the first set of lines.
/// * `lines_2` - A slice of strings (`&[String]`) representing the second set of lines.
///
/// # Returns
///
/// An `Option` containing a tuple:
/// - If differing lines are found, it returns the index of the differing line and its content.
/// - If the slices have different lengths, it returns the index and content of the first
///   additional line from `lines_1` compared to `lines_2`.
/// - If the slices are identical, it returns `None`.
///
fn find_first_difference(lines_1: &[String], lines_2: &[String]) -> Option<(usize, String)> {
    let min_length = std::cmp::min(lines_1.len(), lines_2.len());

    for i in 0..min_length {
        if lines_1[i] != lines_2[i] {
            return Some((i, lines_1[i].clone()));
        }
    }

    if lines_1.len() != lines_2.len() {
        return Some((min_length, lines_1.get(min_length).unwrap().to_string()));
    }

    None
}

/// Creates range fields based on the specified key range and arguments.
///
/// This function takes a key range string (`key_range`) and a reference to an `Args` struct (`args`)
/// containing additional configuration options. It splits the key range string into individual
/// range components separated by commas and generates corresponding `RangeField` structs for each
/// component using the `generate_range` function.
///
/// # Arguments
///
/// * `key_range` - A string slice (`&str`) representing the key range configuration.
/// * `args` - A reference to an `Args` struct containing additional configuration options.
///
/// # Returns
///
/// A `Result` containing a tuple `(RangeField, Option<RangeField>)` if successful, or a `Box`
/// containing a dynamic `Error` trait object if parsing or construction fails.
///
fn create_ranges(
    key_range: &str,
    args: &Args,
) -> Result<(RangeField, Option<RangeField>), Box<dyn std::error::Error>> {
    // Split the key range with commas
    let key_ranges: Vec<&str> = key_range.split(',').collect();
    let mut key_ranges = key_ranges.iter();

    // Convert key ranges to numeric representations
    let mut ranges: (RangeField, Option<RangeField>) = (RangeField::default(), None);

    ranges.0 = {
        let key_range = key_ranges.next().unwrap().to_string();
        if key_range == "0" {
            return Err(Box::new(Error::new(
                ErrorKind::Other,
                "the key can't be zero.",
            )));
        }
        generate_range(&key_range, args, true)?
    };
    ranges.1 = {
        if let Some(key_range) = key_ranges.next() {
            Some(generate_range(key_range, args, false)?)
        } else {
            None
        }
    };
    if let Some(range_2) = ranges.1 {
        let (range_1, range_2) = update_range_field(ranges.0, range_2);
        if !compare_range_fields(&range_1, &range_2) {
            return Err(Box::new(Error::new(
                ErrorKind::Other,
                "keys fields with end position before start!",
            )));
        }

        ranges = (range_1, Some(range_2));
    }

    Ok(ranges)
}

/// Sorts strings based on specified sorting criteria and writes the result to the output.
///
/// This function reads lines from the provided input reader and sorts them based on the
/// specified sorting criteria in the `Args` struct. It supports sorting by key ranges,
/// dictionary ordering, case folding, numeric sorting, and other options. The sorted
/// lines are then written to the output, which can be a file or the standard output.
///
/// # Arguments
///
/// * `args` - A reference to an `Args` struct containing sorting and configuration options.
/// * `reader` - A boxed reader implementing the `Read` trait, representing the input source.
///
/// # Returns
///
/// A `Result` indicating success or failure:
/// * `Ok(())` if the sorting and writing process completes successfully.
/// * `Err(Box<dyn Error>)` if an error occurs during sorting, reading, or writing.
///
fn sort_lines(args: &Args, lines: Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
    let mut result_lines = lines.clone();
    let mut duplicates = vec![];

    if !args.key_definition.is_empty() {
        let key_range = &args.key_definition[0];

        if key_range.is_empty() {
            return Err(Box::new(Error::new(
                ErrorKind::Other,
                "key must be non-empty",
            )));
        }

        let ranges = create_ranges(key_range, args)?;
        let ranges_2 = match args.key_definition.get(1) {
            Some(key_range_2) => Some(create_ranges(key_range_2, args)?),
            None => None,
        };

        // Sort strings by keys
        result_lines.sort_by(|a, b| {
            let mut ordering = compare_key(a, b, &ranges, args.field_separator);
            if let Ordering::Equal = ordering {
                if let Some(ranges_2) = &ranges_2 {
                    let ordering_2 = compare_key(a, b, ranges_2, args.field_separator);
                    if let Ordering::Equal = ordering_2 {
                        duplicates.push(a.to_string());
                    }
                    ordering = ordering_2
                }
            }
            ordering
        });
        if args.unique {
            result_lines.retain(|line| !duplicates.contains(line));
        }
    } else {
        result_lines.sort_by(|a, b| {
            let ord = compare_lines(
                a,
                b,
                args.dictionary_order,
                args.fold_case,
                args.ignore_nonprintable,
                args.numeric_sort,
            );
            if let Ordering::Equal = ord {
                duplicates.push(a.to_string());
            }
            ord
        });

        if args.unique {
            result_lines.retain(|line| !duplicates.contains(line));
        }
    }

    if args.reverse {
        result_lines.reverse();
    }

    if args.check_order_without_war_mess {
        if find_first_difference(&lines, &result_lines).is_some() {
            return Err(Box::new(Error::new(
                ErrorKind::Other,
                "The order of the lines is not correct",
            )));
        } else {
            return Ok(());
        }
    } else if args.check_order {
        if args.unique && !duplicates.is_empty() {
            let message = format!("Duplicate key was found! `{}`", duplicates.first().unwrap());
            return Err(Box::new(Error::new(ErrorKind::Other, message)));
        }
        if let Some((index, line)) = find_first_difference(&lines, &result_lines) {
            let message = format!(
                "The order of the lines is not correct on line {}:`{}`",
                index + 1,
                line
            );
            return Err(Box::new(Error::new(ErrorKind::Other, message)));
        }
        return Ok(());
    } else if let Some(file_path) = &args.output_file {
        // Open the file for writing
        let file_out = File::create(file_path)?;
        let mut writer = BufWriter::new(file_out);

        // Write the sorted strings to a file
        for line in result_lines {
            writeln!(writer, "{}", line)?;
        }
    } else {
        let result = result_lines.join("\n");
        println!("{result}");
    }

    Ok(())
}

/// Merges contents from multiple sorted files into a single output.
///
/// This function takes a vector of mutable references to readers (`paths`) representing
/// sorted input files and an optional output file path (`output_path`). It reads from each
/// input file sequentially and writes the contents to the output file or the standard output.
///
/// # Arguments
///
/// * `paths` - A mutable reference to a vector of readers (`Vec<Box<dyn Read>>`) representing
///             sorted input files.
/// * `output_path` - An optional string (`Option<String>`) representing the output file path.
///                   If `Some`, the merged contents are written to the specified file; if `None`,
///                   the contents are written to the standard output.
///
/// # Returns
///
/// An `io::Result` indicating success or failure:
/// * `Ok(())` if the merging process completes successfully.
/// * `Err(io::Error)` if an error occurs during file I/O or copying.
///
fn merge_files(paths: &mut Vec<Box<dyn Read>>, output_path: &Option<PathBuf>) -> io::Result<()> {
    let mut output_file: Box<dyn Write> = match output_path {
        Some(path) => Box::new(File::create(path)?),
        None => Box::new(io::stdout()),
    };

    for path in paths {
        let mut input_file = path;

        // Copy the contents of the input file to the output file or stdout
        io::copy(&mut input_file, &mut output_file)?;
    }

    Ok(())
}

/// Merges consecutive empty strings in the input vector with the nearest non-empty string.
/// Spaces are added to empty strings to align them with the nearest non-empty string.
///
/// # Arguments
///
/// * `vec` - A vector of string references (`&str`).
///
/// # Returns
///
/// A vector of strings (`Vec<String>`) where consecutive empty strings are merged with the nearest non-empty string.
///
/// # Examples
///
/// ```
/// let result = merge_empty_lines(vec!["line1", "line2", "", "", "", "lineN"]);
/// assert_eq!(result, vec!["line1", "line2", "   lineN"]);
/// ```
///
fn merge_empty_lines(vec: Vec<&str>) -> Vec<String> {
    let mut empty_count = 0;
    let mut result = vec![];

    for i in vec {
        if i.is_empty() {
            empty_count += 1;
        } else if empty_count > 0 {
            let spaces = " ".repeat(empty_count);
            result.push(format!("{}{}", spaces, i));
            empty_count = 0;
        } else {
            result.push(i.to_string());
        }
    }

    result
}

/// Sorts the contents of input files or standard input based on specified criteria.
///
/// This function takes an `Args` struct containing sorting options and configuration and sorts
/// the contents of input files or standard input accordingly. It supports sorting by key ranges,
/// dictionary ordering, case folding, numeric sorting, and other options. The sorted contents
/// can be written to the output or merged directly if specified.
///
/// # Arguments
///
/// * `args` - A reference to an `Args` struct containing sorting and configuration options.
///
/// # Returns
///
/// A `Result` indicating success or failure:
/// * `Ok(())` if the sorting process completes successfully.
/// * `Err(Box<dyn Error>)` if an error occurs during sorting or merging.
///
fn sort(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let mut readers: Vec<Box<dyn Read>> = if (args.filenames.len() == 1
        && args.filenames[0] == PathBuf::from("-"))
        || args.filenames.is_empty()
    {
        vec![Box::new(io::stdin().lock())]
    } else {
        let mut bufs: Vec<Box<dyn Read>> = vec![];
        for file in &args.filenames {
            bufs.push(Box::new(std::fs::File::open(file)?))
        }
        bufs
    };

    if args.merge_only {
        merge_files(&mut readers, &args.output_file)?;
        return Ok(());
    }
    let mut all_lines: Vec<String> = Vec::new();
    for reader in readers {
        let reader = io::BufReader::new(reader);
        let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();
        all_lines.extend(lines);
    }
    sort_lines(args, all_lines)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    args.validate_args()?;

    let mut exit_code = 0;

    if let Err(err) = sort(&args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_empty_lines() {
        let result = merge_empty_lines(vec!["line1", "line2", "", "", "", "lineN"]);
        assert_eq!(result, vec!["line1", "line2", "   lineN"]);
    }
}
