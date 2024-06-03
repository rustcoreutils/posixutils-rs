use clap::Parser;
use deunicode::deunicode_char;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::io::{self, Read};

/// tr - translate or delete characters
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Delete characters in STRING1 from the input
    #[arg(short = 'd')]
    delete: bool,

    /// Replace each input sequence of a repeated character that is listed in the last specified SET, with a single occurrence of that character
    #[arg(short = 's')]
    squeeze_repeats: bool,

    /// Use the complement of STRING1's values
    #[arg(short = 'c')]
    complement_val: bool,

    /// Use the complement of STRING1's characters
    #[arg(short = 'C')]
    complement_char: bool,

    /// First string
    string1: String,

    /// Second string (not required if delete mode is on)
    string2: Option<String>,
}

impl Args {
    fn validate_args(&self) -> Result<(), String> {
        // Check if conflicting options are used together
        if self.complement_char && self.complement_val {
            return Err("Options '-c' and '-C' cannot be used together".to_string());
        }
        if self.squeeze_repeats
            && self.string2.is_none()
            && (self.complement_char || self.complement_val)
            && !self.delete
        {
            return Err("Option '-c' or '-C' may only be used with 2 strings".to_string());
        }

        if !self.squeeze_repeats && !self.delete && self.string2.is_none() {
            return Err("Need two strings operand".to_string());
        }

        if self.string1.is_empty() {
            return Err("At least 1 string operand is required".to_string());
        }

        Ok(())
    }
}

// The Char struct represents a character along with its repetition count.
#[derive(Debug, Clone)]
struct Char {
    // The character.
    char: char,
    // The number of times the character is repeated
    repeated: usize,
}

// The Equiv struct represents a character equivalent.
#[derive(Debug, Clone)]
struct Equiv {
    // The character equivalent.
    char: char,
}

// The Operand enum can be either a Char or an Equiv
#[derive(Debug, Clone)]
enum Operand {
    Char(Char),
    Equiv(Equiv),
}

impl Operand {
    /// Checks if a target character exists in a vector of `Operand` elements.
    ///
    /// # Arguments
    ///
    /// * `operands` - A reference to a vector of `Operand` elements.
    /// * `target` - A reference to the target character to search for.
    ///
    /// # Returns
    ///
    /// `true` if the target character is found, `false` otherwise.
    fn contains(operands: &Vec<Operand>, target: &char) -> bool {
        for operand in operands {
            match operand {
                Operand::Equiv(e) => {
                    if compare_deunicoded_chars(e.char, *target) {
                        return true;
                    }
                }
                Operand::Char(c) => {
                    if c.char == *target {
                        return true;
                    }
                }
            }
        }
        false
    }
}

/// Filters out `Char` elements from a vector of `Operand` elements.
///
/// # Arguments
///
/// * `operands` - A vector of `Operand` elements.
///
/// # Returns
///
/// A vector of `Char` elements.
fn filter_chars(operands: Vec<Operand>) -> Vec<Char> {
    operands
        .into_iter()
        .filter_map(|operand| {
            if let Operand::Char(c) = operand {
                Some(c)
            } else {
                None
            }
        })
        .collect()
}

/// Creates a minimal string with a given size using a vector of `Char` elements.
///
/// # Arguments
///
/// * `chars` - A vector of `Char` elements.
/// * `size` - The desired size of the resulting string.
///
/// # Returns
///
/// A vector of characters representing the minimal string.
fn create_minimal_string(chars: Vec<Char>, size: usize) -> Vec<char> {
    let mut result = vec![];
    let mut remaining_space = size;
    let mut overflow_chars: Vec<(usize, Char)> = vec![];

    // Add chars with repeated == 1 to the result
    for ch in &chars {
        if ch.repeated == 1 {
            if remaining_space > 0 {
                result.push(ch.char);
                remaining_space -= 1;
            }
        } else if remaining_space >= ch.repeated {
            for _ in 0..ch.repeated {
                result.push(ch.char);
            }

            remaining_space -= ch.repeated;
        } else {
            overflow_chars.push((result.len(), ch.clone()));
        }
    }

    // Add remaining chars from overflow_chars if there's still space
    if !overflow_chars.is_empty() {
        for (insert_position, char) in overflow_chars.iter().rev() {
            if remaining_space > 0 {
                let chars_to_add = remaining_space.min(char.repeated);
                let replace_with = vec![char.char; chars_to_add];
                result.splice(insert_position..insert_position, replace_with);

                remaining_space -= chars_to_add;
            }
        }
    }

    if result.len() < size {
        let last = *result.last().unwrap();
        for _ in 0..size - result.len() {
            result.push(last);
        }
    }

    result
}

/// Parses a sequence in the format `[=equiv=]` from the given character iterator.
///
/// The function expects the iterator to be positioned just before the first `=`
/// character. It reads the equivalent characters between the `=` symbols and
/// creates a list of `Operand::Equiv` entries, one for each character.
///
/// # Arguments
///
/// * `chars` - A mutable reference to a peekable character iterator.
///
/// # Returns
///
/// A `Result` containing a vector of `Operand::Equiv` entries if successful, or a
/// `String` describing the error if parsing fails.
///
/// # Errors
///
/// This function will return an error if:
/// - The sequence does not contain a closing `=` before `]`.
/// - The sequence does not contain a closing `]`.
/// - The sequence contains no characters between the `=` symbols.
///
fn parse_equiv(chars: &mut std::iter::Peekable<std::str::Chars>) -> Result<Vec<Operand>, String> {
    chars.next(); // Skip '='
    let mut equiv = String::new();

    while let Some(&next_ch) = chars.peek() {
        if next_ch == '=' {
            break;
        }
        equiv.push(next_ch);
        chars.next();
    }

    if equiv.is_empty() {
        return Err("Error: Missing equiv symbol after '[='".to_string());
    }

    // Skip '='
    let Some('=') = chars.next() else {
        return Err("Error: Missing '=' before ']' for '[=equiv=]'".to_string());
    };

    // Skip ']'
    let Some(']') = chars.next() else {
        return Err("Error: Missing closing ']' for '[=equiv=]'".to_string());
    };

    let mut operands = Vec::new();
    for equiv_char in equiv.chars() {
        operands.push(Operand::Equiv(Equiv { char: equiv_char }));
    }

    Ok(operands)
}

/// Parses a sequence in the format `[x*n]` from the given character iterator.
///
/// The function expects the iterator to be positioned just before the symbol `x`.
/// It reads the symbol, the repetition count `n`, and creates an `Operand::Char`
/// entry with the specified number of repetitions.
///
/// # Arguments
///
/// * `chars` - A mutable reference to a peekable character iterator.
/// * `symbol` - The symbol that is being repeated.
///
/// # Returns
///
/// A `Result` containing an `Operand::Char` entry if successful, or a `String`
/// describing the error if parsing fails.
///
/// # Errors
///
/// This function will return an error if:
/// - The sequence does not contain a `*` after the symbol.
/// - The sequence does not contain a closing `]`.
/// - The repetition count `n` is not a valid positive integer.
///
fn parse_repeated_char(
    chars: &mut std::iter::Peekable<std::str::Chars>,
    symbol: char,
) -> Result<Operand, String> {
    // Skip '*'
    let Some('*') = chars.next() else {
        return Err(format!(
            "Error: Missing '*' after '[' for symbol '{}'",
            symbol
        ));
    };

    let mut repeat_str = String::new();
    while let Some(&digit) = chars.peek() {
        if !digit.is_ascii_digit() {
            break;
        }
        repeat_str.push(digit);
        chars.next();
    }

    // Skip ']'
    let Some(']') = chars.next() else {
        return Err("Error: Missing closing ']'".to_string());
    };

    let repeated = match repeat_str.parse::<usize>() {
        Ok(n) if n > 0 => n,
        _ => usize::MAX,
    };

    Ok(Operand::Char(Char {
        char: symbol,
        repeated,
    }))
}

/// Parses an input string and converts it into a vector of `Operand` entries.
///
/// This function processes the input string, looking for sequences in the formats
/// `[=equiv=]` and `[x*n]`, as well as regular characters. It delegates the parsing
/// of the specific formats to helper functions `parse_equiv` and `parse_repeated_char`.
///
/// # Arguments
///
/// * `input` - A string slice containing the input to be parsed.
///
/// # Returns
///
/// A `Result` containing a vector of `Operand` entries if successful, or a `String`
/// describing the error if parsing fails.
///
/// # Errors
///
/// This function will return an error if:
/// - It encounters an invalid format.
/// - It encounters any specific error from `parse_equiv` or `parse_repeated_char`.
///
fn parse_symbols(input: &str) -> Result<Vec<Operand>, String> {
    let mut operands: Vec<Operand> = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        if ch == '[' {
            chars.next(); // Skip '['
            let Some(&'=') = chars.peek() else {
                let symbol = chars
                    .next()
                    .ok_or("Error: Missing symbol after '['".to_string())?;
                operands.push(parse_repeated_char(&mut chars, symbol)?);
                continue;
            };

            operands.extend(parse_equiv(&mut chars)?);
        } else {
            // Add a regular character with a repetition of 1
            operands.push(Operand::Char(Char {
                char: ch,
                repeated: 1,
            }));
            chars.next();
        }
    }

    Ok(operands)
}

/// Represents the case sensitivity of character classes.
#[derive(Debug, PartialEq)]
enum CaseSensitive {
    UpperCase,
    LowerCase,
    None,
}

/// Compares two characters after normalizing them.
/// This function uses the hypothetical `deunicode_char` function to normalize
/// the input characters and then compares them for equality.
/// # Arguments
///
/// * `char1` - The first character to compare.
/// * `char2` - The second character to compare.
///
/// # Returns
///
/// * `true` if the normalized characters are equal.
/// * `false` otherwise.
fn compare_deunicoded_chars(char1: char, char2: char) -> bool {
    let normalized_char1 = deunicode_char(char1);
    let normalized_char2 = deunicode_char(char2);
    normalized_char1 == normalized_char2
}

/// Expands a character class name into a vector of `Operand` elements and determines the case sensitivity.
/// This function expands the given character class name into its corresponding characters
/// and wraps them in `Operand::Char`. It also determines the case sensitivity of the class
/// based on the class name. If the class name is invalid, it returns an error message.
/// # Arguments
///
/// * `class` - A string slice representing the character class name.
///
/// # Returns
///
/// * `Result<(Vec<Operand>, CaseSensitive), String>` - On success, returns a tuple containing a vector of `Operand` elements and a `CaseSensitive` value.
///   On failure, returns an error message.
fn expand_character_class(class: &str) -> Result<(Vec<Operand>, CaseSensitive), String> {
    let mut case_sensitive = CaseSensitive::None;
    let result = match class {
        "alnum" => ('0'..='9').chain('A'..='Z').chain('a'..='z').collect(),
        "alpha" => ('A'..='Z').chain('a'..='z').collect(),
        "digit" => ('0'..='9').collect(),
        "lower" => {
            case_sensitive = CaseSensitive::LowerCase;
            ('a'..='z').collect()
        }
        "upper" => {
            case_sensitive = CaseSensitive::UpperCase;
            ('A'..='Z').collect()
        }
        "space" => vec![' ', '\t', '\n', '\r', '\x0b', '\x0c'],
        "blank" => vec![' ', '\t'],
        "cntrl" => (0..=31)
            .chain(std::iter::once(127))
            .map(|c| c as u8 as char)
            .collect(),
        "graph" => (33..=126).map(|c| c as u8 as char).collect(),
        "print" => (32..=126).map(|c| c as u8 as char).collect(),
        "punct" => (33..=47)
            .chain(58..=64)
            .chain(91..=96)
            .chain(123..=126)
            .map(|c| c as u8 as char)
            .collect(),
        "xdigit" => ('0'..='9').chain('A'..='F').chain('a'..='f').collect(),
        _ => return Err("Error: Invalid class name ".to_string()),
    };
    Ok((
        result
            .into_iter()
            .map(|c| {
                Operand::Char(Char {
                    char: c,
                    repeated: 1,
                })
            })
            .collect(),
        case_sensitive,
    ))
}

/// Parses character classes from a string input and expands them into a vector of `Operand` elements.
///
/// This function parses the input string for character classes in the `[:class:]` format and expands them
/// using the `expand_character_class` function. It collects the resulting `Operand` elements and determines
/// the case sensitivity of the character classes encountered.
///
/// If an invalid format is detected, the function returns an error message.
/// # Arguments
///
/// * `input` - A string slice containing the input to parse.
///
/// # Returns
///
/// * `Result<(Vec<Operand>, CaseSensitive), String>` - On success, returns a tuple containing a vector of `Operand` elements and a `CaseSensitive` value.
///   On failure, returns an error message.
fn parse_classes(input: &str) -> Result<(Vec<Operand>, CaseSensitive), String> {
    let mut classes: Vec<Operand> = Vec::new();
    let mut chars = input.chars().peekable();
    let mut case_sensitive = CaseSensitive::None;
    while let Some(ch) = chars.next() {
        if ch == '[' {
            let Some(':') = chars.next() else {
                continue;
            };
            // Processing the [:class:] format

            let mut class = String::new();
            while let Some(&next_ch) = chars.peek() {
                if next_ch == ':' {
                    break;
                }
                class.push(next_ch);
                chars.next();
            }
            if class.is_empty() {
                return Err("Error: Missing class name after '[:'".to_string());
            }
            // Skip ':'
            let Some(':') = chars.next() else {
                return Err("Error: Missing ':' before ']' for '[:class:]'".to_string());
            };
            // Skip ']'
            let Some(']') = chars.next() else {
                return Err("Error: Missing closing ']' for '[:class:]'".to_string());
            };

            let res = expand_character_class(&class)?;
            case_sensitive = res.1;
            classes.extend(res.0);
        }
    }

    Ok((classes, case_sensitive))
}

/// Parses an octal string and returns the corresponding character, if valid.
///
/// # Arguments
///
/// * `s` - A string slice that holds the octal representation of the character.
///
/// # Returns
///
/// * `Option<char>` - Returns `Some(char)` if the input string is a valid octal
///   representation of a Unicode character. Returns `None` if the string is
///   not a valid octal number or if the resulting number does not correspond
///   to a valid Unicode character.
///
fn parse_octal(s: &str) -> Option<char> {
    u32::from_str_radix(s, 8).ok().and_then(char::from_u32)
}

/// Parses a string representing a range of characters or octal values and returns a vector of `Operand`s.
///
/// This function handles ranges specified in square brackets, such as `[a-z]` or `[\\141-\\172]`.
/// It supports ranges of plain characters and ranges of octal-encoded characters. The function
/// trims the square brackets from the input string, splits the range into start and end parts,
/// and then expands the range into a list of `Operand`s.
///
/// # Arguments
///
/// * `input` - A string slice containing the range to be parsed. The range can be in the form of
///   `[a-z]`, `[\\141-\\172]`, etc.
///
/// # Returns
///
/// * `Result<Vec<Operand>, String>` - Returns `Ok(Vec<Operand>)` if the input string represents
///   a valid range. Returns `Err(String)` with an error message if the input is invalid.
///
/// # Errors
///
/// This function returns an error if:
/// - The input string does not contain a valid range.
/// - The octal values in the range cannot be parsed into valid characters.
///
fn parse_ranges(input: &str) -> Result<Vec<Operand>, String> {
    let mut chars = Vec::new();
    let s = input.trim_matches(|c| c == '[' || c == ']'); // Remove square brackets
    let parts: Vec<&str> = s.split('-').collect();

    let start = parts[0];
    let end = parts[1];

    if start.starts_with('\\') && end.starts_with('\\') {
        // Processing the \octal-\octal range
        if let (Some(start_char), Some(end_char)) =
            (parse_octal(&start[1..]), parse_octal(&end[1..]))
        {
            let start_u32 = start_char as u32;
            let end_u32 = end_char as u32;

            for code in start_u32..=end_u32 {
                if let Some(c) = char::from_u32(code) {
                    chars.push(c);
                }
            }
        }
    } else if !start.starts_with('\\') && !end.starts_with('\\') {
        // Processing the c-c range
        let start_char = start.chars().next().unwrap();
        let end_char = end.chars().next().unwrap();

        for c in start_char..=end_char {
            chars.push(c);
        }
    }

    Ok(chars
        .into_iter()
        .map(|c| {
            Operand::Char(Char {
                char: c,
                repeated: 1,
            })
        })
        .collect())
}

/// Parses a set expression and returns a vector of `Operand`s and a `CaseSensitive` flag.
///
/// The function determines the type of set based on its format and delegates
/// the parsing to the appropriate helper function. The supported formats are:
/// - Character classes in the form of `[:class:]`
/// - Single ranges such as `[a-z]` or `[\\141-\\172]`
/// - Symbols and characters not fitting the above categories
///
/// # Arguments
///
/// * `set` - A string slice representing the set expression to be parsed.
///
/// # Returns
///
/// * `Result<(Vec<Operand>, CaseSensitive), String>` - Returns `Ok((Vec<Operand>, CaseSensitive))`
///   on successful parsing. Returns `Err(String)` with an error message if the input is invalid.
///
fn parse_set(set: &str) -> Result<(Vec<Operand>, CaseSensitive), String> {
    if set.starts_with("[:") && set.ends_with(":]") {
        Ok(parse_classes(set)?)
    } else if contains_single_range(set) {
        Ok((parse_ranges(set)?, CaseSensitive::None))
    } else {
        Ok((parse_symbols(set)?, CaseSensitive::None))
    }
}

/// Determines if a string contains a single valid range expression.
///
/// This function uses a regular expression to check if the input string matches any
/// of the following range formats:
/// - `[a-z]` or `[A-Z]` or `[0-9]`: Character ranges enclosed in square brackets
/// - `\\octal-\\octal`: Ranges of octal-encoded characters
/// - `a-z` or `A-Z` or `0-9`: Simple character-symbol ranges
///
/// # Arguments
///
/// * `s` - A string slice to be checked for containing a single valid range.
///
/// # Returns
///
/// * `bool` - Returns `true` if the input string matches any of the valid range formats.
///   Returns `false` otherwise.
///
fn contains_single_range(s: &str) -> bool {
    // Regular expression for a range of characters or \octal
    let re = Regex::new(
        r"(?x)
        ^ \[ [a-zA-Z0-9\\]+ - [a-zA-Z0-9\\]+ \] $ |   # Range in square brackets
        ^ \\ [0-7]{1,3} - \\ [0-7]{1,3} $ |           # Range \octal-\octal
        ^ [a-zA-Z0-9] - [a-zA-Z0-9] $                 # Character-symbol range
    ",
    )
    .unwrap();

    re.is_match(s)
}

/// Computes the complement of a string with respect to two sets of characters.
///
/// This function takes an input string and two sets of characters (`chars1` and `chars2`)
/// and computes the complement of the input string with respect to the characters in `chars1`.
/// For each character in the input string:
/// - If the character is present in `chars1`, it remains unchanged in the result.
/// - If the character is not present in `chars1`, it is replaced with characters from `chars2`
///   sequentially until all characters in `chars2` are exhausted, and then the process repeats.
///
/// # Arguments
///
/// * `input` - A string slice representing the input string.
/// * `chars1` - A vector of `Operand` representing the first set of characters.
/// * `chars2` - A vector of `Operand` representing the second set of characters.
///
/// # Returns
///
/// * `String` - Returns a string representing the complement of the input string.
///
fn complement_chars(input: &str, chars1: Vec<Operand>, mut chars2: Vec<Operand>) -> String {
    // Create a variable to store the result
    let mut result = String::new();
    let chars_2_const = chars2.clone();

    // Initialize the index for the chars2 vector
    let mut chars2_index = 0;

    // Convert the input string to a character vector for easy processing
    let input_chars: Vec<char> = input.chars().collect();

    // Go through each character in the input string
    for &ch in &input_chars {
        // Check if the character is in the chars1 vector
        if Operand::contains(&chars1, &ch) {
            // If the character is in the chars1 vector, add it to the result without changing it
            result.push(ch);
            continue;
        }
        // If the character is not in the chars1 vector, replace it with a character from the chars2 vector
        // Add the character from the chars2 vector to the result
        let operand = &mut chars2[chars2_index];
        match operand {
            Operand::Char(char) => {
                result.push(char.char);
                char.repeated -= 1;

                if char.repeated > 0 {
                    continue;
                }
            }
            Operand::Equiv(equiv) => {
                result.push(equiv.char);
            }
        }

        // Increase the index for the chars2 vector
        chars2_index += 1;

        // If the index has reached the end of the chars2 vector, reset it to zero
        if chars2_index >= chars2.len() {
            chars2_index = 0;
            chars2.clone_from(&chars_2_const);
        }
    }

    result
}

/// Checks if a character is repeatable based on certain conditions.
///
/// This function determines if a character `c` is repeatable based on the following conditions:
/// - The character occurs more than once in the input string.
/// - The character is present in the set `set2`.
///
/// If the conditions are met and the character has not been seen before, it is considered repeatable.
///
/// # Arguments
///
/// * `c` - A character to be checked for repeatability.
/// * `char_counts` - A reference to a hashmap containing character counts in the input string.
/// * `seen` - A mutable reference to a hash set to keep track of characters seen so far.
/// * `set2` - A reference to a vector of `Operand` representing the second set of characters.
///
/// # Returns
///
/// * `bool` - Returns `true` if the character is repeatable based on the conditions specified above.
///            Returns `false` otherwise.
///
fn check_repeatable(
    c: char,
    char_counts: &HashMap<char, usize>,
    seen: &mut HashSet<char>,
    set2: &Vec<Operand>,
) -> bool {
    if char_counts[&c] > 1 && Operand::contains(set2, &c) {
        if seen.contains(&c) {
            false
        } else {
            seen.insert(c);
            true
        }
    } else {
        true
    }
}

/// Translates or deletes characters from standard input, according to specified arguments.
///
/// This function reads from standard input, processes the input string based on the specified arguments,
/// and prints the result to standard output. It supports translation of characters, deletion of characters,
/// and squeezing repeated characters.
///
/// # Arguments
///
/// * `args` - A reference to an `Args` struct containing the command-line arguments.
///
/// # Returns
///
/// * `Result<(), Box<dyn std::error::Error>>` - Returns `Ok(())` on success. Returns an error wrapped in `Box<dyn std::error::Error>`
///   if there is an error reading from standard input or processing the input string.
///
fn tr(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read input");

    let (set1, set_1_collection) = parse_set(&args.string1)?;
    let (mut set2, mut set_2_collection) = (None, CaseSensitive::None);
    if let Some(string2) = &args.string2 {
        let result = parse_set(string2)?;
        set2 = Some(result.0);
        set_2_collection = result.1;
    }

    if args.delete {
        let mut filtered_string: String;

        if args.complement_char || args.complement_val {
            filtered_string = input
                .chars()
                .filter(|c| Operand::contains(&set1, c))
                .collect();
        } else {
            filtered_string = input
                .chars()
                .filter(|c| !Operand::contains(&set1, c))
                .collect();
        }

        if args.squeeze_repeats && set2.is_some() {
            // Counting the frequency of characters in the chars vector
            let mut char_counts = HashMap::new();
            for c in filtered_string.chars() {
                *char_counts.entry(c).or_insert(0) += 1;
            }

            let mut seen = HashSet::new();
            filtered_string = filtered_string
                .chars()
                .filter(|&c| check_repeatable(c, &char_counts, &mut seen, set2.as_ref().unwrap()))
                .collect();
        }

        print!("{filtered_string}");
        Ok(())
    } else if args.squeeze_repeats && set2.is_none() {
        let mut char_counts = HashMap::new();
        for c in input.chars() {
            *char_counts.entry(c).or_insert(0) += 1;
        }

        let mut seen = HashSet::new();
        let filtered_string: String = input
            .chars()
            .filter(|&c| {
                if char_counts[&c] > 1 && Operand::contains(&set1, &c) {
                    if seen.contains(&c) {
                        false
                    } else {
                        seen.insert(c);
                        true
                    }
                } else {
                    true
                }
            })
            .collect();
        print!("{filtered_string}");
        return Ok(());
    } else {
        let mut result_string: String;

        if args.complement_char || args.complement_val {
            if args.complement_char {
                result_string = complement_chars(&input, set1, set2.clone().unwrap());
            } else {
                let mut set2 = set2.clone().unwrap();
                set2.sort_by(|a, b| match (a, b) {
                    (Operand::Char(char1), Operand::Char(char2)) => char1.char.cmp(&char2.char),
                    (Operand::Equiv(equiv1), Operand::Equiv(equiv2)) => {
                        equiv1.char.cmp(&equiv2.char)
                    }
                    (Operand::Char(char1), Operand::Equiv(equiv2)) => char1.char.cmp(&equiv2.char),
                    (Operand::Equiv(equiv1), Operand::Char(char2)) => equiv1.char.cmp(&char2.char),
                });

                result_string = complement_chars(&input, set1, set2);
            }
        } else {
            if set_1_collection != CaseSensitive::None
                && set_2_collection != CaseSensitive::None
                && set_1_collection != set_2_collection
            {
                match set_1_collection {
                    CaseSensitive::UpperCase => print!("{}", input.to_lowercase()),

                    CaseSensitive::LowerCase => print!("{}", input.to_uppercase()),
                    _ => (),
                }
                return Ok(());
            }

            let set_2 = set2.clone().unwrap();
            let input_chars: Vec<char> = input.chars().collect();

            let mut result_chars = input_chars.clone();
            let input_len = input_chars.len();

            let mut start = 0;
            let end_loop = input_len;

            while start < end_loop {
                let mut match_len = 0;
                let mut j = 0;
                let mut end = start;

                while j < set1.len() && end < input_len {
                    let mut count = 0;

                    if let Operand::Equiv(equiv) = &set1[j] {
                        if end < input_len && compare_deunicoded_chars(equiv.char, input_chars[end])
                        {
                            j += 1;
                            end += 1;
                            match_len = end - start;
                        }
                    } else if let Operand::Char(char_struct) = &set1[j] {
                        while end < input_len && input_chars[end] == char_struct.char {
                            count += 1;
                            end += 1;
                        }
                        if count != 0 && count <= char_struct.repeated {
                            j += 1;
                            match_len = end - start;
                        } else {
                            break;
                        }
                    }
                }

                if match_len > 0 {
                    let set_2_chars = filter_chars(set_2.clone());
                    let string_for_replace = create_minimal_string(set_2_chars, match_len);

                    result_chars.splice(start..start + match_len, string_for_replace);

                    start += match_len;
                    continue;
                }

                start += 1;
            }

            result_string = result_chars.into_iter().collect();
        }

        if args.squeeze_repeats {
            // Counting the frequency of characters in the chars vector
            let mut char_counts = HashMap::new();
            for c in result_string.chars() {
                *char_counts.entry(c).or_insert(0) += 1;
            }

            let mut seen = HashSet::new();
            result_string = result_string
                .chars()
                .filter(|&c| check_repeatable(c, &char_counts, &mut seen, set2.as_ref().unwrap()))
                .collect();
        }

        print!("{result_string}");
        return Ok(());
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();
    args.validate_args()?;
    let mut exit_code = 0;

    if let Err(err) = tr(&args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    std::process::exit(exit_code)
}
