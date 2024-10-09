use clap::Parser;
use deunicode::deunicode_char;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::{self, Read, Write};
use std::iter::{self, Peekable};
use std::str::Chars;
use std::sync::OnceLock;

/// tr - translate or delete characters
#[derive(Parser)]
#[command(version, about)]
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
            return Err("Options '-c' and '-C' cannot be used together".to_owned());
        }
        if self.squeeze_repeats
            && self.string2.is_none()
            && (self.complement_char || self.complement_val)
            && !self.delete
        {
            return Err("Option '-c' or '-C' may only be used with 2 strings".to_owned());
        }

        if !self.squeeze_repeats && !self.delete && self.string2.is_none() {
            return Err("Need two strings operand".to_owned());
        }

        if self.string1.is_empty() {
            return Err("At least 1 string operand is required".to_owned());
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
enum RepeatSetting {
    AsManyAsNeeded,
    N(usize),
}

// The Char struct represents a character along with its repetition count.
#[derive(Clone, Debug)]
struct Char {
    // The character.
    char: char,
    // The number of times the character is repeated
    repeated: RepeatSetting,
}

// The Equiv struct represents a character equivalent
#[derive(Clone, Debug)]
struct Equiv {
    // The character equivalent
    char: char,
}

// The Operand enum can be either a Char or an Equiv
#[derive(Clone, Debug)]
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
    fn contains(operands: &[Operand], target: &char) -> bool {
        for operand in operands {
            match operand {
                Operand::Equiv(eq) => {
                    if compare_deunicoded_chars(eq.char, *target) {
                        return true;
                    }
                }
                Operand::Char(ch) => {
                    if ch.char == *target {
                        return true;
                    }
                }
            }
        }

        false
    }
}

/// Represents the case sensitivity of character classes
#[derive(PartialEq)]
enum CaseSensitivity {
    UpperCase,
    LowerCase,
    None,
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
fn parse_equiv(chars: &mut Peekable<Chars>) -> Result<Vec<Operand>, String> {
    // Skip '='
    chars.next();

    let mut equiv = String::new();

    while let Some(&next_ch) = chars.peek() {
        if next_ch == '=' {
            break;
        }

        chars.next();

        equiv.push(next_ch);
    }

    if equiv.is_empty() {
        return Err("Error: Missing equiv symbol after '[='".to_owned());
    }

    // Skip '='
    let Some('=') = chars.next() else {
        return Err("Error: Missing '=' before ']' for '[=equiv=]'".to_owned());
    };

    // Skip ']'
    let Some(']') = chars.next() else {
        return Err("Error: Missing closing ']' for '[=equiv=]'".to_owned());
    };

    let mut operands = Vec::<Operand>::new();

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

        chars.next();

        repeat_str.push(digit);
    }

    // Skip ']'
    let Some(']') = chars.next() else {
        return Err("Error: Missing closing ']'".to_owned());
    };

    // "If n has a leading zero, it shall be interpreted as an octal value. Otherwise, it shall be interpreted as a decimal value."
    // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/tr.html
    let radix = if repeat_str.starts_with('0') {
        // Octal
        8_u32
    } else {
        10_u32
    };

    let repeated = match usize::from_str_radix(&repeat_str, radix) {
        Ok(n) if n > 0 => RepeatSetting::N(n),
        _ => RepeatSetting::AsManyAsNeeded,
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
fn parse_symbols(string1_or_string2: &str) -> Result<Vec<Operand>, String> {
    // This capacity will be sufficient at least some of the time
    let mut operands = Vec::<Operand>::with_capacity(string1_or_string2.len());

    let mut iterator = string1_or_string2.chars().peekable();

    while let Some(&ch) = iterator.peek() {
        match ch {
            '[' => {
                // Move past '['
                iterator.next();

                let Some('=') = iterator.peek() else {
                    let symbol = iterator
                        .next()
                        .ok_or("Error: Missing symbol after '['".to_owned())?;

                    operands.push(parse_repeated_char(&mut iterator, symbol)?);

                    continue;
                };

                operands.extend(parse_equiv(&mut iterator)?);
            }
            // A single backslash character (0x5C)
            // https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap05.html#tagtcjh_2
            // https://www.unicode.org/Public/UCD/latest/ucd/NameAliases.txt
            '\\' => {
                // Move past '\'
                iterator.next();

                let char_for_operand = match iterator.peek() {
                    /* #region \octal */
                    Some(&first_octal_digit @ '0'..='7') => {
                        // Move past `first_octal_digit`
                        iterator.next();

                        let mut st = String::with_capacity(3_usize);

                        st.push(first_octal_digit);

                        for _ in 0_usize..2_usize {
                            if let Some(&octal_digit @ '0'..='7') = iterator.peek() {
                                // Move past `octal_digit`
                                iterator.next();

                                st.push(octal_digit);
                            } else {
                                break;
                            }
                        }

                        let from_str_radix_result = u16::from_str_radix(&st, 8_u32);

                        let octal_digits_parsed = match from_str_radix_result {
                            Ok(uo) => uo,
                            Err(pa) => {
                                return Err(format!(
                                    "tr: failed to parse octal sequence '{st}' ({pa})"
                                ));
                            }
                        };

                        let byte = match u8::try_from(octal_digits_parsed) {
                            Ok(ue) => ue,
                            Err(tr) => {
                                return Err(format!("tr: invalid octal sequence '{st}' ({tr})"));
                            }
                        };

                        operands.push(Operand::Char(Char {
                            char: char::from(byte),
                            repeated: RepeatSetting::N(1),
                        }));

                        continue;
                    }
                    /* #endregion */
                    //
                    /* #region \character */
                    // <alert>
                    // Code point 0007
                    Some('a') => '\u{0007}',
                    // <backspace>
                    // Code point 0008
                    Some('b') => '\u{0008}',
                    // <tab>
                    // Code point 0009
                    Some('t') => '\u{0009}',
                    // <newline>
                    // Code point 000A
                    Some('n') => '\u{000A}',
                    // <vertical-tab>
                    // Code point 000B
                    Some('v') => '\u{000B}',
                    // <form-feed>
                    // Code point 000C
                    Some('f') => '\u{000C}',
                    // <carriage-return>
                    // Code point 000D
                    Some('r') => '\u{000D}',
                    // <backslash>
                    // Code point 005C
                    Some('\\') => {
                        // An escaped backslash
                        '\u{005C}'
                    }
                    /* #endregion */
                    //
                    Some(&cha) => {
                        // If a backslash is not at the end of the string, and is not followed by one of the valid
                        // escape characters (including another backslash), the backslash is basically just ignored:
                        // the following character is the character added to the set.
                        cha
                    }
                    None => {
                        eprintln!(
                            "tr: warning: an unescaped backslash at end of string is not portable"
                        );

                        // If an unescaped backslash is the last character of the string, treat it as though it were
                        // escaped (backslash is added to the set)
                        '\u{005C}'
                    }
                };

                // Move past character following '\'
                iterator.next();

                operands.push(Operand::Char(Char {
                    char: char_for_operand,
                    repeated: RepeatSetting::N(1),
                }));
            }
            cha => {
                // Move past `cha`
                iterator.next();

                // Add a regular character with a repetition of 1
                operands.push(Operand::Char(Char {
                    char: cha,
                    repeated: RepeatSetting::N(1),
                }));
            }
        }
    }

    Ok(operands)
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
fn expand_character_class(class: &str) -> Result<(Vec<Operand>, CaseSensitivity), String> {
    let mut case_sensitive = CaseSensitivity::None;

    let char_vec = match class {
        "alnum" => ('0'..='9')
            .chain('A'..='Z')
            .chain('a'..='z')
            .collect::<Vec<_>>(),
        "alpha" => ('A'..='Z').chain('a'..='z').collect::<Vec<_>>(),
        "digit" => ('0'..='9').collect::<Vec<_>>(),
        "lower" => {
            case_sensitive = CaseSensitivity::LowerCase;

            ('a'..='z').collect::<Vec<_>>()
        }
        "upper" => {
            case_sensitive = CaseSensitivity::UpperCase;

            ('A'..='Z').collect::<Vec<_>>()
        }
        "space" => vec![' ', '\t', '\n', '\r', '\x0b', '\x0c'],
        "blank" => vec![' ', '\t'],
        "cntrl" => (0..=31)
            .chain(iter::once(127))
            .map(|it| char::from(it as u8))
            .collect::<Vec<_>>(),
        "graph" => (33..=126)
            .map(|it| char::from(it as u8))
            .collect::<Vec<_>>(),
        "print" => (32..=126)
            .map(|it| char::from(it as u8))
            .collect::<Vec<_>>(),
        "punct" => (33..=47)
            .chain(58..=64)
            .chain(91..=96)
            .chain(123..=126)
            .map(|it| char::from(it as u8))
            .collect::<Vec<_>>(),
        "xdigit" => ('0'..='9')
            .chain('A'..='F')
            .chain('a'..='f')
            .collect::<Vec<_>>(),
        _ => return Err("Error: Invalid class name ".to_owned()),
    };

    let operand_vec = char_vec
        .into_iter()
        .map(|ch| {
            Operand::Char(Char {
                char: ch,
                repeated: RepeatSetting::N(1),
            })
        })
        .collect::<Vec<_>>();

    Ok((operand_vec, case_sensitive))
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
fn parse_classes(string1_or_string2: &str) -> Result<(Vec<Operand>, CaseSensitivity), String> {
    let mut classes = Vec::<Operand>::new();
    let mut chars = string1_or_string2.chars().peekable();
    let mut case_sensitive = CaseSensitivity::None;

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

                chars.next();

                class.push(next_ch);
            }

            if class.is_empty() {
                return Err("Error: Missing class name after '[:'".to_owned());
            }

            // Skip ':'
            let Some(':') = chars.next() else {
                return Err("Error: Missing ':' before ']' for '[:class:]'".to_owned());
            };

            // Skip ']'
            let Some(']') = chars.next() else {
                return Err("Error: Missing closing ']' for '[:class:]'".to_owned());
            };

            let (ve, ca) = expand_character_class(&class)?;

            classes.extend(ve);

            case_sensitive = ca;
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
fn parse_ranges(string1_or_string2: &str) -> Result<Vec<Operand>, String> {
    // Remove square brackets
    let input_without_square_brackets =
        string1_or_string2.trim_matches(|ch| ch == '[' || ch == ']');

    let mut split = input_without_square_brackets.split('-');

    let start = split.next().ok_or("Iteration failed")?;
    let end = split.next().ok_or("Iteration failed")?;

    let mut chars = Vec::<char>::new();

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

        for ch in start_char..=end_char {
            chars.push(ch);
        }
    }

    let vec = chars
        .into_iter()
        .map(|ch| {
            Operand::Char(Char {
                char: ch,
                repeated: RepeatSetting::N(1),
            })
        })
        .collect::<Vec<_>>();

    Ok(vec)
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
fn parse_string1_or_string2(
    string1_or_string2: &str,
) -> Result<(Vec<Operand>, CaseSensitivity), String> {
    if string1_or_string2.starts_with("[:") && string1_or_string2.ends_with(":]") {
        Ok(parse_classes(string1_or_string2)?)
    } else if contains_single_range(string1_or_string2) {
        Ok((parse_ranges(string1_or_string2)?, CaseSensitivity::None))
    } else {
        Ok((parse_symbols(string1_or_string2)?, CaseSensitivity::None))
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
fn contains_single_range(string1_or_string2: &str) -> bool {
    static REGEX_ONCE_CELL: OnceLock<Regex> = OnceLock::<Regex>::new();

    let regex = REGEX_ONCE_CELL.get_or_init(|| {
        // Regular expression for a range of characters or \octal
        Regex::new(
            r"(?x)
            ^ \[ [a-zA-Z0-9\\]+ - [a-zA-Z0-9\\]+ \] $ |   # Range in square brackets
            ^ \\ [0-7]{1,3} - \\ [0-7]{1,3} $ |           # Range \octal-\octal
            ^ [a-zA-Z0-9] - [a-zA-Z0-9] $                 # Character-symbol range
        ",
        )
        .unwrap()
    });

    regex.is_match(string1_or_string2)
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
fn complement_chars(
    input: &str,
    chars1: &[Operand],
    chars2: &[Operand],
) -> Result<String, Box<dyn Error>> {
    let mut depleted = Vec::<usize>::with_capacity(chars2.len());

    for op in chars2 {
        if let Operand::Char(ch) = op {
            if let RepeatSetting::N(n) = ch.repeated {
                depleted.push(n);

                continue;
            }
        }

        depleted.push(usize::MAX);
    }

    let depleted_clone = depleted.clone();

    // Create a variable to store the result
    let mut result = String::new();

    // Initialize the index for the chars2 vector
    let mut chars2_index = 0;

    // Go through each character in the input string
    for ch in input.chars() {
        // Check if the character is in the chars1 vector
        if Operand::contains(chars1, &ch) {
            // If the character is in the chars1 vector, add it to the result without changing it
            result.push(ch);

            continue;
        }

        // If the character is not in the chars1 vector, replace it with a character from the chars2 vector
        // Add the character from the chars2 vector to the result
        // TODO
        // Indexing
        let operand = chars2.get(chars2_index).ok_or("Indexing failed")?;

        match operand {
            Operand::Char(char) => {
                result.push(char.char);

                let mut_ref = depleted.get_mut(chars2_index).ok_or("Indexing failed")?;

                let decremented = (*mut_ref) - 1_usize;

                *mut_ref = decremented;

                if decremented > 0_usize {
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

            depleted.clone_from(&depleted_clone);
        }
    }

    Ok(result)
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
    ch: char,
    char_counts: &HashMap<char, usize>,
    seen: &mut HashSet<char>,
    set2: &[Operand],
) -> bool {
    if char_counts[&ch] > 1 && Operand::contains(set2, &ch) {
        if seen.contains(&ch) {
            false
        } else {
            seen.insert(ch);

            true
        }
    } else {
        true
    }
}

// TODO
// This should be optimized
fn generate_transformation_map(
    string1_operands: &[Operand],
    string2_operands: &[Operand],
) -> Result<HashMap<char, char>, Box<dyn Error>> {
    let mut char_repeating_total = 0_usize;

    let mut string1_operands_flattened = Vec::<char>::new();

    for op in string1_operands {
        match op {
            Operand::Char(ch) => match ch.repeated {
                RepeatSetting::AsManyAsNeeded => {
                    return Err(Box::from(
                        "tr: the [c*] repeat construct may not appear in string1".to_owned(),
                    ));
                }
                RepeatSetting::N(n) => {
                    char_repeating_total = char_repeating_total
                        .checked_add(n)
                        .ok_or("Arithmetic overflow")?;

                    for _ in 0_usize..n {
                        string1_operands_flattened.push(ch.char);
                    }
                }
            },
            _ => {
                return Err(Box::from("Expectation violated".to_owned()));
            }
        }
    }

    let mut as_many_as_needed_index = Option::<usize>::None;

    let mut replacement_char_repeating_total = 0_usize;

    for (us, op) in string2_operands.iter().enumerate() {
        match op {
            Operand::Char(ch) => match ch.repeated {
                RepeatSetting::AsManyAsNeeded => {
                    if as_many_as_needed_index.is_some() {
                        return Err(Box::from(
                            "tr: only one [c*] repeat construct may appear in string2".to_owned(),
                        ));
                    }

                    as_many_as_needed_index = Some(us);
                }
                RepeatSetting::N(n) => {
                    replacement_char_repeating_total = replacement_char_repeating_total
                        .checked_add(n)
                        .ok_or("Arithmetic overflow")?;
                }
            },
            _ => {
                return Err(Box::from("Expectation violated".to_owned()));
            }
        }
    }

    let mut string2_operands_with_leftover: Vec<Operand>;

    let string2_operands_to_use = if replacement_char_repeating_total < char_repeating_total {
        let leftover = char_repeating_total
            .checked_sub(replacement_char_repeating_total)
            .ok_or("Arithmetic overflow")?;

        string2_operands_with_leftover = string2_operands.to_vec();

        match as_many_as_needed_index {
            Some(us) => {
                let op = string2_operands_with_leftover
                    .get_mut(us)
                    .ok_or("Indexing failed")?;

                match op {
                    Operand::Char(ch) => {
                        *op = Operand::Char(Char {
                            char: ch.char,
                            repeated: RepeatSetting::N(leftover),
                        });
                    }
                    _ => {
                        return Err(Box::from("Expectation violated".to_owned()));
                    }
                }
            }
            None => {
                let op = string2_operands_with_leftover
                    .last_mut()
                    .ok_or("Unexpected empty collection")?;

                match op {
                    Operand::Char(ch) => {
                        let current_n = match ch.repeated {
                            RepeatSetting::N(n) => n,
                            _ => {
                                return Err(Box::from("Expectation violated".to_owned()));
                            }
                        };

                        let current_n_plus_leftover = current_n
                            .checked_add(leftover)
                            .ok_or("Arithmetic overflow")?;

                        *op = Operand::Char(Char {
                            char: ch.char,
                            repeated: RepeatSetting::N(current_n_plus_leftover),
                        });
                    }
                    _ => {
                        return Err(Box::from("Expectation violated".to_owned()));
                    }
                }
            }
        }

        &string2_operands_with_leftover
    } else {
        string2_operands
    };

    // TODO
    // Capacity
    let mut string2_operands_to_use_flattened = Vec::<char>::new();

    for op in string2_operands_to_use {
        match op {
            Operand::Char(ch) => match ch.repeated {
                RepeatSetting::N(n) => {
                    for _ in 0_usize..n {
                        string2_operands_to_use_flattened.push(ch.char);
                    }
                }
                RepeatSetting::AsManyAsNeeded => {
                    // The "[c*]" construct was not needed, ignore it
                }
            },
            _ => {
                return Err(Box::from("Expectation violated".to_owned()));
            }
        }
    }

    let mut translation_hash_map = HashMap::<char, char>::new();

    for (us, ch) in string1_operands_flattened.into_iter().enumerate() {
        let cha = string2_operands_to_use_flattened
            .get(us)
            .ok_or("Indexing failed")?;

        translation_hash_map.insert(ch, *cha);
    }

    Ok(translation_hash_map)
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

    let (string1_operands, string1_case_sensitivity) = parse_string1_or_string2(&args.string1)?;

    let (string2_operands_option, string2_case_sensitivity) = match &args.string2 {
        Some(st) => {
            let (ve, ca) = parse_string1_or_string2(st)?;

            (Some(ve), ca)
        }
        None => (None, CaseSensitivity::None),
    };

    let mut stdout_lock = io::stdout().lock();

    if args.delete {
        let filtered_string = if args.complement_char || args.complement_val {
            input
                .chars()
                .filter(|c| Operand::contains(&string1_operands, c))
                .collect::<String>()
        } else {
            input
                .chars()
                .filter(|c| !Operand::contains(&string1_operands, c))
                .collect::<String>()
        };

        let filtered_string_to_use = if args.squeeze_repeats && string2_operands_option.is_some() {
            // Counting the frequency of characters in the chars vector
            let mut char_counts = HashMap::<char, usize>::new();

            for ch in filtered_string.chars() {
                *(char_counts.entry(ch).or_insert(0)) += 1;
            }

            let mut seen = HashSet::<char>::new();

            filtered_string
                .chars()
                .filter(|&ch| {
                    check_repeatable(
                        ch,
                        &char_counts,
                        &mut seen,
                        string2_operands_option.as_deref().unwrap(),
                    )
                })
                .collect::<String>()
        } else {
            filtered_string
        };

        stdout_lock.write_all(filtered_string_to_use.as_bytes())?;

        Ok(())
    } else if args.squeeze_repeats && string2_operands_option.is_none() {
        let mut char_counts = HashMap::<char, i32>::new();

        for ch in input.chars() {
            *(char_counts.entry(ch).or_insert(0)) += 1;
        }

        let mut seen = HashSet::<char>::new();

        let filtered_string = input
            .chars()
            .filter(|&ch| {
                if char_counts[&ch] > 1 && Operand::contains(&string1_operands, &ch) {
                    if seen.contains(&ch) {
                        false
                    } else {
                        seen.insert(ch);

                        true
                    }
                } else {
                    true
                }
            })
            .collect::<String>();

        stdout_lock.write_all(filtered_string.as_bytes())?;

        return Ok(());
    } else {
        let result_string = if args.complement_char || args.complement_val {
            if args.complement_char {
                complement_chars(
                    &input,
                    &string1_operands,
                    string2_operands_option.as_deref().unwrap(),
                )?
            } else {
                let mut set2 = string2_operands_option.as_deref().unwrap().to_vec();

                set2.sort_by(|a, b| match (a, b) {
                    (Operand::Char(char1), Operand::Char(char2)) => char1.char.cmp(&char2.char),
                    (Operand::Equiv(equiv1), Operand::Equiv(equiv2)) => {
                        equiv1.char.cmp(&equiv2.char)
                    }
                    (Operand::Char(char1), Operand::Equiv(equiv2)) => char1.char.cmp(&equiv2.char),
                    (Operand::Equiv(equiv1), Operand::Char(char2)) => equiv1.char.cmp(&char2.char),
                });

                complement_chars(&input, &string1_operands, &set2)?
            }
        } else {
            if string1_case_sensitivity != CaseSensitivity::None
                && string2_case_sensitivity != CaseSensitivity::None
                && string1_case_sensitivity != string2_case_sensitivity
            {
                match string1_case_sensitivity {
                    CaseSensitivity::UpperCase => {
                        stdout_lock.write_all(input.to_lowercase().as_bytes())?;
                    }
                    CaseSensitivity::LowerCase => {
                        stdout_lock.write_all(input.to_uppercase().as_bytes())?;
                    }
                    _ => {}
                }

                return Ok(());
            }

            let string2_operands = match string2_operands_option.as_deref() {
                Some(op) => op,
                None => {
                    return Err(Box::from("tr: missing operand\n".to_owned()));
                }
            };

            if string2_operands.is_empty() {
                return Err(Box::from(
                    "tr: when not truncating set1, string2 must be non-empty\n".to_owned(),
                ));
            }

            let transformation_map =
                generate_transformation_map(&string1_operands, string2_operands)?;

            let mut result = String::with_capacity(input.len());

            for ch in input.chars() {
                let char_to_use = match transformation_map.get(&ch) {
                    Some(cha) => *cha,
                    None => ch,
                };

                result.push(char_to_use);
            }

            result
        };

        let result_string_to_use = if args.squeeze_repeats {
            // Counting the frequency of characters in the chars vector
            let mut char_counts = HashMap::<char, usize>::new();

            for ch in result_string.chars() {
                *(char_counts.entry(ch).or_insert(0)) += 1;
            }

            let mut seen = HashSet::<char>::new();

            result_string
                .chars()
                .filter(|&ch| {
                    check_repeatable(
                        ch,
                        &char_counts,
                        &mut seen,
                        string2_operands_option.as_deref().unwrap(),
                    )
                })
                .collect::<String>()
        } else {
            result_string
        };

        stdout_lock.write_all(result_string_to_use.as_bytes())?;

        return Ok(());
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();

    args.validate_args()?;

    if let Err(err) = tr(&args) {
        eprint!("{}", err);

        std::process::exit(1);
    }

    Ok(())
}
