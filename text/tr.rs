use clap::Parser;
use deunicode::deunicode_char;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use parsing::parse_string1_or_string2;
use plib::PROJECT_NAME;
use setup::{generate_for_removal, generate_for_translation};
use std::error::Error;
use std::process;
use transformation::delete::{DeleteState, DeleteTransformation};
use transformation::delete_and_squeeze::{DeleteAndSqueezeState, DeleteAndSqueezeTransformation};
use transformation::squeeze::{SqueezeState, SqueezeTransformation};
use transformation::squeeze_and_translate::{
    SqueezeAndTranslateState, SqueezeAndTranslateTransformation,
};
use transformation::translate::TranslateTransformation;
use transformation::Transformation;

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
            return Err("tr: options '-c' and '-C' cannot be used together".to_owned());
        }

        if self.squeeze_repeats
            && self.string2.is_none()
            && (self.complement_char || self.complement_val)
            && !self.delete
        {
            return Err("tr: option '-c' or '-C' may only be used with 2 strings".to_owned());
        }

        if !self.squeeze_repeats && !self.delete && self.string2.is_none() {
            return Err(format!(
                "tr: missing operand after ‘{}’. Two strings must be given when translating.",
                self.string1
            ));
        }

        Ok(())
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
    let string1_operands = parse_string1_or_string2(&args.string1)?;

    let string2_operands_option = match &args.string2 {
        Some(st) => Some(parse_string1_or_string2(st)?),
        None => None,
    };

    // TODO
    // How are these different?
    let complement = args.complement_char || args.complement_val;

    let mut transformation: Box<dyn Transformation> = match (args.delete, args.squeeze_repeats) {
        (false, false) => {
            // "tr"
            let string2_operands = match string2_operands_option {
                Some(op) => op,
                None => {
                    return Err(Box::from("tr: missing operand".to_owned()));
                }
            };

            if string2_operands.is_empty() {
                return Err(Box::from(
                    "tr: when not truncating set1, string2 must be non-empty".to_owned(),
                ));
            }

            let for_translation =
                generate_for_translation(complement, string1_operands, string2_operands)?;

            let bo: Box<dyn Transformation> =
                Box::from(TranslateTransformation { for_translation });

            bo
        }
        (true, false) => {
            // "tr -d"
            let for_removal = generate_for_removal(complement, string1_operands)?;

            let bo: Box<dyn Transformation> = Box::from(DeleteTransformation {
                for_removal,
                delete_state: DeleteState::Start,
            });

            bo
        }
        (false, true) => {
            // "tr -s"
            match string2_operands_option {
                Some(string2_operands) => {
                    if string2_operands.is_empty() {
                        return Err(Box::from(
                            "tr: when not truncating set1, string2 must be non-empty".to_owned(),
                        ));
                    }

                    let for_translation =
                        generate_for_translation(complement, string1_operands, string2_operands)?;

                    let bo: Box<dyn Transformation> =
                        Box::from(SqueezeAndTranslateTransformation {
                            for_translation,
                            squeeze_and_translate_state: SqueezeAndTranslateState::Start,
                        });

                    bo
                }
                None => {
                    let for_removal = generate_for_removal(complement, string1_operands)?;

                    let bo: Box<dyn Transformation> = Box::from(SqueezeTransformation {
                        for_removal,
                        squeeze_state: SqueezeState::Start,
                    });

                    bo
                }
            }
        }
        (true, true) => {
            // "tr -d -s"
            let string2_operands = match string2_operands_option {
                Some(op) => op,
                None => {
                    return Err(Box::from("tr: missing operand".to_owned()));
                }
            };

            // "The same string cannot be used for both the -d and the -s option; when both options are specified, both string1 (used for deletion) and string2 (used for squeezing) shall be required."
            let delete = generate_for_removal(complement, string1_operands)?;

            let squeeze = generate_for_removal(complement, string2_operands)?;

            let bo: Box<dyn Transformation> = Box::from(DeleteAndSqueezeTransformation {
                delete,
                squeeze,
                delete_and_squeeze_state: DeleteAndSqueezeState::Start,
            });

            bo
        }
    };

    transformation.streaming_transform()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();

    if let Err(st) = args.validate_args() {
        eprintln!("{st}");

        process::exit(1_i32);
    }

    if let Err(err) = tr(&args) {
        eprintln!("{err}");

        process::exit(1_i32);
    }

    Ok(())
}

mod parsing {
    use std::iter::{self, Peekable};
    use std::slice::Iter;
    use std::str::Chars;

    #[derive(Clone)]
    pub enum CharRepetition {
        AsManyAsNeeded,
        N(usize),
    }

    #[derive(Clone)]
    pub struct CharOperand {
        // The character
        pub char: char,
        // The number of times the character is repeated
        pub char_repetition: CharRepetition,
    }

    #[derive(Clone)]
    pub struct EquivOperand {
        // The character equivalent
        pub char: char,
    }

    #[derive(Clone)]
    pub enum Operand {
        Char(CharOperand),
        Equiv(EquivOperand),
    }

    pub fn parse_string1_or_string2(string1_or_string2: &str) -> Result<Vec<Operand>, String> {
        // The longest valid "[:class:]", "[=equiv=]", or "[x*n]" construct is a "[x*n]" construct
        // These are (seemingly) the shortest invalid "[x*n]" constructs (octal and decimal):
        // [a*010000000000000000000000]
        // [a*100000000000000000000]
        // Therefore, the longest valid one should be:
        // [a*01000000000000000000000]
        // Rounding up to 32
        const SQUARE_BRACKET_CONSTRUCTS_BUFFER_CAPACITY: usize = 32_usize;

        // This capacity will be sufficient at least some of the time
        let mut operand_vec = Vec::<Operand>::with_capacity(string1_or_string2.len());

        let mut peekable = string1_or_string2.chars().peekable();

        let mut parse_left_square_bracket_normally = false;

        while let Some(&ch) = peekable.peek() {
            match (ch, parse_left_square_bracket_normally) {
                ('[', false) => {
                    // Save the state of `peekable` before advancing it, see note below
                    let peekable_saved = peekable.clone();

                    // TODO
                    // Avoid repeated allocation
                    let mut square_bracket_constructs_buffer =
                        Vec::<char>::with_capacity(SQUARE_BRACKET_CONSTRUCTS_BUFFER_CAPACITY);

                    let mut found_closing_square_bracket = false;

                    for ch in peekable.by_ref() {
                        square_bracket_constructs_buffer.push(ch);

                        let vec_len = square_bracket_constructs_buffer.len();

                        // Length check is a hacky fix for "[:]", "[=]", "[]*]", etc.
                        if ch == ']' && vec_len > 3_usize {
                            found_closing_square_bracket = true;

                            break;
                        }
                    }

                    if found_closing_square_bracket {
                        let after_opening_square_bracket =
                            square_bracket_constructs_buffer.get(1_usize);

                        let before_closing_square_bracket =
                            square_bracket_constructs_buffer.iter().rev().nth(1_usize);

                        if after_opening_square_bracket == Some(&':')
                            && before_closing_square_bracket == Some(&':')
                        {
                            expand_character_class(
                                &square_bracket_constructs_buffer,
                                &mut operand_vec,
                            )?;

                            continue;
                        }

                        if after_opening_square_bracket == Some(&'=')
                            && before_closing_square_bracket == Some(&'=')
                        {
                            // "[=equiv=]" construct
                            let operand = parse_equiv(&square_bracket_constructs_buffer)?;

                            operand_vec.push(operand);

                            continue;
                        }

                        if square_bracket_constructs_buffer.get(2_usize) == Some(&'*') {
                            // "[x*n]" construct
                            operand_vec
                                .push(parse_repeated_char(&square_bracket_constructs_buffer)?);

                            continue;
                        }
                    }

                    // Not a "[:class:]", "[=equiv=]", or "[x*n]" construct
                    // The hacky way to continue is to reset `peekable` (to `peekable_saved`)
                    // This moves the Peekable back to the point it was at before attempting to parse square bracket
                    // constructs
                    parse_left_square_bracket_normally = true;
                    peekable = peekable_saved;
                }
                (cha, bo) => {
                    // '[' is not the start of a square bracket construct, so handle it normally
                    if bo {
                        assert!(cha == '[');

                        // When encountering '[' in the future, try to parse square bracket constructs first
                        parse_left_square_bracket_normally = false;
                    }

                    if let Some(char) = parse_single_char(&mut peekable)? {
                        parse_range_or_single_char(char, &mut peekable, &mut operand_vec)?;
                    }
                }
            }
        }

        Ok(operand_vec)
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
    fn parse_equiv(square_bracket_constructs_buffer: &[char]) -> Result<Operand, String> {
        let mut iter = square_bracket_constructs_buffer.iter();

        // Skip '[='
        assert!(iter.next() == Some(&'['));
        assert!(iter.next() == Some(&'='));

        let mut between_equals_signs = Vec::<char>::with_capacity(1_usize);

        between_equals_signs.extend(iter.take_while(|&&ch| ch != '='));

        let char_between_equals_signs = match between_equals_signs.as_slice() {
            &[ch] => ch,
            &[] => {
                return Err("tr: missing equivalence class character '[==]'".to_owned());
            }
            sl => {
                const ERROR_MESSAGE_PREFIX: &str = "tr: ";
                const ERROR_MESSAGE_SUFFIX: &str =
                    ": equivalence class operand must be a single character";

                const ERROR_MESSAGE_PREFIX_AND_SUFFIX_LENGTH: usize =
                    ERROR_MESSAGE_PREFIX.len() + ERROR_MESSAGE_SUFFIX.len();

                let mut error_message =
                    String::with_capacity(ERROR_MESSAGE_PREFIX_AND_SUFFIX_LENGTH + sl.len());

                error_message.push_str(ERROR_MESSAGE_PREFIX);

                for &ch in sl {
                    error_message.push(ch);
                }

                error_message.push_str(ERROR_MESSAGE_SUFFIX);

                return Err(error_message);
            }
        };

        let operand = Operand::Equiv(EquivOperand {
            char: char_between_equals_signs,
        });

        Ok(operand)
    }

    fn parse_repeated_char(square_bracket_constructs_buffer: &[char]) -> Result<Operand, String> {
        // TODO
        // Clean this up
        fn fill_repeat_str(iter: &mut Iter<char>, repeat_string: &mut String) {
            while let Some(&ch) = iter.next() {
                if ch == ']' {
                    assert!(iter.next().is_none());

                    return;
                }

                repeat_string.push(ch);
            }

            unreachable!();
        }

        let mut iter = square_bracket_constructs_buffer.iter();

        // Skip '['
        assert!(iter.next() == Some(&'['));

        // Get character before '*'
        let char = iter.next().unwrap().to_owned();

        // Skip '*'
        assert!(iter.next() == Some(&'*'));

        let mut repeat_string = String::with_capacity(square_bracket_constructs_buffer.len());

        fill_repeat_str(&mut iter, &mut repeat_string);

        // "If n is omitted or is zero, it shall be interpreted as large enough to extend the string2-based sequence to the length of the string1-based sequence. If n has a leading zero, it shall be interpreted as an octal value. Otherwise, it shall be interpreted as a decimal value."
        // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/tr.html
        let char_repetition = match repeat_string.as_str() {
            "" => CharRepetition::AsManyAsNeeded,
            st => {
                let radix = if st.starts_with('0') {
                    // Octal
                    8_u32
                } else {
                    10_u32
                };

                match usize::from_str_radix(st, radix) {
                    Ok(0_usize) => CharRepetition::AsManyAsNeeded,
                    Ok(n) => CharRepetition::N(n),
                    Err(_pa) => {
                        return Err(format!(
                            "tr: invalid repeat count ‘{st}’ in [c*n] construct",
                        ));
                    }
                }
            }
        };

        let operand = Operand::Char(CharOperand {
            char,
            char_repetition,
        });

        Ok(operand)
    }

    fn parse_octal_sequence(
        first_octal_digit: char,
        peekable: &mut Peekable<Chars>,
    ) -> Result<char, String> {
        let mut st = String::with_capacity(3_usize);

        st.push(first_octal_digit);

        let mut added_octal_digit_to_buffer = |pe: &mut Peekable<Chars>| {
            if let Some(&second_octal_digit @ '0'..='7') = pe.peek() {
                st.push(second_octal_digit);

                true
            } else {
                false
            }
        };

        let advance_peekable_if_parsing_succeed = if added_octal_digit_to_buffer(peekable) {
            peekable.next();

            added_octal_digit_to_buffer(peekable)
        } else {
            false
        };

        let from_str_radix_result = u16::from_str_radix(&st, 8_u32);

        let octal_digits_parsed = match from_str_radix_result {
            Ok(uo) => uo,
            Err(pa) => {
                return Err(format!("tr: failed to parse octal sequence '{st}' ({pa})"));
            }
        };

        // There is no consensus on how to handle this:
        //
        // BusyBox and GNU Core Utilities:
        //     parse "\501" as \050 (which is '(') and '1'
        //         GNU Core Utilities prints a warning, BusyBox does not
        // uutils' coreutils:
        //     parses "\501" as '1'
        // bsdutils:
        //     parses "\501" as 'Ł' (U+0141)
        //
        // None of these implementations treat this as a fatal error
        // POSIX says: "Multi-byte characters require multiple, concatenated escape sequences of this type, including the leading <backslash> for each byte."
        //
        // Following BusyBox and GNU Core Utilities, because their handling seems to be most in keeping with the POSIX
        // specification
        let byte = match u8::try_from(octal_digits_parsed) {
            Ok(ue) => {
                if advance_peekable_if_parsing_succeed {
                    peekable.next();
                }

                ue
            }
            Err(_tr) => {
                // This should only happen when the sequence is \400 and above
                // Cannot happen with a two character sequence like \77, because 8^2 is 64 (within u8 bounds)
                assert!(st.len() == 3_usize);

                let mut chars = st.chars();

                let third_octal_digit = chars.next_back().unwrap();

                // `chars_str` is a view of the first two octal digits
                let chars_str = chars.as_str();

                assert!(chars_str.len() == 2_usize);

                // Treat the sequence \abc (where a, b, and c are octal digits) as \0abc
                // The byte represented by \0ab is what will be returned from this function
                // Parsing of c is handled outside this function
                match u8::from_str_radix(chars_str, 8_u32) {
                    Ok(ue) => {
                        eprintln!(
                        "tr: warning: the ambiguous octal escape \\{st} is being interpreted as the 2-byte sequence \\0{chars_str}, {third_octal_digit}"
                    );

                        ue
                    }
                    Err(pa) => {
                        return Err(format!("tr: invalid octal sequence '{chars_str}' ({pa})"));
                    }
                }
            }
        };

        let char = char::from(byte);

        Ok(char)
    }

    fn parse_single_char(peekable: &mut Peekable<Chars>) -> Result<Option<char>, String> {
        let option = match peekable.next() {
            Some('\\') => {
                let char = match peekable.next() {
                    /* #region \octal */
                    Some(first_octal_digit @ '0'..='7') => {
                        parse_octal_sequence(first_octal_digit, peekable)?
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
                    Some(cha) => {
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

                Some(char)
            }
            op => op,
        };

        Ok(option)
    }

    fn parse_range_or_single_char(
        starting_char: char,
        peekable: &mut Peekable<Chars>,
        operand_vec: &mut Vec<Operand>,
    ) -> Result<(), String> {
        match peekable.peek() {
            Some(&hyphen @ '-') => {
                // Possible "c-c" construct
                // Move past `hyphen`
                peekable.next();

                // The parsed character after the hyphen
                // e.g. "tr 'A-Z' '\044-1'"
                match parse_single_char(peekable)? {
                    Some(after_hyphen) => {
                        // Ranges are inclusive
                        let range_inclusive = starting_char..=after_hyphen;

                        if range_inclusive.is_empty() {
                            let message =
                                format!(
                                    "tr: range-endpoints of '{}-{}' are in reverse collating sequence order",
                                    starting_char.escape_default(),
                                    after_hyphen.escape_default()
                                );

                            return Err(message);
                        }

                        let range_inclusive_to_operand_iterator = range_inclusive.map(|ch| {
                            Operand::Char(CharOperand {
                                char: ch,
                                char_repetition: CharRepetition::N(1_usize),
                            })
                        });

                        // TODO
                        // Does this reserve the right capacity?
                        operand_vec.extend(range_inclusive_to_operand_iterator);
                    }
                    None => {
                        // End of input, do not handle as a range
                        // e.g. "tr 'ab' 'c-'"
                        operand_vec.extend_from_slice(&[
                            Operand::Char(CharOperand {
                                char: starting_char,
                                char_repetition: CharRepetition::N(1_usize),
                            }),
                            Operand::Char(CharOperand {
                                char: hyphen,
                                char_repetition: CharRepetition::N(1_usize),
                            }),
                        ]);
                    }
                }
            }
            _ => {
                // Not a "c-c" construct
                operand_vec.push(Operand::Char(CharOperand {
                    char: starting_char,
                    char_repetition: CharRepetition::N(1_usize),
                }))
            }
        }

        Ok(())
    }

    fn expand_character_class(
        square_bracket_constructs_buffer: &[char],
        operand_vec: &mut Vec<Operand>,
    ) -> Result<(), String> {
        // "[:class:]" construct
        let mut into_iter = square_bracket_constructs_buffer.iter();

        assert!(into_iter.next() == Some(&'['));
        assert!(into_iter.next() == Some(&':'));

        assert!(into_iter.next_back() == Some(&']'));
        assert!(into_iter.next_back() == Some(&':'));

        // TODO
        // Performance
        let class = into_iter.collect::<String>();

        let char_vec = match class.as_str() {
            "alnum" => ('0'..='9')
                .chain('A'..='Z')
                .chain('a'..='z')
                .collect::<Vec<_>>(),
            "alpha" => ('A'..='Z').chain('a'..='z').collect::<Vec<_>>(),
            "digit" => ('0'..='9').collect::<Vec<_>>(),
            "lower" => ('a'..='z').collect::<Vec<_>>(),
            "upper" => ('A'..='Z').collect::<Vec<_>>(),
            "space" => vec![' ', '\t', '\n', '\r', '\x0b', '\x0c'],
            "blank" => vec![' ', '\t'],
            "cntrl" => (0_u8..=31_u8)
                .chain(iter::once(127_u8))
                .map(char::from)
                .collect::<Vec<_>>(),
            "graph" => (33_u8..=126_u8).map(char::from).collect::<Vec<_>>(),
            "print" => (32_u8..=126_u8).map(char::from).collect::<Vec<_>>(),
            "punct" => (33_u8..=47_u8)
                .chain(58_u8..=64_u8)
                .chain(91_u8..=96_u8)
                .chain(123_u8..=126_u8)
                .map(char::from)
                .collect::<Vec<_>>(),
            "xdigit" => ('0'..='9')
                .chain('A'..='F')
                .chain('a'..='f')
                .collect::<Vec<_>>(),
            "" => {
                return Err("tr: missing character class name '[::]'".to_string());
            }
            st => return Err(format!("tr: invalid character class ‘{st}’")),
        };

        operand_vec.extend(char_vec.into_iter().map(|ch| {
            Operand::Char(CharOperand {
                char: ch,
                char_repetition: CharRepetition::N(1_usize),
            })
        }));

        Ok(())
    }
}

mod setup {
    use crate::compare_deunicoded_chars;
    use crate::parsing::{CharOperand, CharRepetition, EquivOperand, Operand};
    use std::collections::{HashMap, HashSet};
    use std::error::Error;

    fn complement_operands(operand_slice: &[Operand]) -> Vec<Operand> {
        let mut hash_set = HashSet::<u8>::with_capacity(operand_slice.len());

        for op in operand_slice {
            match op {
                Operand::Char(char_operand) => {
                    let char = char_operand.char;

                    match u8::try_from(char) {
                        Ok(ue) => {
                            hash_set.insert(ue);
                        }
                        Err(_) => {
                            // Ignore
                        }
                    }
                }
                Operand::Equiv(_) => {
                    unreachable!();
                }
            }
        }

        let mut vec = Vec::<Operand>::new();

        for ue in 0_u8..=255_u8 {
            if !hash_set.contains(&ue) {
                vec.push(Operand::Char(CharOperand {
                    char: char::from(ue),
                    char_repetition: CharRepetition::N(1_usize),
                }));
            }
        }

        vec
    }

    // TODO
    // This should be optimized
    pub fn generate_for_translation(
        complement: bool,
        string1_operands: Vec<Operand>,
        string2_operands: Vec<Operand>,
    ) -> Result<ForTranslation, Box<dyn Error>> {
        let string1_operands_to_use = if complement {
            complement_operands(&string1_operands)
        } else {
            string1_operands
        };

        let mut char_repeating_total = 0_usize;

        let mut string1_operands_flattened = Vec::<Operand>::new();

        for op in string1_operands_to_use {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => match char_repetition {
                    CharRepetition::AsManyAsNeeded => {
                        return Err(Box::from(
                            "tr: the [c*] repeat construct may not appear in string1".to_owned(),
                        ));
                    }
                    CharRepetition::N(n) => {
                        char_repeating_total = char_repeating_total
                            .checked_add(n)
                            .ok_or("Arithmetic overflow")?;

                        let new_char = Operand::Char(CharOperand {
                            char,
                            char_repetition: CharRepetition::N(1_usize),
                        });

                        for _ in 0_usize..n {
                            string1_operands_flattened.push(new_char.clone());
                        }
                    }
                },
                op @ Operand::Equiv(_) => {
                    // Take up one position?
                    string1_operands_flattened.push(op);
                }
            }
        }

        // TODO
        // Indexing is a workaround for the borrow checker
        let mut as_many_as_needed_index = Option::<usize>::None;

        let mut replacement_char_repeating_total = 0_usize;

        for (us, op) in string2_operands.iter().enumerate() {
            match op {
                Operand::Char(CharOperand {
                    char_repetition, ..
                }) => match char_repetition {
                    CharRepetition::AsManyAsNeeded => {
                        if as_many_as_needed_index.is_some() {
                            // TODO
                            // Do this validation earlier?
                            return Err(Box::from(
                                "tr: only one [c*] repeat construct may appear in string2"
                                    .to_owned(),
                            ));
                        }

                        as_many_as_needed_index = Some(us);
                    }
                    CharRepetition::N(n) => {
                        replacement_char_repeating_total = replacement_char_repeating_total
                            .checked_add(*n)
                            .ok_or("Arithmetic overflow")?;
                    }
                },
                Operand::Equiv { .. } => {
                    // TODO
                    // Do this validation earlier?
                    return Err(Box::from(
                        "tr: [=c=] expressions may not appear in string2 when translating"
                            .to_owned(),
                    ));
                }
            }
        }

        let string2_operands_to_use = if replacement_char_repeating_total < char_repeating_total {
            let leftover = char_repeating_total
                .checked_sub(replacement_char_repeating_total)
                .ok_or("Arithmetic overflow")?;

            // TODO
            // to_vec
            let mut string2_operands_with_leftover = string2_operands.to_vec();

            match as_many_as_needed_index {
                Some(us) => {
                    let op = string2_operands_with_leftover
                        .get_mut(us)
                        .ok_or("Indexing failed")?;

                    match op {
                        Operand::Char(CharOperand {
                            ref mut char_repetition,
                            ..
                        }) => {
                            *char_repetition = CharRepetition::N(leftover);
                        }
                        Operand::Equiv(_) => {
                            unreachable!();
                        }
                    }
                }
                None => {
                    let mut n_updated = false;

                    for op in string2_operands_with_leftover.iter_mut().rev() {
                        if let Operand::Char(CharOperand {
                            char_repetition: CharRepetition::N(ref mut n),
                            ..
                        }) = op
                        {
                            let n_plus_leftover =
                                n.checked_add(leftover).ok_or("Arithmetic overflow")?;

                            *n = n_plus_leftover;

                            n_updated = true;

                            break;
                        }
                    }

                    assert!(n_updated);
                }
            }

            string2_operands_with_leftover
        } else {
            string2_operands
        };

        // TODO
        // Capacity
        let mut string2_operands_to_use_flattened = Vec::<char>::new();

        for op in string2_operands_to_use {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => match char_repetition {
                    CharRepetition::N(n) => {
                        for _ in 0_usize..n {
                            string2_operands_to_use_flattened.push(char);
                        }
                    }
                    CharRepetition::AsManyAsNeeded => {
                        // The "[c*]" construct was not needed, ignore it
                    }
                },
                Operand::Equiv(_) => {
                    unreachable!();
                }
            }
        }

        // TODO
        // Capacities
        let mut equiv_translation_chars = Vec::<(char, char)>::new();
        let mut multi_byte_translation_hash_map = HashMap::<char, char>::new();
        let mut single_byte_translation_lookup_table = [Option::<char>::None; 128_usize];

        let mut encoding_buffer = [0_u8; 4_usize];

        let mut add_normal_char = |ch: char, replacement_char: char| {
            let sl = ch.encode_utf8(&mut encoding_buffer);

            match sl.as_bytes() {
                &[ue] => {
                    single_byte_translation_lookup_table[usize::from(ue)] = Some(replacement_char);
                }
                _ => {
                    multi_byte_translation_hash_map.insert(ch, replacement_char);
                }
            }
        };

        for (us, op) in string1_operands_flattened.into_iter().enumerate() {
            let replacement_char = *(string2_operands_to_use_flattened
                .get(us)
                .ok_or("Indexing failed")?);

            match op {
                Operand::Char(CharOperand {
                    char,
                    char_repetition,
                }) => {
                    // TODO
                    // Enforce with types
                    assert!(matches!(char_repetition, CharRepetition::N(1_usize)));

                    add_normal_char(char, replacement_char);
                }
                Operand::Equiv(EquivOperand { char }) => {
                    equiv_translation_chars.push((char, replacement_char));

                    // TODO
                    // Fix for `tr_equivalence_class_low_priority`
                    add_normal_char(char, replacement_char);
                }
            }
        }

        let for_translate = ForTranslation {
            equiv_translation_chars,
            multi_byte_translation_hash_map,
            single_byte_translation_lookup_table: Box::new(single_byte_translation_lookup_table),
        };

        Ok(for_translate)
    }

    pub fn generate_for_removal(
        complement: bool,
        string1_operands: Vec<Operand>,
    ) -> Result<ForRemoval, Box<dyn Error>> {
        let mut equiv_removal_chars = Vec::<char>::new();
        let mut multi_byte_removal_hash_set = HashSet::<char>::new();
        let mut single_byte_removal_lookup_table = [false; 128_usize];

        let mut encoding_buffer = [0_u8; 4_usize];

        for op in string1_operands {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => match char_repetition {
                    CharRepetition::AsManyAsNeeded => {
                        return Err(Box::from(
                            "tr: the [c*] repeat construct may not appear in string1".to_owned(),
                        ));
                    }
                    CharRepetition::N(_) => {
                        let sl = char.encode_utf8(&mut encoding_buffer);

                        match sl.as_bytes() {
                            &[ue] => {
                                single_byte_removal_lookup_table[usize::from(ue)] = true;
                            }
                            _ => {
                                multi_byte_removal_hash_set.insert(char);
                            }
                        }
                    }
                },
                Operand::Equiv(EquivOperand { char }) => {
                    equiv_removal_chars.push(char);
                }
            }
        }

        let for_removal = ForRemoval {
            complement,
            multi_byte_removal_hash_set,
            single_byte_removal_lookup_table: Box::new(single_byte_removal_lookup_table),
            equiv_removal_chars,
        };

        Ok(for_removal)
    }

    pub struct ForTranslation {
        pub equiv_translation_chars: Vec<(char, char)>,
        pub multi_byte_translation_hash_map: HashMap<char, char>,
        pub single_byte_translation_lookup_table: Box<[Option<char>; 128_usize]>,
    }

    impl ForTranslation {
        pub fn char_has_translation(&self, ch: char, char_slice: &[u8]) -> Option<char> {
            let first_check = match char_slice {
                &[ue] => self.single_byte_translation_lookup_table[usize::from(ue)],
                _ => self.multi_byte_translation_hash_map.get(&ch).copied(),
            };

            match first_check {
                op @ Some(_) => {
                    return op;
                }
                None => {
                    for (cha, char) in &self.equiv_translation_chars {
                        if compare_deunicoded_chars(ch, *cha) {
                            return Some(*char);
                        }
                    }
                }
            }

            None
        }
    }

    pub struct ForRemoval {
        complement: bool,
        equiv_removal_chars: Vec<char>,
        multi_byte_removal_hash_set: HashSet<char>,
        single_byte_removal_lookup_table: Box<[bool; 128_usize]>,
    }

    impl ForRemoval {
        pub fn char_matches(&self, ch: char, char_slice: &[u8]) -> bool {
            let first_check = match char_slice {
                &[ue] => self.single_byte_removal_lookup_table[usize::from(ue)],
                _ => self.multi_byte_removal_hash_set.contains(&ch),
            };

            let first_check_to_use = if self.complement {
                !first_check
            } else {
                first_check
            };

            if first_check_to_use {
                return true;
            }

            // Second check
            for &cha in &self.equiv_removal_chars {
                let second_check = compare_deunicoded_chars(ch, cha);

                let second_check_to_use = if self.complement {
                    !second_check
                } else {
                    second_check
                };

                if second_check_to_use {
                    return true;
                }
            }

            false
        }
    }
}

mod transformation {
    use std::error::Error;
    use std::io::{self, ErrorKind, Read, Write};

    pub trait Transformation {
        fn transform(&mut self, input: &str, output: &mut Vec<u8>);
    }

    impl dyn Transformation {
        pub fn streaming_transform(&mut self) -> Result<(), Box<dyn Error>> {
            // Must be big enough to hold at least one UTF-8 character
            const SIZE: usize = 8_usize * 1_024_usize;

            // Buffers
            let mut input = vec![0_u8; SIZE];
            let mut output = Vec::<u8>::with_capacity(SIZE);
            let mut temporary_leftover = Vec::<u8>::new();

            // TODO
            // Improve this
            let mut leftover_bytes = 0_usize;

            let mut stdin_lock = io::stdin().lock();
            let mut stdout_lock = io::stdout().lock();

            loop {
                let buf = &mut input[leftover_bytes..];

                match stdin_lock.read(buf) {
                    Ok(0_usize) => {
                        assert!(leftover_bytes == 0_usize);

                        assert!(!buf.is_empty());

                        break;
                    }
                    Ok(us) => {
                        let read_slice = &input[..(leftover_bytes + us)];

                        let for_transform = match std::str::from_utf8(read_slice) {
                            Ok(st) => st,
                            Err(ut) => {
                                let (valid, remainder) = read_slice.split_at(ut.valid_up_to());

                                temporary_leftover.extend_from_slice(remainder);

                                // https://doc.rust-lang.org/std/str/struct.Utf8Error.html#examples
                                unsafe { std::str::from_utf8_unchecked(valid) }
                            }
                        };

                        self.transform(for_transform, &mut output);

                        // TODO
                        // Do this in a nicer way
                        for (us, ue) in temporary_leftover.iter().enumerate() {
                            // TODO
                            // Indexing
                            input[us] = *ue;
                        }

                        leftover_bytes = temporary_leftover.len();

                        temporary_leftover.clear();

                        stdout_lock.write_all(&output)?;

                        output.clear();
                    }
                    Err(er) => {
                        if er.kind() == ErrorKind::Interrupted {
                            continue;
                        }

                        return Err(Box::from(er));
                    }
                }
            }

            Ok(())
        }
    }

    pub mod delete {
        use super::Transformation;
        use crate::setup::ForRemoval;

        struct PreviousChar<'a> {
            char: char,
            bytes_to_print: Option<&'a [u8]>,
        }

        impl PreviousChar<'_> {
            fn get_owned(&self) -> PreviousCharOwned {
                PreviousCharOwned {
                    char: self.char,
                    bytes_to_print: self.bytes_to_print.map(|sl| sl.to_vec()),
                }
            }
        }

        #[derive(Clone)]
        pub struct PreviousCharOwned {
            char: char,
            bytes_to_print: Option<Vec<u8>>,
        }

        impl<'a> PreviousCharOwned {
            fn get_borrowed(&'a self) -> PreviousChar<'a> {
                PreviousChar {
                    char: self.char,
                    bytes_to_print: self.bytes_to_print.as_deref(),
                }
            }
        }

        #[derive(Clone)]
        pub enum DeleteState {
            Start,
            Continuation(PreviousCharOwned),
        }

        pub struct DeleteTransformation {
            pub delete_state: DeleteState,
            pub for_removal: ForRemoval,
        }

        impl Transformation for DeleteTransformation {
            fn transform(&mut self, input: &str, output: &mut Vec<u8>) {
                let mut chars = input.chars();

                let mut encoding_buffer = [0_u8; 4_usize];

                let previous_char_owned = match self.delete_state.to_owned() {
                    DeleteState::Continuation(pr) => pr,
                    DeleteState::Start => {
                        // First character
                        if let Some(ch) = chars.next() {
                            let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                            let delete_char = self.for_removal.char_matches(ch, char_slice);

                            if delete_char {
                                PreviousCharOwned {
                                    char: ch,
                                    bytes_to_print: None,
                                }
                            } else {
                                output.extend_from_slice(char_slice);

                                PreviousCharOwned {
                                    bytes_to_print: Some(char_slice.to_vec()),
                                    char: ch,
                                }
                            }
                        } else {
                            // No input to process
                            return;
                        }
                    }
                };

                let mut previous_char = previous_char_owned.get_borrowed();

                for ch in chars {
                    // Optimization for repeated deletions
                    // TODO
                    // Track state across calls to `transform`
                    if ch == previous_char.char {
                        if let Some(sl) = previous_char.bytes_to_print {
                            output.extend_from_slice(sl);
                        }

                        continue;
                    }

                    let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                    let delete_char = self.for_removal.char_matches(ch, char_slice);

                    previous_char = if delete_char {
                        PreviousChar {
                            char: ch,
                            bytes_to_print: None,
                        }
                    } else {
                        output.extend_from_slice(char_slice);

                        PreviousChar {
                            char: ch,
                            bytes_to_print: Some(char_slice),
                        }
                    };
                }

                self.delete_state = DeleteState::Continuation(previous_char.get_owned());
            }
        }
    }

    pub mod delete_and_squeeze {
        use super::Transformation;
        use crate::setup::ForRemoval;

        enum Behavior<'a> {
            Delete,
            Print {
                squeeze_char: bool,
                bytes_to_print: &'a [u8],
            },
        }

        #[derive(Clone)]
        pub enum BehaviorOwned {
            Delete,
            Print {
                squeeze_char: bool,
                bytes_to_print: Vec<u8>,
            },
        }

        #[derive(Clone)]
        pub struct LastPrintedChar {
            char: char,
            squeeze_char: bool,
        }

        pub struct PreviousChar<'a> {
            char: char,
            on_consecutive_appearance: Behavior<'a>,
        }

        impl PreviousChar<'_> {
            fn get_owned(&self) -> PreviousCharOwned {
                PreviousCharOwned {
                    char: self.char,
                    on_consecutive_appearance: match self.on_consecutive_appearance {
                        Behavior::Delete => BehaviorOwned::Delete,
                        Behavior::Print {
                            squeeze_char,
                            bytes_to_print,
                        } => BehaviorOwned::Print {
                            squeeze_char,
                            bytes_to_print: bytes_to_print.to_vec(),
                        },
                    },
                }
            }
        }

        #[derive(Clone)]
        pub struct PreviousCharOwned {
            char: char,
            on_consecutive_appearance: BehaviorOwned,
        }

        impl<'a> PreviousCharOwned {
            fn get_borrowed(&'a self) -> PreviousChar<'a> {
                PreviousChar {
                    char: self.char,
                    on_consecutive_appearance: match &self.on_consecutive_appearance {
                        BehaviorOwned::Delete => Behavior::Delete,
                        BehaviorOwned::Print {
                            squeeze_char,
                            bytes_to_print,
                        } => Behavior::Print {
                            squeeze_char: *squeeze_char,
                            bytes_to_print,
                        },
                    },
                }
            }
        }

        pub enum DeleteAndSqueezeState {
            Start,
            Continuation(Option<LastPrintedChar>, PreviousCharOwned),
        }

        pub struct DeleteAndSqueezeTransformation {
            pub delete_and_squeeze_state: DeleteAndSqueezeState,
            pub delete: ForRemoval,
            pub squeeze: ForRemoval,
        }

        impl Transformation for DeleteAndSqueezeTransformation {
            fn transform(&mut self, input: &str, output: &mut Vec<u8>) {
                let mut chars = input.chars();

                let mut encoding_buffer = [0_u8; 4_usize];

                // TODO
                // to_owned()
                let (mut last_printed_char_option, previous_char_state_owned) = match &self
                    .delete_and_squeeze_state
                {
                    DeleteAndSqueezeState::Continuation(op, pr) => (op.to_owned(), pr.to_owned()),
                    DeleteAndSqueezeState::Start => {
                        // First character
                        if let Some(ch) = chars.next() {
                            let sl = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                            let (op, behavior) = if self.delete.char_matches(ch, sl) {
                                // Do not print anything
                                (None, BehaviorOwned::Delete)
                            } else {
                                let squeeze_char = self.squeeze.char_matches(ch, sl);

                                output.extend_from_slice(sl);

                                (
                                    Some(LastPrintedChar {
                                        char: ch,
                                        squeeze_char,
                                    }),
                                    BehaviorOwned::Print {
                                        squeeze_char,
                                        bytes_to_print: sl.to_vec(),
                                    },
                                )
                            };

                            (
                                op,
                                PreviousCharOwned {
                                    char: ch,
                                    on_consecutive_appearance: behavior,
                                },
                            )
                        } else {
                            // No input to process
                            return;
                        }
                    }
                };

                let mut previous_char_state = previous_char_state_owned.get_borrowed();

                for ch in chars {
                    if ch == previous_char_state.char {
                        match previous_char_state.on_consecutive_appearance {
                            Behavior::Delete => {
                                // Do not print anything
                            }
                            Behavior::Print {
                                squeeze_char,
                                bytes_to_print,
                            } => {
                                if squeeze_char {
                                    match last_printed_char_option {
                                        Some(LastPrintedChar { char, .. }) if ch == char => {
                                            // Do not print anything
                                        }
                                        _ => {
                                            output.extend_from_slice(bytes_to_print);
                                        }
                                    }
                                } else {
                                    output.extend_from_slice(bytes_to_print);
                                }
                            }
                        }

                        continue;
                    }

                    let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                    /* #region Delete */
                    let delete_char = self.delete.char_matches(ch, char_slice);

                    if delete_char {
                        previous_char_state = PreviousChar {
                            char: ch,
                            on_consecutive_appearance: Behavior::Delete,
                        };

                        continue;
                    }
                    /* #endregion */

                    /* #region Squeeze */
                    match last_printed_char_option {
                        Some(LastPrintedChar { char, squeeze_char }) if ch == char => {
                            if squeeze_char {
                                // Squeeze
                            } else {
                                // Do not squeeze
                                output.extend_from_slice(char_slice);
                            }

                            previous_char_state = PreviousChar {
                                char: ch,
                                on_consecutive_appearance: Behavior::Print {
                                    squeeze_char,
                                    bytes_to_print: char_slice,
                                },
                            };
                        }
                        _ => {
                            let squeeze_char = self.squeeze.char_matches(ch, char_slice);

                            previous_char_state = PreviousChar {
                                char: ch,
                                on_consecutive_appearance: Behavior::Print {
                                    squeeze_char,
                                    bytes_to_print: char_slice,
                                },
                            };

                            last_printed_char_option = Some(LastPrintedChar {
                                char: ch,
                                squeeze_char,
                            });

                            output.extend_from_slice(char_slice);
                        }
                    }
                    /* #endregion */
                }

                self.delete_and_squeeze_state = DeleteAndSqueezeState::Continuation(
                    last_printed_char_option,
                    previous_char_state.get_owned(),
                );
            }
        }
    }

    pub mod squeeze {
        use super::Transformation;
        use crate::setup::ForRemoval;

        pub struct PreviousChar {
            char: char,
            squeeze_char: bool,
            bytes_to_print: Vec<u8>,
        }

        pub enum SqueezeState {
            Start,
            Continuation(PreviousChar),
        }

        pub struct SqueezeTransformation {
            pub for_removal: ForRemoval,
            pub squeeze_state: SqueezeState,
        }

        impl Transformation for SqueezeTransformation {
            fn transform(&mut self, input: &str, output: &mut Vec<u8>) {
                let mut chars = input.chars();

                let mut encoding_buffer = [0_u8; 4_usize];

                // TODO
                // Improve code style
                let (
                    mut previous_char,
                    mut previous_char_squeeze_char,
                    mut previous_char_bytes_to_print,
                ) = match &self.squeeze_state {
                    SqueezeState::Continuation(sq) => {
                        (sq.char, sq.squeeze_char, sq.bytes_to_print.as_slice())
                    }
                    SqueezeState::Start => {
                        // First character
                        let Some(char) = chars.next() else {
                            // No input to process
                            return;
                        };

                        let char_slice = char.encode_utf8(&mut encoding_buffer).as_bytes();

                        // Add the first char
                        output.extend_from_slice(char_slice);

                        let squeeze_char = self.for_removal.char_matches(char, char_slice);

                        (char, squeeze_char, char_slice)
                    }
                };

                for ch in chars {
                    if ch == previous_char {
                        if !previous_char_squeeze_char {
                            output.extend_from_slice(previous_char_bytes_to_print);
                        }

                        continue;
                    }

                    let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                    output.extend_from_slice(char_slice);

                    // Atomic, do all of these or none
                    {
                        previous_char = ch;
                        previous_char_bytes_to_print = char_slice;
                        previous_char_squeeze_char = self.for_removal.char_matches(ch, char_slice);
                    }
                }

                // Persist state for handling of next chunk
                self.squeeze_state = SqueezeState::Continuation(PreviousChar {
                    bytes_to_print: previous_char_bytes_to_print.to_vec(),
                    char: previous_char,
                    squeeze_char: previous_char_squeeze_char,
                });
            }
        }
    }

    pub mod squeeze_and_translate {
        use super::Transformation;
        use crate::setup::ForTranslation;

        #[derive(Clone)]
        pub struct PreviousChar {
            char: char,
            translate_char: Option<char>,
            last_printed_bytes: Vec<u8>,
        }

        pub enum SqueezeAndTranslateState {
            Start,
            Continuation(PreviousChar),
        }

        pub struct SqueezeAndTranslateTransformation {
            pub for_translation: ForTranslation,
            pub squeeze_and_translate_state: SqueezeAndTranslateState,
        }

        impl Transformation for SqueezeAndTranslateTransformation {
            fn transform(&mut self, input: &str, output: &mut Vec<u8>) {
                let mut chars = input.chars();

                let mut encoding_buffer = [0_u8; 4_usize];

                let mut previous_char = match &self.squeeze_and_translate_state {
                    SqueezeAndTranslateState::Continuation(pr) => pr.to_owned(),
                    SqueezeAndTranslateState::Start => {
                        // First character
                        let Some(char) = chars.next() else {
                            // No input to process
                            return;
                        };

                        let char_vec = char.encode_utf8(&mut encoding_buffer).as_bytes().to_vec();

                        let translate_char =
                            self.for_translation.char_has_translation(char, &char_vec);

                        let char_vec_to_use = match translate_char {
                            Some(replacement_char) => {
                                let replacement_char_str =
                                    replacement_char.encode_utf8(&mut encoding_buffer);

                                replacement_char_str.as_bytes().to_vec()
                            }
                            None => char_vec.clone(),
                        };

                        // Add the first char
                        output.extend_from_slice(char_vec_to_use.as_slice());

                        PreviousChar {
                            char,
                            last_printed_bytes: char_vec_to_use,
                            translate_char,
                        }
                    }
                };

                for ch in chars {
                    if ch == previous_char.char {
                        if previous_char.translate_char.is_some() {
                            // Have the same translation because they're the same character, so will need to be squeezed
                        } else {
                            // Fast path for repeated, non-squeezed characters
                            output.extend_from_slice(previous_char.last_printed_bytes.as_slice());
                        }

                        continue;
                    }

                    let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                    let translate_char = self.for_translation.char_has_translation(ch, char_slice);

                    match (translate_char, previous_char.translate_char) {
                        (Some(ch), Some(cha)) if ch == cha => {
                            // Squeeze if not the same character as the last, but has the same translation
                            previous_char.char = ch;

                            continue;
                        }
                        _ => {}
                    }

                    let char_slice_to_use = match translate_char {
                        Some(replacement_char) => {
                            let replacement_char_str =
                                replacement_char.encode_utf8(&mut encoding_buffer);

                            replacement_char_str.as_bytes()
                        }
                        None => char_slice,
                    };

                    output.extend_from_slice(char_slice_to_use);

                    previous_char.char = ch;
                    previous_char.translate_char = translate_char;

                    // TODO
                    previous_char.last_printed_bytes.clear();
                    previous_char
                        .last_printed_bytes
                        .extend_from_slice(char_slice_to_use);
                }

                // Persist state for handling of next chunk
                // TODO
                // Improve code style
                self.squeeze_and_translate_state =
                    SqueezeAndTranslateState::Continuation(previous_char);
            }
        }
    }

    pub mod translate {
        use super::Transformation;
        use crate::{compare_deunicoded_chars, setup::ForTranslation};

        pub struct TranslateTransformation {
            pub for_translation: ForTranslation,
        }

        impl Transformation for TranslateTransformation {
            fn transform(&mut self, input: &str, output: &mut Vec<u8>) {
                let mut encoding_buffer = [0_u8; 4_usize];

                for ch in input.chars() {
                    let char_slice = ch.encode_utf8(&mut encoding_buffer).as_bytes();

                    let replacement_char_option = match char_slice {
                        &[ue] => {
                            self.for_translation.single_byte_translation_lookup_table
                                [usize::from(ue)]
                        }
                        _ => self
                            .for_translation
                            .multi_byte_translation_hash_map
                            .get(&ch)
                            .copied(),
                    };

                    // TODO
                    // What is the precedence of equivalence classes?
                    // GNU Core Utilities de-prioritizes them

                    // Only use equivalence classes if normal translation failed
                    let replacement_char_option_to_use = match replacement_char_option {
                        Some(cha) => Some(cha),
                        None => {
                            let mut op = Option::<char>::None;

                            for (equiv_char, equiv_char_replacement) in
                                &self.for_translation.equiv_translation_chars
                            {
                                if compare_deunicoded_chars(ch, *equiv_char) {
                                    op = Some(*equiv_char_replacement);

                                    break;
                                }
                            }

                            op
                        }
                    };

                    let for_output = match replacement_char_option_to_use {
                        Some(replacement_char) => {
                            let replacement_char_str =
                                replacement_char.encode_utf8(&mut encoding_buffer);

                            replacement_char_str.as_bytes()
                        }
                        None => char_slice,
                    };

                    output.extend_from_slice(for_output);
                }
            }
        }
    }
}

// TODO
// No other implementations actually seem to do anything with equivalence classes:
// they seem to just treat them as the parsed character (e.g. "[=a=]" is treated as "a").
// Should this implementation continue to try to actually handle them?

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
pub fn compare_deunicoded_chars(char1: char, char2: char) -> bool {
    let normalized_char1 = deunicode_char(char1);
    let normalized_char2 = deunicode_char(char2);

    match (normalized_char1, normalized_char2) {
        (Some(st), Some(str)) => st == str,
        (None, None) => {
            // Purpose of match: prevent this from being considered equal
            false
        }
        _ => false,
    }
}
