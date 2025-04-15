use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use setup::{ForRemoval, ForTranslation};
use std::error::Error;
use std::process;
use transformation::delete::DeleteTransformation;
use transformation::delete_and_squeeze::{DeleteAndSqueezeState, DeleteAndSqueezeTransformation};
use transformation::squeeze::{SqueezeState, SqueezeTransformation};
use transformation::squeeze_and_translate::{
    SqueezeAndTranslateState, SqueezeAndTranslateTransformation,
};
use transformation::streaming_transform;
use transformation::translate::TranslateTransformation;

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
            return Err("options '-c' and '-C' cannot be used together".to_owned());
        }

        match &self.string2 {
            Some(st) => {
                if self.delete && !self.squeeze_repeats {
                    return Err(format!(
                        "\
extra operand '{st}'
Only one string may be given when deleting without squeezing repeats."
                    ));
                }
            }
            None => {
                if !self.delete && !self.squeeze_repeats {
                    return Err(format!(
                        "missing operand after '{}'. Two strings must be given when translating.",
                        self.string1
                    ));
                }
            }
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
    let string1_operands = parsing::parse_string1_or_string2(&args.string1)?;

    let string2_operands = match &args.string2 {
        Some(st) => Some(parsing::parse_string1_or_string2(st)?),
        None => None,
    };

    // TODO
    // How are these different?
    let complement = args.complement_char || args.complement_val;

    let result = match (args.delete, args.squeeze_repeats) {
        (false, false) => {
            // "tr"
            let string2_operands = match string2_operands {
                Some(ve) => ve,
                None => {
                    return Err(Box::from("missing operand".to_owned()));
                }
            };

            if string2_operands.is_empty() {
                return Err(Box::from(
                    "when not truncating set1, string2 must be non-empty".to_owned(),
                ));
            }

            let for_translation = setup::generate_for_translation(
                complement,
                string1_operands,
                string2_operands.as_slice(),
            )?;

            match for_translation {
                ForTranslation::Complemented(complemented_translation) => {
                    let mut t = TranslateTransformation {
                        translation: *complemented_translation,
                    };

                    streaming_transform(&mut t)
                }
                ForTranslation::NotComplemented(not_complemented_translation) => {
                    let mut t = TranslateTransformation {
                        translation: *not_complemented_translation,
                    };

                    streaming_transform(&mut t)
                }
            }
        }
        (true, false) => {
            // "tr -d"
            let for_removal = setup::generate_for_removal(complement, string1_operands, true)?;

            match for_removal {
                ForRemoval::Complemented(complemented_removal) => {
                    let mut t = DeleteTransformation {
                        removal: *complemented_removal,
                    };

                    streaming_transform(&mut t)
                }
                ForRemoval::NotComplemented(not_complemented_removal) => {
                    let mut t = DeleteTransformation {
                        removal: *not_complemented_removal,
                    };

                    streaming_transform(&mut t)
                }
            }
        }
        (false, true) => {
            // "tr -s"
            match string2_operands {
                Some(string2_operands) => {
                    if string2_operands.is_empty() {
                        return Err(Box::from(
                            "when not truncating set1, string2 must be non-empty".to_owned(),
                        ));
                    }

                    let for_translation = setup::generate_for_translation(
                        complement,
                        string1_operands,
                        string2_operands.as_slice(),
                    )?;

                    // Complement does not apply to string2
                    let squeeze_for_removal =
                        setup::generate_for_removal(false, string2_operands, false)?;

                    let ForRemoval::NotComplemented(squeeze) = squeeze_for_removal else {
                        unreachable!()
                    };

                    match for_translation {
                        ForTranslation::Complemented(bo) => {
                            let mut t = SqueezeAndTranslateTransformation {
                                translation: *bo,
                                squeeze: *squeeze,
                                squeeze_and_translate_state: SqueezeAndTranslateState {
                                    last_printed_character: None,
                                },
                            };

                            streaming_transform(&mut t)
                        }
                        ForTranslation::NotComplemented(bo) => {
                            let mut t = SqueezeAndTranslateTransformation {
                                translation: *bo,
                                squeeze: *squeeze,
                                squeeze_and_translate_state: SqueezeAndTranslateState {
                                    last_printed_character: None,
                                },
                            };

                            streaming_transform(&mut t)
                        }
                    }
                }
                None => {
                    let for_removal =
                        setup::generate_for_removal(complement, string1_operands, true)?;

                    match for_removal {
                        ForRemoval::Complemented(bo) => {
                            let mut t = SqueezeTransformation {
                                squeeze: *bo,
                                squeeze_state: SqueezeState {
                                    last_printed_character: None,
                                },
                            };

                            streaming_transform(&mut t)
                        }
                        ForRemoval::NotComplemented(bo) => {
                            let mut t = SqueezeTransformation {
                                squeeze: *bo,
                                squeeze_state: SqueezeState {
                                    last_printed_character: None,
                                },
                            };

                            streaming_transform(&mut t)
                        }
                    }
                }
            }
        }
        (true, true) => {
            // "tr -d -s"
            let string2_operands = match string2_operands {
                Some(ve) => ve,
                None => {
                    return Err(Box::from("missing operand".to_owned()));
                }
            };

            // "The same string cannot be used for both the -d and the -s option; when both options are specified, both string1 (used for deletion) and string2 (used for squeezing) shall be required."
            let delete_for_removal =
                setup::generate_for_removal(complement, string1_operands, true)?;

            // Complement does not apply to squeeze, only delete, in this case
            let squeeze_for_removal = setup::generate_for_removal(false, string2_operands, false)?;

            let ForRemoval::NotComplemented(squeeze) = squeeze_for_removal else {
                unreachable!()
            };

            match delete_for_removal {
                ForRemoval::Complemented(bo) => {
                    let mut t = DeleteAndSqueezeTransformation {
                        delete: *bo,
                        delete_and_squeeze_state: DeleteAndSqueezeState {
                            last_printed_character: None,
                        },
                        squeeze: *squeeze,
                    };

                    streaming_transform(&mut t)
                }
                ForRemoval::NotComplemented(bo) => {
                    let mut t = DeleteAndSqueezeTransformation {
                        delete: *bo,
                        delete_and_squeeze_state: DeleteAndSqueezeState {
                            last_printed_character: None,
                        },
                        squeeze: *squeeze,
                    };

                    streaming_transform(&mut t)
                }
            }
        }
    };

    result?;

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if let Err(error_string) = args.validate_args() {
        eprintln!("tr: {error_string}");

        process::exit(1_i32);
    }

    if let Err(error) = tr(&args) {
        eprintln!("tr: {error}");

        process::exit(1_i32);
    }

    Ok(())
}

mod parsing {
    use std::iter::{self, Peekable};
    use std::str::Chars;

    use crate::setup::FullChar;

    #[derive(Clone)]
    pub enum CharRepetition {
        AsManyAsNeeded,
        N(usize),
    }

    #[derive(Clone)]
    pub struct CharOperand {
        // The character
        pub char: DataTypeWithData,
        // The number of times the character is repeated
        pub char_repetition: CharRepetition,
    }

    #[derive(Clone)]
    pub struct EquivOperand {
        // The character equivalent
        pub char: DataTypeWithData,
    }

    #[derive(Clone)]
    pub enum Operand {
        Char(CharOperand),
        Equiv(EquivOperand),
    }

    // TODO
    // Optimize
    pub fn categorize_char(char: char) -> DataTypeWithData {
        let mut encoding_buffer = [0_u8; 4_usize];

        let slice = char.encode_utf8(&mut encoding_buffer).as_bytes();

        match slice {
            &[single_byte] => categorize_byte(single_byte),
            _ => DataTypeWithData::IsMultiByte(char),
        }
    }

    pub fn categorize_byte(byte: u8) -> DataTypeWithData {
        if (0_u8..128_u8).contains(&byte) {
            DataTypeWithData::Is7Bit(byte)
        } else {
            DataTypeWithData::Is8Bit(byte)
        }
    }

    #[derive(Clone)]
    pub enum DataTypeWithData {
        Is7Bit(u8),
        Is8Bit(u8),
        IsMultiByte(char),
    }

    impl DataTypeWithData {
        pub fn convert_to_replacement(&self) -> FullChar {
            match self {
                Self::IsMultiByte(ch) => FullChar::new_from_char(*ch),
                Self::Is7Bit(ue) | Self::Is8Bit(ue) => FullChar::new_from_u8(*ue),
            }
        }

        pub fn convert_to_char(&self) -> char {
            match *self {
                Self::IsMultiByte(ch) => ch,
                Self::Is7Bit(ue) => char::from(ue),
                _ => {
                    // 8-bit equiv?
                    unreachable!()
                }
            }
        }

        // TODO
        // Optimize
        fn printable(&self) -> String {
            match *self {
                DataTypeWithData::IsMultiByte(ch) => ch.escape_default().to_string(),
                DataTypeWithData::Is7Bit(ue) => char::from(ue).escape_default().to_string(),
                DataTypeWithData::Is8Bit(ue) => {
                    format!("\\{ue:03o}")
                }
            }
        }
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

                        if let Some(op) = parse_repeated_char(&square_bracket_constructs_buffer)? {
                            // "[x*n]" construct
                            operand_vec.push(op);

                            continue;
                        }
                    }

                    // Not a "[:class:]", "[=equiv=]", or "[x*n]" construct
                    // The hacky way to continue is to reset `peekable` (to `peekable_saved`)
                    // This moves the `Peekable` back to the point it was at before attempting to parse square
                    // bracket constructs
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
                return Err(
                    "input '[==]' is invalid: missing equivalence class character".to_owned(),
                );
            }
            sl => {
                const ERROR_MESSAGE_SUFFIX: &str =
                    ": equivalence class operand must be a single character";

                const ERROR_MESSAGE_SUFFIX_LENGTH: usize = ERROR_MESSAGE_SUFFIX.len();

                let mut error_message =
                    String::with_capacity(ERROR_MESSAGE_SUFFIX_LENGTH + sl.len());

                for &ch in sl {
                    error_message.push(ch);
                }

                error_message.push_str(ERROR_MESSAGE_SUFFIX);

                return Err(error_message);
            }
        };

        let char = categorize_char(char_between_equals_signs);

        // TODO
        // Validate this char
        let operand = Operand::Equiv(EquivOperand { char });

        Ok(operand)
    }

    fn parse_repeated_char(
        square_bracket_constructs_buffer: &[char],
    ) -> Result<Option<Operand>, String> {
        // TODO
        // Clean this up
        fn fill_repeat_str(iter: &mut Peekable<Chars>, repeat_string: &mut String) {
            while let Some(ch) = iter.next() {
                if ch == ']' {
                    assert!(iter.next().is_none());

                    return;
                }

                repeat_string.push(ch);
            }

            unreachable!();
        }

        // TODO
        // Performance
        let square_bracket_constructs_buffer_string =
            square_bracket_constructs_buffer.iter().collect::<String>();

        let mut peekable = square_bracket_constructs_buffer_string.chars().peekable();

        // Skip '['
        assert!(peekable.next() == Some('['));

        let parse_single_char_result = parse_single_char(&mut peekable)?;

        let Some(char) = parse_single_char_result else {
            return Err(format!("could not parse [x*n] construct: bad input near \"{square_bracket_constructs_buffer_string}\""));
        };

        // Skip '*'
        if peekable.next() != Some('*') {
            // Cannot parse as a repeated character
            return Ok(None);
        }

        let mut repeat_string = String::with_capacity(square_bracket_constructs_buffer.len());

        fill_repeat_str(&mut peekable, &mut repeat_string);

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
                        return Err(format!("invalid repeat count '{st}' in [c*n] construct",));
                    }
                }
            }
        };

        let operand = Operand::Char(CharOperand {
            char,
            char_repetition,
        });

        Ok(Some(operand))
    }

    // TODO
    // How should multiple consecutive octal sequences which together compose a valid UTF-8 character be handled?
    // For example:
    // ❯ printf '\303\274\n'
    // ü
    fn parse_octal_sequence(
        first_octal_digit: char,
        peekable: &mut Peekable<Chars>,
    ) -> Result<DataTypeWithData, String> {
        let mut st = String::with_capacity(3_usize);

        st.push(first_octal_digit);

        let mut added_octal_digit_to_buffer = |pe: &mut Peekable<Chars>| {
            if let Some(&octal_digit @ '0'..='7') = pe.peek() {
                st.push(octal_digit);

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
                return Err(format!("failed to parse octal sequence '{st}' ({pa})"));
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
                        return Err(format!("invalid octal sequence '{chars_str}' ({pa})"));
                    }
                }
            }
        };

        let data_type_with_data = categorize_byte(byte);

        Ok(data_type_with_data)
    }

    fn parse_single_char(
        peekable: &mut Peekable<Chars>,
    ) -> Result<Option<DataTypeWithData>, String> {
        let option = match peekable.next() {
            Some('\\') => {
                let data_type_with_data = match peekable.next() {
                    /* #region \octal */
                    Some(first_octal_digit @ '0'..='7') => {
                        parse_octal_sequence(first_octal_digit, peekable)?
                    }
                    /* #endregion */
                    //
                    /* #region \character */
                    // <alert>
                    // Code point 0007
                    Some('a') => categorize_char('\u{0007}'),
                    // <backspace>
                    // Code point 0008
                    Some('b') => categorize_char('\u{0008}'),
                    // <tab>
                    // Code point 0009
                    Some('t') => categorize_char('\u{0009}'),
                    // <newline>
                    // Code point 000A
                    Some('n') => categorize_char('\u{000A}'),
                    // <vertical-tab>
                    // Code point 000B
                    Some('v') => categorize_char('\u{000B}'),
                    // <form-feed>
                    // Code point 000C
                    Some('f') => categorize_char('\u{000C}'),
                    // <carriage-return>
                    // Code point 000D
                    Some('r') => categorize_char('\u{000D}'),
                    // <backslash>
                    // Code point 005C
                    Some('\\') => {
                        // An escaped backslash
                        categorize_char('\u{005C}')
                    }
                    /* #endregion */
                    //
                    Some(cha) => {
                        // If a backslash is not at the end of the string, and is not followed by one of the valid
                        // escape characters (including another backslash), the backslash is basically just ignored:
                        // the following character is the character added to the set.
                        categorize_char(cha)
                    }
                    None => {
                        eprintln!(
                            "tr: warning: an unescaped backslash at end of string is not portable"
                        );

                        // If an unescaped backslash is the last character of the string, treat it as though it were
                        // escaped (backslash is added to the set)
                        categorize_char('\u{005C}')
                    }
                };

                Some(data_type_with_data)
            }
            op => op.map(categorize_char),
        };

        Ok(option)
    }

    fn parse_range_or_single_char(
        starting: DataTypeWithData,
        peekable: &mut Peekable<Chars>,
        operand_vec: &mut Vec<Operand>,
    ) -> Result<(), String> {
        fn backwards_range_error(da: DataTypeWithData, dat: DataTypeWithData) -> String {
            format!(
                "range-endpoints of '{}-{}' are in reverse collating sequence order",
                da.printable(),
                dat.printable()
            )
        }

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
                        let operands_to_add = match (starting, after_hyphen) {
                            (
                                da @ DataTypeWithData::Is7Bit(ue)
                                | da @ DataTypeWithData::Is8Bit(ue),
                                dat @ DataTypeWithData::Is7Bit(uei)
                                | dat @ DataTypeWithData::Is8Bit(uei),
                            ) => {
                                let range_inclusive = ue..=uei;

                                if range_inclusive.is_empty() {
                                    return Err(backwards_range_error(da, dat));
                                }

                                range_inclusive
                                    .map(|ue| {
                                        Operand::Char(CharOperand {
                                            char: categorize_byte(ue),
                                            char_repetition: CharRepetition::N(1_usize),
                                        })
                                    })
                                    .collect::<Vec<_>>()
                            }
                            (
                                da @ DataTypeWithData::Is7Bit(_)
                                | da @ DataTypeWithData::IsMultiByte(_),
                                dat @ DataTypeWithData::Is7Bit(_)
                                | dat @ DataTypeWithData::IsMultiByte(_),
                            ) => {
                                let ch = da.convert_to_char();
                                let cha = dat.convert_to_char();

                                let range_inclusive = ch..=cha;

                                if range_inclusive.is_empty() {
                                    return Err(backwards_range_error(da, dat));
                                }

                                range_inclusive
                                    .map(|ch| {
                                        Operand::Char(CharOperand {
                                            char: categorize_char(ch),
                                            char_repetition: CharRepetition::N(1_usize),
                                        })
                                    })
                                    .collect::<Vec<_>>()
                            }
                            _ => {
                                // TODO
                                return Err("cannot produce a range between 8 bit and multi-byte characters".to_string());
                            }
                        };

                        operand_vec.extend_from_slice(operands_to_add.as_slice());
                    }
                    None => {
                        // End of input, do not handle as a range
                        // e.g. "tr 'ab' 'c-'"
                        operand_vec.extend_from_slice(
                            [
                                Operand::Char(CharOperand {
                                    char: starting,
                                    char_repetition: CharRepetition::N(1_usize),
                                }),
                                Operand::Char(CharOperand {
                                    // TODO
                                    // Optimize
                                    char: categorize_char(hyphen),
                                    char_repetition: CharRepetition::N(1_usize),
                                }),
                            ]
                            .as_slice(),
                        );
                    }
                }
            }
            _ => {
                // Not a "c-c" construct
                operand_vec.push(Operand::Char(CharOperand {
                    char: starting,
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
                return Err("input '[::]' is invalid: missing character class name".to_string());
            }
            st => {
                return Err(format!(
                    "input '[:{st}:]' is invalid: invalid character class '{st}'"
                ))
            }
        };

        operand_vec.extend(char_vec.into_iter().map(|ch| {
            Operand::Char(CharOperand {
                char: categorize_char(ch),
                char_repetition: CharRepetition::N(1_usize),
            })
        }));

        Ok(())
    }
}

mod setup {
    use crate::parsing::{CharOperand, CharRepetition, DataTypeWithData, EquivOperand, Operand};
    use std::error::Error;

    fn add_normal_char(
        data_type_with_data: DataTypeWithData,
        seven_bit: &mut [bool; 128_usize],
        eight_bit: &mut [bool; 128_usize],
        multi_byte: &mut [Option<Vec<Search>>; 128_usize],
    ) {
        match data_type_with_data {
            DataTypeWithData::Is7Bit(ue) => {
                let index = usize::from(ue);

                seven_bit[index] = true;
            }
            DataTypeWithData::Is8Bit(ue) => {
                let adjusted = ue - 128_u8;

                let index = usize::from(adjusted);

                eight_bit[index] = true;
            }
            DataTypeWithData::IsMultiByte(ch) => {
                let mut encoding_buffer = [0_u8; 4_usize];

                let st = ch.encode_utf8(&mut encoding_buffer);

                let &[ue, ref rest @ ..] = st.as_bytes() else {
                    unreachable!();
                };

                let adjusted = ue - 128_u8;

                let index = usize::from(adjusted);

                // TODO
                if multi_byte[index].is_none() {
                    multi_byte[index] = Some(Vec::<Search>::new())
                }

                // TODO
                let vec = multi_byte.get_mut(index).unwrap().as_mut().unwrap();

                let search = match *rest {
                    [a] => Search {
                        number_of_bytes: SearchNumberOfBytes::One,
                        payload: [a, 0_u8, 0_u8],
                    },
                    [a, b] => Search {
                        number_of_bytes: SearchNumberOfBytes::Two,
                        payload: [a, b, 0_u8],
                    },
                    [a, b, c] => Search {
                        number_of_bytes: SearchNumberOfBytes::Three,
                        payload: [a, b, c],
                    },
                    _ => {
                        unreachable!();
                    }
                };

                // TODO
                // Order?
                vec.push(search);
            }
        };
    }

    // TODO
    // This should be optimized
    pub fn generate_for_translation(
        complement: bool,
        string1_operands: Vec<Operand>,
        string2_operands: &[Operand],
    ) -> Result<ForTranslation, Box<dyn Error>> {
        let mut char_repeating_total = 0_usize;

        let mut string1_operands_flattened = Vec::<Operand>::new();

        for op in string1_operands {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => match char_repetition {
                    CharRepetition::AsManyAsNeeded => {
                        return Err(Box::from(
                            "the [c*] repeat construct may not appear in string1".to_owned(),
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
                                "only one [c*] repeat construct may appear in string2".to_owned(),
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
                        "[=c=] expressions may not appear in string2 when translating".to_owned(),
                    ));
                }
            }
        }

        let translation = if complement {
            // Replacement is: the only repeat construct, or the last character. No equiv is allowed.
            let replacement = match as_many_as_needed_index {
                Some(us) => {
                    let Some(Operand::Char(CharOperand {
                        char_repetition: CharRepetition::AsManyAsNeeded,
                        char,
                    })) = string2_operands.get(us)
                    else {
                        unreachable!();
                    };

                    char.convert_to_replacement()
                }
                None => {
                    let Some(Operand::Char(CharOperand {
                        char_repetition: CharRepetition::N(_),
                        char,
                    })) = string2_operands.iter().next_back()
                    else {
                        // TODO
                        unreachable!();
                    };

                    char.convert_to_replacement()
                }
            };

            let mut equiv = Vec::<DataTypeWithData>::new();

            let mut seven_bit = [false; 128_usize];
            let mut eight_bit = [false; 128_usize];
            let mut multi_byte = [const { Option::<Vec<Search>>::None }; 128_usize];

            {
                for op in string1_operands_flattened.into_iter() {
                    match op {
                        Operand::Char(CharOperand {
                            char,
                            char_repetition,
                        }) => {
                            // TODO
                            // Enforce with types
                            assert!(matches!(char_repetition, CharRepetition::N(1_usize)));

                            add_normal_char(char, &mut seven_bit, &mut eight_bit, &mut multi_byte);
                        }
                        Operand::Equiv(EquivOperand { char }) => {
                            equiv.push(char.clone());

                            // TODO
                            // Fix for `tr_equivalence_class_low_priority`
                            add_normal_char(char, &mut seven_bit, &mut eight_bit, &mut multi_byte);
                        }
                    }
                }
            }

            ForTranslation::Complemented(Box::new(ComplementedTranslation {
                replacement,
                seven_bit,
                eight_bit,
                multi_byte,
                equiv,
            }))
        } else {
            // Hoist for lifetime
            let string2_operands_with_leftover;

            let string2_operands_to_use = if replacement_char_repeating_total < char_repeating_total
            {
                let leftover = char_repeating_total
                    .checked_sub(replacement_char_repeating_total)
                    .ok_or("Arithmetic overflow")?;

                // TODO
                // to_vec
                let mut vec = string2_operands.to_vec();

                match as_many_as_needed_index {
                    Some(us) => {
                        let op = vec.get_mut(us).ok_or("Indexing failed")?;

                        match op {
                            Operand::Char(CharOperand {
                                char_repetition, ..
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

                        for op in vec.iter_mut().rev() {
                            if let Operand::Char(CharOperand {
                                char_repetition: CharRepetition::N(n),
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

                string2_operands_with_leftover = vec;

                &string2_operands_with_leftover
            } else {
                string2_operands
            };

            // TODO
            // Capacity
            let mut string2_operands_to_use_flattened = Vec::<DataTypeWithData>::new();

            for op in string2_operands_to_use {
                match op {
                    Operand::Char(CharOperand {
                        char_repetition,
                        char,
                    }) => match char_repetition {
                        CharRepetition::N(n) => {
                            for _ in 0_usize..(*n) {
                                string2_operands_to_use_flattened.push(char.to_owned());
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

            let mut equiv = Vec::<(DataTypeWithData, FullChar)>::new();

            let mut seven_bit = [const { Option::<FullChar>::None }; 128_usize];
            let mut eight_bit = [const { Option::<FullChar>::None }; 256_usize];
            let mut multi_byte = [const { Option::<Vec<SearchAndReplace>>::None }; 256_usize];

            let mut encoding_buffer = [0_u8; 4_usize];

            let mut add_normal_char_with_replacement =
                |da: DataTypeWithData, replacement_char: FullChar| {
                    match da {
                        DataTypeWithData::Is7Bit(ue) => {
                            let index = usize::from(ue);

                            seven_bit[index] = Some(replacement_char);
                        }
                        DataTypeWithData::Is8Bit(ue) => {
                            let adjusted = ue - 128_u8;

                            let index = usize::from(adjusted);

                            eight_bit[index] = Some(replacement_char);
                        }
                        DataTypeWithData::IsMultiByte(ch) => {
                            let st = ch.encode_utf8(&mut encoding_buffer);

                            let &[ue, ref rest @ ..] = st.as_bytes() else {
                                unreachable!();
                            };

                            let adjusted = ue - 128_u8;

                            let index = usize::from(adjusted);

                            // TODO
                            if multi_byte[index].is_none() {
                                multi_byte[index] = Some(Vec::<SearchAndReplace>::new())
                            }

                            // TODO
                            let vec = multi_byte.get_mut(index).unwrap().as_mut().unwrap();

                            let search_and_replace = match *rest {
                                [a] => SearchAndReplace {
                                    replacement: replacement_char,
                                    number_of_bytes: SearchNumberOfBytes::One,
                                    payload: [a, 0_u8, 0_u8],
                                },
                                [a, b] => SearchAndReplace {
                                    replacement: replacement_char,
                                    number_of_bytes: SearchNumberOfBytes::Two,
                                    payload: [a, b, 0_u8],
                                },
                                [a, b, c] => SearchAndReplace {
                                    replacement: replacement_char,
                                    number_of_bytes: SearchNumberOfBytes::Three,
                                    payload: [a, b, c],
                                },
                                _ => {
                                    unreachable!();
                                }
                            };

                            // TODO
                            // Order?
                            vec.push(search_and_replace);
                        }
                    }
                };

            for (us, op) in string1_operands_flattened.into_iter().enumerate() {
                let da = string2_operands_to_use_flattened
                    .get(us)
                    .ok_or("Indexing failed")?;

                let replacement = da.convert_to_replacement();

                match op {
                    Operand::Char(CharOperand {
                        char,
                        char_repetition,
                    }) => {
                        // TODO
                        // Enforce with types
                        assert!(matches!(char_repetition, CharRepetition::N(1_usize)));

                        add_normal_char_with_replacement(char, replacement);
                    }
                    Operand::Equiv(EquivOperand { char }) => {
                        equiv.push((char.clone(), replacement.clone()));

                        // TODO
                        // Fix for `tr_equivalence_class_low_priority`
                        add_normal_char_with_replacement(char, replacement);
                    }
                }
            }

            ForTranslation::NotComplemented(Box::new(NotComplementedTranslation {
                seven_bit,
                eight_bit,
                multi_byte,
                equiv,
            }))
        };

        Ok(translation)
    }

    pub fn generate_for_removal(
        complement: bool,
        string1_or_string2_operands: Vec<Operand>,
        is_string1: bool,
    ) -> Result<ForRemoval, Box<dyn Error>> {
        let mut equiv = Vec::<DataTypeWithData>::new();

        let mut seven_bit = [false; 128_usize];
        let mut eight_bit = [false; 128_usize];
        let mut multi_byte = [const { Option::<Vec<Search>>::None }; 128_usize];

        for op in string1_or_string2_operands {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => match char_repetition {
                    CharRepetition::AsManyAsNeeded => {
                        if is_string1 {
                            return Err(Box::from(
                                "the [c*] repeat construct may not appear in string1".to_owned(),
                            ));
                        } else {
                            // Squeezing, allowed
                            // See `tr_non_standard_d_s`
                            add_normal_char(char, &mut seven_bit, &mut eight_bit, &mut multi_byte);
                        }
                    }
                    CharRepetition::N(_) => {
                        add_normal_char(char, &mut seven_bit, &mut eight_bit, &mut multi_byte);
                    }
                },
                Operand::Equiv(EquivOperand { char }) => {
                    equiv.push(char);
                }
            }
        }

        let removal = RemovalShared {
            eight_bit,
            equiv,
            multi_byte,
            seven_bit,
        };

        let for_removal = if complement {
            ForRemoval::Complemented(Box::new(ComplementedRemoval { removal }))
        } else {
            ForRemoval::NotComplemented(Box::new(NotComplementedRemoval { removal }))
        };

        Ok(for_removal)
    }

    #[derive(Clone, Copy)]
    #[repr(u8)]
    pub enum FullCharNumberOfBytes {
        One = 1_u8,
        Two = 2_u8,
        Three = 3_u8,
        Four = 4_u8,
    }

    #[derive(Clone)]
    pub struct FullChar {
        // TODO
        // Determine how many bytes to write by the payload?
        pub number_of_bytes: FullCharNumberOfBytes,
        pub payload: [u8; 4_usize],
    }

    impl FullChar {
        pub fn fast_check(&self, byte_a: u8, next_bytes: &[u8]) -> bool {
            // Fast check
            if byte_a == self.payload[0_usize] {
                let number_of_bytes_usize = self.number_of_bytes as usize;

                let check_against = &self.payload[1_usize..number_of_bytes_usize];

                return next_bytes.starts_with(check_against);
            }

            false
        }

        pub fn write_full_char(&self, to: &mut [u8]) -> usize {
            let to_write = self.number_of_bytes as usize;

            to[..to_write].clone_from_slice(&self.payload[..to_write]);

            to_write
        }

        pub fn new_from_u8(byte: u8) -> FullChar {
            FullChar {
                number_of_bytes: FullCharNumberOfBytes::One,
                payload: [byte, 0_u8, 0_u8, 0_u8],
            }
        }

        pub fn new_from_char(char: char) -> FullChar {
            let mut encoding_buffer = [0_u8; 4_usize];

            let st = char.encode_utf8(&mut encoding_buffer);

            match *st.as_bytes() {
                [a, b] => FullChar {
                    number_of_bytes: FullCharNumberOfBytes::Two,
                    payload: [a, b, 0_u8, 0_u8],
                },
                [a, b, c] => FullChar {
                    number_of_bytes: FullCharNumberOfBytes::Three,
                    payload: [a, b, c, 0_u8],
                },
                [a, b, c, d] => FullChar {
                    number_of_bytes: FullCharNumberOfBytes::Four,
                    payload: [a, b, c, d],
                },
                _ => {
                    unreachable!();
                }
            }
        }
    }

    #[derive(Clone, Copy)]
    #[repr(u8)]
    pub enum SearchNumberOfBytes {
        One = 1_u8,
        Two = 2_u8,
        Three = 3_u8,
    }

    impl SearchNumberOfBytes {
        pub fn increment(&self) -> FullCharNumberOfBytes {
            match self {
                SearchNumberOfBytes::One => FullCharNumberOfBytes::Two,
                SearchNumberOfBytes::Two => FullCharNumberOfBytes::Three,
                SearchNumberOfBytes::Three => FullCharNumberOfBytes::Four,
            }
        }
    }

    pub struct SearchAndReplace {
        number_of_bytes: SearchNumberOfBytes,
        payload: [u8; 3_usize],
        replacement: FullChar,
    }

    pub enum ForTranslation {
        NotComplemented(Box<NotComplementedTranslation>),
        Complemented(Box<ComplementedTranslation>),
    }

    pub struct ReplacementCheckResult<'a> {
        pub replacement: Option<&'a FullChar>,
        pub match_lookahead_length: Option<SearchNumberOfBytes>,
        pub found_match: bool,
    }

    impl Translation for NotComplementedTranslation {
        #[inline]
        fn get_seven_bit_replacement(&self, ue: u8) -> ReplacementCheckResult {
            let index = usize::from(ue);

            let replacement = self.seven_bit[index].as_ref();

            ReplacementCheckResult {
                replacement,
                match_lookahead_length: None,
                found_match: replacement.is_some(),
            }
        }

        #[inline]
        fn get_eight_bit_replacement<'a>(
            &'a self,
            ue: u8,
            next_bytes: &[u8],
        ) -> ReplacementCheckResult<'a> {
            let index = usize::from(ue);

            if let Some(fu) = self.eight_bit[index].as_ref() {
                ReplacementCheckResult {
                    replacement: Some(fu),
                    match_lookahead_length: None,
                    found_match: true,
                }
            } else {
                match &self.multi_byte[index] {
                    Some(ve) => {
                        // TODO
                        // Order
                        for se in ve {
                            let number_of_bytes = se.number_of_bytes;

                            let test =
                                next_bytes.starts_with(&se.payload[..(number_of_bytes as usize)]);

                            if test {
                                return ReplacementCheckResult {
                                    replacement: Some(&se.replacement),
                                    match_lookahead_length: Some(number_of_bytes),
                                    found_match: true,
                                };
                            }
                        }

                        ReplacementCheckResult {
                            replacement: None,
                            match_lookahead_length: None,
                            found_match: false,
                        }
                    }
                    None => ReplacementCheckResult {
                        replacement: None,
                        match_lookahead_length: None,
                        found_match: false,
                    },
                }
            }
        }

        #[inline]
        fn get_equiv_result(&self, ue: u8) -> ReplacementCheckResult<'_> {
            for (da, fu) in &self.equiv {
                match da {
                    DataTypeWithData::Is7Bit(uei) | DataTypeWithData::Is8Bit(uei) => {
                        if ue == *uei {
                            return ReplacementCheckResult {
                                replacement: Some(fu),
                                match_lookahead_length: None,
                                found_match: true,
                            };
                        }
                    }
                    DataTypeWithData::IsMultiByte(_) => {
                        unreachable!();
                    }
                }
            }

            ReplacementCheckResult {
                replacement: None,
                match_lookahead_length: None,
                found_match: false,
            }
        }
    }

    impl Translation for ComplementedTranslation {
        #[inline]
        fn get_seven_bit_replacement(&self, ue: u8) -> ReplacementCheckResult {
            let index = usize::from(ue);

            let value_is_true = self.seven_bit[index];

            ReplacementCheckResult {
                replacement: if value_is_true {
                    None
                } else {
                    Some(&self.replacement)
                },
                match_lookahead_length: None,
                found_match: value_is_true,
            }
        }

        #[inline]
        fn get_eight_bit_replacement(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            let index = usize::from(ue);

            if self.eight_bit[index] {
                ReplacementCheckResult {
                    replacement: None,
                    match_lookahead_length: None,
                    found_match: true,
                }
            } else {
                match &self.multi_byte[index] {
                    Some(ve) => {
                        // TODO
                        // Order
                        for se in ve {
                            let number_of_bytes = se.number_of_bytes;

                            let test =
                                next_bytes.starts_with(&se.payload[..(number_of_bytes as usize)]);

                            if test {
                                return ReplacementCheckResult {
                                    replacement: None,
                                    match_lookahead_length: Some(number_of_bytes),
                                    found_match: true,
                                };
                            }
                        }

                        ReplacementCheckResult {
                            replacement: Some(&self.replacement),
                            match_lookahead_length: None,
                            found_match: false,
                        }
                    }
                    None => ReplacementCheckResult {
                        replacement: Some(&self.replacement),
                        match_lookahead_length: None,
                        found_match: false,
                    },
                }
            }
        }

        #[inline]
        fn get_equiv_result(&self, ue: u8) -> ReplacementCheckResult<'_> {
            for da in &self.equiv {
                match da {
                    DataTypeWithData::Is7Bit(uei) | DataTypeWithData::Is8Bit(uei) => {
                        if ue == *uei {
                            return ReplacementCheckResult {
                                replacement: None,
                                match_lookahead_length: None,
                                found_match: true,
                            };
                        }
                    }
                    DataTypeWithData::IsMultiByte(_) => {
                        unreachable!();
                    }
                }
            }

            ReplacementCheckResult {
                replacement: Some(&self.replacement),
                match_lookahead_length: None,
                found_match: false,
            }
        }
    }

    pub trait Translation {
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            let first_check = if ue < 128_u8 {
                self.get_seven_bit_replacement(ue)
            } else {
                self.get_eight_bit_replacement(ue - 128_u8, next_bytes)
            };

            if first_check.found_match {
                return first_check;
            }

            self.get_equiv_result(ue)
        }

        fn get_seven_bit_replacement(&self, ue: u8) -> ReplacementCheckResult;

        fn get_eight_bit_replacement<'a>(
            &'a self,
            ue: u8,
            next_bytes: &[u8],
        ) -> ReplacementCheckResult<'a>;

        fn get_equiv_result(&self, ue: u8) -> ReplacementCheckResult<'_>;
    }

    pub struct NotComplementedTranslation {
        pub seven_bit: [Option<FullChar>; 128_usize],
        pub eight_bit: [Option<FullChar>; 256_usize],
        pub multi_byte: [Option<Vec<SearchAndReplace>>; 256_usize],
        pub equiv: Vec<(DataTypeWithData, FullChar)>,
    }

    pub struct ComplementedTranslation {
        pub replacement: FullChar,
        pub seven_bit: [bool; 128_usize],
        pub eight_bit: [bool; 128_usize],
        pub multi_byte: [Option<Vec<Search>>; 128_usize],
        pub equiv: Vec<DataTypeWithData>,
    }

    pub struct Search {
        number_of_bytes: SearchNumberOfBytes,
        payload: [u8; 3_usize],
    }

    pub struct RemovalShared {
        seven_bit: [bool; 128_usize],
        eight_bit: [bool; 128_usize],
        multi_byte: [Option<Vec<Search>>; 128_usize],
        equiv: Vec<DataTypeWithData>,
    }

    pub struct RemovalCheckResult {
        pub matched: bool,
        pub match_lookahead_length: Option<SearchNumberOfBytes>,
        pub found_match: bool,
    }

    impl RemovalShared {
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult {
            let first_check = if ue < 128_u8 {
                self.get_seven_bit_deletion(ue)
            } else {
                self.get_eight_bit_deletion(ue - 128_u8, next_bytes)
            };

            if first_check.found_match {
                return first_check;
            }

            for da in &self.equiv {
                match da {
                    DataTypeWithData::Is7Bit(uei) | DataTypeWithData::Is8Bit(uei) => {
                        if ue == *uei {
                            return RemovalCheckResult {
                                matched: true,
                                match_lookahead_length: None,
                                found_match: true,
                            };
                        }
                    }
                    DataTypeWithData::IsMultiByte(_) => {
                        unreachable!();
                    }
                }
            }

            RemovalCheckResult {
                matched: false,
                match_lookahead_length: None,
                found_match: false,
            }
        }

        #[inline]
        fn get_seven_bit_deletion(&self, ue: u8) -> RemovalCheckResult {
            let index = usize::from(ue);

            let value_is_true = self.seven_bit[index];

            RemovalCheckResult {
                match_lookahead_length: None,
                matched: value_is_true,
                found_match: value_is_true,
            }
        }

        #[inline]
        fn get_eight_bit_deletion(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult {
            let index = usize::from(ue);

            if self.eight_bit[index] {
                RemovalCheckResult {
                    matched: true,
                    match_lookahead_length: None,
                    found_match: true,
                }
            } else {
                match &self.multi_byte[index] {
                    Some(ve) => {
                        // TODO
                        // Order
                        for se in ve {
                            let number_of_bytes = se.number_of_bytes;

                            let test =
                                next_bytes.starts_with(&se.payload[..(number_of_bytes as usize)]);

                            if test {
                                return RemovalCheckResult {
                                    matched: true,
                                    match_lookahead_length: Some(number_of_bytes),
                                    found_match: true,
                                };
                            }
                        }

                        RemovalCheckResult {
                            matched: false,
                            match_lookahead_length: None,
                            found_match: false,
                        }
                    }
                    None => RemovalCheckResult {
                        matched: false,
                        match_lookahead_length: None,
                        found_match: false,
                    },
                }
            }
        }
    }

    pub struct NotComplementedRemoval {
        removal: RemovalShared,
    }

    pub struct ComplementedRemoval {
        removal: RemovalShared,
    }

    pub enum ForRemoval {
        NotComplemented(Box<NotComplementedRemoval>),
        Complemented(Box<ComplementedRemoval>),
    }

    pub trait Removal {
        fn check(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult;
    }

    impl Removal for NotComplementedRemoval {
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult {
            self.removal.check(ue, next_bytes)
        }
    }

    impl Removal for ComplementedRemoval {
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult {
            let mut re = self.removal.check(ue, next_bytes);

            re.matched = !re.matched;

            re
        }
    }
}

mod transformation {
    use std::error::Error;
    use std::io::{self, ErrorKind, Read, Write};

    pub struct TransformResult {
        pub bytes_written: usize,
        pub leftover_bytes: usize,
    }

    pub trait Transformation {
        fn process_current_byte_with_window(
            &mut self,
            byte_a: u8,
            next_bytes: &[u8],
            index: &mut usize,
            bytes_written: &mut usize,
            output: &mut [u8],
        );

        #[inline]
        fn transform_buffer(
            &mut self,
            input: &mut [u8],
            output: &mut [u8],
            last_iteration: bool,
        ) -> TransformResult {
            let input_len = input.len();

            let mut bytes_written = 0_usize;

            let mut index = 0_usize;

            loop {
                let option = input.get(index..(index + 4_usize));

                let Some(&[byte_a, byte_b, byte_c, byte_d]) = option else {
                    break;
                };

                self.process_current_byte_with_window(
                    byte_a,
                    &[byte_b, byte_c, byte_d],
                    &mut index,
                    &mut bytes_written,
                    output,
                );
            }

            debug_assert!((input_len - index) <= 4_usize);

            let leftover_bytes = if last_iteration {
                loop {
                    let Some(&byte_a) = input.get(index) else {
                        break;
                    };

                    let next_bytes = &input[(index + 1_usize)..];

                    debug_assert!(next_bytes.len() <= 4_usize);

                    self.process_current_byte_with_window(
                        byte_a,
                        next_bytes,
                        &mut index,
                        &mut bytes_written,
                        output,
                    );
                }

                0_usize
            } else {
                let range = index..input_len;

                let range_len = range.len();

                input.copy_within(range, 0_usize);

                range_len
            };

            TransformResult {
                bytes_written,
                leftover_bytes,
            }
        }
    }

    pub fn streaming_transform<T: Transformation>(t: &mut T) -> Result<(), Box<dyn Error>> {
        const SIZE: usize = 8_usize * 1_024_usize;

        // Buffers
        let mut input = vec![0_u8; SIZE];
        // Most pessimistic case is every input character is a one byte character, and is being translated to a four byte character
        let mut output = vec![0_u8; SIZE * 4_usize];

        let mut leftover_bytes = 0_usize;

        // TODO
        // Improve this
        let mut stdin_lock = io::stdin().lock();
        let mut stdout_lock = io::stdout().lock();

        loop {
            let buf = &mut input[leftover_bytes..];

            match stdin_lock.read(buf) {
                Ok(0_usize) => {
                    let transform_result =
                        t.transform_buffer(&mut input[..leftover_bytes], &mut output, true);

                    stdout_lock.write_all(&output[..(transform_result.bytes_written)])?;

                    break;
                }
                Ok(us) => {
                    let read_slice = &mut input[..(leftover_bytes + us)];

                    let transform_result = t.transform_buffer(read_slice, &mut output, false);

                    leftover_bytes = transform_result.leftover_bytes;

                    stdout_lock.write_all(&output[..(transform_result.bytes_written)])?;
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

    pub mod delete {
        use super::Transformation;
        use crate::setup::Removal;

        pub struct DeleteTransformation<T: Removal> {
            pub removal: T,
        }

        impl<T: Removal> Transformation for DeleteTransformation<T> {
            #[inline]
            fn process_current_byte_with_window(
                &mut self,
                byte_a: u8,
                next_bytes: &[u8],
                index: &mut usize,
                bytes_written: &mut usize,
                output: &mut [u8],
            ) {
                let output_slice = &mut output[(*bytes_written)..];

                let removal_check_result = self.removal.check(byte_a, next_bytes);

                if let Some(se) = removal_check_result.match_lookahead_length {
                    *index += se as usize;
                }

                *index += 1_usize;

                if removal_check_result.matched {
                    return;
                }

                // Not deleted
                {
                    output_slice[0_usize] = byte_a;

                    *bytes_written += 1_usize;
                }
            }
        }
    }

    pub mod delete_and_squeeze {
        use super::Transformation;
        use crate::setup::{FullChar, FullCharNumberOfBytes, NotComplementedRemoval, Removal};

        pub struct LastPrintedChar {
            char: FullChar,
            squeeze_char: bool,
        }

        pub struct DeleteAndSqueezeState {
            pub last_printed_character: Option<LastPrintedChar>,
        }

        pub struct DeleteAndSqueezeTransformation<T: Removal> {
            pub delete_and_squeeze_state: DeleteAndSqueezeState,
            pub delete: T,
            pub squeeze: NotComplementedRemoval,
        }

        impl<T: Removal> Transformation for DeleteAndSqueezeTransformation<T> {
            #[inline]
            fn process_current_byte_with_window(
                &mut self,
                byte_a: u8,
                next_bytes: &[u8],
                index: &mut usize,
                bytes_written: &mut usize,
                output: &mut [u8],
            ) {
                let delete_removal_check_result = self.delete.check(byte_a, next_bytes);

                if let Some(se) = delete_removal_check_result.match_lookahead_length {
                    *index += se as usize;
                }

                if delete_removal_check_result.matched {
                    *index += 1_usize;

                    return;
                }

                if let Some(la) = &self.delete_and_squeeze_state.last_printed_character {
                    if la.squeeze_char {
                        let full_char = &la.char;

                        if full_char.fast_check(byte_a, next_bytes) {
                            *index += full_char.number_of_bytes as usize;

                            return;
                        }
                    }
                }

                let output_slice = &mut output[(*bytes_written)..];

                let squeeze_removal_check_result = self.squeeze.check(byte_a, next_bytes);

                let squeeze_char = squeeze_removal_check_result.matched;

                if squeeze_char {
                    let full_char_number_of_bytes =
                        if let Some(se) = squeeze_removal_check_result.match_lookahead_length {
                            se.increment()
                        } else {
                            FullCharNumberOfBytes::One
                        };

                    let mut payload = [0_u8; 4_usize];

                    // TODO
                    for (us, &ue) in [byte_a]
                        .iter()
                        .chain(next_bytes)
                        .take(full_char_number_of_bytes as usize)
                        .enumerate()
                    {
                        payload[us] = ue;
                    }

                    let char = FullChar {
                        number_of_bytes: full_char_number_of_bytes,
                        payload,
                    };

                    let additional_bytes_written = char.write_full_char(output_slice);

                    *bytes_written += additional_bytes_written;
                    *index += additional_bytes_written;

                    self.delete_and_squeeze_state = DeleteAndSqueezeState {
                        last_printed_character: Some(LastPrintedChar { squeeze_char, char }),
                    };
                } else {
                    output_slice[0_usize] = byte_a;

                    *bytes_written += 1_usize;
                    *index += 1_usize;

                    self.delete_and_squeeze_state = DeleteAndSqueezeState {
                        last_printed_character: Some(LastPrintedChar {
                            char: FullChar::new_from_u8(byte_a),
                            squeeze_char,
                        }),
                    };
                }
            }
        }
    }

    pub mod squeeze {
        use super::Transformation;
        use crate::setup::{FullChar, FullCharNumberOfBytes, Removal};

        pub struct LastPrintedChar {
            char: FullChar,
            squeeze_char: bool,
        }

        pub struct SqueezeState {
            pub last_printed_character: Option<LastPrintedChar>,
        }

        pub struct SqueezeTransformation<T: Removal> {
            pub squeeze_state: SqueezeState,
            pub squeeze: T,
        }

        impl<T: Removal> Transformation for SqueezeTransformation<T> {
            #[inline]
            fn process_current_byte_with_window(
                &mut self,
                byte_a: u8,
                next_bytes: &[u8],
                index: &mut usize,
                bytes_written: &mut usize,
                output: &mut [u8],
            ) {
                if let Some(la) = &self.squeeze_state.last_printed_character {
                    if la.squeeze_char {
                        let full_char = &la.char;

                        if full_char.fast_check(byte_a, next_bytes) {
                            *index += full_char.number_of_bytes as usize;

                            return;
                        }
                    }
                }

                let output_slice = &mut output[(*bytes_written)..];

                let squeeze_removal_check_result = self.squeeze.check(byte_a, next_bytes);

                let squeeze_char = squeeze_removal_check_result.matched;

                if squeeze_char {
                    let full_char_number_of_bytes =
                        if let Some(se) = squeeze_removal_check_result.match_lookahead_length {
                            se.increment()
                        } else {
                            FullCharNumberOfBytes::One
                        };

                    let mut payload = [0_u8; 4_usize];

                    // TODO
                    for (us, &ue) in [byte_a]
                        .iter()
                        .chain(next_bytes)
                        .take(full_char_number_of_bytes as usize)
                        .enumerate()
                    {
                        payload[us] = ue;
                    }

                    let char = FullChar {
                        number_of_bytes: full_char_number_of_bytes,
                        payload,
                    };

                    let additional_bytes_written = char.write_full_char(output_slice);

                    *bytes_written += additional_bytes_written;
                    *index += additional_bytes_written;

                    self.squeeze_state = SqueezeState {
                        last_printed_character: Some(LastPrintedChar { squeeze_char, char }),
                    };
                } else {
                    output_slice[0_usize] = byte_a;

                    *bytes_written += 1_usize;
                    *index += 1_usize;

                    self.squeeze_state = SqueezeState {
                        last_printed_character: Some(LastPrintedChar {
                            char: FullChar::new_from_u8(byte_a),
                            squeeze_char,
                        }),
                    };
                }
            }
        }
    }

    pub mod squeeze_and_translate {
        use super::Transformation;
        use crate::setup::{FullChar, NotComplementedRemoval, Removal, Translation};

        pub struct LastPrintedChar {
            char: FullChar,
            squeeze_char: bool,
        }

        pub struct SqueezeAndTranslateState {
            pub last_printed_character: Option<LastPrintedChar>,
        }

        pub struct SqueezeAndTranslateTransformation<T: Translation> {
            pub translation: T,
            pub squeeze: NotComplementedRemoval,
            pub squeeze_and_translate_state: SqueezeAndTranslateState,
        }

        impl<T: Translation> Transformation for SqueezeAndTranslateTransformation<T> {
            #[inline]
            fn process_current_byte_with_window(
                &mut self,
                byte_a: u8,
                next_bytes: &[u8],
                index: &mut usize,
                bytes_written: &mut usize,
                output: &mut [u8],
            ) {
                let replacement_check_result = self.translation.check(byte_a, next_bytes);

                let fu_payload: [u8; 4];
                let full_char_from_byte: FullChar;

                let (byte_a_to_use, next_bytes_to_use, full_char_to_write) =
                    match replacement_check_result.replacement {
                        Some(fu) => {
                            fu_payload = fu.payload;

                            let [fu_byte_a, ..] = fu_payload;

                            let fu_rest = &fu_payload[1_usize..(fu.number_of_bytes as usize)];

                            (fu_byte_a, fu_rest, fu)
                        }
                        None => {
                            full_char_from_byte = FullChar::new_from_u8(byte_a);

                            (byte_a, next_bytes, &full_char_from_byte)
                        }
                    };

                // TODO
                // Is this in the right place?
                if let Some(se) = replacement_check_result.match_lookahead_length {
                    *index += se as usize;
                }

                if let Some(la) = &self.squeeze_and_translate_state.last_printed_character {
                    if la.squeeze_char {
                        let full_char = &la.char;

                        // TODO
                        // Verify
                        if full_char.fast_check(byte_a_to_use, next_bytes_to_use) {
                            *index += full_char.number_of_bytes as usize;

                            return;
                        }
                    }
                }

                let output_slice = &mut output[(*bytes_written)..];

                let squeeze_remove_check_result =
                    self.squeeze.check(byte_a_to_use, next_bytes_to_use);

                let squeeze_char = squeeze_remove_check_result.matched;

                let additional_bytes_written = full_char_to_write.write_full_char(output_slice);

                *bytes_written += additional_bytes_written;
                *index += 1_usize;

                self.squeeze_and_translate_state = SqueezeAndTranslateState {
                    last_printed_character: Some(LastPrintedChar {
                        squeeze_char,
                        char: full_char_to_write.to_owned(),
                    }),
                };
            }
        }
    }

    pub mod translate {
        use super::Transformation;
        use crate::setup::{ReplacementCheckResult, Translation};

        pub struct TranslateTransformation<T: Translation> {
            pub translation: T,
        }

        impl<T: Translation> Transformation for TranslateTransformation<T> {
            #[inline]
            fn process_current_byte_with_window(
                &mut self,
                byte_a: u8,
                next_bytes: &[u8],
                index: &mut usize,
                bytes_written: &mut usize,
                output: &mut [u8],
            ) {
                let output_slice = &mut output[(*bytes_written)..];

                let ReplacementCheckResult {
                    replacement,
                    match_lookahead_length,
                    ..
                } = self.translation.check(byte_a, next_bytes);

                let extra_bytes_to_write = if let Some(se) = match_lookahead_length {
                    let match_lookahead_length_usize = se as usize;

                    *index += match_lookahead_length_usize;

                    match_lookahead_length_usize
                } else {
                    0_usize
                };

                *index += 1_usize;

                if let Some(fu) = replacement {
                    let additional_bytes_written = fu.write_full_char(output_slice);

                    *bytes_written += additional_bytes_written;

                    return;
                }

                // No replacement was found, so write the original byte
                {
                    output_slice[0_usize] = byte_a;

                    output_slice[1_usize..(1_usize + extra_bytes_to_write)]
                        .copy_from_slice(&next_bytes[..extra_bytes_to_write]);

                    *bytes_written += 1_usize + extra_bytes_to_write;
                }
            }
        }
    }
}
