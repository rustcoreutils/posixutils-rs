//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use setup::{ForRemoval, ForTranslation};
use std::error::Error;
use std::process;
use transformation::{
    streaming_transform, DeleteAndSqueezeTransformation, DeleteTransformation, LastWritten,
    SqueezeAndTranslateTransformation, SqueezeTransformation, TranslateTransformation,
};

/// tr - translate or delete characters
#[derive(Parser)]
#[command(version, about = gettext("tr - translate or delete characters"))]
struct Args {
    #[arg(short = 'd', help = gettext("Delete characters in STRING1 from the input"))]
    delete: bool,

    #[arg(short = 's', help = gettext("Replace each input sequence of a repeated character that is listed in the last specified SET, with a single occurrence of that character"))]
    squeeze_repeats: bool,

    #[arg(short = 'c', help = gettext("Use the complement of STRING1's values"))]
    complement_val: bool,

    #[arg(short = 'C', help = gettext("Use the complement of STRING1's characters"))]
    complement_char: bool,

    #[arg(help = gettext("First string"))]
    string1: String,

    #[arg(help = gettext("Second string (not required if delete mode is on)"))]
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

/// Enforce POSIX's restriction on `[:class:]` in string2 (118121-118124).
///
/// > When both the -d and -s options are specified, any of the character class
/// > names shall be accepted in string2. Otherwise, only character class names
/// > lower or upper are valid in string2 and then only if the corresponding
/// > character class (upper and lower, respectively) is specified in the same
/// > relative position in string1.
///
/// The relative-position half needs to know which operand a class came from,
/// which is why this runs on the parsed operands rather than re-lexing the raw
/// argument as it used to.
fn validate_string2_classes(
    delete: bool,
    squeeze: bool,
    string1: &[parsing::Operand],
    string2: &[parsing::Operand],
) -> Result<(), String> {
    use parsing::Operand;

    // With both -d and -s, every class name is accepted.
    if delete && squeeze {
        return Ok(());
    }

    for (position, op) in string2.iter().enumerate() {
        let Operand::Class(class) = op else {
            continue;
        };
        let Some(converse) = class.case_converse() else {
            return Err(format!(
                "character class '[:{}:]' is not valid in string2",
                class.as_str()
            ));
        };
        let paired = matches!(
            string1.get(position),
            Some(Operand::Class(other)) if *other == converse
        );
        if !paired {
            return Err(format!(
                "'[:{}:]' is valid in string2 only when '[:{}:]' appears in the \
                 same relative position in string1",
                class.as_str(),
                converse.as_str()
            ));
        }
    }
    Ok(())
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

    if let Some(string2) = string2_operands.as_deref() {
        validate_string2_classes(
            args.delete,
            args.squeeze_repeats,
            &string1_operands,
            string2,
        )?;
    }

    // POSIX 118043/118045: `-c` complements the set of *values* (so a
    // multi-byte character outside the set is replaced once per byte), `-C`
    // the set of *characters* as defined by LC_CTYPE (replaced once).
    let complement = args.complement_char || args.complement_val;
    let complement_chars = args.complement_char;

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
                complement_chars,
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
            let for_removal =
                setup::generate_for_removal(complement, complement_chars, string1_operands, true)?;

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
                        complement_chars,
                        string1_operands,
                        string2_operands.as_slice(),
                    )?;

                    // Complement does not apply to string2
                    let squeeze_for_removal =
                        setup::generate_for_removal(false, false, string2_operands, false)?;

                    let ForRemoval::NotComplemented(squeeze) = squeeze_for_removal else {
                        unreachable!()
                    };

                    match for_translation {
                        ForTranslation::Complemented(bo) => {
                            let mut t = SqueezeAndTranslateTransformation {
                                translation: *bo,
                                squeeze: *squeeze,
                                last: LastWritten::default(),
                            };

                            streaming_transform(&mut t)
                        }
                        ForTranslation::NotComplemented(bo) => {
                            let mut t = SqueezeAndTranslateTransformation {
                                translation: *bo,
                                squeeze: *squeeze,
                                last: LastWritten::default(),
                            };

                            streaming_transform(&mut t)
                        }
                    }
                }
                None => {
                    let for_removal = setup::generate_for_removal(
                        complement,
                        complement_chars,
                        string1_operands,
                        true,
                    )?;

                    match for_removal {
                        ForRemoval::Complemented(bo) => {
                            let mut t = SqueezeTransformation {
                                squeeze: *bo,
                                last: LastWritten::default(),
                            };

                            streaming_transform(&mut t)
                        }
                        ForRemoval::NotComplemented(bo) => {
                            let mut t = SqueezeTransformation {
                                squeeze: *bo,
                                last: LastWritten::default(),
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
                setup::generate_for_removal(complement, complement_chars, string1_operands, true)?;

            // Complement does not apply to squeeze, only delete, in this case
            let squeeze_for_removal =
                setup::generate_for_removal(false, false, string2_operands, false)?;

            let ForRemoval::NotComplemented(squeeze) = squeeze_for_removal else {
                unreachable!()
            };

            match delete_for_removal {
                ForRemoval::Complemented(bo) => {
                    let mut t = DeleteAndSqueezeTransformation {
                        delete: *bo,
                        last: LastWritten::default(),
                        squeeze: *squeeze,
                    };

                    streaming_transform(&mut t)
                }
                ForRemoval::NotComplemented(bo) => {
                    let mut t = DeleteAndSqueezeTransformation {
                        delete: *bo,
                        last: LastWritten::default(),
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
    plib::diag::init_locale("tr");

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
    use std::iter::Peekable;
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

    /// One of POSIX's twelve character-class names (118116-118117).
    ///
    /// Kept symbolic through parsing: membership is a *predicate* over the
    /// current `LC_CTYPE`, not a finite set of characters, and the
    /// `[:lower:]`/`[:upper:]` pair carries the case-conversion meaning of
    /// 118125-118130. Expanding a class to characters at parse time — as this
    /// did — destroys both.
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum ClassName {
        Alnum,
        Alpha,
        Blank,
        Cntrl,
        Digit,
        Graph,
        Lower,
        Print,
        Punct,
        Space,
        Upper,
        Xdigit,
    }

    impl ClassName {
        pub fn parse(name: &str) -> Option<Self> {
            Some(match name {
                "alnum" => ClassName::Alnum,
                "alpha" => ClassName::Alpha,
                "blank" => ClassName::Blank,
                "cntrl" => ClassName::Cntrl,
                "digit" => ClassName::Digit,
                "graph" => ClassName::Graph,
                "lower" => ClassName::Lower,
                "print" => ClassName::Print,
                "punct" => ClassName::Punct,
                "space" => ClassName::Space,
                "upper" => ClassName::Upper,
                "xdigit" => ClassName::Xdigit,
                _ => return None,
            })
        }

        pub fn as_str(self) -> &'static str {
            match self {
                ClassName::Alnum => "alnum",
                ClassName::Alpha => "alpha",
                ClassName::Blank => "blank",
                ClassName::Cntrl => "cntrl",
                ClassName::Digit => "digit",
                ClassName::Graph => "graph",
                ClassName::Lower => "lower",
                ClassName::Print => "print",
                ClassName::Punct => "punct",
                ClassName::Space => "space",
                ClassName::Upper => "upper",
                ClassName::Xdigit => "xdigit",
            }
        }

        /// The converse class for case conversion, if this is a case class.
        pub fn case_converse(self) -> Option<Self> {
            match self {
                ClassName::Lower => Some(ClassName::Upper),
                ClassName::Upper => Some(ClassName::Lower),
                _ => None,
            }
        }

        /// Membership under the current `LC_CTYPE`.
        pub fn contains(self, c: char) -> bool {
            match self {
                ClassName::Alnum => plib::locale::isalnum(c),
                ClassName::Alpha => plib::locale::isalpha(c),
                ClassName::Blank => plib::locale::isblank(c),
                ClassName::Cntrl => plib::locale::iscntrl(c),
                ClassName::Digit => plib::locale::isdigit(c),
                ClassName::Graph => plib::locale::isgraph(c),
                ClassName::Lower => plib::locale::islower(c),
                ClassName::Print => plib::locale::isprint(c),
                ClassName::Punct => plib::locale::ispunct(c),
                ClassName::Space => plib::locale::isspace(c),
                ClassName::Upper => plib::locale::isupper(c),
                ClassName::Xdigit => plib::locale::isxdigit(c),
            }
        }

        /// The ASCII members of this class, in ascending code-point order.
        ///
        /// Used only where a class must be paired positionally with individual
        /// characters of string2 — a case POSIX calls out as having undefined
        /// order (118159-118162) and discourages. Set membership and case
        /// conversion use [`ClassName::contains`] instead and never enumerate.
        /// The class's members, in ascending order.
        ///
        /// Only a translation that spreads distinct replacements across a class
        /// needs these; the common many-to-one case keeps the class a
        /// predicate. POSIX leaves the order unspecified (118133-4).
        ///
        /// The scan stops at the Basic Multilingual Plane. `LC_CTYPE` can put
        /// members above it, but a translation that pairs them positionally
        /// would have to enumerate a million code points to find a handful, and
        /// the construct POSIX defines for that case -- mapping the class to
        /// one character -- never reaches here.
        pub fn members(self) -> impl Iterator<Item = char> {
            (0_u32..=0xFFFF_u32)
                .filter_map(char::from_u32)
                .filter(move |&c| self.contains(c))
        }
    }

    #[derive(Clone)]
    pub enum Operand {
        Char(CharOperand),
        Equiv(EquivOperand),
        /// A `[:class:]` kept symbolic; see [`ClassName`].
        Class(ClassName),
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

        /// The character this represents, or `None` for a raw byte from a
        /// `\ooo` escape, which is a byte value and not a character.
        /// [`DataTypeWithData::convert_to_char`] panics on that case.
        pub fn as_char(&self) -> Option<char> {
            match self {
                DataTypeWithData::Is7Bit(ue) => Some(char::from(*ue)),
                DataTypeWithData::IsMultiByte(ch) => Some(*ch),
                DataTypeWithData::Is8Bit(_) => None,
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

        // The equivalence-class machinery indexes the byte tables, so a
        // character wider than a byte has nowhere to go. It used to be built
        // anyway and reached three `unreachable!()` arms downstream, aborting
        // with exit 101 and no diagnostic -- in every mode, and on input that
        // did not itself contain the character.
        //
        // Refused here, where the operand is still the text the user wrote, so
        // the diagnostic can quote it. Supporting such a class is a feature:
        // the delete and squeeze paths have an `EquivMatcher` that asks libc
        // and so follows LC_COLLATE, and translate would need the same.
        if let DataTypeWithData::IsMultiByte(ch) = char {
            // Quoted raw, like the multi-character arm above: it came from the
            // command line, so it renders in the locale the user typed it in.
            // `escape_default` would print Rust's own `\u{e9}` syntax.
            return Err(format!(
                "{ch}: equivalence class operand must be a single-byte character"
            ));
        }

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
                // Reject if repeat count starts with a sign
                if st.starts_with('+') || st.starts_with('-') {
                    return Err(format!("invalid repeat count '{st}' in [c*n] construct",));
                }

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
                                            char: crate::parsing::categorize_char(ch),
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

        let class = into_iter.collect::<String>();

        if class.is_empty() {
            return Err("input '[::]' is invalid: missing character class name".to_string());
        }

        // Emitted symbolically: membership follows LC_CTYPE, and the
        // lower/upper pair carries the case-conversion meaning. Both are lost
        // if the class is flattened to characters here.
        let name = ClassName::parse(&class).ok_or_else(|| {
            format!("input '[:{class}:]' is invalid: invalid character class '{class}'")
        })?;
        operand_vec.push(Operand::Class(name));

        Ok(())
    }
}

mod setup {
    use crate::parsing::{
        CharOperand, CharRepetition, ClassName, DataTypeWithData, EquivOperand, Operand,
    };
    use std::error::Error;

    /// One `[=c=]`, answered by libc.
    ///
    /// POSIX defines the equivalence class by `LC_COLLATE` (118137-118138), and
    /// there is no portable call to enumerate one — but every POSIX regex
    /// engine must implement `[[=c=]]` in a bracket expression, so ask it.
    /// On glibc in a UTF-8 locale the class is a singleton, which is why this
    /// changes no output here; on a platform or locale where it is not, this is
    /// correct where a hardcoded "the class is the character itself" was not.
    struct EquivMatcher {
        regex: Option<plib::regex::Regex>,
    }

    impl EquivMatcher {
        fn new(source: char) -> Self {
            // A bracket expression is BRE/ERE-agnostic. If the character cannot
            // be placed in one (it is the bracket syntax itself, say), fall
            // back to no extra members: the literal is already in the tables.
            let pattern = format!("[[={source}=]]");
            EquivMatcher {
                regex: plib::regex::Regex::new(&pattern, plib::regex::RegexFlags::bre()).ok(),
            }
        }

        fn contains(&self, c: char) -> bool {
            let mut buf = [0_u8; 4_usize];
            match &self.regex {
                Some(re) => re.is_match(c.encode_utf8(&mut buf)),
                None => false,
            }
        }
    }

    /// Membership in a set of characters, and nothing else.
    ///
    /// POSIX describes string1 and string2 as *arrays*, but `-d`, `-s` and the
    /// complement forms only ever ask one question of them: is this element a
    /// member? This type answers that and never enumerates itself — a class has
    /// as many members as `LC_CTYPE` says, which for `[:alpha:]` in a UTF-8
    /// locale is upwards of a hundred thousand.
    ///
    /// It is the one membership type. Having two, only one of which consulted
    /// the locale, is what made `tr -d '[:alpha:]'` delete `é` while
    /// `tr '[:alpha:]' X` left it untouched.
    pub struct Set {
        /// Characters named literally, and `\ooo` below 128 — an octal escape
        /// there names an ASCII character, not a byte lacking one.
        ascii: [bool; 128_usize],
        /// Non-ASCII characters named literally.
        chars: Vec<char>,
        /// `\ooo` at or above 128: a byte with no character identity, matched
        /// byte-wise. `tr -d '\251'` takes the second byte of `é` and leaves
        /// the first, as a byte-oriented tr does.
        high_bytes: [bool; 128_usize],
        classes: Vec<ClassName>,
        equivalences: Vec<EquivMatcher>,
    }

    impl Default for Set {
        fn default() -> Self {
            // `[bool; 128]` has no `Default`, so the derive cannot be used.
            Set {
                ascii: [false; 128_usize],
                chars: Vec::new(),
                high_bytes: [false; 128_usize],
                classes: Vec::new(),
                equivalences: Vec::new(),
            }
        }
    }

    impl Set {
        pub fn push_element(&mut self, element: &DataTypeWithData) {
            match *element {
                DataTypeWithData::Is7Bit(ue) => self.ascii[usize::from(ue)] = true,
                DataTypeWithData::Is8Bit(ue) => self.high_bytes[usize::from(ue) - 128] = true,
                DataTypeWithData::IsMultiByte(ch) => self.chars.push(ch),
            }
        }

        pub fn push_class(&mut self, name: ClassName) {
            self.classes.push(name);
        }

        pub fn push_equivalence(&mut self, source: char) {
            self.equivalences.push(EquivMatcher::new(source));
        }

        /// Is the element at the front of the input a member? If so, how many
        /// bytes does it occupy?
        ///
        /// A raw byte member is tried first and matches byte-wise; anything
        /// else is one character, decoded under `LC_CTYPE`.
        pub fn matches_at(&self, lead: u8, next_bytes: &[u8]) -> Option<usize> {
            if lead < 128_u8 {
                // ASCII is one byte in every locale tr supports, so the hot
                // path never decodes.
                let c = char::from(lead);
                if self.ascii[usize::from(lead)] || self.matches_predicates(c) {
                    return Some(1_usize);
                }
                return None;
            }

            if self.high_bytes[usize::from(lead) - 128] {
                return Some(1_usize);
            }

            // Only a non-ASCII byte reaches a decoder, and only when the set
            // has a member one could match.
            if self.chars.is_empty() && self.classes.is_empty() && self.equivalences.is_empty() {
                return None;
            }

            let (c, width) = decode_one(lead, next_bytes)?;
            if self.chars.contains(&c) || self.matches_predicates(c) {
                Some(width)
            } else {
                None
            }
        }

        /// How wide the element at this position is, member or not. Used by
        /// `-C`, which advances over a whole non-member character where `-c`
        /// advances one byte.
        pub fn element_width(&self, lead: u8, next_bytes: &[u8]) -> usize {
            if lead < 128_u8 {
                return 1_usize;
            }
            decode_one(lead, next_bytes).map_or(1_usize, |(_, width)| width)
        }

        fn matches_predicates(&self, c: char) -> bool {
            self.classes.iter().any(|cl| cl.contains(c))
                || self.equivalences.iter().any(|eq| eq.contains(c))
        }
    }

    /// Decode the character beginning at `lead`, with its total width in bytes.
    /// `None` when the bytes are not a complete valid character, in which case
    /// the caller keeps its byte-wise behavior.
    ///
    /// **tr's character model is UTF-8, in every locale.** Its operands arrive
    /// as `String`s, so string1 and string2 are UTF-8 by construction; decoding
    /// the *input* by `LC_CTYPE` instead would make the two disagree, and in
    /// the C locale — where `LC_CTYPE` says every byte is its own character —
    /// a set holding `é` would stop matching the `é` in its input. Under that
    /// reading `tr -d 'ᛆᚠ'` deletes nothing, where it currently deletes those
    /// two characters. One model, applied to both sides, is worth more here
    /// than a literal reading that only agrees with itself. Recorded in
    /// NONPOSIX.md.
    fn decode_one(lead: u8, next_bytes: &[u8]) -> Option<(char, usize)> {
        if lead < 128_u8 {
            return Some((char::from(lead), 1_usize));
        }
        let width = match lead {
            0xC2..=0xDF => 2_usize,
            0xE0..=0xEF => 3_usize,
            0xF0..=0xF4 => 4_usize,
            _ => return None,
        };
        let tail = next_bytes.get(..(width - 1_usize))?;
        let mut buf = [0_u8; 4_usize];
        buf[0] = lead;
        buf[1..width].copy_from_slice(tail);
        let st = std::str::from_utf8(&buf[..width]).ok()?;
        st.chars().next().map(|c| (c, width))
    }

    /// Split out `[:lower:]`↔`[:upper:]` pairings that sit at the same relative
    /// position in the two operand lists (POSIX 118123-118130).
    ///
    /// Only attempted when the lists align one operand to one operand: a class
    /// paired against individual characters has to be enumerated instead, which
    /// is the case POSIX calls out as having undefined order (118159-118162).
    /// Returns the folds plus the two lists with those operands removed, so the
    /// remaining positional pairing is unaffected.
    fn extract_case_folds(
        string1: &[Operand],
        string2: &[Operand],
    ) -> Option<(Vec<CaseFold>, Vec<Operand>, Vec<Operand>)> {
        if string1.len() != string2.len() {
            return None;
        }
        let pairs: Vec<Option<CaseFold>> = string1
            .iter()
            .zip(string2.iter())
            .map(|(a, b)| match (a, b) {
                (Operand::Class(from), Operand::Class(to)) if from.case_converse() == Some(*to) => {
                    Some(CaseFold {
                        from: *from,
                        to_upper: *to == ClassName::Upper,
                    })
                }
                _ => None,
            })
            .collect();

        if pairs.iter().all(Option::is_none) {
            return None;
        }

        let folds = pairs.iter().flatten().copied().collect::<Vec<_>>();
        let keep = |ops: &[Operand]| -> Vec<Operand> {
            ops.iter()
                .zip(pairs.iter())
                .filter(|(_, fold)| fold.is_none())
                .map(|(op, _)| op.clone())
                .collect()
        };
        Some((folds, keep(string1), keep(string2)))
    }

    /// What string2 supplies at each position of the translation, without
    /// materialising any of it.
    ///
    /// POSIX describes string2 as an array padded out to string1's length, but
    /// the padding is always *one repeating element* — the `[c*]` fill, or the
    /// last element when string2 simply runs short. So the array is a handful
    /// of runs, and a position is answered by walking them. `[x*4294967296]`
    /// costs one run rather than four billion elements.
    struct Replacements {
        /// `(element, length)`. A `None` length is the unbounded `[c*]` fill.
        runs: Vec<(DataTypeWithData, Option<usize>)>,
        /// Which run is the `[c*]` fill, recorded at build time. `size_fill`
        /// replaces its `None` with a number, so it cannot be found by looking
        /// for one afterwards -- and it is still the run that absorbs the
        /// slack, which is what makes it the answer to both questions below.
        fill: Option<usize>,
    }

    impl Replacements {
        fn build(operands: &[Operand]) -> Result<Self, Box<dyn Error>> {
            let mut runs = Vec::<(DataTypeWithData, Option<usize>)>::new();
            let mut fills = 0_usize;

            for op in operands {
                match op {
                    Operand::Char(CharOperand {
                        char,
                        char_repetition,
                    }) => match char_repetition {
                        CharRepetition::AsManyAsNeeded => {
                            fills += 1_usize;
                            if fills > 1_usize {
                                return Err(Box::from(
                                    "only one [c*] repeat construct may appear in string2"
                                        .to_owned(),
                                ));
                            }
                            runs.push((char.clone(), None));
                        }
                        CharRepetition::N(n) => runs.push((char.clone(), Some(*n))),
                    },
                    Operand::Equiv(_) => {
                        return Err(Box::from(
                            "[=c=] expressions may not appear in string2 when translating"
                                .to_owned(),
                        ));
                    }
                    Operand::Class(name) => {
                        // POSIX 118122-5 allows a class in string2 only as the
                        // case-conversion counterpart of its converse at the
                        // same relative position in string1. The pass that
                        // extracts those pairs gives up when the two operand
                        // lists differ in length, and is not attempted at all
                        // under `-c`/`-C`, so an unpaired class arrives here.
                        return Err(Box::from(format!(
                            "character class '[:{}:]' is valid in string2 only as the \
                             case conversion counterpart of '[:{}:]' in string1",
                            name.as_str(),
                            name.case_converse().map_or("upper", ClassName::as_str),
                        )));
                    }
                }
            }

            let fill = runs.iter().position(|(_, len)| len.is_none());
            Ok(Replacements { runs, fill })
        }

        /// Is there an unbounded `[c*]` fill with elements after it? Only then
        /// does anything need string1's total length: the fill has to be sized
        /// to push those trailing elements to the end.
        fn fill_needs_string1_length(&self) -> bool {
            match self.fill {
                Some(index) => index + 1_usize < self.runs.len(),
                None => false,
            }
        }

        /// Resolve the unbounded fill now that string1's length is known.
        fn size_fill(&mut self, string1_len: usize) {
            let Some(index) = self.fill else {
                return;
            };
            let explicit: usize = self.runs.iter().filter_map(|&(_, len)| len).sum();
            self.runs[index].1 = Some(string1_len.saturating_sub(explicit));
        }

        /// The position from which every element is the same, and that element.
        ///
        /// The final run extends forever — either it is the fill, or it is the
        /// last element and string2 pads with it — so this is the total length
        /// of everything before it. Once string1 reaches this position, no
        /// later position can differ, which is what lets a character class be
        /// mapped without enumerating it.
        fn constant_from(&self) -> (usize, DataTypeWithData) {
            match self.runs.split_last() {
                Some(((element, _), rest)) => {
                    let before = rest.iter().map(|&(_, len)| len.unwrap_or(0_usize)).sum();
                    (before, element.clone())
                }
                // An empty string2 is rejected before here.
                None => (0_usize, DataTypeWithData::Is7Bit(0_u8)),
            }
        }

        /// The element that covers the bulk of a long span: the `[c*]` fill,
        /// which is sized to absorb whatever string1 has left over, or else
        /// the final element, which repeats once string2 runs short. Either
        /// way it is the one a large character class mostly maps to.
        fn bulk(&self) -> DataTypeWithData {
            self.slack_absorber()
        }

        /// The single character a complement maps every non-member to.
        ///
        /// `[c*]` is the construct that means "as many as needed", so when
        /// string2 has one it covers the complement -- `tr -c a '[x*]y'`
        /// replaces with `x`, not with the `y` that merely happens to be
        /// written last. Without a fill, the last character stands.
        fn complement_replacement(&self) -> DataTypeWithData {
            self.slack_absorber()
        }

        /// The element that stands in for however much string1 has left over:
        /// the `[c*]` fill, or the final element when there is none.
        fn slack_absorber(&self) -> DataTypeWithData {
            self.fill
                .and_then(|index| self.runs.get(index))
                .or_else(|| self.runs.last())
                .map_or(DataTypeWithData::Is7Bit(0_u8), |(element, _)| {
                    element.clone()
                })
        }

        /// The element at `index`.
        fn at(&self, index: usize) -> DataTypeWithData {
            let mut seen = 0_usize;
            for (element, len) in &self.runs {
                match len {
                    // The fill, or the final run: everything from here on.
                    None => return element.clone(),
                    Some(n) => {
                        if index < seen + n {
                            return element.clone();
                        }
                        seen += n;
                    }
                }
            }
            // Past the end: string2 pads with its last element.
            self.runs
                .last()
                .map_or(DataTypeWithData::Is7Bit(0_u8), |(element, _)| {
                    element.clone()
                })
        }
    }

    /// How many positions of the translation array each string1 operand fills.
    ///
    /// A character's repeat count is a number, an equivalence class is one
    /// position — the question `// Take up one position?` used to leave open,
    /// and whose absence from the old length counter made every padded string2
    /// fail with "Indexing failed". A class is as many positions as `LC_CTYPE`
    /// gives it, which is the one answer that costs something to compute.
    fn string1_span(op: &Operand) -> Result<usize, Box<dyn Error>> {
        match op {
            Operand::Char(CharOperand {
                char_repetition, ..
            }) => match char_repetition {
                CharRepetition::AsManyAsNeeded => Err(Box::from(
                    "the [c*] repeat construct may not appear in string1".to_owned(),
                )),
                CharRepetition::N(n) => Ok(*n),
            },
            Operand::Equiv(_) => Ok(1_usize),
            Operand::Class(name) => Ok(name.members().count()),
        }
    }

    pub fn generate_for_translation(
        complement: bool,
        complement_chars: bool,
        string1_operands: Vec<Operand>,
        string2_operands: &[Operand],
    ) -> Result<ForTranslation, Box<dyn Error>> {
        // Case conversion is a function over the locale's toupper/tolower
        // mapping, not a pairing of two enumerated arrays.
        let (case_folds, string1_operands, string2_owned) = match (!complement)
            .then(|| extract_case_folds(&string1_operands, string2_operands))
            .flatten()
        {
            Some((folds, s1, s2)) => (folds, s1, s2),
            None => (Vec::new(), string1_operands, string2_operands.to_vec()),
        };

        // The complemented forms map every *non*-member to one character, so
        // they need membership and a single replacement — never an array.
        if complement {
            let mut set = Set::default();
            for op in &string1_operands {
                match op {
                    Operand::Char(CharOperand {
                        char,
                        char_repetition,
                    }) => {
                        if matches!(char_repetition, CharRepetition::AsManyAsNeeded) {
                            return Err(Box::from(
                                "the [c*] repeat construct may not appear in string1".to_owned(),
                            ));
                        }
                        set.push_element(char);
                    }
                    Operand::Equiv(EquivOperand { char }) => {
                        set.push_element(char);
                        if let Some(c) = char.as_char() {
                            set.push_equivalence(c);
                        }
                    }
                    Operand::Class(name) => set.push_class(*name),
                }
            }

            let replacements = Replacements::build(&string2_owned)?;
            let last = replacements.complement_replacement();

            return Ok(ForTranslation::Complemented(Box::new(
                ComplementedTranslation {
                    set,
                    char_wise: complement_chars,
                    replacement: last.convert_to_replacement(),
                },
            )));
        }

        let mut replacements = Replacements::build(&string2_owned)?;
        if replacements.fill_needs_string1_length() {
            // The one shape that has to know: `[x*]` with elements after it.
            let mut total = 0_usize;
            for op in &string1_operands {
                total = total
                    .checked_add(string1_span(op)?)
                    .ok_or("Arithmetic overflow")?;
            }
            replacements.size_fill(total);
        }
        let (constant_from, constant) = replacements.constant_from();

        let mut equiv = Vec::<(DataTypeWithData, FullChar)>::new();
        let mut class_rules = Vec::<ClassRule>::new();

        let mut seven_bit = [const { Option::<FullChar>::None }; 128_usize];
        let mut eight_bit = [const { Option::<FullChar>::None }; 256_usize];
        let mut multi_byte = [const { Option::<Vec<SearchAndReplace>>::None }; 256_usize];

        let mut encoding_buffer = [0_u8; 4_usize];

        let mut add_normal_char_with_replacement =
            |da: DataTypeWithData, replacement_char: FullChar| match da {
                DataTypeWithData::Is7Bit(ue) => {
                    seven_bit[usize::from(ue)] = Some(replacement_char);
                }
                DataTypeWithData::Is8Bit(ue) => {
                    eight_bit[usize::from(ue - 128_u8)] = Some(replacement_char);
                }
                DataTypeWithData::IsMultiByte(ch) => {
                    let st = ch.encode_utf8(&mut encoding_buffer);
                    let &[ue, ref rest @ ..] = st.as_bytes() else {
                        unreachable!();
                    };
                    let index = usize::from(ue - 128_u8);
                    let vec = multi_byte[index].get_or_insert_with(Vec::new);
                    let number_of_bytes = match rest.len() {
                        1_usize => SearchNumberOfBytes::One,
                        2_usize => SearchNumberOfBytes::Two,
                        _ => SearchNumberOfBytes::Three,
                    };
                    let mut payload = [0_u8; 3_usize];
                    payload[..rest.len()].copy_from_slice(rest);
                    vec.push(SearchAndReplace {
                        replacement: replacement_char,
                        number_of_bytes,
                        payload,
                    });
                }
            };

        // Walk string1 by position. Once past `constant_from`, every remaining
        // position holds the same replacement, so positions stop mattering --
        // and a class reached there needs no enumeration.
        let mut position = 0_usize;

        for op in &string1_operands {
            let uniform = position >= constant_from;

            match op {
                Operand::Char(CharOperand {
                    char,
                    char_repetition,
                }) => {
                    let n = match char_repetition {
                        CharRepetition::AsManyAsNeeded => {
                            return Err(Box::from(
                                "the [c*] repeat construct may not appear in string1".to_owned(),
                            ));
                        }
                        CharRepetition::N(n) => *n,
                    };
                    if uniform {
                        add_normal_char_with_replacement(
                            char.clone(),
                            constant.convert_to_replacement(),
                        );
                    } else {
                        // A character repeated in string1 is unspecified
                        // (118151-2), and every repetition writes the same
                        // table slot -- so only the last one is observable.
                        // Pair that position directly: looping to reach it
                        // meant `tr '[x*18446744073709551615]y' ab` never
                        // returned, and stopping the loop early would pair the
                        // first position instead, which is a different answer.
                        let last = position.saturating_add(n.saturating_sub(1_usize));
                        let replacement = replacements.at(last);
                        add_normal_char_with_replacement(
                            char.clone(),
                            replacement.convert_to_replacement(),
                        );
                    }
                    position = position.saturating_add(n);
                }
                Operand::Equiv(EquivOperand { char }) => {
                    // One position -- the question the old counter left open.
                    let replacement = if uniform {
                        constant.convert_to_replacement()
                    } else {
                        replacements.at(position).convert_to_replacement()
                    };
                    equiv.push((char.clone(), replacement));
                    // The literal itself also maps, so an explicit character
                    // beside its own equivalence class keeps priority.
                    add_normal_char_with_replacement(char.clone(), replacement);
                    position += 1_usize;
                }
                Operand::Class(name) => {
                    if uniform {
                        // Every member maps to the same character, so the class
                        // stays a predicate: membership follows LC_CTYPE and
                        // nothing is enumerated. This is the case POSIX blesses
                        // (118159-62, "map several characters into one").
                        class_rules.push(ClassRule {
                            name: *name,
                            replacement: constant.convert_to_replacement(),
                        });
                    } else {
                        // string2 spreads distinct characters across part of
                        // the class, so those members have to be paired one by
                        // one. POSIX leaves the order unspecified (118133-4);
                        // ascending is as good as any, and matches what the old
                        // ASCII enumeration gave.
                        //
                        // Most members map to whichever element absorbs the
                        // slack -- the `[c*]` fill, or the trailing element --
                        // so that one becomes the class rule and only the
                        // members that differ go in the byte tables, where they
                        // take priority over it.
                        //
                        // Enumerating all of them instead put tens of thousands
                        // of entries behind a single lead byte, each scanned
                        // linearly for every input byte: 0.6s per 900 KB of
                        // CJK, against nothing at all when the class is a rule.
                        let bulk = replacements.bulk().convert_to_replacement();
                        let mut members = 0_usize;
                        for (offset, member) in name.members().enumerate() {
                            members += 1_usize;
                            let replacement =
                                replacements.at(position + offset).convert_to_replacement();
                            if replacement.same_char(&bulk) {
                                continue;
                            }
                            add_normal_char_with_replacement(
                                crate::parsing::categorize_char(member),
                                replacement,
                            );
                        }
                        class_rules.push(ClassRule {
                            name: *name,
                            replacement: bulk,
                        });
                        position = position.saturating_add(members);
                    }
                }
            }
        }

        Ok(ForTranslation::NotComplemented(Box::new(
            NotComplementedTranslation {
                seven_bit,
                eight_bit,
                multi_byte,
                equiv,
                case_folds,
                class_rules,
            },
        )))
    }

    pub fn generate_for_removal(
        complement: bool,
        complement_chars: bool,
        string1_or_string2_operands: Vec<Operand>,
        is_string1: bool,
    ) -> Result<ForRemoval, Box<dyn Error>> {
        let mut set = Set::default();

        for op in string1_or_string2_operands {
            match op {
                Operand::Char(CharOperand {
                    char_repetition,
                    char,
                }) => {
                    if matches!(char_repetition, CharRepetition::AsManyAsNeeded) && is_string1 {
                        return Err(Box::from(
                            "the [c*] repeat construct may not appear in string1".to_owned(),
                        ));
                    }
                    // A repeat count says nothing about membership: `[x*5]` and
                    // `x` name the same one-element set. (`[c*]` in string2 is
                    // allowed when squeezing -- see `tr_non_standard_d_s`.)
                    set.push_element(&char);
                }
                Operand::Equiv(EquivOperand { char }) => {
                    // The literal itself, plus whatever else LC_COLLATE puts in
                    // its equivalence class.
                    set.push_element(&char);
                    if let Some(c) = char.as_char() {
                        set.push_equivalence(c);
                    }
                }
                // Membership follows LC_CTYPE, so a class is a predicate rather
                // than an enumeration.
                Operand::Class(name) => set.push_class(name),
            }
        }

        let removal = RemovalShared { set };

        let for_removal = if complement {
            ForRemoval::Complemented(Box::new(ComplementedRemoval {
                removal,
                char_wise: complement_chars,
            }))
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

    #[derive(Clone, Copy)]
    pub struct FullChar {
        // TODO
        // Determine how many bytes to write by the payload?
        pub number_of_bytes: FullCharNumberOfBytes,
        pub payload: [u8; 4_usize],
    }

    impl FullChar {
        /// Do these spell the same character? Compared over the used bytes
        /// only, since the payload beyond `number_of_bytes` is padding.
        pub fn same_char(&self, other: &FullChar) -> bool {
            let width = self.number_of_bytes as usize;
            width == other.number_of_bytes as usize
                && self.payload[..width] == other.payload[..width]
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
                // A one-byte (ASCII) character was unhandled and fell into the
                // `unreachable!()` below; case conversion is the first caller
                // to pass one.
                [a] => FullChar {
                    number_of_bytes: FullCharNumberOfBytes::One,
                    payload: [a, 0_u8, 0_u8, 0_u8],
                },
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
        /// The lookahead for a match of `extra` bytes beyond the lead byte.
        /// `None` for a single-byte match, which needs no lookahead at all.
        pub fn from_extra(extra: usize) -> Option<Self> {
            match extra {
                1_usize => Some(SearchNumberOfBytes::One),
                2_usize => Some(SearchNumberOfBytes::Two),
                3_usize => Some(SearchNumberOfBytes::Three),
                _ => None,
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

    pub struct ReplacementCheckResult {
        pub replacement: Option<FullChar>,
        pub match_lookahead_length: Option<SearchNumberOfBytes>,
        pub found_match: bool,
    }

    impl Translation for NotComplementedTranslation {
        fn case_folds(&self) -> &[CaseFold] {
            &self.case_folds
        }

        fn class_rules(&self) -> &[ClassRule] {
            &self.class_rules
        }

        #[inline]
        fn get_seven_bit_replacement(&self, ue: u8) -> ReplacementCheckResult {
            let index = usize::from(ue);

            let replacement = self.seven_bit[index];

            ReplacementCheckResult {
                replacement,
                match_lookahead_length: None,
                found_match: replacement.is_some(),
            }
        }

        #[inline]
        fn get_eight_bit_replacement(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            let index = usize::from(ue);

            if let Some(fu) = self.eight_bit[index] {
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
                                    replacement: Some(se.replacement),
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
        fn get_equiv_result(&self, ue: u8) -> ReplacementCheckResult {
            for (da, fu) in &self.equiv {
                match da {
                    DataTypeWithData::Is7Bit(uei) | DataTypeWithData::Is8Bit(uei) => {
                        if ue == *uei {
                            return ReplacementCheckResult {
                                replacement: Some(*fu),
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
        /// A complement maps every *non*-member to one character, so it needs
        /// membership and a replacement -- never an array. Asking the same
        /// `Set` the removal paths ask is what makes `-c '[:alpha:]'` agree
        /// with `-d '[:alpha:]'` about whether `é` is a letter.
        ///
        /// `found_match: true` here means "in the set, leave it alone"; a miss
        /// is the result, not the absence of one.
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            match self.set.matches_at(ue, next_bytes) {
                Some(width) => ReplacementCheckResult {
                    replacement: None,
                    match_lookahead_length: SearchNumberOfBytes::from_extra(width - 1_usize),
                    found_match: true,
                },
                None => {
                    // `-C` consumes the whole non-member character; `-c`
                    // replaces it one byte at a time. That is the entire
                    // difference between the two options.
                    let lookahead = if self.char_wise {
                        let width = self.set.element_width(ue, next_bytes);
                        SearchNumberOfBytes::from_extra(width - 1_usize)
                    } else {
                        None
                    };
                    ReplacementCheckResult {
                        replacement: Some(self.replacement),
                        match_lookahead_length: lookahead,
                        found_match: false,
                    }
                }
            }
        }

        fn get_seven_bit_replacement(&self, _ue: u8) -> ReplacementCheckResult {
            unreachable!("ComplementedTranslation overrides check")
        }

        fn get_eight_bit_replacement(&self, _ue: u8, _next: &[u8]) -> ReplacementCheckResult {
            unreachable!("ComplementedTranslation overrides check")
        }

        fn get_equiv_result(&self, _ue: u8) -> ReplacementCheckResult {
            unreachable!("ComplementedTranslation overrides check")
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

            // Case folds go ahead of the equivalence step, but the equivalence
            // step stays the final fall-through: on the complemented path it
            // returns the replacement with `found_match: false`, meaning "not in
            // the set, so substitute" — a result, not the absence of one.
            if !self.case_folds().is_empty() {
                let fold_check = self.get_case_fold_result(ue, next_bytes);
                if fold_check.found_match {
                    return fold_check;
                }
            }

            if !self.class_rules().is_empty() {
                let class_check = self.get_class_rule_result(ue, next_bytes);
                if class_check.found_match {
                    return class_check;
                }
            }

            self.get_equiv_result(ue)
        }

        /// Case conversion, applied per character rather than from a table.
        /// The default has no folds, so implementations without them pay only a
        /// slice check.
        fn get_case_fold_result(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            let folds = self.case_folds();
            if !folds.is_empty() {
                if let Some((c, width)) = decode_one(ue, next_bytes) {
                    for fold in folds {
                        if let Some(replacement) = fold.apply(c) {
                            return ReplacementCheckResult {
                                replacement: Some(replacement),
                                match_lookahead_length: SearchNumberOfBytes::from_extra(
                                    width - 1_usize,
                                ),
                                found_match: true,
                            };
                        }
                    }
                }
            }
            ReplacementCheckResult {
                replacement: None,
                match_lookahead_length: None,
                found_match: false,
            }
        }

        fn case_folds(&self) -> &[CaseFold] {
            &[]
        }

        /// A whole class mapped to one character, asked of the *decoded*
        /// character so membership follows `LC_CTYPE`. Enumerating the class
        /// into the byte tables instead is what stopped `tr '[:alpha:]' X` at
        /// ASCII while `tr -d '[:alpha:]'` went by the locale.
        fn get_class_rule_result(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult {
            let rules = self.class_rules();
            if !rules.is_empty() {
                if let Some((c, width)) = decode_one(ue, next_bytes) {
                    for rule in rules {
                        if rule.name.contains(c) {
                            return ReplacementCheckResult {
                                replacement: Some(rule.replacement),
                                match_lookahead_length: SearchNumberOfBytes::from_extra(
                                    width - 1_usize,
                                ),
                                found_match: true,
                            };
                        }
                    }
                }
            }
            ReplacementCheckResult {
                replacement: None,
                match_lookahead_length: None,
                found_match: false,
            }
        }

        fn class_rules(&self) -> &[ClassRule] {
            &[]
        }

        fn get_seven_bit_replacement(&self, ue: u8) -> ReplacementCheckResult;

        fn get_eight_bit_replacement(&self, ue: u8, next_bytes: &[u8]) -> ReplacementCheckResult;

        fn get_equiv_result(&self, ue: u8) -> ReplacementCheckResult;
    }

    /// A `[:lower:]`↔`[:upper:]` pairing, applied as a *function* rather than a
    /// table.
    ///
    /// POSIX 118125-118130: when `[:lower:]` appears in string1 and
    /// `[:upper:]` in string2, "the arrays shall contain the characters from
    /// the toupper mapping in the LC_CTYPE category of the current locale".
    /// Materializing those arrays and pairing them by index — which is how case
    /// conversion used to happen, entirely by accident of both ASCII classes
    /// having 26 members in the same order — cannot work once the classes
    /// follow the locale: they differ in length and in order, and the
    /// "string2 is shorter" rule would silently pad with the last character.
    #[derive(Clone, Copy)]
    pub struct CaseFold {
        pub from: ClassName,
        pub to_upper: bool,
    }

    impl CaseFold {
        fn apply(&self, c: char) -> Option<FullChar> {
            if !self.from.contains(c) {
                return None;
            }
            let mapped = if self.to_upper {
                plib::locale::to_upper(c)
            } else {
                plib::locale::to_lower(c)
            };
            Some(FullChar::new_from_char(mapped))
        }
    }

    pub struct NotComplementedTranslation {
        pub seven_bit: [Option<FullChar>; 128_usize],
        pub eight_bit: [Option<FullChar>; 256_usize],
        pub multi_byte: [Option<Vec<SearchAndReplace>>; 256_usize],
        pub equiv: Vec<(DataTypeWithData, FullChar)>,
        /// Case-conversion pairings, consulted after the tables miss.
        pub case_folds: Vec<CaseFold>,
        /// Whole classes mapped to one character. Membership follows LC_CTYPE,
        /// so `é` is translated by `[:alpha:]` exactly as it is deleted by it.
        pub class_rules: Vec<ClassRule>,
    }

    /// A `[:class:]` in string1 whose every member maps to the same character.
    ///
    /// The common shape -- `tr '[:space:]' ' '`, `tr -cs '[:alpha:]' '[\n*]'`
    /// -- and the only one POSIX defines a meaning for (118159-62). Keeping it
    /// a predicate is what makes class membership follow the locale instead of
    /// stopping at ASCII.
    #[derive(Clone, Copy)]
    pub struct ClassRule {
        pub name: ClassName,
        pub replacement: FullChar,
    }

    pub struct ComplementedTranslation {
        /// `-C`: complement over characters rather than byte values, so a
        /// non-member multi-byte character is consumed and replaced whole.
        pub char_wise: bool,
        pub replacement: FullChar,
        pub set: Set,
    }

    pub struct RemovalShared {
        pub set: Set,
    }

    pub struct RemovalCheckResult {
        pub matched: bool,
        pub match_lookahead_length: Option<SearchNumberOfBytes>,
    }

    impl RemovalShared {
        /// Ask the set once. Every representation the set needs to consult --
        /// literal characters, raw bytes, `LC_CTYPE` classes, `LC_COLLATE`
        /// equivalence classes -- lives behind `matches_at`, so `-d`, `-s` and
        /// the complement forms cannot disagree about what a class contains.
        ///
        /// The result keeps `check`'s established contract: a lookahead is
        /// reported only for a *match* wider than one byte, because a caller
        /// that missed advances one byte at a time by design. The complement
        /// forms supply their own width on a miss.
        #[inline]
        fn check(&self, ue: u8, next_bytes: &[u8]) -> RemovalCheckResult {
            match self.set.matches_at(ue, next_bytes) {
                Some(width) => RemovalCheckResult {
                    matched: true,
                    match_lookahead_length: SearchNumberOfBytes::from_extra(width - 1_usize),
                },
                None => RemovalCheckResult {
                    matched: false,
                    match_lookahead_length: None,
                },
            }
        }
    }

    pub struct NotComplementedRemoval {
        removal: RemovalShared,
    }

    pub struct ComplementedRemoval {
        removal: RemovalShared,
        /// See [`ComplementedTranslation::char_wise`].
        char_wise: bool,
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

            // A character *not* in the set is the one being acted on here, and
            // the inner check reported no lookahead for it because it matched
            // nothing. Under `-C` that whole character is the unit, so `tr -C`
            // deletes or squeezes it once rather than byte by byte -- which is
            // the whole of the `-c`/`-C` difference. The width comes from the
            // set, so it follows LC_CTYPE like everything else.
            if self.char_wise && re.matched && re.match_lookahead_length.is_none() {
                let width = self.removal.set.element_width(ue, next_bytes);
                re.match_lookahead_length = SearchNumberOfBytes::from_extra(width - 1_usize);
            }

            re
        }
    }
}

mod transformation {
    use std::error::Error;
    use std::io::{self, ErrorKind, Read, Write};

    use crate::setup::{FullChar, FullCharNumberOfBytes, Removal, Translation};

    /// What an operation does with the unit at the front of the input.
    pub enum Action {
        /// Write the consumed input bytes through unchanged.
        Emit,
        /// Write this character instead of them.
        Replace(FullChar),
        /// Write nothing.
        Drop,
    }

    /// One operation step: what to do, and how much input it accounted for.
    ///
    /// `consumed` is the *input* width, and the driver advances by exactly it.
    /// That is the whole point of this type. When each operation moved the
    /// cursor itself, the width had to be reconstructed from whichever of three
    /// unrelated counts was at hand -- and the squeeze paths charged the number
    /// of bytes *written to output*, which is only the same number when nothing
    /// is being translated. It was not, once, and the stream slipped a byte per
    /// squeezed character.
    pub struct Step {
        pub consumed: usize,
        pub action: Action,
    }

    /// How much input a check accounted for: the byte under the cursor, plus
    /// any continuation bytes the match looked ahead over.
    fn consumed(lookahead: Option<crate::setup::SearchNumberOfBytes>) -> usize {
        1_usize + lookahead.map_or(0_usize, |se| se as usize)
    }

    /// One transformation, asked about the front of the input.
    pub trait Operation {
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step;
    }

    pub struct TransformResult {
        pub bytes_written: usize,
        pub leftover_bytes: usize,
    }

    /// Drive one buffer through an operation.
    ///
    /// A character is at most four bytes, so the main loop stops four bytes
    /// short and the tail is finished only on the last buffer, where no further
    /// input can complete a sequence.
    fn transform_buffer<T: Operation>(
        op: &mut T,
        input: &mut [u8],
        output: &mut [u8],
        last_iteration: bool,
    ) -> TransformResult {
        let input_len = input.len();
        let mut bytes_written = 0_usize;
        let mut index = 0_usize;

        let mut apply = |op: &mut T, index: &mut usize, bytes_written: &mut usize| {
            let lead = input[*index];
            let next_end = input_len.min(*index + 4_usize);
            let next_bytes = &input[(*index + 1_usize)..next_end];

            let step = op.step(lead, next_bytes);
            debug_assert!(step.consumed >= 1_usize);

            match step.action {
                Action::Drop => {}
                Action::Emit => {
                    // Exactly the bytes that were consumed -- so an output
                    // width can no longer stand in for an input width.
                    if step.consumed == 1_usize {
                        // Overwhelmingly the common case, and a slice copy of
                        // one byte is not free: this is the hot loop for every
                        // byte tr passes through.
                        output[*bytes_written] = lead;
                        *bytes_written += 1_usize;
                    } else {
                        let end = input_len.min(*index + step.consumed);
                        let taken = end - *index;
                        output[*bytes_written..(*bytes_written + taken)]
                            .copy_from_slice(&input[*index..end]);
                        *bytes_written += taken;
                    }
                }
                Action::Replace(full_char) => {
                    *bytes_written += full_char.write_full_char(&mut output[*bytes_written..]);
                }
            }

            *index += step.consumed;
        };

        while index + 4_usize <= input_len {
            apply(op, &mut index, &mut bytes_written);
        }

        let leftover_bytes = if last_iteration {
            while index < input_len {
                apply(op, &mut index, &mut bytes_written);
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

    pub fn streaming_transform<T: Operation>(t: &mut T) -> Result<(), Box<dyn Error>> {
        const SIZE: usize = 8_usize * 1_024_usize;

        let mut input = vec![0_u8; SIZE];
        // The worst case is every one-byte input character translated to a
        // four-byte one.
        let mut output = vec![0_u8; SIZE * 4_usize];

        let mut leftover_bytes = 0_usize;

        let mut stdin_lock = io::stdin().lock();
        let mut stdout_lock = io::stdout().lock();

        loop {
            let buf = &mut input[leftover_bytes..];

            match stdin_lock.read(buf) {
                Ok(0_usize) => {
                    let result =
                        transform_buffer(t, &mut input[..leftover_bytes], &mut output, true);
                    stdout_lock.write_all(&output[..(result.bytes_written)])?;
                    break;
                }
                Ok(us) => {
                    let read_slice = &mut input[..(leftover_bytes + us)];
                    let result = transform_buffer(t, read_slice, &mut output, false);
                    leftover_bytes = result.leftover_bytes;
                    stdout_lock.write_all(&output[..(result.bytes_written)])?;
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

    /// The character last written, and whether it is one the squeeze set
    /// collapses. One type, where each squeezing operation used to declare its
    /// own identical copy.
    #[derive(Default)]
    pub struct LastWritten {
        printed: Option<(FullChar, bool)>,
    }

    impl LastWritten {
        /// Would writing `candidate` repeat a squeezable character?
        fn repeats(&self, candidate: &FullChar) -> bool {
            match &self.printed {
                Some((last, true)) => last.same_char(candidate),
                _ => false,
            }
        }

        fn record(&mut self, char: FullChar, squeezable: bool) {
            self.printed = Some((char, squeezable));
        }
    }

    /// The input unit at the cursor, as a character to compare and write.
    fn input_char(lead: u8, next_bytes: &[u8], consumed: usize) -> FullChar {
        let mut payload = [0_u8; 4_usize];
        payload[0_usize] = lead;
        let extra = consumed - 1_usize;
        payload[1_usize..(1_usize + extra)].copy_from_slice(&next_bytes[..extra]);
        FullChar {
            number_of_bytes: match consumed {
                1_usize => FullCharNumberOfBytes::One,
                2_usize => FullCharNumberOfBytes::Two,
                3_usize => FullCharNumberOfBytes::Three,
                _ => FullCharNumberOfBytes::Four,
            },
            payload,
        }
    }

    pub struct DeleteTransformation<T: Removal> {
        pub removal: T,
    }

    impl<T: Removal> Operation for DeleteTransformation<T> {
        #[inline]
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step {
            let check = self.removal.check(lead, next_bytes);
            Step {
                consumed: consumed(check.match_lookahead_length),
                action: if check.matched {
                    Action::Drop
                } else {
                    Action::Emit
                },
            }
        }
    }

    pub struct TranslateTransformation<T: Translation> {
        pub translation: T,
    }

    impl<T: Translation> Operation for TranslateTransformation<T> {
        #[inline]
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step {
            let check = self.translation.check(lead, next_bytes);
            Step {
                consumed: consumed(check.match_lookahead_length),
                action: match check.replacement {
                    Some(full_char) => Action::Replace(full_char),
                    None => Action::Emit,
                },
            }
        }
    }

    pub struct SqueezeTransformation<T: Removal> {
        pub squeeze: T,
        pub last: LastWritten,
    }

    impl<T: Removal> Operation for SqueezeTransformation<T> {
        #[inline]
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step {
            let check = self.squeeze.check(lead, next_bytes);
            let consumed = consumed(check.match_lookahead_length);
            let candidate = input_char(lead, next_bytes, consumed);

            if check.matched && self.last.repeats(&candidate) {
                return Step {
                    consumed,
                    action: Action::Drop,
                };
            }

            self.last.record(candidate, check.matched);
            Step {
                consumed,
                action: Action::Emit,
            }
        }
    }

    pub struct DeleteAndSqueezeTransformation<T: Removal> {
        pub delete: T,
        pub squeeze: crate::setup::NotComplementedRemoval,
        pub last: LastWritten,
    }

    impl<T: Removal> Operation for DeleteAndSqueezeTransformation<T> {
        #[inline]
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step {
            let delete_check = self.delete.check(lead, next_bytes);
            if delete_check.matched {
                return Step {
                    consumed: consumed(delete_check.match_lookahead_length),
                    action: Action::Drop,
                };
            }

            // Survived deletion, so the squeeze set decides. Its own check
            // supplies the width, since the two sets need not agree on how far
            // a member reaches.
            let squeeze_check = self.squeeze.check(lead, next_bytes);
            let consumed = consumed(squeeze_check.match_lookahead_length)
                .max(consumed(delete_check.match_lookahead_length));
            let candidate = input_char(lead, next_bytes, consumed);

            if squeeze_check.matched && self.last.repeats(&candidate) {
                return Step {
                    consumed,
                    action: Action::Drop,
                };
            }

            self.last.record(candidate, squeeze_check.matched);
            Step {
                consumed,
                action: Action::Emit,
            }
        }
    }

    pub struct SqueezeAndTranslateTransformation<T: Translation> {
        pub translation: T,
        pub squeeze: crate::setup::NotComplementedRemoval,
        pub last: LastWritten,
    }

    impl<T: Translation> Operation for SqueezeAndTranslateTransformation<T> {
        #[inline]
        fn step(&mut self, lead: u8, next_bytes: &[u8]) -> Step {
            let check = self.translation.check(lead, next_bytes);
            // The translation's lookahead is over the *input*; what gets
            // written may be a different width entirely, and charging that to
            // the cursor is the defect this design removes.
            let consumed = consumed(check.match_lookahead_length);

            let written = match check.replacement {
                Some(full_char) => full_char,
                None => input_char(lead, next_bytes, consumed),
            };

            // POSIX 118172-4: squeezing happens *after* translation, on the
            // character that is about to be written.
            let payload = written.payload;
            let width = written.number_of_bytes as usize;
            let squeeze_check = self
                .squeeze
                .check(payload[0_usize], &payload[1_usize..width]);

            if squeeze_check.matched && self.last.repeats(&written) {
                return Step {
                    consumed,
                    action: Action::Drop,
                };
            }

            self.last.record(written, squeeze_check.matched);
            Step {
                consumed,
                action: Action::Replace(written),
            }
        }
    }
}
