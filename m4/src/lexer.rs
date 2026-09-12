//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! REQUIEREMENTS:
//!
//! * Because m4 supports streaming input and output, it seems like we should probably support
//!   streaming lexing/parsing so that we don't run out of RAM.
//! * For good performance it seems like the lexer should probably take into account the current state of the macro
//!   definitions, otherwise potentially any input word not matching builtin macros could be a macro and we will need to re-analyze it in a second phase. Also I think there is the possibility to undefine builtin macros? in which case this is absolutely necessary. This seems relevant for nom https://github.com/rust-bakery/nom/issues/1419
//!   So it seems like a good optimization that once we know a word is not a current macro name, to
//!   forget trying to parse the rest of it as a macro.
//! * Perhaps this might be useful https://github.com/fflorent/nom_locate/blob/master/README.md
//!
//! Taking a look at this BSD licensed code
//! https://github.com/chimera-linux/bsdm4/blob/master/main.c
//!
//! TODO: Recoverable parsing warnings should be emitted to stderr
//!
//! TODO: should quotes support alphanumeric characters? Seems like GNU m4 at least doesn't.
//!
use std::ffi::OsStr;

use nom::IResult;

#[derive(Clone, Hash, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MacroParseConfig {
    pub name: MacroName,
    /// Some builtin macros (like `define`) require args (in brackets, even if the brackets are
    /// empty, this counts as a single empty argument). At least this many args are required in
    /// order to parse a macro using this macro name.
    pub min_args: usize,
}

/// Configuration for parsing, affects what are considered macros, quotes or comments. Also keeps a
/// record of the recusion limit for processing a [`Symbol`].
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub(crate) struct ParseConfig {
    // TODO(performance): Can probably optimize using something like smallvec
    pub quote_open_tag: Vec<u8>,
    pub quote_close_tag: Vec<u8>,
    pub comment_open_tag: Vec<u8>,
    pub comment_close_tag: Vec<u8>,
    pub comment_enabled: bool,
}

pub const DEFAULT_QUOTE_OPEN_TAG: &[u8] = b"`";
pub const DEFAULT_QUOTE_CLOSE_TAG: &[u8] = b"'";
pub const DEFAULT_COMMENT_OPEN_TAG: &[u8] = b"#";
pub const DEFAULT_COMMENT_CLOSE_TAG: &[u8] = b"\n";

impl Default for ParseConfig {
    fn default() -> Self {
        Self {
            quote_open_tag: DEFAULT_QUOTE_OPEN_TAG.to_vec(),
            quote_close_tag: DEFAULT_QUOTE_CLOSE_TAG.to_vec(),
            comment_open_tag: DEFAULT_COMMENT_OPEN_TAG.to_vec(),
            comment_close_tag: DEFAULT_COMMENT_CLOSE_TAG.to_vec(),
            comment_enabled: true,
        }
    }
}

// TODO(performance): small vec optimization could be possible
/// The name of a macro.
#[derive(PartialEq, Clone, Hash, Eq)]
pub struct MacroName(pub Vec<u8>);

impl std::fmt::Debug for MacroName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MacroName")
            .field(&String::from_utf8_lossy(&self.0))
            .finish()
    }
}

impl std::fmt::Display for MacroName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&String::from_utf8_lossy(&self.0))
    }
}

impl MacroName {
    /// Macro names shall consist of letters, digits, and underscores, where the first character is
    /// not a digit. Tokens not of this form shall not be treated as macros.
    /// `[_a-zA-Z][_a-zA-Z0-9]*`
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        log::trace!(
            "MacroName::parse() input {:?}",
            String::from_utf8_lossy(input)
        );
        if input.is_empty() {
            log::trace!("MacroName::parse() empty macro name");
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::NonEmpty,
            )));
        }
        log::trace!("MacroName::parse() parsing the start of the macro name");
        let (remaining, start) = nom::bytes::complete::take_while1(is_word_char_start)(input)?;
        log::trace!(
            "MacroName::parse() found macro name start: {:?}",
            String::from_utf8_lossy(input)
        );
        let (remaining, rest) = nom::bytes::complete::take_while(is_word_char_end)(remaining)?;
        Ok((
            remaining,
            Self(input[..(start.len() + rest.len())].to_vec()),
        ))
    }

    pub fn parse_cmd(input: &OsStr) -> std::result::Result<Self, clap::Error> {
        let input_bytes = input.as_encoded_bytes();
        MacroName::try_from_slice(input_bytes).map_err(|_error| {
            invalid_name_error(&format!("-U <{UNDEFINE_VALUE_NAME}>"), input_bytes)
        })
    }

    /// Parse macro name from a complete slice, not including the EOF byte.
    /// Mostly used for testing, use [`MacroName::parse`] instead for parsing.
    pub fn try_from_slice(input: &[u8]) -> crate::error::Result<Self> {
        let (_remaining, name) = nom::combinator::all_consuming(Self::parse)(input)
            .map_err(|e| crate::Error::new(crate::ErrorKind::Parsing).add_context(e.to_string()))?;
        Ok(name)
    }
}

/// How `-D` and `-U` spell their option-argument. These are the strings the
/// diagnostic quotes back, so they have to agree with the `value_name` each
/// `clap::Arg` is built with; naming them once is what keeps the two in step.
pub(crate) const DEFINE_VALUE_NAME: &str = "name[=value]";
pub(crate) const UNDEFINE_VALUE_NAME: &str = "name";

/// A `clap::Error` for an option-argument that is not a name token, naming
/// both the option it came from and the value, so the rendered diagnostic
/// reads like every other bad-option-argument message.
///
/// Rejecting the option is a deliberate divergence: GNU m4 accepts a `-D` or
/// `-U` whose name is not a name token and silently ignores it, so a caller
/// asking for something m4 cannot do hears nothing back. Saying so costs a
/// build that passes, say, a hyphenated name, which is the point -- silent
/// acceptance is the defect, not the diagnostic.
pub(crate) fn invalid_name_error(arg: &str, value: &[u8]) -> clap::Error {
    use clap::error::{ContextKind, ContextValue, ErrorKind};
    let mut e = clap::Error::new(ErrorKind::ValueValidation);
    e.insert(
        ContextKind::InvalidArg,
        ContextValue::String(arg.to_string()),
    );
    e.insert(
        ContextKind::InvalidValue,
        ContextValue::String(String::from_utf8_lossy(value).to_string()),
    );
    e
}

// These classify one byte, in the locale `plib::diag::init_locale` installed.
// `c.into()` widens a u8 to 0..=255, which is the `unsigned char` domain
// `isalnum` and friends are defined over, so no call below can index outside
// the ctype table.
//
// Byte classification is the whole contract, not a step towards a wider one.
// A name token is a sequence of bytes the locale calls alphanumeric, which is
// right for every single-byte encoding -- under ISO-8859-1 `isalpha(0xe9)` is
// true and `café` is a name -- and under a multibyte encoding it means a name
// is ASCII, which is what GNU m4 does too. Probed against GNU m4 1.4.19 in a
// UTF-8 locale: `define(café, ...)` is refused by both, and every expansion
// agrees byte for byte. Decoding names through `mbrtowc`/`iswalpha` would
// create that divergence rather than remove one.
//
// Character semantics belong to the built-ins that count or index characters,
// where POSIX 103776 puts them, and they already have them: `len`, `substr`,
// `index` and `translit` go through `plib::locale::mb_char_slices`. That is
// where m4 is ahead of GNU, not behind it -- `len(café)` is 4 here and 5
// there, and GNU's `substr` will hand back half a UTF-8 sequence.
fn is_word_char_end(c: u8) -> bool {
    (unsafe { libc::isalnum(c.into()) } != 0) || c == b'_'
}

fn is_word_char_start(c: u8) -> bool {
    (unsafe { libc::isalpha(c.into()) } != 0) || c == b'_'
}

pub(crate) fn is_whitespace(c: u8) -> bool {
    (unsafe { libc::isblank(c.into()) != 0 }) || c == b'\n'
}

pub(crate) fn is_space(c: u8) -> bool {
    unsafe { libc::isspace(c.into()) != 0 }
}

pub(crate) fn is_alphnumeric(c: u8) -> bool {
    unsafe { libc::isalnum(c.into()) != 0 }
}

pub(crate) fn is_alpha(c: u8) -> bool {
    unsafe { libc::isalpha(c.into()) != 0 }
}
