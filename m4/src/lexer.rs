//! REQUIEREMENTS:
//!
//! * Because m4 supports streaming input and output, it seems like we should probably support
//! streaming lexing/parsing so that we don't run out of RAM.
//! * For good performance it seems like the lexer should probably take into account the current state of the macro
//! definitions, otherwise potentially any input word not matching builtin macros could be a macro and we will need to re-analyze it in a second phase. Also I think there is the possibility to undefine builtin macros? in which case this is absolutely necessary. This seems relevant for nom https://github.com/rust-bakery/nom/issues/1419
//!  So it seems like a good optimization that once we know a word is not a current macro name, to
//!  forget trying to parse the rest of it as a macro.
//! * Perhaps this might be useful https://github.com/fflorent/nom_locate/blob/master/README.md
//!
//! Taking a look at this BSD licensed code
//! https://github.com/chimera-linux/bsdm4/blob/master/main.c
//!
//! TODO: Recoverable parsing warnings should be emitted to stderr
//!
//! TODO: should quotes support alphanumeric characters? Seems like GNU m4 at least doesn't.
//!
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
    // TODO: Can probably optimize using something like smallvec
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

// TODO: small vec optimization could be possible
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

    /// Parse macro name from a complete slice, not including the EOF byte.
    /// Mostly used for testing, use [`MacroName::parse`] instead for parsing.
    pub fn try_from_slice(input: &[u8]) -> crate::error::Result<Self> {
        let (_remaining, name) = Self::parse(input)
            .map_err(|e| crate::Error::new(crate::ErrorKind::Parsing).add_context(e.to_string()))?;
        Ok(name)
    }
}

fn is_word_char_end(c: u8) -> bool {
    // TODO: check safety!
    (unsafe { libc::isalnum(c.into()) } != 0) || c == b'_'
}

fn is_word_char_start(c: u8) -> bool {
    // TODO: check safety!
    (unsafe { libc::isalpha(c.into()) } != 0) || c == b'_'
}

//TODO: these don't handle multibyte characters!
//
//It seems like we might want to use https://linux.die.net/man/3/mbrtowc for UTF-8 and any other
//multibyte encodings. Then https://linux.die.net/man/3/iswblank
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
