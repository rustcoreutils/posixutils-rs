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

use std::io::{Read, Write};

/// Configuration for parsing, affects what are considered macros, quotes or comments. Also keeps a
/// record of the recusion limit for processing a [`Symbol`].
// TODO: Can probably optimize using something like smallvec
#[derive(Clone)]
pub(crate) struct ParseConfig {
    current_macro_names: Vec<MacroName>,
    quote_open_tag: Vec<u8>,
    quote_close_tag: Vec<u8>,
    comment_open_tag: Vec<u8>,
    comment_close_tag: Vec<u8>,
    symbol_recursion_limit: usize,
    // While this is true, we skip all following input until the end of the line.
    dnl: bool,
}

const DEFAULT_QUOTE_OPEN_TAG: &[u8] = b"`";
const DEFAULT_QUOTE_CLOSE_TAG: &[u8] = b"'";
const DEFAULT_COMMENT_OPEN_TAG: &[u8] = b"#";
const DEFAULT_COMMENT_CLOSE_TAG: &[u8] = b"\n";
const DEFAULT_SYMBOL_RECURSION_LIMIT: usize = 100;
static INBUILT_MACRO_NAMES: once_cell::sync::Lazy<Vec<MacroName>> =
    once_cell::sync::Lazy::new(|| {
        ["define", "dnl"]
            .iter()
            .map(|n| {
                MacroName::try_from_slice(n.as_bytes()).expect(&format!("failed to parse {n:?}"))
            })
            .collect()
    });

impl Default for ParseConfig {
    fn default() -> Self {
        Self {
            current_macro_names: INBUILT_MACRO_NAMES.clone(),
            quote_open_tag: DEFAULT_QUOTE_OPEN_TAG.to_vec(),
            quote_close_tag: DEFAULT_QUOTE_CLOSE_TAG.to_vec(),
            comment_open_tag: DEFAULT_COMMENT_OPEN_TAG.to_vec(),
            comment_close_tag: DEFAULT_COMMENT_CLOSE_TAG.to_vec(),
            symbol_recursion_limit: DEFAULT_SYMBOL_RECURSION_LIMIT,
            dnl: false,
        }
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Macro<'i> {
    name: MacroName,
    // TODO: can also be an expression in the case of the eval macro
    args: Vec<Vec<Symbol<'i>>>,
}

fn parse_macro_arg<'c>(
    config: &'c ParseConfig,
) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], Vec<Symbol<'i>>> + 'c {
    move |input: &[u8]| {
        println!(
            "parse_macro_arg() input: {:?}",
            String::from_utf8_lossy(input)
        );
        let mut symbols = Vec::new();

        let mut remaining = input;

        loop {
            println!(
                "parse_macro_arg() remaining: {:?}",
                String::from_utf8_lossy(remaining)
            );
            if let Some(b',' | b')') = remaining.get(0) {
                return Ok((remaining, symbols));
            }
            let (r, symbol) = nom::combinator::cut(Symbol::parse(config))(remaining)?;
            symbols.push(symbol);

            if r.is_empty() || r == b"\0" {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Fail,
                )));
            }
            remaining = r;
        }
    }
}

impl<'i> Macro<'i> {
    /// ## Notes
    ///
    /// * The parsing of macro arguments happens at the time of definition.
    /// * The unwrapping and parsing of quoted arguments happens at the time of calling.
    /// * The evaluation of expressions defined in macros happens at the time of calling.
    pub fn parse<'c>(config: &'c ParseConfig) -> impl Fn(&'i [u8]) -> IResult<&'i [u8], Self> + 'c {
        move |input: &[u8]| {
            #[cfg(test)]
            println!("Macro::parse() input: {:?}", String::from_utf8_lossy(input));
            let (remaining, name) = MacroName::parse(input)?;
            #[cfg(test)]
            println!("Macro::parse() successfully parsed macro name {name:?}");
            if !config.current_macro_names.contains(&name) {
                println!("Macro::parse() not a current macro name");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Fail,
                )));
            }
            println!(
                "Macro::parse() macro_args: {:?}",
                String::from_utf8_lossy(remaining)
            );
            let (remaining, args) = nom::combinator::opt(nom::sequence::delimited(
                nom::bytes::streaming::tag("("),
                nom::multi::separated_list0(
                    nom::bytes::streaming::tag(","),
                    // TODO: check, we should be allowed to have empty arguments?
                    //
                    // ERROR: this is broken! By excluding ), parsing macro name has no way to know
                    // if it's the end of input or not. If we do include it, text will just attempt
                    // to continually parse it.
                    //
                    // Perhaps we need an EOF input for config?
                    //
                    // Okay I need to do something different here, we need to parse to the closing
                    // tag, but we can't do that because we could encounter other closing tags,
                    // that could be contained within a comment.
                    //
                    // We need a way to tell the parsers that they are inside complete input here
                    parse_macro_arg(config),
                ),
                // Make sure we fail for input that is missing the closing tag, this is what GNU m4 does
                // anyway.
                nom::combinator::cut(nom::bytes::streaming::tag(")")),
            ))(remaining)?;

            Ok((
                remaining,
                Macro {
                    name,
                    args: args.unwrap_or_default(),
                },
            ))
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
pub(crate) enum Symbol<'i> {
    // Comments in m4 are not discarded, their contents (including delimeters) are copied verbatim to output without
    // further processing.
    Comment(&'i [u8]),
    Text(&'i [u8]),
    Quoted(Quoted<'i>),
    Macro(Macro<'i>),
    Newline,
    Eof,
}

#[cfg(test)]
impl<'i> std::fmt::Debug for Symbol<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(arg0) => f
                .debug_tuple("Comment")
                .field(&String::from_utf8_lossy(arg0))
                .finish(),
            Self::Text(arg0) => f
                .debug_tuple("Text")
                .field(&String::from_utf8_lossy(arg0))
                .finish(),
            Self::Quoted(arg0) => f.debug_tuple("Quoted").field(arg0).finish(),
            Self::Macro(arg0) => f.debug_tuple("Macro").field(arg0).finish(),
            Self::Newline => f.debug_tuple("Newline").finish(),
            Self::Eof => f.debug_tuple("Eof").finish(),
        }
    }
}

impl<'c, 'i: 'c> Symbol<'i> {
    fn parse(config: &'c ParseConfig) -> impl Fn(&'i [u8]) -> IResult<&'i [u8], Symbol<'i>> + 'c {
        move |input: &'i [u8]| {
            println!(
                "Symbol::parse() {:?}, current_macro_names: {:?}",
                String::from_utf8_lossy(input),
                config
                    .current_macro_names
                    .iter()
                    .map(|n| String::from_utf8_lossy(&n.0))
                    .collect::<Vec<_>>()
            );
            if input.is_empty() {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::NonEmpty,
                )));
            }
            nom::branch::alt((
                nom::combinator::map(nom::bytes::streaming::tag(b"\n"), |_| Symbol::Newline),
                nom::combinator::map(nom::bytes::streaming::tag(b"\0"), |_| Symbol::Eof),
                nom::combinator::map(
                    Quoted::parse(&config.quote_open_tag, &config.quote_close_tag),
                    Symbol::Quoted,
                ),
                nom::combinator::map(
                    parse_comment(&config.comment_open_tag, &config.comment_close_tag),
                    Symbol::Comment,
                ),
                nom::combinator::map(Macro::parse(config), Symbol::Macro),
                nom::combinator::map(
                    parse_text(&config.quote_open_tag, &config.comment_open_tag),
                    Symbol::Text,
                ),
            ))(input)
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
struct Quoted<'i> {
    pub contents: &'i [u8],
}

#[cfg(test)]
impl std::fmt::Debug for Quoted<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Quoted")
            .field("contents", &String::from_utf8_lossy(self.contents))
            .finish()
    }
}

impl<'i> Quoted<'i> {
    fn parse(
        open_tag: &'i [u8],
        close_tag: &'i [u8],
    ) -> impl for<'c> Fn(&'c [u8]) -> IResult<&'c [u8], Quoted<'c>> + 'i {
        |input: &[u8]| {
            let mut nest_level = 0;

            let (mut remaining, _) = nom::bytes::complete::tag(&*open_tag)(input)?;
            nest_level += 1;

            let quote_start_index = input.len() - remaining.len();

            let quote_end_index = loop {
                if remaining.is_empty() {
                    return Err(nom::Err::Incomplete(nom::Needed::Unknown));
                }

                if remaining.starts_with(&*close_tag) {
                    if nest_level == 1 {
                        break input.len() - remaining.len();
                    }
                    remaining = &remaining[close_tag.len()..];
                    nest_level -= 1;
                    continue;
                }
                if remaining.starts_with(&*open_tag) {
                    remaining = &remaining[close_tag.len()..];
                    nest_level += 1;
                    continue;
                }

                remaining = &remaining[1..];
            };

            let quoted = &input[quote_start_index..quote_end_index];

            Ok((remaining, Quoted { contents: quoted }))
        }
    }
}

#[derive(PartialEq, Clone)]
struct MacroName(Vec<u8>);

#[cfg(test)]
impl std::fmt::Debug for MacroName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MacroName")
            .field(&String::from_utf8_lossy(&self.0))
            .finish()
    }
}

impl MacroName {
    /// Macro names shall consist of letters, digits, and underscores, where the first character is not a digit. Tokens not of this form shall not be treated as macros.
    /// `[_a-zA-Z][_a-zA-Z0-9]*`
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        println!(
            "MacroName::parse() input {:?}",
            String::from_utf8_lossy(input)
        );
        if input.is_empty() {
            println!("MacroName::parse() empty macro name");
            return Err(nom::Err::Incomplete(nom::Needed::Unknown));
        }
        println!("MacroName::parse() parsing the start of the macro name");
        let (remaining, start) = nom::bytes::streaming::take_while1(is_word_char_start)(input)?;
        println!(
            "MacroName::parse() found macro name start: {:?}",
            String::from_utf8_lossy(input)
        );
        let (remaining, rest) = nom::bytes::streaming::take_while(is_word_char_end)(remaining)?;
        Ok((
            remaining,
            Self(input[..(start.len() + rest.len())].to_vec()),
        ))
    }

    /// Parse macro name from a complete slice, not including the EOF byte.
    fn try_from_slice(input: &[u8]) -> Result<Self, String> {
        let mut input = input.to_vec();
        input.push(b'\0');
        Self::parse(input.as_slice())
            .map(|ok| ok.1)
            .map_err(|e| e.to_string())
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
fn is_whitespace(c: u8) -> bool {
    unsafe { libc::isblank(c.into()) != 0 }
}

fn is_alphnumeric(c: u8) -> bool {
    unsafe { libc::isalnum(c.into()) != 0 }
}

/// Parse input that we already know is not quoted and not a macro, consume input until it could
/// possibly be the beginning of either a quote or a macro, such as when an open quote is
/// encountered, or when we encounter the first alpha character after a non-alphanumeric character.
/// TODO: what happens if we encounter a close quote?
fn parse_text<'c>(
    quote_open_tag: &'c [u8],
    comment_open_tag: &'c [u8],
) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], &'i [u8]> + 'c {
    move |input: &[u8]| {
        println!("parse_text() input: {:?}", String::from_utf8_lossy(input));
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::Unknown));
        }

        let stop_tags = &[quote_open_tag, comment_open_tag, b"\n", b",", b")", b"\0"];
        let allowed_to_include_tags: &[&[u8]] = &[b",", b")"];
        let mut previous_was_alphanumeric = true;
        let mut stop_index = 0;
        for i in 0..input.len() {
            let current_is_alphanumeric = is_alphnumeric(input[i]);
            if current_is_alphanumeric && !previous_was_alphanumeric {
                println!("parse_text() found possible start of macro");
                let (matched, remaining) = input.split_at(stop_index + 1);
                println!(
                    "parse_text() successfully parsed text. matched: {:?}, remaining: {:?}, stop_index: {stop_index}",
                    String::from_utf8_lossy(matched),
                    String::from_utf8_lossy(remaining),
                );
                return Ok((remaining, matched));
            }
            for tag in stop_tags {
                if input[i..].starts_with(tag) {
                    println!(
                        "parse_text() found start of tag {:?}",
                        String::from_utf8_lossy(tag)
                    );
                    if i == 0 && !allowed_to_include_tags.contains(tag) {
                        println!("parse_text() Error found tag at start of input");
                        return Err(nom::Err::Error(nom::error::Error::new(
                            input,
                            nom::error::ErrorKind::Fail,
                        )));
                    }

                    let (matched, remaining) = input.split_at(stop_index + 1);
                    println!(
                        "parse_text() successfully parsed text. matched: {:?}, remaining: {:?}, stop_index: {stop_index}",
                        String::from_utf8_lossy(matched),
                        String::from_utf8_lossy(remaining),
                    );
                    assert!(!matched.is_empty());
                    return Ok((remaining, matched));
                }
            }
            stop_index = i;
            previous_was_alphanumeric = current_is_alphanumeric;
        }

        return Err(nom::Err::Incomplete(nom::Needed::Unknown));
    }
}

// TODO: changequote support
fn parse_comment<'a, 'b>(
    open_comment_tag: &'b [u8],
    close_comment_tag: &'b [u8],
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> + 'b {
    move |input: &[u8]| {
        let mut total_len = 0;
        let (remaining, open_tag) = nom::bytes::streaming::tag(open_comment_tag)(input)?;
        total_len += open_tag.len();
        // TODO this could probably be optimized
        let (remaining, contents) = nom::branch::alt((
            // This can be complete because the other branch is streaming and won't stop until the
            // end of the file.
            nom::bytes::complete::take_until(close_comment_tag),
            nom::bytes::streaming::take_until("\0"),
        ))(remaining)?;

        total_len += contents.len();
        let remaining = if !remaining.is_empty() && remaining[0] != b'\0' {
            let (remaining, final_tag) = nom::bytes::complete::tag(close_comment_tag)(remaining)?;
            total_len += final_tag.len();
            remaining
        } else {
            remaining
        };
        Ok((remaining, &input[..total_len]))
    }
}

/// Parse all input until either a newline or an EOF.
pub fn parse_dnl(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (remaining, _) = nom::bytes::streaming::take_till(|c| c == b'\0' || c == b'\n')(input)?;
    let (remaining, _) = nom::branch::alt((
        nom::bytes::streaming::tag("\n"),
        nom::combinator::peek(nom::bytes::streaming::tag("\0")),
    ))(remaining)?;
    Ok((remaining, &input[0..(input.len() - remaining.len())]))
}

/// `evaluate` is a function which takes the current [`ParseConfig`], and evaluates the provided
/// [`Symbol`] writing output to `writer`, and returns a [`ParseConfig`] which has been
/// modified during the process of evaluation (for example a new macro was defined, or
/// changequote).
pub(crate) fn process_streaming<'c, R: Read, W: Write, STATE>(
    mut evaluation_state: STATE,
    initial_config: ParseConfig,
    evaluate: impl for<'i, 'w> Fn(STATE, ParseConfig, Symbol<'i>, &'w mut W) -> (STATE, ParseConfig),
    mut reader: R,
    mut writer: W,
) {
    let buffer_size = 10;
    let buffer_growth_factor = 2;
    let mut buffer = circular::Buffer::with_capacity(buffer_size);
    let mut config = initial_config;
    let mut eof = false;

    loop {
        if eof {
            break;
        }
        let read = reader.read(buffer.space()).unwrap();

        // Insert an EOF null byte, this should not be used in any string encoding so it should be
        // fine, the streaming parsers rely on this to know whether they have reached the end of
        // the input.
        let fill_amount = if read == 0 {
            println!("process_streaming() inserting EOF");
            let eof_bytes = &[b'\0'];
            let space = buffer.space();
            assert!(space.len() > eof_bytes.len());
            space[0..eof_bytes.len()].copy_from_slice(eof_bytes);
            eof = true;
            eof_bytes.len()
        } else {
            read
        };
        buffer.fill(fill_amount);

        loop {
            // TODO: what to do if the buffer is empty after previous iteration?
            let input = buffer.data();
            dbg!(String::from_utf8_lossy(input));

            if input.is_empty() {
                break;
            }

            let remaining = if config.dnl {
                let result = parse_dnl(input);
                let remaining = match result {
                    Ok((remaining, _)) => remaining,
                    Err(nom::Err::Incomplete(_)) => {
                        let new_capacity = buffer_growth_factor * buffer.capacity();
                        buffer.grow(new_capacity);
                        break;
                    }
                    // TODO: handle unwrap
                    Err(error) => panic!("{}", error),
                };
                config.dnl = false;

                remaining
            } else {
                let result = Symbol::parse(&config)(input);
                let (remaining, symbol) = match result {
                    Ok(ok) => ok,
                    Err(nom::Err::Incomplete(_)) => {
                        let new_capacity = buffer_growth_factor * buffer.capacity();
                        buffer.grow(new_capacity);
                        break;
                    }
                    // TODO: handle unwrap
                    Err(error) => panic!("{}", error),
                };

                #[cfg(test)]
                dbg!(String::from_utf8_lossy(input), &symbol, remaining);

                if matches!(symbol, Symbol::Eof) {
                    return;
                }
                (evaluation_state, config) =
                    evaluate(evaluation_state, config, symbol, &mut writer);
                remaining
            };

            buffer.consume(input.len() - remaining.len());
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Symbol, DEFAULT_COMMENT_CLOSE_TAG, DEFAULT_COMMENT_OPEN_TAG};
    use std::io::Write;

    use super::{
        parse_comment, parse_dnl, parse_text, process_streaming, Macro, MacroName, ParseConfig,
        Quoted, DEFAULT_QUOTE_CLOSE_TAG, DEFAULT_QUOTE_OPEN_TAG,
    };

    fn snapshot_symbols_as_stream(initial_config: ParseConfig, input: &[u8]) -> String {
        let mut writer = Vec::new();
        process_streaming(
            (),
            initial_config,
            |state, config, symbol, writer| {
                writer.write_all(format!("{symbol:#?}").as_bytes()).unwrap();
                writer.write(b"\n").unwrap();
                (state, config)
            },
            input,
            &mut writer,
        );

        String::from_utf8(writer).unwrap()
    }

    fn utf8(input: &[u8]) -> String {
        String::from_utf8(input.to_vec()).unwrap()
    }

    fn macro_name(input: &[u8]) -> MacroName {
        MacroName::try_from_slice(input).expect("Error parsing macro name")
    }

    #[test]
    fn test_parse_macro_name_end_eof() {
        let (remaining, name) = MacroName::parse(b"hello\0").unwrap();
        assert_eq!(name, MacroName(b"hello".into()));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_macro_name_end_bracket() {
        let (remaining, name) = MacroName::parse(b"hello)").unwrap();
        assert_eq!(name, MacroName(b"hello".into()));
        assert_eq!(")", utf8(remaining));
    }

    #[test]
    fn test_parse_macro_name_end_newline() {
        let (remaining, name) = MacroName::parse(b"hello\n").unwrap();
        assert_eq!(name, MacroName(b"hello".into()));
        assert_eq!("\n", utf8(remaining));
    }

    #[test]
    fn test_parse_macro_name_start_newline() {
        let _error = MacroName::parse(b"\nhello").unwrap_err();
    }

    #[test]
    fn test_parse_macro_name_underscore_number() {
        let (remaining, name) = MacroName::parse(b"some_word_23\0").unwrap();
        assert_eq!(name, MacroName(b"some_word_23".into()));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_macro_name_fail_number_start() {
        MacroName::parse(b"22word\0").unwrap_err();
    }

    #[test]
    fn test_parse_macro_with_name_fail_not_in_list() {
        Macro::parse(&ParseConfig {
            current_macro_names: vec![],
            ..ParseConfig::default()
        })(b"some_word_23")
        .unwrap_err();
    }

    #[test]
    fn test_parse_macro_with_name_only() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(&config)(b"some_word_23\0").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_macro_args_empty() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(&config)(b"some_word_23()").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        assert_eq!(m.args.get(0).unwrap().len(), 0);
        assert!(remaining.is_empty());
    }

    #[test]
    fn test_parse_macro_args_1_symbol() {
        let current_macro_names = vec![macro_name(b"some_word_23"), macro_name(b"hello")];
        let config = &ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(config)(b"some_word_23(hello)").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        assert!(remaining.is_empty());
        let m1 = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Macro(m1) => m1,
            _ => panic!(),
        };
        assert_eq!(m1.name, MacroName(b"hello".into()));
    }

    #[test]
    fn test_parse_macro_args_1_text_after() {
        let (remaining, m) = Macro::parse(&ParseConfig::default())(b"define(hello)m1").unwrap();
        assert_eq!(m.name, MacroName(b"define".into()));
        assert_eq!(m.args.len(), 1);
        assert_eq!("m1", utf8(remaining));
        match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Text(text) => {
                assert_eq!("hello", utf8(text));
            }
            _ => panic!(),
        };
    }

    #[test]
    fn test_parse_macro_args_1_symbol_quoted() {
        let current_macro_names = vec![macro_name(b"some_word_23"), macro_name(b"hello")];
        let config = &ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(config)(b"some_word_23(`hello')").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        assert!(remaining.is_empty());
        let q = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Quoted(q) => q,
            _ => panic!(),
        };
        assert_eq!(q.contents, b"hello");
    }

    #[test]
    fn test_parse_macro_args_1_2_symbols() {
        let current_macro_names = vec![
            macro_name(b"some_word_23"),
            macro_name(b"hello"),
            macro_name(b"world"),
        ];
        let config = &ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let m = Macro::parse(&config)(b"some_word_23(hello world)")
            .unwrap()
            .1;
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        let m1 = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Macro(m) => m,
            _ => panic!(),
        };
        assert_eq!(m1.name, MacroName(b"hello".into()));
        let t = match m.args.get(0).unwrap().get(1).unwrap() {
            Symbol::Text(t) => t,
            _ => panic!(),
        };
        assert_eq!(t, b" ");
        let m3 = match m.args.get(0).unwrap().get(2).unwrap() {
            Symbol::Macro(m) => m,
            _ => panic!(),
        };
        assert_eq!(m3.name, MacroName(b"world".into()));
    }

    #[test]
    fn test_parse_macro_args_2() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(&config)(b"some_word_23(hello,world)").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        dbg!(&m.args);
        assert_eq!(m.args.len(), 2);
        assert_eq!(m.args.get(0).unwrap().len(), 1);
        assert_eq!(m.args.get(1).unwrap().len(), 1);
        let arg0 = m.args.get(0).unwrap().get(0).unwrap();
        match arg0 {
            Symbol::Text(text) => assert_eq!("hello", utf8(text)),
            _ => panic!(),
        }
        let arg1 = m.args.get(1).unwrap().get(0).unwrap();
        match arg1 {
            Symbol::Text(text) => assert_eq!("world", utf8(text)),
            _ => panic!(),
        }
        assert!(remaining.is_empty());
    }

    #[test]
    fn test_parse_macro_args_3_middle_empty() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let (remaining, m) = Macro::parse(&config)(b"some_word_23(hello,,world)").unwrap();
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        dbg!(&m.args);
        assert_eq!(m.args.len(), 3);
        assert_eq!(m.args.get(0).unwrap().len(), 1);
        assert_eq!(m.args.get(1).unwrap().len(), 0);
        assert_eq!(m.args.get(2).unwrap().len(), 1);
        assert!(remaining.is_empty());
    }

    #[test]
    fn test_parse_macro_args_fail_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        let current_macro_names = vec![macro_name(b"some_word_23")];
        Macro::parse(&ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        })(b"some_word_23(hello")
        .unwrap_err();
    }

    #[test]
    fn test_parse_macro_args_fail_empty_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        let current_macro_names = vec![macro_name(b"some_word_23")];
        Macro::parse(&ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        })(b"some_word_23(")
        .unwrap_err();
    }

    #[test]
    fn test_parse_quoted() {
        let quote = Quoted::parse(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`hello'")
            .unwrap()
            .1;
        assert_eq!("hello".as_bytes(), quote.contents);
    }

    #[test]
    fn test_parse_quoted_incomplete() {
        let error =
            Quoted::parse(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`hello").unwrap_err();
        assert!(matches!(error, nom::Err::Incomplete(_)));
    }

    #[test]
    fn test_parse_quoted_nested() {
        let quote =
            Quoted::parse(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`a `quote' is good!'")
                .unwrap()
                .1;
        assert_eq!("a `quote' is good!".as_bytes(), quote.contents);
    }

    #[test]
    fn test_parse_quoted_nested_incomplete() {
        let error =
            Quoted::parse(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`a `quote is good!'")
                .unwrap_err();
        assert!(matches!(error, nom::Err::Incomplete(_)));
    }

    #[test]
    fn test_parse_comment_standard() {
        let (remaining, comment) = parse_comment(
            DEFAULT_COMMENT_OPEN_TAG,
            DEFAULT_COMMENT_CLOSE_TAG,
        )(b"# hello world\ngoodbye\0")
        .unwrap();
        assert_eq!("# hello world\n", utf8(comment));
        assert_eq!("goodbye\0", utf8(remaining));
    }

    #[test]
    fn test_parse_comment_standard_no_newline() {
        let (remaining, comment) =
            parse_comment(DEFAULT_COMMENT_OPEN_TAG, DEFAULT_COMMENT_CLOSE_TAG)(b"# hello world\0")
                .unwrap();
        assert_eq!("# hello world", utf8(comment));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_comment_standard_incomplete() {
        let error =
            parse_comment(DEFAULT_COMMENT_OPEN_TAG, DEFAULT_COMMENT_CLOSE_TAG)(b"# hello world")
                .unwrap_err();
        assert!(matches!(error, nom::Err::Incomplete(_)));
    }

    #[test]
    fn test_parse_text() {
        let (remaining, text) =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"hello\0").unwrap();
        assert_eq!("hello", utf8(text));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_text_incomplete() {
        let error =
            parse_text(DEFAULT_COMMENT_OPEN_TAG, DEFAULT_COMMENT_CLOSE_TAG)(b"hello").unwrap_err();
        assert!(matches!(error, nom::Err::Incomplete(_)));
    }

    #[test]
    fn test_parse_text_before_quote_space() {
        let (remaining, text) =
            parse_text(b"`", DEFAULT_COMMENT_OPEN_TAG)(b"hello `world'").unwrap();
        assert_eq!("hello ", utf8(text));
        assert_eq!("`world'", utf8(remaining));
    }

    #[test]
    fn test_parse_text_before_close_bracket() {
        let (remaining, text) = parse_text(b"`", DEFAULT_COMMENT_OPEN_TAG)(b"hello)").unwrap();
        assert_eq!("hello", utf8(text));
        assert_eq!(")", utf8(remaining));
    }

    #[test]
    fn test_parse_text_before_comma() {
        let (remaining, text) = parse_text(b"`", DEFAULT_COMMENT_OPEN_TAG)(b"hello,").unwrap();
        assert_eq!("hello", utf8(text));
        assert_eq!(",", utf8(remaining));
    }

    #[test]
    fn test_parse_text_before_quote_no_space() {
        let (remaining, text) =
            parse_text(b"`", DEFAULT_COMMENT_OPEN_TAG)(b"hello`world'").unwrap();
        assert_eq!("hello", utf8(text));
        assert_eq!("`world'", utf8(remaining));
    }

    #[test]
    fn test_parse_text_before_non_alphanum() {
        let (remaining, text) =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"hello|world\0").unwrap();
        assert_eq!("hello|", utf8(text));
        assert_eq!("world\0", utf8(remaining));
        let (remaining, text) =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(remaining).unwrap();
        assert_eq!("world", utf8(text));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_text_before_newline() {
        let (remaining, text) =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"hello\nworld\0")
                .unwrap();
        assert_eq!("hello", utf8(text));
        assert_eq!("\nworld\0", utf8(remaining));
    }

    #[test]
    fn test_parse_text_newline_only() {
        let error =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"\n").unwrap_err();
        assert!(matches!(error, nom::Err::Error(_)));
    }

    #[test]
    fn test_parse_text_eof_only() {
        let error =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"\0").unwrap_err();
        assert!(matches!(error, nom::Err::Error(_)));
    }

    #[test]
    fn test_parse_symbol_newline() {
        let (remaining, symbol) = Symbol::parse(&ParseConfig::default())(b"\n\0").unwrap();
        match symbol {
            Symbol::Newline => {}
            _ => panic!(),
        }
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_symbol_macro_not_current() {
        let (remaining, symbol) = Symbol::parse(&ParseConfig::default())(b"m2(hello)\0").unwrap();
        match symbol {
            Symbol::Text(text) => assert_eq!("m2(", utf8(text)),
            _ => panic!(),
        }
        assert_eq!("hello)\0", utf8(remaining));
    }

    #[test]
    fn test_parse_symbol_text_then_close_bracket() {
        let (remaining, symbol) = Symbol::parse(&ParseConfig::default())(b"hello)\0").unwrap();
        match symbol {
            Symbol::Text(text) => assert_eq!("hello", utf8(text)),
            _ => panic!(),
        }
        assert_eq!(")\0", utf8(remaining));
    }

    #[test]
    fn test_parse_symbol_close_bracket() {
        let (remaining, symbol) = Symbol::parse(&ParseConfig::default())(b")\0").unwrap();
        match symbol {
            Symbol::Text(text) => assert_eq!(")", utf8(text)),
            _ => panic!(),
        }
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_symbol_macro_arg_1() {
        let (remaining, symbol) =
            Symbol::parse(&ParseConfig::default())(b"define(hello, error)dnl\0").unwrap();
        match symbol {
            Symbol::Macro(_) => {}
            _ => panic!(),
        }
        assert_eq!("dnl\0", utf8(remaining));
    }

    #[test]
    fn test_parse_symbol_newline_eof() {
        let (remaining, symbol) = Symbol::parse(&ParseConfig::default())(b"\n\0").unwrap();
        match symbol {
            Symbol::Newline => {}
            _ => panic!(),
        }
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_dnl() {
        let (remaining, matched) = parse_dnl(b" hello world\n\0").unwrap();
        assert_eq!(" hello world\n", utf8(matched));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_dnl_only_newline() {
        let (remaining, matched) = parse_dnl(b"\n\0").unwrap();
        assert_eq!("\n", utf8(matched));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_dnl_only_eof() {
        let (remaining, matched) = parse_dnl(b"\0").unwrap();
        assert_eq!("", utf8(matched));
        assert_eq!("\0", utf8(remaining));
    }

    #[test]
    fn test_parse_stream_symbols_dnl() {
        insta::assert_snapshot!(snapshot_symbols_as_stream(
            ParseConfig {
                dnl: true,
                ..ParseConfig::default()
            },
            b" this is some text\n"),
            @""
        );
    }

    #[test]
    fn test_parse_stream_symbols_evaluation_order() {
        let f = std::fs::read("fixtures/integration_tests/evaluation_order.m4").unwrap();
        insta::assert_snapshot!(snapshot_symbols_as_stream(ParseConfig::default(), &f));
    }
}
