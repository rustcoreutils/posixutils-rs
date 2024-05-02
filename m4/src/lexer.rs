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
//! TODO: It's been decided that we want to support both single byte character encodings, and
//! UTF-8. We will ignore other variable length encodings, and other wide character encodings.
//! Will have to think how best to do this, perhaps we have two different parser implementations,
//! one that uses nom::bytes and the libc functions, and another that uses UTF-8.
//! We need to decide what to do in abscence of the LC_* telling us which encoding, do we just
//! assume it's UTF-8?
//! We'd like to try having UTF-8 support be the default and have a flag to disable it if extra
//! compatibility with legacy utilities is required, as it's technically feasible that with some
//! input it will result in different behaviour.
//!
//! TODO: Recoverable parsing warnings should be emitted to stderr
//!
//! TODO: should quotes support alphanumeric characters? Seems like GNU m4 at least doesn't.
//!
//! If the

use nom::IResult;

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Macro<'i> {
    name: MacroName,
    args: Vec<Vec<Symbol<'i>>>,
}

impl<'c, 'i: 'c> Macro<'i> {
    /// ## Notes
    ///
    /// * The parsing of macro arguments happens at the time of definition.
    /// * The unwrapping and parsing of quoted arguments happens at the time of calling.
    /// * The evaluation of expressions defined in macros happens at the time of calling.
    pub fn parse(
        // TODO: perhaps faster with a hashset?
        config: &'c ParseConfig,
    ) -> impl Fn(&'i [u8]) -> IResult<&'i [u8], Self> + 'c {
        move |input: &'i [u8]| {
            #[cfg(test)]
            dbg!(&config.current_macro_names);
            println!("parse_macro {:?}", String::from_utf8_lossy(input));
            let (remaining, name) = MacroName::parse(input)?;
            if !config.current_macro_names.contains(&name) {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Fail,
                )));
            }
            println!("macro_args {:?}", String::from_utf8_lossy(remaining));
            let (remaining, args) = nom::combinator::opt(nom::sequence::delimited(
                nom::bytes::complete::tag("("),
                nom::multi::separated_list0(
                    nom::bytes::complete::tag(","),
                    // TODO: check, we should be allowed to have empty arguments?
                    nom::combinator::map_parser(
                        nom::bytes::complete::is_not(")"),
                        nom::combinator::cut(parse_symbols(config)),
                    ),
                ),
                // Make sure we fail for input that is missing the closing tag, this is what GNU m4 does
                // anyway.
                nom::combinator::cut(nom::bytes::complete::tag(")")),
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
enum Symbol<'i> {
    // Comments in m4 are not discarded, their contents (including delimeters) are copied verbatim to output without
    // further processing.
    Comment(&'i [u8]),
    Text(&'i [u8]),
    Quoted(Quoted<'i>),
    Macro(Macro<'i>),
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
        }
    }
}

impl<'c, 'i: 'c> Symbol<'i> {
    fn parse(config: &'c ParseConfig) -> impl Fn(&'i [u8]) -> IResult<&'i [u8], Symbol<'i>> + 'c {
        move |input: &'i [u8]| {
            println!(
                "parse_symbol: {:?}, current_macro_names: {:?}",
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
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::TagClosure,
                    )));
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

/// Macro names shall consist of letters, digits, and underscores, where the first character is not a digit. Tokens not of this form shall not be treated as macros.
/// `[_a-zA-Z][_a-zA-Z0-9]*`
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
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        println!("parsing macro name {:?}", String::from_utf8_lossy(input));
        if input.is_empty() {
            println!("empty macro name");
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::NonEmpty,
            )));
        }
        let (remaining, start) = nom::bytes::complete::take_while1(is_word_char_start)(input)?;
        let (remaining, rest) =
            nom::bytes::complete::take_while_m_n(0, remaining.len(), is_word_char_end)(remaining)?;
        // TODO: check whether the name matches any names in the current state.
        Ok((
            remaining,
            Self(input[..(start.len() + rest.len())].to_vec()),
        ))
    }

    fn try_from_slice(input: &[u8]) -> Result<Self, nom::Err<nom::error::Error<&[u8]>>> {
        Self::parse(input.into()).map(|ok| ok.1)
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

// Can probably optimize using something like smallvec
#[derive(Clone)]
struct ParseConfig {
    current_macro_names: Vec<MacroName>,
    quote_open_tag: Vec<u8>,
    quote_close_tag: Vec<u8>,
    comment_open_tag: Vec<u8>,
    comment_close_tag: Vec<u8>,
    symbol_recursion_limit: usize,
}

const DEFAULT_QUOTE_OPEN_TAG: &[u8] = b"`";
const DEFAULT_QUOTE_CLOSE_TAG: &[u8] = b"'";
const DEFAULT_COMMENT_OPEN_TAG: &[u8] = b"#";
const DEFAULT_COMMENT_CLOSE_TAG: &[u8] = b"\n";
const DEFAULT_SYMBOL_RECURSION_LIMIT: usize = 100;
static INBUILT_MACRO_NAMES: once_cell::sync::Lazy<Vec<MacroName>> =
    once_cell::sync::Lazy::new(|| {
        ["define", "dnf"]
            .iter()
            .map(|n| MacroName::parse(n.as_bytes()).unwrap().1)
            .collect()
    });

impl Default for ParseConfig {
    fn default() -> Self {
        Self {
            current_macro_names: vec![],
            quote_open_tag: DEFAULT_QUOTE_OPEN_TAG.to_vec(),
            quote_close_tag: DEFAULT_QUOTE_CLOSE_TAG.to_vec(),
            comment_open_tag: DEFAULT_COMMENT_OPEN_TAG.to_vec(),
            comment_close_tag: DEFAULT_COMMENT_CLOSE_TAG.to_vec(),
            symbol_recursion_limit: DEFAULT_SYMBOL_RECURSION_LIMIT,
        }
    }
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
/// TODO: comment tag
fn parse_text<'c>(
    quote_open_tag: &'c [u8],
    comment_open_tag: &'c [u8],
) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], &'i [u8]> + 'c {
    move |input: &[u8]| {
        if input.is_empty() {
            return Ok((input, input));
        }

        let stop_tags = &[quote_open_tag, comment_open_tag];
        let mut previous_was_alphanumeric = true;
        let mut stop_index = 0;
        'forloop: for i in 0..input.len() {
            let current_is_alphanumeric = is_alphnumeric(input[i]);
            if current_is_alphanumeric && !previous_was_alphanumeric {
                println!("found possible start of macro");
                break 'forloop;
            }
            for tag in stop_tags {
                if input[i..].starts_with(tag) {
                    println!("found start of tag {tag:?}");
                    break 'forloop;
                }
            }
            stop_index = i;
            previous_was_alphanumeric = current_is_alphanumeric;
        }

        let (matched, remaining) = input.split_at(stop_index + 1);
        println!(
            "input: {}, matched: {}, remaining: {}, stop_index: {stop_index}",
            String::from_utf8_lossy(input),
            String::from_utf8_lossy(matched),
            String::from_utf8_lossy(remaining),
        );
        Ok((remaining, matched))
    }
}

// TODO: changequote support
fn parse_comment<'a, 'b>(
    open_comment_tag: &'b [u8],
    close_comment_tag: &'b [u8],
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> + 'b {
    move |input: &[u8]| {
        let mut total_len = 0;
        println!(
            "parsing open tag {:?}",
            String::from_utf8_lossy(open_comment_tag)
        );
        let (remaining, open_tag) = nom::bytes::complete::tag(open_comment_tag)(input)?;
        total_len += open_tag.len();
        println!(
            "finding close tag {:?}",
            String::from_utf8_lossy(close_comment_tag)
        );
        // TODO this could probably be optimized
        let (remaining, contents) = nom::branch::alt((
            nom::bytes::complete::take_until(close_comment_tag),
            nom::combinator::rest,
        ))(remaining)?;
        total_len += contents.len();
        let remaining = if !remaining.is_empty() {
            let (remaining, close_tag) = nom::bytes::complete::tag(close_comment_tag)(remaining)?;
            total_len += close_tag.len();
            remaining
        } else {
            remaining
        };
        Ok((remaining, &input[..total_len]))
    }
}

fn process_streaming<'c, R: std::io::Read, W: std::io::Write>(
    config: ParseConfig,
    execute: impl for<'cc, 'i> Fn(ParseConfig, Symbol<'i>) -> ParseConfig,
    mut reader: R,
    mut writer: W,
) {
    let buffer_size = 10;
    let buffer_growth_factor = 2;
    let mut previous_buffer = circular::Buffer::with_capacity(buffer_size);
    let mut current_buffer = circular::Buffer::with_capacity(buffer_size);
    let mut config = config;

    loop {
        let read = reader.read(current_buffer.space()).unwrap();
        if read == 0 {
            break;
        }
        current_buffer.fill(read);

        loop {
            let input = current_buffer.data();

            let result = Symbol::parse(&config)(input);
            let (remaining, symbol) = match result {
                Ok(ok) => ok,
                Err(nom::Err::Incomplete(_)) => {
                    let new_capacity = buffer_growth_factor * current_buffer.capacity();
                    current_buffer.grow(new_capacity);
                    previous_buffer.grow(new_capacity);
                    break;
                }
                Err(error) => panic!("{}", error),
            };
            let remaining_len = remaining.len();
            config = execute(config, symbol);

            // current_buffer.consume(input.len() - remaining_len);
            // std::mem::swap(&mut previous_buffer, &mut current_buffer);
        }
    }
}

fn parse_symbols<'b, 'a: 'b>(
    config: &'b ParseConfig,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<Symbol<'a>>> + 'b {
    move |input: &[u8]| {
        if config.symbol_recursion_limit == 0 {
            // TODO: Add a better error
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Fail,
            )));
        }
        println!("parse_symbols: {:?}", String::from_utf8_lossy(input));
        if input.is_empty() {
            return Ok((input, Vec::new()));
        }
        let result = nom::multi::many0(Symbol::parse(config))(input);
        #[cfg(test)]
        dbg!(&result);
        result
    }
}
// TODO: probably these tests will be deleted later in favour of integration test suite.

#[cfg(test)]
mod test {
    use crate::lexer::{Symbol, DEFAULT_COMMENT_CLOSE_TAG, DEFAULT_COMMENT_OPEN_TAG};

    use super::{
        parse_comment, parse_symbols, parse_text, Macro, MacroName, ParseConfig, Quoted,
        DEFAULT_QUOTE_CLOSE_TAG, DEFAULT_QUOTE_OPEN_TAG, INBUILT_MACRO_NAMES,
    };
    // TODO: add tests based on input in
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/m4.html#tag_20_74_17
    const M4SRC: &str = r#"The value of `VER' is "VER".
        ifdef(`VER', ``VER'' is defined to be VER., VER is not defined.)
        ifelse(VER, 1, ``VER'' is `VER'.)
        ifelse(VER, 2, ``VER'' is `VER'., ``VER'' is not 2.)
        end"#;

    fn utf8(input: &[u8]) -> String {
        String::from_utf8(input.to_vec()).unwrap()
    }

    #[test]
    fn test_parse_macro_name_underscore_number() {
        let name = MacroName::parse(b"some_word_23").unwrap().1;
        assert_eq!(name, MacroName(b"some_word_23".into()));
    }

    #[test]
    fn test_parse_macro_name_fail_number_start() {
        MacroName::parse(b"22word").unwrap_err();
    }

    fn macro_name(input: &[u8]) -> MacroName {
        MacroName::try_from_slice(input).unwrap()
    }

    #[test]
    fn test_parse_macro_name_fail_not_in_list() {
        Macro::parse(&ParseConfig {
            current_macro_names: vec![],
            ..ParseConfig::default()
        })(b"some_word_23")
        .unwrap_err();
    }

    #[test]
    fn test_parse_macro_name_only() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let m = Macro::parse(&config)(b"some_word_23").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
    }

    #[test]
    fn test_parse_macro_args_empty() {
        let current_macro_names = vec![macro_name(b"some_word_23")];
        let config = ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let m = Macro::parse(&config)(b"some_word_23()").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 0);
    }

    #[test]
    fn test_parse_macro_args_1() {
        let current_macro_names = vec![macro_name(b"some_word_23"), macro_name(b"hello")];
        let config = &ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let m = Macro::parse(config)(b"some_word_23(hello)").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        let m1 = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Macro(m1) => m1,
            _ => panic!(),
        };
        assert_eq!(m1.name, MacroName(b"hello".into()));
    }

    #[test]
    fn test_parse_macro_args_1_quoted() {
        let current_macro_names = vec![macro_name(b"some_word_23"), macro_name(b"hello")];
        let config = &ParseConfig {
            current_macro_names,
            ..ParseConfig::default()
        };
        let m = Macro::parse(config)(b"some_word_23(`hello')").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23".into()));
        assert_eq!(m.args.len(), 1);
        let q = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Quoted(q) => q,
            _ => panic!(),
        };
        assert_eq!(q.contents, b"hello");
    }

    #[test]
    fn test_parse_macro_args_2() {
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
    fn test_parse_quoted_nested() {
        let quote =
            Quoted::parse(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`a `quote' is good!'")
                .unwrap()
                .1;
        assert_eq!("a `quote' is good!".as_bytes(), quote.contents);
    }

    #[test]
    fn test_parse_comment_standard() {
        let comment = parse_comment(DEFAULT_COMMENT_OPEN_TAG, DEFAULT_COMMENT_CLOSE_TAG)(
            b"# hello world\ngoodbye",
        )
        .unwrap()
        .1;
        assert_eq!("# hello world\n", utf8(comment));
    }

    #[test]
    fn test_parse_comment_standard_no_newline() {
        let comment =
            parse_comment(DEFAULT_COMMENT_OPEN_TAG, DEFAULT_COMMENT_CLOSE_TAG)(b"# hello world")
                .unwrap()
                .1;
        assert_eq!("# hello world", utf8(comment));
    }

    #[test]
    fn test_parse_text_before_quote_space() {
        let (remaining, text) =
            parse_text(b"`", DEFAULT_COMMENT_OPEN_TAG)(b"hello `world'").unwrap();
        assert_eq!("hello ", utf8(text));
        assert_eq!("`world'", utf8(remaining));
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
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(b"hello|world").unwrap();
        assert_eq!("hello|", utf8(text));
        assert_eq!("world", utf8(remaining));
        let (remaining, text) =
            parse_text(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_COMMENT_OPEN_TAG)(remaining).unwrap();
        assert_eq!("world", utf8(text));
        assert_eq!("", utf8(remaining));
    }

    #[test]
    fn test_parse_symbols_evaluation_order() {
        let f = std::fs::read("fixtures/integration_tests/evaluation_order.m4").unwrap();
        let config = ParseConfig {
            current_macro_names: (*INBUILT_MACRO_NAMES).clone(),
            ..ParseConfig::default()
        };
        let (remaining, symbols) = parse_symbols(&config)(&f).unwrap();

        insta::assert_debug_snapshot!(symbols);
        assert!(remaining.is_empty())
    }
}
