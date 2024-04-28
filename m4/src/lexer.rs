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

use nom::{error::ContextError, IResult};

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Macro<'a> {
    name: MacroName<'a>,
    args: Vec<Vec<Symbol<'a>>>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
enum Symbol<'a> {
    // Comments in m4 are not discarded, their contents (including delimeters) are copied verbatim to output without
    // further processing.
    Comment(&'a [u8]),
    Text(&'a [u8]),
    Quoted(Quoted<'a>),
    Macro(Macro<'a>),
}

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Quoted<'a> {
    pub quoted: &'a [u8],
}

/// Macro names shall consist of letters, digits, and underscores, where the first character is not a digit. Tokens not of this form shall not be treated as macros.
/// `[_a-zA-Z][_a-zA-Z0-9]*`
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
struct MacroName<'a>(&'a [u8]);

impl<'a> MacroName<'a> {
    pub fn parse(input: &'a [u8]) -> IResult<&'a [u8], Self> {
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
        Ok((remaining, Self(&input[..(start.len() + rest.len())])))
    }

    fn try_from_slice(input: &'a [u8]) -> Result<Self, nom::Err<nom::error::Error<&'a [u8]>>> {
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

fn parse_macro<'a, 'b: 'a>(
    // TODO: perhaps faster with a hashset?
    current_macro_names: &'b [MacroName<'a>],
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Macro<'a>> {
    move |input: &'a [u8]| {
        #[cfg(test)]
        dbg!(current_macro_names);
        println!("parse_macro {:?}", String::from_utf8_lossy(input));
        let (remaining, name) = MacroName::parse(input)?;
        if current_macro_names.contains(&name) {}
        println!("macro_args {:?}", String::from_utf8_lossy(remaining));
        let (remaining, args) = nom::combinator::opt(nom::sequence::delimited(
            nom::bytes::complete::tag("("),
            nom::multi::separated_list0(
                nom::bytes::complete::tag(","),
                // TODO: check, we should be allowed to have empty arguments?
                nom::combinator::map_parser(
                    nom::bytes::complete::is_not(")"),
                    nom::combinator::cut(parse_symbols),
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

const DEFAULT_QUOTE_OPEN_TAG: &[u8] = b"`";
const DEFAULT_QUOTE_CLOSE_TAG: &[u8] = b"'";

fn parse_quoted(open_tag: &[u8], close_tag: &[u8]) -> impl Fn(&[u8]) -> IResult<&[u8], Quoted<'_>> {
    |input: &[u8]| {
        let mut nest_level = 0;

        let (mut remaining, _) = nom::bytes::complete::tag(DEFAULT_QUOTE_OPEN_TAG)(input)?;
        nest_level += 1;

        let quote_start_index = input.len() - remaining.len();

        let quote_end_index = loop {
            if remaining.is_empty() {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::TagClosure,
                )));
            }

            if remaining.starts_with(DEFAULT_QUOTE_CLOSE_TAG) {
                if nest_level == 1 {
                    break input.len() - remaining.len();
                }
                remaining = &remaining[DEFAULT_QUOTE_CLOSE_TAG.len()..];
                nest_level -= 1;
                continue;
            }
            if remaining.starts_with(DEFAULT_QUOTE_OPEN_TAG) {
                remaining = &remaining[DEFAULT_QUOTE_OPEN_TAG.len()..];
                nest_level += 1;
                continue;
            }

            remaining = &remaining[1..];
        };

        let quoted = &input[quote_start_index..quote_end_index];

        Ok((remaining, Quoted { quoted }))
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

// TODO: needs to change with changequote
fn is_open_quote(c: u8) -> bool {
    c == b'`'
}

// TODO: needs to change with changequote
fn is_close_quote(c: u8) -> bool {
    c == b'\''
}

/// Parse input that we already know is not quoted and not a macro, consume input until it could
/// possibly be the beginning of either a quote or a macro, such as when an open quote is
/// encountered, or when we encounter the first alpha character after a non-alphanumeric character.
/// TODO: what happens if we encounter a close quote?
fn parse_text(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() {
        return Ok((input, input));
    }

    let (remaining, text) = if is_whitespace(*input.first().unwrap()) {
        nom::bytes::complete::take_till(|c| is_alphnumeric(c) || is_open_quote(c))(input)?
    } else {
        nom::bytes::complete::take_till(|c| !is_alphnumeric(c) || is_open_quote(c))(input)?
    };
    Ok((remaining, text))
}

fn parse_comment(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut total_len = 0;
    let (remaining, open_tag) = nom::bytes::complete::tag("#")(input)?;
    total_len += open_tag.len();
    let (remaining, contents) = nom::bytes::complete::take_till(|c| c == b'\n')(remaining)?;
    total_len += contents.len();
    let remaining = if !remaining.is_empty() {
        let (remaining, close_tag) = nom::bytes::complete::tag("\n")(remaining)?;
        total_len += close_tag.len();
        remaining
    } else {
        remaining
    };
    Ok((remaining, &input[..total_len]))
}

fn parse_symbol<'a, 'b: 'a>(
    current_macro_names: &'b [MacroName<'a>],
    quote_open_tag: &'b [u8],
    quote_close_tag: &'b [u8],
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Symbol<'a>> + 'a {
    |input: &'a [u8]| {
        println!("parse_symbol: {:?}", String::from_utf8_lossy(input));
        if input.is_empty() {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::NonEmpty,
            )));
        }
        nom::branch::alt((
            nom::combinator::map(
                parse_quoted(quote_open_tag, quote_close_tag),
                Symbol::Quoted,
            ),
            nom::combinator::map(parse_macro(current_macro_names), Symbol::Macro),
            nom::combinator::map(parse_text, Symbol::Text),
        ))(input)
    }
}

fn parse_symbols(input: &[u8]) -> IResult<&[u8], Vec<Symbol<'_>>> {
    println!("parse_symbols: {:?}", String::from_utf8_lossy(input));
    if input.is_empty() {
        return Ok((input, Vec::new()));
    }
    let result = nom::multi::many0(parse_symbol(
        &[],
        DEFAULT_QUOTE_OPEN_TAG,
        DEFAULT_QUOTE_CLOSE_TAG,
    ))(input);
    #[cfg(test)]
    dbg!(&result);
    result
}

// TODO: probably these tests will be deleted later in favour of integration test suite.
#[cfg(test)]
mod test {
    use crate::lexer::Symbol;

    use super::{
        parse_comment, parse_macro, parse_quoted, MacroName, DEFAULT_QUOTE_CLOSE_TAG,
        DEFAULT_QUOTE_OPEN_TAG,
    };
    // TODO: add tests based on input in
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/m4.html#tag_20_74_17
    const M4SRC: &str = r#"The value of `VER' is "VER".
        ifdef(`VER', ``VER'' is defined to be VER., VER is not defined.)
        ifelse(VER, 1, ``VER'' is `VER'.)
        ifelse(VER, 2, ``VER'' is `VER'., ``VER'' is not 2.)
        end"#;

    #[test]
    fn test_parse_macro_name_underscore_number() {
        let name = MacroName::parse(b"some_word_23").unwrap().1;
        assert_eq!(name, MacroName(b"some_word_23"));
    }

    #[test]
    fn test_parse_macro_name_fail_number_start() {
        MacroName::parse(b"22word").unwrap_err();
    }

    fn macro_name(input: &[u8]) -> MacroName<'_> {
        MacroName::try_from_slice(input).unwrap()
    }

    #[test]
    fn test_parse_macro_name_only() {
        let macro_names = &[macro_name(b"some_word_23")];
        let m = parse_macro(macro_names)(b"some_word_23").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
    }

    #[test]
    fn test_parse_macro_args_empty() {
        let macro_names = &[macro_name(b"some_word_23")];
        let m = parse_macro(macro_names)(b"some_word_23()").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
        assert_eq!(m.args.len(), 0);
    }

    #[test]
    fn test_parse_macro_args_1() {
        let macro_names = &[macro_name(b"some_word_23")];
        let m = parse_macro(macro_names)(b"some_word_23(hello)").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
        assert_eq!(m.args.len(), 1);
        let m1 = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Macro(m1) => m1,
            _ => panic!(),
        };
        assert_eq!(m1.name, MacroName(b"hello"));
    }

    #[test]
    fn test_parse_macro_args_2() {
        let macro_names = &[macro_name(b"some_word_23")];
        let m = parse_macro(macro_names)(b"some_word_23(hello world)")
            .unwrap()
            .1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
        assert_eq!(m.args.len(), 1);
        let m1 = match m.args.get(0).unwrap().get(0).unwrap() {
            Symbol::Macro(m) => m,
            _ => panic!(),
        };
        assert_eq!(m1.name, MacroName(b"hello"));
        let t = match m.args.get(0).unwrap().get(1).unwrap() {
            Symbol::Text(t) => t,
            _ => panic!(),
        };
        assert_eq!(t, b" ");
        let m3 = match m.args.get(0).unwrap().get(2).unwrap() {
            Symbol::Macro(m) => m,
            _ => panic!(),
        };
        assert_eq!(m3.name, MacroName(b"world"));
    }

    #[test]
    fn test_parse_macro_args_fail_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        parse_macro(&[macro_name(b"some_word_23")])(b"some_word_23(hello").unwrap_err();
    }

    #[test]
    fn test_parse_macro_args_fail_empty_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        parse_macro(&[macro_name(b"some_word_23")])(b"some_word_23(").unwrap_err();
    }

    #[test]
    fn test_parse_quoted() {
        let quote = parse_quoted(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`hello'")
            .unwrap()
            .1;
        assert_eq!("hello".as_bytes(), quote.quoted);
    }

    #[test]
    fn test_parse_quoted_nested() {
        let quote =
            parse_quoted(DEFAULT_QUOTE_OPEN_TAG, DEFAULT_QUOTE_CLOSE_TAG)(b"`a `quote' is good!'")
                .unwrap()
                .1;
        assert_eq!("a `quote' is good!".as_bytes(), quote.quoted);
    }

    #[test]
    fn test_parse_comment_standard() {
        let comment = parse_comment(b"# hello world\ngoodbye").unwrap().1;
        assert_eq!(b"# hello world\n", comment);
    }

    #[test]
    fn test_parse_comment_standard_no_newline() {
        let comment = parse_comment(b"# hello world").unwrap().1;
        assert_eq!(b"# hello world", comment);
    }
}
