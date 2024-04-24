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

use nom::IResult;

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Macro<'a> {
    name: MacroName<'a>,
    args: Vec<Macro<'a>>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
enum Symbol<'a> {
    Text(&'a [u8]),
    Quoted(Quoted<'a>),
    Macro(Macro<'a>),
}

#[cfg_attr(test, derive(Debug, PartialEq))]
struct Quoted<'a> {
    pub open_quote_symbol: &'a [u8],
    pub quoted: Vec<Symbol<'a>>,
    pub close_quote_symbol: &'a [u8],
}

#[cfg_attr(test, derive(Debug, PartialEq))]
struct MacroName<'a>(&'a [u8]);

fn is_word_char_end(c: u8) -> bool {
    // TODO: check safety!
    (unsafe { libc::isalnum(c.into()) } != 0) || c == b'_'
}

fn is_word_char_start(c: u8) -> bool {
    // TODO: check safety!
    (unsafe { libc::isalpha(c.into()) } != 0) || c == b'_'
}

/// Macro names shall consist of letters, digits, and underscores, where the first character is not a digit. Tokens not of this form shall not be treated as macros.
/// `[_a-zA-Z][_a-zA-Z0-9]*`
fn parse_macro_name(input: &[u8]) -> IResult<&[u8], MacroName<'_>> {
    println!("parsing macro name {:?}", String::from_utf8_lossy(input));
    let (remaining, start) = nom::bytes::complete::take_while1(is_word_char_start)(input)?;
    let (remaining, rest) =
        nom::bytes::complete::take_while_m_n(0, remaining.len(), is_word_char_end)(remaining)?;
    // TODO: check whether the name matches any names in the current state.
    Ok((remaining, MacroName(&input[..(start.len() + rest.len())])))
}

fn parse_macro(input: &[u8]) -> IResult<&[u8], Macro<'_>> {
    println!("parsing macro {:?}", String::from_utf8_lossy(input));
    let (remaining, name) = parse_macro_name(input)?;
    println!(
        "parsing macro args {:?}",
        String::from_utf8_lossy(remaining)
    );
    let (remaining, args) = nom::combinator::opt(nom::sequence::delimited(
        nom::bytes::complete::tag("("),
        nom::multi::separated_list0(
            nom::bytes::complete::tag(","),
            // TODO: check, we should be allowed to have empty arguments?
            // TODO: replace with parse_symbol
            nom::combinator::map_parser(
                nom::bytes::complete::is_not(")"),
                nom::combinator::cut(parse_macro),
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

fn parse_quoted(input: &[u8]) -> IResult<&[u8], Quoted<'_>> {
    let (remaining, quoted) = nom::sequence::delimited(
        nom::bytes::complete::tag("`"),
        // TODO: replace with parse symbol and some combinator with take_until consuming the output
        // of that.
        nom::combinator::map_parser(nom::bytes::complete::take_until("'"), parse_symbols),
        nom::bytes::complete::tag("'"),
    )(input)?;

    Ok((
        remaining,
        Quoted {
            open_quote_symbol: b"`",
            quoted,
            close_quote_symbol: b"'",
        },
    ))
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
    let (remaining, text) =
        nom::bytes::complete::take_till(|c| !is_alphnumeric(c) || is_open_quote(c))(input)?;
    if remaining.is_empty() {
        return Ok((remaining, text));
    }

    todo!()
}

fn parse_symbols(input: &[u8]) -> IResult<&[u8], Vec<Symbol<'_>>> {
    nom::multi::many0(parse_symbol)(input)
}

fn parse_symbol(input: &[u8]) -> IResult<&[u8], Symbol<'_>> {
    nom::branch::alt((
        nom::combinator::map(parse_quoted, Symbol::Quoted),
        nom::combinator::map(parse_macro, Symbol::Macro),
        nom::combinator::map(parse_text, Symbol::Text),
    ))(input)
}

// TODO: probably these tests will be deleted later in favour of integration test suite.
#[cfg(test)]
mod test {
    use crate::lexer::Symbol;

    use super::{parse_macro, parse_macro_name, parse_quoted, Macro, MacroName};
    // TODO: add tests based on input in
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/m4.html#tag_20_74_17
    const M4SRC: &str = r#"The value of `VER' is "VER".
        ifdef(`VER', ``VER'' is defined to be VER., VER is not defined.)
        ifelse(VER, 1, ``VER'' is `VER'.)
        ifelse(VER, 2, ``VER'' is `VER'., ``VER'' is not 2.)
        end"#;

    #[test]
    fn test_parse_macro_name_underscore_number() {
        let name = parse_macro_name(b"some_word_23").unwrap().1;
        assert_eq!(name, MacroName(b"some_word_23"));
    }

    #[test]
    fn test_parse_macro_name_fail_number_start() {
        parse_macro_name(b"22word").unwrap_err();
    }

    #[test]
    fn test_parse_macro_name_only() {
        let m = parse_macro(b"some_word_23").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
    }

    #[test]
    fn test_parse_macro_args_empty() {
        let m = parse_macro(b"some_word_23()").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
        assert_eq!(m.args.len(), 0);
    }

    #[test]
    fn test_parse_macro_args() {
        let m = parse_macro(b"some_word_23(hello)").unwrap().1;
        assert_eq!(m.name, MacroName(b"some_word_23"));
        assert_eq!(m.args.len(), 1);
        assert_eq!(m.args.get(0).unwrap().name, MacroName(b"hello"));
    }

    #[test]
    fn test_parse_macro_args_fail_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        parse_macro(b"some_word_23(hello").unwrap_err();
    }

    #[test]
    fn test_parse_macro_args_fail_empty_no_closing_bracket() {
        // TODO: produce and check for a more specific error
        parse_macro(b"some_word_23(").unwrap_err();
    }

    #[test]
    fn test_parse_quoted() {
        let quote = parse_quoted(b"`hello'").unwrap().1;
        assert_eq!(b"`", quote.open_quote_symbol);
        assert_eq!(b"'", quote.close_quote_symbol);
        assert_eq!(
            vec![Symbol::Macro(Macro {
                name: MacroName(b"hello"),
                args: Vec::new(),
            })],
            quote.quoted
        );
    }
}
