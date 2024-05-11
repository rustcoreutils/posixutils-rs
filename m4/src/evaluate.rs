use std::io::Write;

use crate::lexer::{ParseConfig, Symbol};

#[derive(Default)]
pub(crate) struct State;

pub(crate) fn evaluate<W: Write>(
    state: State,
    mut config: ParseConfig,
    symbol: Symbol,
    writer: &mut W,
) -> crate::error::Result<(State, ParseConfig)> {
    log::debug!("{symbol:?}");
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!config.dnl);
    match symbol {
        Symbol::Comment(comment) => writer.write_all(comment)?,
        Symbol::Text(text) => writer.write_all(text)?,
        Symbol::Quoted(quoted) => {
            writer.write_all(&config.quote_open_tag)?;
            writer.write_all(quoted.contents)?;
            writer.write_all(&config.quote_close_tag)?;
        }
        Symbol::Macro(m) => match m.name.as_bytes() {
            b"dnl" => {
                config.dnl = true;
            }
            unsupported => {
                write!(writer, "TODO({})", String::from_utf8_lossy(unsupported))?;
            }
        },
        Symbol::Newline => write!(writer, "\n")?,
        Symbol::Eof => {}
    }

    Ok((state, config))
}

#[cfg(test)]
mod test {
    use crate::lexer::Macro;
    use test_log::test;
    use crate::test_utils::{utf8, macro_name};
    
    #[test]
    fn test_text() {
        let mut output: Vec<u8> = Vec::new();
        let (_state, config) = evaluate(
            State::default(),
            ParseConfig::default(),
            Symbol::Text(b"Some text to evaluate"),
            &mut output,
        )
        .unwrap();

        assert_eq!(config, ParseConfig::default());
        assert_eq!("Some text to evaluate", utf8(&output));
    }
    use super::{evaluate, ParseConfig, State, Symbol};

    #[test]
    fn test_macro_dnl() {
        let mut output: Vec<u8> = Vec::new();
        let (_state, config) = evaluate(
            State::default(),
            ParseConfig::default(),
            Symbol::Macro(Macro {
                input: b"dnl",
                name: macro_name(b"dnl"),
                args: vec![],
            }),
            &mut output,
        )
        .unwrap();

        assert_eq!(true, config.dnl);
        assert!(output.is_empty());
    }
}
