use std::{collections::HashMap, io::Write};

use crate::lexer::{MacroName, ParseConfig, Symbol};

#[derive(Default)]
pub(crate) struct State {
    map: HashMap<MacroName, MacroDefinition>,
}

enum MacroDefinition {
    Builtin(fn(State, ParseConfig, Symbol) -> crate::error::Result<(State, ParseConfig)>),
}

pub(crate) fn evaluate(
    mut state: State,
    mut config: ParseConfig,
    symbol: Symbol,
    stdout: &mut impl Write,
    stderror: &mut impl Write,
) -> crate::error::Result<(State, ParseConfig)> {
    log::debug!("{symbol:?}");
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!config.dnl);
    match symbol {
        Symbol::Comment(comment) => stdout.write_all(comment)?,
        Symbol::Text(text) => stdout.write_all(text)?,
        Symbol::Quoted(quoted) => {
            stdout.write_all(&config.quote_open_tag)?;
            stdout.write_all(quoted.contents)?;
            stdout.write_all(&config.quote_close_tag)?;
        }
        Symbol::Macro(m) => match m.name.as_bytes() {
            b"dnl" => {
                config.dnl = true;
            }
            b"define" => {}
            unsupported => {
                write!(stdout, "TODO({})", String::from_utf8_lossy(unsupported))?;
            }
        },
        Symbol::Newline => write!(stdout, "\n")?,
        Symbol::Eof => {}
    }

    Ok((state, config))
}

#[cfg(test)]
mod test {
    use super::{evaluate, ParseConfig, State, Symbol};
    use crate::lexer::Macro;
    use crate::test_utils::{macro_name, utf8};
    use test_log::test;

    #[test]
    fn test_text() {
        let mut stdout: Vec<u8> = Vec::new();
        let mut stderr: Vec<u8> = Vec::new();
        let (_state, config) = evaluate(
            State::default(),
            ParseConfig::default(),
            Symbol::Text(b"Some text to evaluate"),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert_eq!(config, ParseConfig::default());
        assert_eq!("Some text to evaluate", utf8(&stdout));
        assert!(stderr.is_empty());
    }

    #[test]
    fn test_macro_dnl() {
        let mut stdout: Vec<u8> = Vec::new();
        let mut stderr: Vec<u8> = Vec::new();
        let (_state, config) = evaluate(
            State::default(),
            ParseConfig::default(),
            Symbol::Macro(Macro {
                input: b"dnl",
                name: macro_name(b"dnl"),
                args: vec![],
            }),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert_eq!(true, config.dnl);
        assert!(stdout.is_empty());
        assert!(stderr.is_empty());
    }
}
