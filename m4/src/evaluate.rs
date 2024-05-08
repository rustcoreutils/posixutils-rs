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
