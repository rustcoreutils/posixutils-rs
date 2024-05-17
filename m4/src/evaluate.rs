use std::{collections::HashMap, io::Write};

use crate::error::Result;
use crate::lexer::{BuiltinMacro, Macro, MacroName, MacroParseConfig, ParseConfig, Symbol};

pub(crate) struct State<STDERR> {
    macro_definitions: HashMap<MacroName, MacroDefinition<STDERR>>,
    pub parse_config: ParseConfig,
}

impl<STDERR: Write> Default for State<STDERR> {
    fn default() -> Self {
        Self {
            macro_definitions: BuiltinMacro::enumerate()
                .into_iter()
                .map(|builtin| {
                    let parse_config = builtin.parse_config();
                    (parse_config.name.clone(), MacroDefinition { parse_config })
                })
                .collect(),
            parse_config: ParseConfig::default(),
        }
    }
}

impl<STDERR> State<STDERR> {
    fn current_macro_parse_configs(&self) -> HashMap<MacroName, MacroParseConfig> {
        self.macro_definitions
            .iter()
            .map(|(name, definition)| {
                let config = definition.parse_config;
                (config.name.clone(), config)
            })
            .collect()
    }
}

struct MacroDefinition<STDERR> {
    parse_config: MacroParseConfig,
    // TODO: improve performance with enum dispatch
    implementation: Box<dyn MacroImplementation<STDERR>>,
}

trait MacroImplementation<STDERR: Write> {
    fn evaluate(
        &self,
        state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<State<STDERR>>;
}

struct DnlMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for DnlMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        _m: Macro,
    ) -> Result<State<STDERR>> {
        state.parse_config.dnl = true;
        Ok(state)
    }
}

struct DefineMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for DefineMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<State<STDERR>> {
        let mut args = m.args.into_iter();
        let name = if let Some(i) = args.next() {
            let mut name_bytes = Vec::new();
            (name_bytes, state) = evaluate_to_text(state, i, stderror)?;

            if let Ok(name) = MacroName::try_from_slice(&name_bytes) {
                name
            } else {
                return Ok(state);
            }
        } else {
            return Ok(state);
        };
        let definition = if let Some(i) = args.next() {
            let mut definition = Vec::new();
            (definition, state) = evaluate_to_text(state, i, stderror)?;
            definition
        } else {
            return Ok(state);
        };
        let definition = MacroDefinition {
            parse_config: MacroParseConfig { name, min_args: 0 },
            implementation: Box::new(UserDefinedMacro { definition }),
        };
        state.macro_definitions.insert(m.name, definition);
        state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
        Ok(state)
    }
}

struct UserDefinedMacro {
    definition: Vec<u8>,
}

impl<STDERR: Write> MacroImplementation<STDERR> for UserDefinedMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<State<STDERR>> {
        todo!()
    }
}

struct UndefineMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for UndefineMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<State<STDERR>> {
        if let Some(first_arg_symbols) = m.args.into_iter().next() {
            let mut text = Vec::new();
            (text, state) = evaluate_to_text(state, first_arg_symbols, stderror)?;
            if let Ok(name) = MacroName::try_from_slice(&text) {
                state.macro_definitions.remove(&name);
                state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
            }
        }
        Ok(state)
    }
}

struct ErrprintMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for ErrprintMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<State<STDERR>> {
        for input in m.args {
            let mut text = Vec::new();
            (text, state) = evaluate_to_text(state, input, stderror)?;
            stderror.write_all(&text)?
        }
        Ok(state)
    }
}

fn errprint(stderror: &mut impl Write, args: &[&[u8]]) -> Result<()> {
    Ok(())
}

fn evaluate_to_text<STDERR: Write>(
    mut state: State<STDERR>,
    symbols: Vec<Symbol>,
    stderror: &mut STDERR,
) -> Result<(Vec<u8>, State<STDERR>)> {
    let mut buffer = Vec::new();
    for symbol in symbols {
        state = evaluate(state, symbol, &mut buffer, stderror)?;
    }
    Ok((buffer, state))
}

/// It seems like macro arguments are parsed in their entirety including unwrapping quotes inside a
/// define
///
///
/// From the manual:
///
/// > If the token matches the name of a macro, then the token shall be replaced by the macro's
/// defining text, if any, and rescanned for matching macro names. Once no portion of the token
/// matches the name of a macro, it shall be written to standard output. Macros may have arguments,
/// in which case the arguments shall be substituted into the defining text before it is rescanned.
pub(crate) fn evaluate<STDOUT: Write, STDERR: Write>(
    mut state: State<STDERR>,
    symbol: Symbol,
    stdout: &mut STDOUT,
    stderror: &mut STDERR,
) -> Result<State<STDERR>> {
    log::debug!("{symbol:?}");
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!state.parse_config.dnl);
    match symbol {
        Symbol::Comment(comment) => stdout.write_all(comment)?,
        Symbol::Text(text) => stdout.write_all(text)?,
        Symbol::Quoted(quoted) => {
            stdout.write_all(quoted.contents)?;
        }
        Symbol::Macro(m) => {
            state = if let Some(definition) = state.macro_definitions.get(&m.name) {
                todo!()
                // Some(MacroDefinition::Builtin(BuiltinMacro::Define)) => define(state, config, m)?,
            } else {
                write!(
                    stdout,
                    "TODO({})",
                    String::from_utf8_lossy(m.name.as_bytes())
                )?;
                state
            }
        }
        Symbol::Newline => write!(stdout, "\n")?,
        Symbol::Eof => {}
    }

    Ok(state)
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
        let state = evaluate(
            State::default(),
            Symbol::Text(b"Some text to evaluate"),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert_eq!(state.parse_config, ParseConfig::default());
        assert_eq!("Some text to evaluate", utf8(&stdout));
        assert!(stderr.is_empty());
    }

    #[test]
    fn test_macro_dnl() {
        let mut stdout: Vec<u8> = Vec::new();
        let mut stderr: Vec<u8> = Vec::new();
        let state = evaluate(
            State::default(),
            Symbol::Macro(Macro {
                input: b"dnl",
                name: macro_name(b"dnl"),
                args: vec![],
            }),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert_eq!(true, state.parse_config.dnl);
        assert!(stdout.is_empty());
        assert!(stderr.is_empty());
    }
}
