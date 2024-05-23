use std::{collections::HashMap, io::Write, rc::Rc};

use crate::error::Result;
use crate::lexer::{self, Macro, MacroName, MacroParseConfig, ParseConfig, Symbol};

pub struct State<STDERR> {
    macro_definitions: HashMap<MacroName, Rc<MacroDefinition<STDERR>>>,
    pub parse_config: ParseConfig,
}

impl<STDERR> std::fmt::Debug for State<STDERR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State")
            .field("macro_definitions", &self.macro_definitions)
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

impl<STDERR: Write> Default for State<STDERR> {
    fn default() -> Self {
        Self {
            macro_definitions: BuiltinMacro::enumerate()
                .into_iter()
                .map(|builtin| {
                    let parse_config = builtin.parse_config();
                    (
                        parse_config.name.clone(),
                        Rc::new(MacroDefinition {
                            parse_config,
                            implementation: inbuilt_macro_implementation(builtin),
                        }),
                    )
                })
                .collect(),
            parse_config: ParseConfig::default(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum BuiltinMacro {
    Dnl,
    Define,
    Undefine,
    Errprint,
}

impl AsRef<[u8]> for BuiltinMacro {
    fn as_ref(&self) -> &'static [u8] {
        match self {
            BuiltinMacro::Dnl => b"dnl",
            BuiltinMacro::Define => b"define",
            BuiltinMacro::Undefine => b"undefine",
            BuiltinMacro::Errprint => b"errprint",
        }
    }
}

impl BuiltinMacro {
    pub fn enumerate() -> &'static [Self] {
        &[Self::Dnl, Self::Define, Self::Undefine, Self::Errprint]
    }
    pub fn name(&self) -> MacroName {
        MacroName::try_from_slice(self.as_ref()).expect("Expected valid builtin macro name")
    }

    pub fn min_args(&self) -> usize {
        match self {
            BuiltinMacro::Dnl => 0,
            BuiltinMacro::Define => 1,
            BuiltinMacro::Undefine => 1,
            BuiltinMacro::Errprint => 1,
        }
    }

    pub fn parse_config(&self) -> MacroParseConfig {
        MacroParseConfig {
            name: self.name(),
            min_args: self.min_args(),
        }
    }
}

// TODO: refactor this is not great
fn inbuilt_macro_implementation<STDERR: Write>(
    builtin: &BuiltinMacro,
) -> Box<dyn MacroImplementation<STDERR>> {
    match builtin {
        BuiltinMacro::Dnl => Box::new(DnlMacro),
        BuiltinMacro::Define => Box::new(DefineMacro),
        BuiltinMacro::Undefine => Box::new(UndefineMacro),
        BuiltinMacro::Errprint => Box::new(ErrprintMacro),
    }
}

impl<STDERR> State<STDERR> {
    fn current_macro_parse_configs(&self) -> HashMap<MacroName, MacroParseConfig> {
        self.macro_definitions
            .iter()
            .map(|(name, definition)| {
                let config = definition.parse_config.clone();
                (name.clone(), config)
            })
            .collect()
    }
}

pub(crate) struct MacroDefinition<STDERR> {
    pub parse_config: MacroParseConfig,
    // TODO: improve performance with enum dispatch
    implementation: Box<dyn MacroImplementation<STDERR>>,
}

impl<STDERR> std::fmt::Debug for MacroDefinition<STDERR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MacroDefinition")
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

trait MacroImplementation<STDERR: Write> {
    fn evaluate(
        &self,
        state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)>;
}

struct DnlMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for DnlMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        _stderror: &mut STDERR,
        _m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)> {
        state.parse_config.dnl = true;
        Ok((Vec::new(), state))
    }
}

struct DefineMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for DefineMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)> {
        let mut args = m.args.into_iter();
        let name = if let Some(i) = args.next() {
            let name_bytes: Vec<u8>;
            (name_bytes, state) = evaluate_to_text(state, i, stderror)?;

            if let Ok(name) = MacroName::try_from_slice(&name_bytes) {
                name
            } else {
                return Ok((Vec::new(), state));
            }
        } else {
            return Ok((Vec::new(), state));
        };
        let definition = if let Some(i) = args.next() {
            let definition: Vec<u8>;
            (definition, state) = evaluate_to_text(state, i, stderror)?;
            definition
        } else {
            return Ok((Vec::new(), state));
        };
        let definition = MacroDefinition {
            parse_config: MacroParseConfig {
                name: name.clone(),
                min_args: 0,
            },
            implementation: Box::new(UserDefinedMacro { definition }),
        };
        state.macro_definitions.insert(name, Rc::new(definition));
        state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
        return Ok((Vec::new(), state));
    }
}

struct UserDefinedMacro {
    definition: Vec<u8>,
}

impl<STDERR: Write> MacroImplementation<STDERR> for UserDefinedMacro {
    fn evaluate(
        &self,
        state: State<STDERR>,
        stderror: &mut STDERR,
        _m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)> {
        // TODO: handle arguments
        parse_and_evaluate_to_text(state, &self.definition, stderror)
    }
}

struct UndefineMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for UndefineMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)> {
        if let Some(first_arg_symbols) = m.args.into_iter().next() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_text(state, first_arg_symbols, stderror)?;
            if let Ok(name) = MacroName::try_from_slice(&text) {
                state.macro_definitions.remove(&name);
                state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
            }
        }
        return Ok((Vec::new(), state));
    }
}

struct ErrprintMacro;

impl<STDERR: Write> MacroImplementation<STDERR> for ErrprintMacro {
    fn evaluate(
        &self,
        mut state: State<STDERR>,
        stderror: &mut STDERR,
        m: Macro,
    ) -> Result<(Vec<u8>, State<STDERR>)> {
        for input in m.args {
            let text: Vec<u8>;
            (text, state) = evaluate_to_text(state, input, stderror)?;
            stderror.write_all(&text)?
        }
        return Ok((Vec::new(), state));
    }
}

fn parse_and_evaluate_to_text<STDERR: Write>(
    mut state: State<STDERR>,
    text: &[u8],
    stderror: &mut STDERR,
) -> Result<(Vec<u8>, State<STDERR>)> {
    // TODO: hint size so it deosn't need to re-allocate
    let mut stdout: Vec<u8> = Vec::new();
    state = lexer::process_streaming(state, evaluate, text, &mut stdout, stderror)?;
    Ok((stdout, state))
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
    log::debug!("symbol: {symbol:?}");
    log::debug!("macro_definitions: {:?}", state.macro_definitions.keys());
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!state.parse_config.dnl);
    match symbol {
        Symbol::Comment(comment) => stdout.write_all(comment)?,
        Symbol::Text(text) => stdout.write_all(text)?,
        Symbol::Quoted(quoted) => {
            stdout.write_all(quoted.contents)?;
        }
        Symbol::Macro(m) => {
            state = if let Some(definition) = state.macro_definitions.get(&m.name).cloned() {
                let (output, state) = definition.implementation.evaluate(state, stderror, m)?;
                stdout.write_all(&output)?;
                state
            } else {
                // TODO: remove this branch it should probably panic
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

    #[test]
    fn test_macro_define() {
        let mut stdout: Vec<u8> = Vec::new();
        let mut stderr: Vec<u8> = Vec::new();
        let mut state = State::default();
        assert!(matches!(
            state.macro_definitions.get(&macro_name(b"hello")),
            None
        ));
        state = evaluate(
            state,
            Symbol::Macro(Macro {
                input: b"define(hello,hi $1)",
                name: macro_name(b"define"),
                args: vec![vec![Symbol::Text(b"hello")], vec![Symbol::Text(b"hi $1")]],
            }),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert!(stdout.is_empty());
        assert!(stderr.is_empty());

        state.macro_definitions.get(&macro_name(b"hello")).unwrap();
        evaluate(
            state,
            Symbol::Macro(Macro {
                input: b"hello(friend)",
                name: macro_name(b"hello"),
                args: vec![vec![Symbol::Text(b"friend")]],
            }),
            &mut stdout,
            &mut stderr,
        )
        .unwrap();

        assert_eq!("hi friend", utf8(&stdout));
        assert!(stderr.is_empty());
    }
}
