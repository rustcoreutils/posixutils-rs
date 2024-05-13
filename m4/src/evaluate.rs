use std::{collections::HashMap, io::Write};

use crate::error::Result;
use crate::lexer::{
    BuiltinMacro, Macro, MacroName, MacroParseConfig, ParseConfig, Symbol,
    BUILTIN_MACRO_NAME_CONFIGS,
};

pub(crate) struct State {
    macro_definitions: HashMap<MacroName, MacroDefinition>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            macro_definitions: BuiltinMacro::enumerate()
                .into_iter()
                .map(|builtin| {
                    (
                        builtin.parse_config().name,
                        MacroDefinition::Builtin(*builtin),
                    )
                })
                .collect(),
        }
    }
}

impl State {
    fn current_macro_parse_configs(&self) -> HashMap<MacroName, MacroParseConfig> {
        self.macro_definitions
            .iter()
            .map(|(name, definition)| {
                let config = match definition {
                    MacroDefinition::Builtin(builtin) => builtin.parse_config(),
                    MacroDefinition::UserDefined(_) => MacroParseConfig {
                        name: name.clone(),
                        min_args: 0,
                    },
                };
                (config.name.clone(), config)
            })
            .collect()
    }
}

enum MacroDefinition {
    Builtin(BuiltinMacro),
    UserDefined(UserDefinedMacro),
}

struct UserDefinedMacro {}

fn evaluate_macro_args(
    m: Macro,
    mut state: State,
    mut config: ParseConfig,
    stdout: &mut impl Write,
    stderror: &mut impl Write,
) {
    todo!()
}

fn dnl(mut config: ParseConfig) -> ParseConfig {
    config.dnl = true;
    config
}

fn define(
    mut state: State,
    mut config: ParseConfig,
    m: Macro,
) -> crate::error::Result<(State, ParseConfig)> {
    config.current_macro_parse_configs = state.current_macro_parse_configs();
    todo!();
    Ok((state, config))
}

fn undefine(
    mut state: State,
    mut config: ParseConfig,
    args: &[&[u8]],
) -> crate::error::Result<(State, ParseConfig)> {
    if let Some(input) = args.first() {
        if let Ok(name) = MacroName::try_from_slice(input) {
            state.macro_definitions.remove(&name);
            config.current_macro_parse_configs = state.current_macro_parse_configs()
        }
    }
    Ok((state, config))
}

fn errprint(stderror: &mut impl Write, args: &[&[u8]]) -> Result<()> {
    for input in args {
        stderror.write_all(input)?
    }
    Ok(())
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
pub(crate) fn evaluate(
    mut state: State,
    mut config: ParseConfig,
    symbol: Symbol,
    stdout: &mut impl Write,
    stderror: &mut impl Write,
) -> Result<(State, ParseConfig)> {
    log::debug!("{symbol:?}");
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!config.dnl);
    match symbol {
        Symbol::Comment(comment) => stdout.write_all(comment)?,
        Symbol::Text(text) => stdout.write_all(text)?,
        Symbol::Quoted(quoted) => {
            stdout.write_all(quoted.contents)?;
        }
        Symbol::Macro(m) => {
            (state, config) = match state.macro_definitions.get(&m.name) {
                Some(MacroDefinition::Builtin(BuiltinMacro::Dnl)) => (state, dnl(config)),
                Some(MacroDefinition::Builtin(BuiltinMacro::Undefine)) => (state, dnl(config)),
                // Some(MacroDefinition::Builtin(BuiltinMacro::Define)) => define(state, config, m)?,
                _ => {
                    write!(
                        stdout,
                        "TODO({})",
                        String::from_utf8_lossy(m.name.as_bytes())
                    )?;
                    (state, config)
                }
            }
        }
        Symbol::Newline => write!(stdout, "\n")?,
        Symbol::Eof => {}
    }

    Ok((state, config))
}

#[cfg(test)]
mod test {
    use super::{evaluate, ParseConfig, State, Symbol};
    use crate::lexer::Macro;
    use crate::test_utils::{macro_name_config, utf8};
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
                name: macro_name_config(b"dnl"),
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

    /// This results in no syntax error because the macro is not evaluated.
    #[test]
    fn test_macro_define_eval_syntax_order_quoted_unevaluated() {
        let input = br#"define(x, `eval(1+)')"#;
        // Output: NOTHING
    }

    #[test]
    fn test_macro_define_eval_syntax_order_quoted_evaluated() {
        let input = br#"define(x, `eval(1+)')x"#;
        // Output: ERROR
    }

    /// This results in syntax error because argument is unquoted.
    #[test]
    fn test_macro_define_eval_syntax_order_unquoted() {
        let input = br#"define(x, eval(1+))"#;
        // Output: ERROR
    }

    /// The first definition of y is used in x
    #[test]
    fn test_macro_define_eval_order_unquoted() {
        let input = br#"define(y, 5)define(x, eval(1+y))define(y, 1)x"#;
        // Output: 6
    }

    /// The first definition of y is used in x (even when quoted!)
    #[test]
    fn test_macro_define_eval_order_quoted() {
        let input = br#"define(y, 5)define(x, `eval(1+y)')define(y, 100)x"#;
        // Output: 6
    }

    /// The first definition of y is used during the expansion of y in the definition of x.
    #[test]
    fn test_macro_define_order_defined() {
        let input = br#"define(y, 1)define(x, y)define(y, 5)x"#;
        // Output: 1
    }

    /// The definition of y from after the x definition is used in the evaluation of x macro.
    #[test]
    fn test_macro_define_order_undefined() {
        let input = br#"define(x, y)define(y, 5)x"#;
        // Output: 5
    }

    #[test]
    fn test_macro_errprint_no_evaluation_quoted() {
        let input = br#"define(x, \`errprint(2)')"#;
        // Output(stderr): NOTHING
    }

    #[test]
    fn test_macro_errprint_no_evaluation() {
        let input = br#"define(x, errprint(2))"#;
        // Output(stderr): 2
    }

    #[test]
    fn test_macro_errprint_evaluation() {
        let input = br#"define(x, errprint(2))x"#;
        // Output(stderr): 2
    }
}
