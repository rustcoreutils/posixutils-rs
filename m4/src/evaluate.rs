use std::ffi::OsString;
use std::os::unix::ffi::OsStringExt;
use std::path::PathBuf;
use std::{collections::HashMap, io::Write, rc::Rc};

use nom::error::{ContextError, FromExternalError};
use nom::IResult;

use crate::error::Result;
use crate::lexer::{
    self, Macro, MacroName, MacroParseConfig, ParseConfig, Symbol, DEFAULT_QUOTE_CLOSE_TAG,
    DEFAULT_QUOTE_OPEN_TAG,
};

const AT_LEAST_ONE_MACRO_DEFINITION_EXPECT: &str =
    "There should always be at least one macro definition";

pub struct State {
    macro_definitions: HashMap<MacroName, Vec<Rc<MacroDefinition>>>,
    pub parse_config: ParseConfig,
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State")
            .field("macro_definitions", &self.macro_definitions)
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            macro_definitions: BuiltinMacro::enumerate()
                .into_iter()
                .map(|builtin| {
                    let parse_config = builtin.parse_config();
                    (
                        parse_config.name.clone(),
                        vec![Rc::new(MacroDefinition {
                            parse_config,
                            implementation: inbuilt_macro_implementation(builtin),
                        })],
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
    Include,
    Sinclude,
    Changecom,
    Changequote,
    Pushdef,
    Popdef,
    Incr,
    Ifelse,
    Shift,
}

impl AsRef<[u8]> for BuiltinMacro {
    fn as_ref(&self) -> &'static [u8] {
        match self {
            BuiltinMacro::Dnl => b"dnl",
            BuiltinMacro::Define => b"define",
            BuiltinMacro::Undefine => b"undefine",
            BuiltinMacro::Errprint => b"errprint",
            BuiltinMacro::Include => b"include",
            BuiltinMacro::Sinclude => b"sinclude",
            BuiltinMacro::Changecom => b"changecom",
            BuiltinMacro::Changequote => b"changequote",
            BuiltinMacro::Pushdef => b"pushdef",
            BuiltinMacro::Popdef => b"popdef",
            BuiltinMacro::Incr => b"incr",
            BuiltinMacro::Ifelse => b"ifelse",
            BuiltinMacro::Shift => b"shift",
        }
    }
}

impl BuiltinMacro {
    pub fn enumerate() -> &'static [Self] {
        &[
            Self::Dnl,
            Self::Define,
            Self::Undefine,
            Self::Errprint,
            Self::Include,
            Self::Sinclude,
            Self::Changecom,
            Self::Changequote,
            Self::Pushdef,
            Self::Popdef,
            Self::Incr,
            Self::Ifelse,
            Self::Shift,
        ]
    }
    pub fn name(&self) -> MacroName {
        MacroName::try_from_slice(self.as_ref()).expect("Expected valid builtin macro name")
    }

    /// The minimum number of args that this macro requires in order for it to be parsed as a
    /// macro.
    pub fn min_args(&self) -> usize {
        match self {
            BuiltinMacro::Dnl => 0,
            BuiltinMacro::Define => 1,
            BuiltinMacro::Undefine => 1,
            BuiltinMacro::Errprint => 1,
            BuiltinMacro::Include => 1,
            BuiltinMacro::Sinclude => 1,
            BuiltinMacro::Changecom => 0,
            BuiltinMacro::Changequote => 0,
            BuiltinMacro::Pushdef => 1,
            BuiltinMacro::Popdef => 1,
            BuiltinMacro::Incr => 1,
            BuiltinMacro::Ifelse => 1,
            BuiltinMacro::Shift => 1,
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
fn inbuilt_macro_implementation(builtin: &BuiltinMacro) -> Box<dyn MacroImplementation> {
    match builtin {
        BuiltinMacro::Dnl => Box::new(DnlMacro),
        BuiltinMacro::Define => Box::new(DefineMacro),
        BuiltinMacro::Undefine => Box::new(UndefineMacro),
        BuiltinMacro::Errprint => Box::new(ErrprintMacro),
        BuiltinMacro::Include => Box::new(IncludeMacro),
        BuiltinMacro::Sinclude => Box::new(SincludeMacro),
        BuiltinMacro::Changecom => Box::new(ChangecomMacro),
        BuiltinMacro::Changequote => Box::new(ChangequoteMacro),
        BuiltinMacro::Pushdef => Box::new(PushdefMacro),
        BuiltinMacro::Popdef => Box::new(PopdefMacro),
        BuiltinMacro::Incr => Box::new(IncrMacro),
        BuiltinMacro::Ifelse => Box::new(IfelseMacro),
        BuiltinMacro::Shift => Box::new(ShiftMacro),
    }
}

impl State {
    fn current_macro_parse_configs(&self) -> HashMap<MacroName, MacroParseConfig> {
        // log::debug!(
        //     "State::current_macro_parse_configs() definitions: {:?}",
        //     self.macro_definitions
        //         .iter()
        //         .map(|(name, e)| (name.to_string(), e.len()))
        //         .collect::<Vec<_>>()
        // );
        self.macro_definitions
            .iter()
            .map(|(name, definitions)| {
                let current_definition = definitions
                    .last()
                    .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT);
                let config = current_definition.parse_config.clone();
                (name.clone(), config)
            })
            .collect()
    }
}

pub(crate) struct MacroDefinition {
    pub parse_config: MacroParseConfig,
    // TODO: improve performance with enum dispatch
    implementation: Box<dyn MacroImplementation>,
}

impl std::fmt::Debug for MacroDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MacroDefinition")
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

trait MacroImplementation {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State>;
}

/// The dnl macro shall cause m4 to discard all input characters up to and including the next
/// `<newline>`.
struct DnlMacro;

impl MacroImplementation for DnlMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        _stderror: &mut dyn Write,
        _m: Macro,
    ) -> Result<State> {
        state.parse_config.dnl = true;
        Ok(state)
    }
}

/// The second argument shall become the defining text of the macro whose name is the first
/// argument. It is unspecified whether the define macro deletes all prior definitions of the macro
/// named by its first argument or preserves all but the current definition of the macro. The
/// behavior is unspecified if define is not immediately followed by a `<left-parenthesis>`.
///
/// This particular implementation matches GNU m4 behaviour, and perserves all but the current
/// definition of the macro.
struct DefineMacro;

impl DefineMacro {
    fn define(
        mut state: State,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<(State, Option<MacroDefinition>)> {
        let mut args = m.args.into_iter();
        let name = if let Some(i) = args.next() {
            let name_bytes: Vec<u8>;
            (name_bytes, state) = evaluate_to_text(state, i, stderror)?;

            if let Ok(name) = MacroName::try_from_slice(&name_bytes) {
                name
            } else {
                log::warn!(
                    "Invalid macro name {:?}, skipping definition",
                    String::from_utf8_lossy(&name_bytes)
                );
                return Ok((state, None));
            }
        } else {
            log::warn!("No macro name specified, skipping definition");
            return Ok((state, None));
        };
        let definition = if let Some(i) = args.next() {
            let definition: Vec<u8>;
            (definition, state) = evaluate_to_text(state, i, stderror)?;
            definition
        } else {
            log::warn!("No macro definition provided, skipping definition");
            return Ok((state, None));
        };
        log::debug!(
            "DefineMacro::define() defined macro {name}: {:?}",
            String::from_utf8_lossy(&definition)
        );
        let definition = MacroDefinition {
            parse_config: MacroParseConfig { name, min_args: 0 },
            implementation: Box::new(UserDefinedMacro { definition }),
        };
        Ok((state, Some(definition)))
    }
}

impl MacroImplementation for DefineMacro {
    fn evaluate(
        &self,
        state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderror, m)?;
        let definition = match definition {
            Some(definition) => definition,
            None => return Ok(state),
        };
        let name = definition.parse_config.name.clone();
        log::trace!("DefineMacro::evaluate() inserting new macro definition for {name}");

        let definition = Rc::new(definition);
        if let Some(e) = state.macro_definitions.get_mut(&name) {
            let last = e.last_mut().expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT);
            *last = definition;
        } else {
            state.macro_definitions.insert(name, vec![definition]);
        }
        state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
        Ok(state)
    }
}

/// The pushdef macro shall be equivalent to the define macro with the exception that it shall
/// preserve any current definition for future retrieval using the popdef macro. The behavior is
/// unspecified if pushdef is not immediately followed by a `<left-parenthesis>`.
struct PushdefMacro;

impl MacroImplementation for PushdefMacro {
    fn evaluate(
        &self,
        state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderror, m)?;
        let definition = match definition {
            Some(definition) => definition,
            None => return Ok(state),
        };
        let name = definition.parse_config.name.clone();
        log::debug!("PushdefMacro::evaluate() pushing new macro definition for {name}");
        let definition = Rc::new(definition);
        if let Some(e) = state.macro_definitions.get_mut(&name) {
            e.push(definition);
        } else {
            state.macro_definitions.insert(name, vec![definition]);
        }
        state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
        Ok(state)
    }
}

/// The popdef macro shall delete the current definition of its arguments, replacing that
/// definition with the previous one. If there is no previous definition, the macro is undefined.
/// The behavior is unspecified if popdef is not immediately followed by a `<left-parenthesis>`.
struct PopdefMacro;

impl MacroImplementation for PopdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(first_arg_symbols) = m.args.into_iter().next() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_text(state, first_arg_symbols, stderror)?;
            if let Ok(name) = MacroName::try_from_slice(&text) {
                if let Some(n_remaining_definitions) =
                    state.macro_definitions.get_mut(&name).map(|e| {
                        e.pop();
                        e.len()
                    })
                {
                    // This was the last definition, so the entry should be removed entirely.
                    if n_remaining_definitions == 0 {
                        state.macro_definitions.remove(&name);
                    }
                    state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
                }
            }
        }
        return Ok(state);
    }
}

/// Arguments are positionally defined and referenced. The string "$1" in the defining text shall
/// be replaced by the first argument. Systems shall support at least nine arguments; only the
/// first nine can be referenced, using the strings "$1" to "$9", inclusive. The string "$0" is
/// replaced with the name of the macro. The string "$#" is replaced by the number of arguments as
/// a string. The string "$*" is replaced by a list of all of the arguments, separated by <comma>
/// characters. The string "$@" is replaced by a list of all of the arguments separated by <comma>
/// characters, and each argument is quoted using the current left and right quoting strings. The
/// string "${" produces unspecified behavior.
enum UserDefinedMacroArg {
    Index(usize),
    List,
    QuotedList,
    NumberOfArgs,
}

struct UserDefinedMacro {
    definition: Vec<u8>,
}

fn parse_index(input: &[u8]) -> IResult<&[u8], usize> {
    log::trace!("parse_index() {}", String::from_utf8_lossy(input));
    let (remaining, found) = nom::bytes::complete::take_while(|c| c >= b'0' && c <= b'9')(input)?;
    let s = std::str::from_utf8(found).expect("Should be valid utf8 betweeen b'0' and b'9'");
    let i: usize = s.parse().map_err(|e| {
        let e = nom::error::Error::from_external_error(found, nom::error::ErrorKind::Digit, e);
        nom::Err::Error(nom::error::Error::add_context(
            found,
            "Error parsing user defined macro argument as an index",
            e,
        ))
    })?;
    log::trace!("parse_index() successfully parsed: {i}");

    Ok((remaining, i))
}

fn parse_user_defined_macro_arg(input: &[u8]) -> IResult<&[u8], UserDefinedMacroArg> {
    log::trace!(
        "parse_user_defined_macro_arg() {}",
        String::from_utf8_lossy(input)
    );
    let (remaining, _) = nom::bytes::complete::tag(b"$")(input)?;
    if remaining.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::NonEmpty,
        )));
    }

    nom::branch::alt((
        nom::combinator::map(nom::bytes::complete::tag(b"*"), |_| {
            UserDefinedMacroArg::List
        }),
        nom::combinator::map(nom::bytes::complete::tag(b"@"), |_| {
            UserDefinedMacroArg::QuotedList
        }),
        nom::combinator::map(nom::bytes::complete::tag(b"#"), |_| {
            UserDefinedMacroArg::NumberOfArgs
        }),
        nom::combinator::map(parse_index, UserDefinedMacroArg::Index),
    ))(remaining)
}

impl MacroImplementation for UserDefinedMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let mut buffer: Vec<u8> = Vec::with_capacity(self.definition.len());
        let mut remaining = self.definition.as_slice();
        let mut found: &[u8];

        loop {
            if remaining.is_empty() {
                break;
            }
            (remaining, found) =
                nom::bytes::complete::take_till::<_, _, ()>(|c| c == b'$')(remaining)
                    .expect("Expect this to always succeed");
            buffer.extend_from_slice(found);
            if remaining.is_empty() {
                break;
            }
            let arg;
            (remaining, arg) = match parse_user_defined_macro_arg(remaining) {
                Ok((remaining, arg)) => (remaining, arg),
                Err(_) => {
                    buffer.push(b'$');
                    if remaining.len() > 1 {
                        remaining = &remaining[1..];
                        continue;
                    } else {
                        break;
                    }
                }
            };

            match arg {
                UserDefinedMacroArg::Index(i) => {
                    if i == 0 {
                        buffer.write_all(&m.name.0)?;
                    } else {
                        if let Some(arg) = m.args.get(i - 1) {
                            log::trace!(
                                "UserDefinedMacro::evaluate() Replacing arg {i} with {arg:?}"
                            );
                            for symbol in arg {
                                state = evaluate(state, symbol.clone(), &mut buffer, stderror)?;
                            }
                        } else {
                            log::trace!(
                                "UserDefinedMacro::evaluate() Cannot find arg with index {i}"
                            );
                        }
                    }
                }
                UserDefinedMacroArg::List => {
                    for (i, arg) in m.args.iter().enumerate() {
                        for symbol in arg {
                            state = evaluate(state, symbol.clone(), &mut buffer, stderror)?;
                        }
                        if i < m.args.len() - 1 {
                            buffer.write_all(b",")?;
                        }
                    }
                }
                UserDefinedMacroArg::QuotedList => {
                    for (i, arg) in m.args.iter().enumerate() {
                        buffer.write_all(&state.parse_config.quote_open_tag)?;
                        for symbol in arg {
                            state = evaluate(state, symbol.clone(), &mut buffer, stderror)?;
                        }
                        buffer.write_all(&state.parse_config.quote_close_tag)?;
                        if i < m.args.len() - 1 {
                            buffer.write_all(b",")?;
                        }
                    }
                }
                UserDefinedMacroArg::NumberOfArgs => {
                    buffer.extend_from_slice(m.args.len().to_string().as_bytes());
                }
            }
        }
        stdout.write_all(&buffer)?;
        Ok(state)
    }
}

/// The undefine macro shall delete all definitions (including those preserved using the pushdef
/// macro) of the macros named by its arguments. The behavior is unspecified if undefine is not
/// immediately followed by a ``left-parenthesis>`.
struct UndefineMacro;

impl MacroImplementation for UndefineMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(first_arg_symbols) = m.args.into_iter().next() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_text(state, first_arg_symbols, stderror)?;
            if let Ok(name) = MacroName::try_from_slice(&text) {
                state.macro_definitions.remove(&name);
                state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
            }
        }
        return Ok(state);
    }
}

struct ErrprintMacro;

impl MacroImplementation for ErrprintMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let args_len = m.args.len();
        for (i, input) in m.args.into_iter().enumerate() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_text(state, input, stderror)?;
            stderror.write_all(&text)?;
            if i < (args_len - 1) {
                stderror.write_all(b" ")?;
            }
        }
        return Ok(state);
    }
}

/// The defining text for the include macro shall be the contents of the file named by the first
/// argument. It shall be an error if the file cannot be read. The behavior is unspecified if
/// include is not immediately followed by a `<left-parenthesis>`.
struct IncludeMacro;

impl IncludeMacro {
    fn get_file_path(
        m: Macro,
        state: State,
        stderror: &mut dyn Write,
    ) -> Result<(Option<PathBuf>, State)> {
        if let Some(path_symbols) = m.args.into_iter().next() {
            let (buffer, state) = evaluate_to_text(state, path_symbols, stderror)?;
            let path = PathBuf::from(OsString::from_vec(buffer));
            Ok((Some(path), state))
        } else {
            Ok((None, state))
        }
    }
}

impl MacroImplementation for IncludeMacro {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (path, state) = Self::get_file_path(m, state, stderror)?;
        if let Some(path) = path {
            lexer::process_streaming(
                state,
                evaluate,
                std::fs::File::open(path)?,
                stdout,
                stderror,
            )
        } else {
            Ok(state)
        }
    }
}

/// The sinclude macro shall be equivalent to the [`IncludeMacro`], except that it shall not be an
/// error if the file is inaccessible. The behavior is unspecified if sinclude is not immediately
/// followed by a `<left-parenthesis>`.
struct SincludeMacro;

impl MacroImplementation for SincludeMacro {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (path, state) = IncludeMacro::get_file_path(m, state, stderror)?;
        if let Some(path) = path {
            if path.is_file() {
                return lexer::process_streaming(
                    state,
                    evaluate,
                    std::fs::File::open(path)?,
                    stdout,
                    stderror,
                );
            }
        }

        Ok(state)
    }
}

struct ChangecomMacro;

impl MacroImplementation for ChangecomMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let args_len = m.args.len();

        if args_len == 0 {
            state.parse_config.comment_enabled = false;
            log::trace!("ChangecomMacro::evaluate() reset to default");
            return Ok(state);
        }

        let mut args = m.args.into_iter();
        let open = args.next().expect("1 argument should be present");
        let open_tag;
        (open_tag, state) = evaluate_to_text(state, open, stderror)?;
        if !open_tag.is_empty() {
            log::trace!(
                "ChangecomMacro::evaluate() comment_open_tag set to {:?}",
                String::from_utf8_lossy(&open_tag)
            );
            state.parse_config.comment_enabled = true;
            state.parse_config.comment_open_tag = open_tag;
        }

        if args_len >= 2 {
            let close = args.next().expect("2 arguments should be present");
            let close_tag;
            (close_tag, state) = evaluate_to_text(state, close, stderror)?;
            if !close_tag.is_empty() {
                log::trace!(
                    "ChangecomMacro::evaluate() comment_close_tag set to {:?}",
                    String::from_utf8_lossy(&close_tag)
                );
                state.parse_config.comment_close_tag = close_tag;
            }
        }

        if args_len > 2 {
            stderror.write_all(b"Warning: excess arguments to builtin `changecom' ignored")?;
        }

        Ok(state)
    }
}

struct ChangequoteMacro;

impl MacroImplementation for ChangequoteMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        match m.args.len() {
            0 => {
                state.parse_config.quote_open_tag = DEFAULT_QUOTE_OPEN_TAG.to_owned();
                state.parse_config.quote_close_tag = DEFAULT_QUOTE_CLOSE_TAG.to_owned();
            }
            1 => {}
            args_len @ 2.. => {
                if args_len > 2 {
                    stderror
                        .write_all(b"Warning: excess arguments to builtin `changequote' ignored")?;
                }
                let mut args = m.args.into_iter();
                let open = args.next().expect("2 arguments should be present");
                let close = args.next().expect("2 arguments should be present");
                let open_tag;
                (open_tag, state) = evaluate_to_text(state, open, stderror)?;
                let close_tag;
                (close_tag, state) = evaluate_to_text(state, close, stderror)?;
                // The behavior is unspecified if there is a single argument or either argument is null.
                if !open_tag.is_empty() && !close_tag.is_empty() {
                    // TODO: it looks like GNU m4 only allows quote strings using non-alphanumeric
                    // characters. The spec I'm following doesn't mention anything about that.
                    state.parse_config.quote_open_tag = open_tag;
                    state.parse_config.quote_close_tag = close_tag;
                }
            } //TODO what about when there are more arguments? Add integration test for this.
        }

        Ok(state)
    }
}

/// The defining text of the incr macro shall be its first argument incremented by 1. It shall be
/// an error to specify an argument containing any non-numeric characters. The behavior is
/// unspecified if incr is not immediately followed by a <left-parenthesis>.
struct IncrMacro;

impl MacroImplementation for IncrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(first) = m.args.into_iter().next() {
            let number_bytes;
            (number_bytes, state) = evaluate_to_text(state, first, stderror)?;
            let number_string = std::str::from_utf8(&number_bytes).map_err(|e| {
                crate::Error::Parsing(format!(
                    "Error parsing number as valid utf8 from {number_bytes:?}: {e}"
                ))
            })?;
            let mut number: i64 = number_string.parse().map_err(|e| {
                crate::Error::Parsing(format!("Error parsing number from {number_string:?}: {e}"))
            })?;
            number += 1;
            stdout.write_all(number.to_string().as_bytes())?;
        }
        Ok(state)
    }
}

/// The ifelse macro takes three or more arguments. If the first two arguments compare as equal
/// strings (after macro expansion of both arguments), the defining text shall be the third
/// argument. If the first two arguments do not compare as equal strings and there are three
/// arguments, the defining text shall be null. If the first two arguments do not compare as equal
/// strings and there are four or five arguments, the defining text shall be the fourth argument.
/// If the first two arguments do not compare as equal strings and there are six or more arguments,
/// the first three arguments shall be discarded and processing shall restart with the remaining
/// arguments. The behavior is unspecified if ifelse is not immediately followed by a
/// <left-parenthesis>.
struct IfelseMacro;

impl MacroImplementation for IfelseMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if m.args.len() < 3 {
            write!(stderror, "Too few arguments to builtin `ifelse'")?;
            return Ok(state);
        }

        let mut args_len = m.args.len();
        let mut args = m.args.into_iter();
        let symbols = args.next().expect("at least 3 args");
        let first;
        (first, state) = evaluate_to_text(state, symbols, stderror)?;
        let symbols = args.next().expect("at least 3 args");
        let second;
        (second, state) = evaluate_to_text(state, symbols, stderror)?;
        loop {
            if first == second {
                let symbols = args.next().expect("at least 3 args");
                for symbol in symbols {
                    state = evaluate(state, symbol, stdout, stderror)?;
                }
                return Ok(state);
            } else {
                match args_len {
                    0 | 1 | 2 => panic!("at least 3 args"),
                    3 => return Ok(state),
                    4 | 5 => {
                        args.next();
                        let symbols = args.next().expect("at least 4 args");
                        for symbol in symbols {
                            state = evaluate(state, symbol, stdout, stderror)?;
                        }
                        return Ok(state);
                    }
                    6.. => {
                        // the first three arguments shall be discarded and processing shall
                        // restart with the remaining arguments.
                        args.next();
                        args_len -= 3;
                    }
                }
            }
        }
    }
}

/// The defining text for the shift macro shall be a comma-separated list of its arguments except
/// the first one. Each argument shall be quoted using the current quoting strings. The behavior is
/// unspecified if shift is not immediately followed by a <left-parenthesis>.
struct ShiftMacro;

impl MacroImplementation for ShiftMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderror: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let args_len = m.args.len();
        for (i, symbols) in m.args.into_iter().enumerate() {
            if i == 0 {
                continue;
            }
            let buffer;
            (buffer, state) = evaluate_to_text(state, symbols, stderror)?;
            stdout.write_all(&buffer)?;
            if i < (args_len - 1) {
                stdout.write_all(b",")?;
            }
        }

        Ok(state)
    }
}

fn evaluate_to_text(
    mut state: State,
    symbols: Vec<Symbol>,
    stderror: &mut dyn Write,
) -> Result<(Vec<u8>, State)> {
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
///
/// great(fantastic)
///
/// It's a macro so evaluate it.
///
/// "hello fantastic"
/// contains a macro so don't output, and evaluate the symbols
///
/// "hello" it's a macro so evaluate it
/// "world world fantastic"
/// "world" it's a macro so evaluate it
/// "amazing amazing fantastic"
/// contains no macros so break and output to stdout
///
///
/// The input to the define macro should only have its quotes unwrapped, not subsequently
/// evaluated. Perhaps input from unwrapping quotes should never be evaluated?
/// Ahh perhaps unwrapping quotes should be a separate step after there are no macros remaining.
///
/// First question, why are the quotes evaluated after being unwrapped for macro arguments?
pub(crate) fn evaluate(
    mut state: State,
    symbol: Symbol,
    stdout: &mut dyn Write,
    stderror: &mut dyn Write,
) -> Result<State> {
    log::debug!("evaluate() EVALUATING {symbol:?}");
    // log::debug!(
    //     "evaluate() macro_definitions: {:?}",
    //     state
    //         .macro_definitions
    //         .iter()
    //         .map(|(name, e)| (name.to_string(), e.len()))
    //         .collect::<Vec<_>>()
    // );
    let mut root_symbol = Some(symbol);
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!state.parse_config.dnl);

    let mut first = true;
    let mut buffer = Vec::new();
    let mut last = false;
    loop {
        log::debug!("evaluate() buffer: {:?}", String::from_utf8_lossy(&buffer));
        let symbols = if first {
            vec![root_symbol.take().expect("First iteration")]
        } else {
            // TODO: parses symbols twice, not ideal! Need to find a way to make an owned
            // Symbol so it can be used in this loop. Or put some kind of self rererential
            // struct in the stack containing the symbol and the buffer it came from.
            let symbols = lexer::parse_symbols_complete(&state.parse_config, &mut buffer)?;
            symbols
        };
        // No symbols which require evaluation are remaining.
        if !symbols
            .iter()
            .find(|s| matches!(s, Symbol::Macro(_)))
            .is_some()
        {
            last = true;
        }
        let mut new_buffer: Vec<u8> = Vec::new();

        if last {
            log::debug!("evaluate() last");
        }
        log::debug!("evaluate() {symbols:?}");

        // TODO(performance): if this is the last we could write directly to stdout
        for symbol in symbols {
            match symbol {
                Symbol::Comment(comment) => new_buffer.write_all(comment)?,
                Symbol::Text(text) => new_buffer.write_all(text)?,
                Symbol::Quoted(quoted) => {
                    log::debug!("evaluate() writing quoted {quoted:?}");
                    if last {
                        new_buffer.write_all(quoted.contents)?;
                    } else {
                        new_buffer.write_all(quoted.all)?;
                    }
                }
                Symbol::Macro(m) => {
                    log::debug!("evaluate() evaluating macro {:?}", m.name.to_string());
                    let definition = state
                        .macro_definitions
                        .get(&m.name)
                        .and_then(|e| e.last().cloned())
                        .expect("There should always be a definition for a parsed macro");
                    state =
                        definition
                            .implementation
                            .evaluate(state, &mut new_buffer, stderror, m)?;
                }
                Symbol::Newline => new_buffer.write_all(b"\n")?,
                Symbol::Eof => {}
            }
        }

        first = false;
        buffer = new_buffer;

        if last {
            log::debug!(
                "evaluate() finished: {:?}",
                String::from_utf8_lossy(&buffer)
            );
            stdout.write_all(&mut buffer)?;
            break;
        }
    }

    Ok(state)
}
