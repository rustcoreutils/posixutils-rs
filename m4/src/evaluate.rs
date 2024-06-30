use std::cell::RefCell;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::process::ExitStatus;
use std::{collections::HashMap, io::Write, rc::Rc};

use nom::error::{ContextError, FromExternalError};
use nom::IResult;

use crate::error::Result;
use crate::eval_macro;
use crate::lexer::{
    self, Macro, MacroName, MacroParseConfig, ParseConfig, Symbol, DEFAULT_QUOTE_CLOSE_TAG,
    DEFAULT_QUOTE_OPEN_TAG,
};

const AT_LEAST_ONE_MACRO_DEFINITION_EXPECT: &str =
    "There should always be at least one macro definition";

pub struct State {
    macro_definitions: HashMap<MacroName, Vec<Rc<MacroDefinition>>>,
    pub parse_config: ParseConfig,
    /// Whether the process should exit with an error once processing has completed.
    pub exit_error: bool,
    /// See [`M4wrapMacro`].
    pub m4wrap: Vec<Vec<u8>>,
    pub last_syscmd_status: Option<ExitStatus>,
    /// Divert buffers 1 through to 9. See [`DivertMacro`].
    pub divert_buffers: [DivertableBuffer; 9],
    /// See [`DivertMacro`].
    pub divert_number: usize,
    /// Stack of filenames. Used in [`FileMacro`].
    pub file: Vec<PathBuf>,
}

impl State {
    fn debug_macro_definitions(&self) -> String {
        format!(
            "macro_definitions: {:?}",
            self.macro_definitions
                .iter()
                .map(|(name, e)| (name.to_string(), e.len()))
                .collect::<Vec<_>>()
        )
    }
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
                            implementation: builtin.implementation(),
                        })],
                    )
                })
                .collect(),
            parse_config: ParseConfig::default(),
            exit_error: false,
            m4wrap: Vec::new(),
            last_syscmd_status: None,
            divert_buffers: [(); 9].map(|_| DivertableBuffer::default()),
            divert_number: 0,
            file: Vec::new(),
        }
    }
}

/// TODO: This is a little wild west in terms of panic occurance and usability
#[derive(Default, Clone)]
pub(crate) struct DivertableBuffer(pub Rc<RefCell<Vec<u8>>>);

impl Write for DivertableBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.0.borrow_mut().write_all(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.borrow_mut().flush()
    }
}

macro_rules! macro_enums {
    (
        $(#[$meta:meta])*
        pub enum BuiltinMacroDefinition {
            $($variant_name:ident($variant_type:ident)),* $(,)?
        }
    ) => {
        $(#[$meta])*
        pub enum BuiltinMacro {
            $($variant_name),*
        }

        enum MacroDefinitionImplementation {
            $($variant_name($variant_type)),*,
            UserDefined(UserDefinedMacro),
        }

        impl MacroImplementation for MacroDefinitionImplementation {
            fn evaluate(
                &self,
                state: State,
                stdout: &mut dyn Write,
                stderr: &mut dyn Write,
                m: Macro,
            ) -> Result<State> {
                match self {
                    $(Self::$variant_name(d) => d.evaluate(state, stdout, stderr, m)),*,
                    Self::UserDefined(d) => d.evaluate(state, stdout, stderr, m),
                }
            }
        }

        impl BuiltinMacro {
            pub fn enumerate() -> &'static [Self] {
                &[$(Self::$variant_name),*]
            }

            fn implementation(&self) -> MacroDefinitionImplementation {
                match self {
                    $(Self::$variant_name => MacroDefinitionImplementation::$variant_name($variant_type)),*
                }
            }
        }
    };
}

// Maketemp intentionally left out, it's obsolete apparently.
macro_enums!(
    #[derive(Clone, Copy)]
    pub enum BuiltinMacroDefinition {
        Dnl(DnlMacro),
        Define(DefineMacro),
        Undefine(UndefineMacro),
        Errprint(ErrprintMacro),
        Include(IncludeMacro),
        Sinclude(SincludeMacro),
        Changecom(ChangecomMacro),
        Changequote(ChangequoteMacro),
        Pushdef(PushdefMacro),
        Popdef(PopdefMacro),
        Incr(IncrMacro),
        Ifelse(IfelseMacro),
        Shift(ShiftMacro),
        Eval(EvalMacro),
        Decr(DecrMacro),
        Len(LenMacro),
        Index(IndexMacro),
        Ifdef(IfdefMacro),
        Translit(TranslitMacro),
        Defn(DefnMacro),
        Substr(SubstrMacro),
        Dumpdef(DumpdefMacro),
        Mkstemp(MkstempMacro),
        M4exit(M4exitMacro),
        M4wrap(M4wrapMacro),
        Syscmd(SyscmdMacro),
        Sysval(SysvalMacro),
        Divert(DivertMacro),
        Divnum(DivnumMacro),
        Undivert(UndivertMacro),
        File(FileMacro),
    }
);
// TODO: implement these macros:
// Traceoff,
// Traceon,

impl AsRef<[u8]> for BuiltinMacro {
    fn as_ref(&self) -> &'static [u8] {
        use BuiltinMacro::*;
        match self {
            Dnl => b"dnl",
            Define => b"define",
            Undefine => b"undefine",
            Defn => b"defn",
            Errprint => b"errprint",
            Include => b"include",
            Sinclude => b"sinclude",
            Changecom => b"changecom",
            Changequote => b"changequote",
            Pushdef => b"pushdef",
            Popdef => b"popdef",
            Incr => b"incr",
            Ifelse => b"ifelse",
            Ifdef => b"ifdef",
            Shift => b"shift",
            Eval => b"eval",
            Decr => b"decr",
            Len => b"len",
            Index => b"index",
            Translit => b"translit",
            Substr => b"substr",
            Dumpdef => b"dumpdef",
            Mkstemp => b"mkstemp",
            M4exit => b"m4exit",
            M4wrap => b"m4wrap",
            Syscmd => b"syscmd",
            Sysval => b"sysval",
            Divert => b"divert",
            Divnum => b"divnum",
            Undivert => b"undivert",
            File => b"__file__",
        }
    }
}

impl BuiltinMacro {
    pub fn name(&self) -> MacroName {
        MacroName::try_from_slice(self.as_ref()).expect("Expected valid builtin macro name")
    }

    /// The minimum number of args that this macro requires in order for it to be parsed as a
    /// macro.
    pub fn min_args(&self) -> usize {
        use BuiltinMacro::*;
        match self {
            Dnl => 0,
            Define => 1,
            Undefine => 1,
            Defn => 1,
            Errprint => 1,
            Include => 1,
            Sinclude => 1,
            Changecom => 0,
            Changequote => 0,
            Pushdef => 1,
            Popdef => 1,
            Incr => 1,
            Decr => 1,
            Ifelse => 1,
            Ifdef => 1,
            Shift => 1,
            Eval => 1,
            Len => 1,
            Index => 1,
            Translit => 1,
            Substr => 1,
            Dumpdef => 1,
            Mkstemp => 1,
            M4exit => 0,
            M4wrap => 1,
            Syscmd => 1,
            Sysval => 0,
            Divert => 0,
            Divnum => 0,
            Undivert => 0,
            File => 0,
        }
    }

    pub fn parse_config(&self) -> MacroParseConfig {
        MacroParseConfig {
            name: self.name(),
            min_args: self.min_args(),
        }
    }
}

impl State {
    fn current_macro_parse_configs(&self) -> HashMap<MacroName, MacroParseConfig> {
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
    implementation: MacroDefinitionImplementation,
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
        stderr: &mut dyn Write,
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
        _stderr: &mut dyn Write,
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
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<(State, Option<MacroDefinition>)> {
        let mut args = m.args.into_iter();
        let name = if let Some(arg) = args.next() {
            let name_bytes: Vec<u8>;
            (name_bytes, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;

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
        let definition = if let Some(arg) = args.next() {
            let definition: Vec<u8>;
            (definition, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
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
            implementation: MacroDefinitionImplementation::UserDefined(UserDefinedMacro {
                definition,
            }),
        };
        Ok((state, Some(definition)))
    }
}

impl MacroImplementation for DefineMacro {
    fn evaluate(
        &self,
        state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderr, m)?;
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
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderr, m)?;
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
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(first) = m.args.into_iter().next() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_buffer(state, first.symbols, stderr, true)?;
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
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        log::debug!(
            "UserDefinedMacro::evaluate() evaluating {:?}: {:?}",
            m.name.to_string(),
            String::from_utf8_lossy(&self.definition)
        );
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
                    if m.args.len() < i {
                        log::warn!("UserDefinedMacro::evaluate() Cannot find arg with index {i}");
                    } else {
                        // TODO: remove new_buffer
                        let mut new_buffer: Vec<u8> = Vec::new();
                        if i == 0 {
                            new_buffer.write_all(&m.name.0)?;
                        } else {
                            let arg = m.args.get(i - 1).expect("Checked args length");
                            for symbol in &arg.symbols {
                                state =
                                    evaluate(state, symbol.clone(), &mut new_buffer, stderr, true)?;
                            }
                        }
                        log::debug!(
                            "UserDefinedMacro::evaluate() Replacing ${i} with {:?}",
                            String::from_utf8_lossy(&new_buffer)
                        );
                        buffer.append(&mut new_buffer);
                    }
                }
                UserDefinedMacroArg::List => {
                    log::debug!(
                        "UserDefinedMacro::evaluate() Replacing $* with {:?}",
                        m.args
                    );
                    for (i, arg) in m.args.iter().enumerate() {
                        for symbol in &arg.symbols {
                            state = evaluate(state, symbol.clone(), &mut buffer, stderr, true)?;
                        }
                        if i < m.args.len() - 1 {
                            buffer.write_all(b",")?;
                        }
                    }
                }
                UserDefinedMacroArg::QuotedList => {
                    // TODO: remove new_buffer
                    let mut new_buffer: Vec<u8> = Vec::new();
                    for (i, arg) in m.args.iter().enumerate() {
                        new_buffer.write_all(&state.parse_config.quote_open_tag)?;
                        for symbol in &arg.symbols {
                            state = evaluate(state, symbol.clone(), &mut new_buffer, stderr, true)?;
                        }
                        new_buffer.write_all(&state.parse_config.quote_close_tag)?;
                        if i < m.args.len() - 1 {
                            new_buffer.write_all(b",")?;
                        }
                    }
                    log::debug!(
                        "UserDefinedMacro::evaluate() Replacing $@ with {:?}",
                        String::from_utf8_lossy(&new_buffer)
                    );
                    buffer.append(&mut new_buffer);
                }
                UserDefinedMacroArg::NumberOfArgs => {
                    buffer.extend_from_slice(m.args.len().to_string().as_bytes());
                }
            }
        }
        log::debug!(
            "UserDefinedMacro::evaluate() substituted {:?}\nargs: {:?}\nold: {:?}\nnew: {:?}",
            m.name.to_string(),
            &m.args,
            String::from_utf8_lossy(&self.definition),
            String::from_utf8_lossy(&buffer)
        );
        state = lexer::process_streaming(state, evaluate, &*buffer, stdout, stderr, false, false)?;
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
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(arg) = m.args.into_iter().next() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
            if let Ok(name) = MacroName::try_from_slice(&text) {
                state.macro_definitions.remove(&name);
                state.parse_config.macro_parse_configs = state.current_macro_parse_configs();
            }
        }
        return Ok(state);
    }
}

/// The defining text of the defn macro shall be the quoted definition (using the current quoting
/// strings) of its arguments. The behavior is unspecified if defn is not immediately followed by a
/// `<left-parenthesis>`.
struct DefnMacro;

impl MacroImplementation for DefnMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        if let Some(definitions) = state
            .macro_definitions
            .get(&MacroName::try_from_slice(&first_arg_text)?)
        {
            let definition = definitions
                .last()
                .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT);
            if let MacroDefinitionImplementation::UserDefined(definition) =
                &definition.implementation
            {
                stdout.write_all(&state.parse_config.quote_open_tag)?;
                stdout.write_all(&definition.definition)?;
                stdout.write_all(&state.parse_config.quote_close_tag)?;
            }
        }
        Ok(state)
    }
}

struct ErrprintMacro;

impl MacroImplementation for ErrprintMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let args_len = m.args.len();
        for (i, arg) in m.args.into_iter().enumerate() {
            let text: Vec<u8>;
            (text, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
            stderr.write_all(&text)?;
            if i < (args_len - 1) {
                stderr.write_all(b" ")?;
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
        stderr: &mut dyn Write,
    ) -> Result<(Option<PathBuf>, State)> {
        if let Some(arg) = m.args.into_iter().next() {
            let (buffer, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
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
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let path;
        (path, state) = Self::get_file_path(m, state, stderr)?;
        if let Some(path) = path {
            state.file.push(path.clone());
            state = lexer::process_streaming(
                state,
                evaluate,
                std::fs::File::open(path)?,
                stdout,
                stderr,
                false,
                false,
            )?;
            state.file.pop();
        }

        Ok(state)
    }
}

/// The sinclude macro shall be equivalent to the [`IncludeMacro`], except that it shall not be an
/// error if the file is inaccessible. The behavior is unspecified if sinclude is not immediately
/// followed by a `<left-parenthesis>`.
struct SincludeMacro;

impl MacroImplementation for SincludeMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let path;
        (path, state) = IncludeMacro::get_file_path(m, state, stderr)?;
        if let Some(path) = path {
            if path.is_file() {
                state.file.push(path.clone());
                state = lexer::process_streaming(
                    state,
                    evaluate,
                    std::fs::File::open(path)?,
                    stdout,
                    stderr,
                    false,
                    false,
                )?;
                state.file.pop();
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
        stderr: &mut dyn Write,
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
        (open_tag, state) = evaluate_to_buffer(state, open.symbols, stderr, true)?;
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
            (close_tag, state) = evaluate_to_buffer(state, close.symbols, stderr, true)?;
            if !close_tag.is_empty() {
                log::trace!(
                    "ChangecomMacro::evaluate() comment_close_tag set to {:?}",
                    String::from_utf8_lossy(&close_tag)
                );
                state.parse_config.comment_close_tag = close_tag;
            }
        }

        if args_len > 2 {
            stderr.write_all(b"Warning: excess arguments to builtin `changecom' ignored")?;
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
        stderr: &mut dyn Write,
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
                    stderr
                        .write_all(b"Warning: excess arguments to builtin `changequote' ignored")?;
                }
                let mut args = m.args.into_iter();
                let open = args.next().expect("2 arguments should be present");
                let close = args.next().expect("2 arguments should be present");
                let open_tag;
                (open_tag, state) = evaluate_to_buffer(state, open.symbols, stderr, true)?;
                let close_tag;
                (close_tag, state) = evaluate_to_buffer(state, close.symbols, stderr, true)?;
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
/// unspecified if incr is not immediately followed by a `<left-parenthesis>`.
struct IncrMacro;

impl MacroImplementation for IncrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        log::debug!("IncrMacro::evaluate() {}", state.debug_macro_definitions());
        if let Some(first) = m.args.into_iter().next() {
            let number_bytes;
            (number_bytes, state) = evaluate_to_buffer(state, first.symbols, stderr, true)?;
            let (remaining, mut number) =
                eval_macro::padded(eval_macro::parse_integer)(&number_bytes)?;
            if !remaining.is_empty() {
                return Err(crate::Error::Parsing(format!(
                    "Error parsing number from {number_bytes:?}, remaining input: {remaining:?}"
                )));
            }
            number += 1;
            stdout.write_all(number.to_string().as_bytes())?;
        }
        Ok(state)
    }
}

/// The defining text of the decr macro shall be its first argument decremented by 1. It shall be
/// an error to specify an argument containing any non-numeric characters. The behavior is
/// unspecified if decr is not immediately followed by a `<left-parenthesis>`.
struct DecrMacro;

impl MacroImplementation for DecrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        log::debug!("DecrMacro::evaluate() {}", state.debug_macro_definitions());
        if let Some(first) = m.args.into_iter().next() {
            let number_bytes;
            (number_bytes, state) = evaluate_to_buffer(state, first.symbols, stderr, true)?;
            let (remaining, mut number) =
                eval_macro::padded(eval_macro::parse_integer)(&number_bytes)?;
            if !remaining.is_empty() {
                return Err(crate::Error::Parsing(format!(
                    "Error parsing number from {number_bytes:?}, remaining input: {remaining:?}"
                )));
            }
            number -= 1;
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
/// `<left-parenthesis>`.
///
/// ifelse (string-1, string-2, equal, [not-equal])
/// ifelse (string-1, string-2, equal-1, string-3, string-4, equal-2, …, [not-equal])
struct IfelseMacro;

impl MacroImplementation for IfelseMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if m.args.len() < 3 {
            write!(stderr, "Too few arguments to builtin `ifelse'")?;
            return Ok(state);
        }

        let mut args_len = m.args.len();
        let mut args = m.args.into_iter();
        let mut i = 0;
        loop {
            let symbols = args.next().expect("at least 3 args").symbols;
            let arg_0;
            (arg_0, state) = evaluate_to_buffer(state, symbols, stderr, true)?;
            let symbols = args.next().expect("at least 3 args").symbols;
            let arg_1;
            (arg_1, state) = evaluate_to_buffer(state, symbols, stderr, true)?;
            if arg_0 == arg_1 {
                log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 2);
                let symbols = args.next().expect("at least 3 args").symbols;
                for symbol in symbols {
                    state = evaluate(state, symbol, stdout, stderr, true)?;
                }
                return Ok(state);
            } else {
                match args_len {
                    0 | 1 | 2 => panic!("at least 3 args"),
                    3 => return Ok(state),
                    4 | 5 => {
                        args.next();
                        log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 3);
                        let symbols = args.next().expect("at least 4 args").symbols;
                        for symbol in symbols {
                            state = evaluate(state, symbol, stdout, stderr, true)?;
                        }
                        if args_len == 5 {
                            write!(
                                stderr,
                                "Excess argument {:?} to builtin `ifelse' will be ignored",
                                args.next().expect("5 arguments")
                            )?;
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
            i += 1;
        }
    }
}

/// If the first argument to the ifdef macro is defined, the defining text shall be the second
/// argument. Otherwise, the defining text shall be the third argument, if specified, or the null
/// string, if not. The behavior is unspecified if ifdef is not immediately followed by a
/// `<left-parenthesis>`.
struct IfdefMacro;

impl MacroImplementation for IfdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let mut args = m.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        let second_arg = args
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let second_arg_text;
        (second_arg_text, state) = evaluate_to_buffer(state, second_arg.symbols, stderr, true)?;
        let name = MacroName::try_from_slice(&first_arg_text)?;

        if state.macro_definitions.contains_key(&name) {
            stdout.write_all(&second_arg_text)?;
        } else {
            if let Some(third_arg) = args.next() {
                let third_arg_text;
                (third_arg_text, state) =
                    evaluate_to_buffer(state, third_arg.symbols, stderr, true)?;
                stdout.write_all(&third_arg_text)?;
            }
        }
        Ok(state)
    }
}

/// The defining text for the shift macro shall be a comma-separated list of its arguments except
/// the first one. Each argument shall be quoted using the current quoting strings. The behavior is
/// unspecified if shift is not immediately followed by a `<left-parenthesis>`.
struct ShiftMacro;

impl MacroImplementation for ShiftMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let args_len = m.args.len();
        for (i, arg) in m.args.into_iter().enumerate() {
            if i == 0 {
                continue;
            }
            let buffer;
            (buffer, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
            stdout.write_all(&buffer)?;
            if i < (args_len - 1) {
                stdout.write_all(b",")?;
            }
        }

        Ok(state)
    }
}

struct EvalMacro;

impl MacroImplementation for EvalMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let buffer;
        (buffer, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        let (_remaining, output) = eval_macro::parse_and_evaluate(&buffer)?;
        stdout.write_all(output.to_string().as_bytes())?;
        Ok(state)
    }
}

/// The defining text of the len macro shall be the length (as a string) of the first argument. The
/// behavior is unspecified if len is not immediately followed by a `<left-parenthesis>`.
struct LenMacro;

impl MacroImplementation for LenMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let buffer;
        (buffer, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        stdout.write_all(buffer.len().to_string().as_bytes())?;
        Ok(state)
    }
}

/// The defining text of the index macro shall be the first character position (as a string) in the
/// first argument where a string matching the second argument begins (zero origin), or -1 if the
/// second argument does not occur. The behavior is unspecified if index is not immediately followed
/// by a `<left-parenthesis>`.
struct IndexMacro;

impl MacroImplementation for IndexMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let mut args = m.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            None => {
                stderr.write_all(b"Warning too few arguments for index macro")?;
                stdout.write_all(b"0")?;
                return Ok(state);
            }
        };
        let second_arg_text;
        (second_arg_text, state) = evaluate_to_buffer(state, second_arg.symbols, stderr, true)?;

        let index = first_arg_text
            .windows(second_arg_text.len())
            .position(|window| window == &second_arg_text);
        match index {
            Some(index) => {
                stdout.write_all(index.to_string().as_bytes())?;
            }
            None => {
                stdout.write_all(b"-1")?;
            }
        }
        Ok(state)
    }
}

/// The defining text of the translit macro shall be the first argument with every character that
/// occurs in the second argument replaced with the corresponding character from the third argument.
/// If no replacement character is specified for some source character because the second argument
/// is longer than the third argument, that character shall be deleted from the first argument in
/// translit's defining text. The behavior is unspecified if the '-' character appears within the
/// second or third argument anywhere besides the first or last character. The behavior is
/// unspecified if the same character appears more than once in the second argument. The behavior is
/// unspecified if translit is not immediately followed by a `<left-parenthesis>`.
struct TranslitMacro;

// TODO: support utf8 characters properly
impl MacroImplementation for TranslitMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let mut args = m.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let mut output_buffer;
        (output_buffer, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            None => {
                stderr.write_all(b"Warning too few arguments for index macro")?;
                stdout.write_all(b"0")?;
                return Ok(state);
            }
        };
        let second_arg_text;
        (second_arg_text, state) = evaluate_to_buffer(state, second_arg.symbols, stderr, true)?;

        let third_arg_text = match args.next() {
            Some(arg) => {
                let arg_text;
                (arg_text, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
                arg_text
            }
            None => Vec::new(),
        };

        for (i, source_char) in second_arg_text.into_iter().enumerate() {
            if let Some(replacement_char) = third_arg_text.get(i) {
                for current_char in output_buffer.iter_mut() {
                    if *current_char == source_char {
                        *current_char = *replacement_char;
                    }
                }
            } else {
                output_buffer = output_buffer
                    .into_iter()
                    .filter(|current_char| *current_char != source_char)
                    .collect();
            }
        }

        stdout.write_all(&output_buffer)?;
        Ok(state)
    }
}

/// The defining text for the substr macro shall be the substring of the first argument beginning at
/// the zero-offset character position specified by the second argument. The third argument, if
/// specified, shall be the number of characters to select; if not specified, the characters from
/// the starting point to the end of the first argument shall become the defining text. It shall not
/// be an error to specify a starting point beyond the end of the first argument and the defining
/// text shall be null. It shall be an error to specify an argument containing any non-numeric
/// characters. The behavior is unspecified if substr is not immediately followed by a
/// `<left-parenthesis>`.
struct SubstrMacro;

impl MacroImplementation for SubstrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let mut args = m.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;

        if first_arg_text.is_empty() {
            return Ok(state);
        }

        let start_index = if let Some(second_arg) = args.next() {
            let second_arg_text;
            (second_arg_text, state) = evaluate_to_buffer(state, second_arg.symbols, stderr, true)?;
            let (_, i) = nom::combinator::all_consuming(parse_index)(&second_arg_text)?;
            i
        } else {
            0
        };

        if start_index > (first_arg_text.len() - 1) {
            return Ok(state);
        }

        let out = if let Some(third_arg) = args.next() {
            let third_arg_text;
            (third_arg_text, state) = evaluate_to_buffer(state, third_arg.symbols, stderr, true)?;
            let (_, number_of_chars) =
                nom::combinator::all_consuming(parse_index)(&third_arg_text)?;

            if number_of_chars > 0 {
                let end_index = usize::min(start_index + number_of_chars, first_arg_text.len());
                &first_arg_text[start_index..end_index]
            } else {
                return Ok(state);
            }
        } else {
            &first_arg_text[start_index..]
        };

        stdout.write_all(&out)?;
        Ok(state)
    }
}

/// The dumpdef macro shall write the defined text to standard error for each of the macros
/// specified as arguments, or, if no arguments are specified, for all macros.
struct DumpdefMacro;

impl MacroImplementation for DumpdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let dumpdef = |stderr: &mut dyn Write, n: &MacroName, d: &MacroDefinition| {
            write!(stderr, "{n}:\t")?;
            match &d.implementation {
                MacroDefinitionImplementation::UserDefined(user_defined) => {
                    stderr.write_all(&user_defined.definition)?;
                }
                _ => {
                    stderr.write_all(b"<")?;
                    stderr.write_all(&n.0)?;
                    stderr.write_all(b">")?;
                }
            }
            Result::Ok(())
        };
        if m.args.is_empty() {
            for (name, definitions) in state.macro_definitions.iter() {
                dumpdef(
                    stderr,
                    &name,
                    definitions
                        .last()
                        .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT),
                )?;
                stderr.write_all(b"\n")?;
            }
        }

        for arg in m.args.into_iter() {
            let arg_text;
            (arg_text, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
            let name = MacroName::try_from_slice(&arg_text)?;
            match state.macro_definitions.get(&name) {
                Some(definition) => dumpdef(
                    stderr,
                    &name,
                    definition
                        .last()
                        .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT),
                )?,
                None => write!(stderr, "undefined macro {name}")?,
            }
            stderr.write_all(b"\n")?;
        }

        Ok(state)
    }
}

/// The defining text shall be as if it were the resulting pathname after a successful call to the
/// [`mkstemp()`](https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html) function
/// defined in the System Interfaces volume of POSIX.1-2017 called with the first argument to the
/// macro invocation. If a file is created, that file shall be closed. If a file could not be
/// created, the m4 utility shall write a diagnostic message to standard error and shall continue
/// processing input but its final exit status shall be non-zero; the defining text of the macro
/// shall be the empty string. The behavior is unspecified if mkstemp is not immediately followed by
/// a `<left-parenthesis>`.
struct MkstempMacro;

/// The mkstemp() function shall replace the contents of the `template` by a unique filename which
/// is returned. The string in template should look like a filename with six trailing 'X' s;
/// mkstemp() replaces each 'X' with a character from the portable filename character set. The
/// characters are chosen such that the resulting name does not duplicate the name of an existing
/// file at the time of a call to mkstemp().
fn mkstemp(mut template: Vec<u8>) -> Result<Vec<u8>> {
    if template.len() < 6 {
        return Err(crate::Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Unable to create temporary file, template should be greater than 6 characters long. template: {:?}", String::from_utf8_lossy(&template))
        )));
    }
    if &template[(template.len() - 6)..] != b"XXXXXX" {
        return Err(crate::Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Unable to create temporary file, template does not finish with six trailing 'X's. template: {:?}", String::from_utf8_lossy(&template))
        )));
    }
    let template_pointer = template.as_mut_ptr();
    // TODO: review safety and add proper comment.
    let file_descriptor = unsafe {
        // Docs: https://pubs.opengroup.org/onlinepubs/009604499/functions/mkstemp.html
        libc::mkstemp(template_pointer as *mut i8)
    };
    if file_descriptor < 0 {
        let e = errno::errno();
        return Err(crate::Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!(
                "Unable to create temporary file. template: {:?}, Error {}: {}",
                String::from_utf8_lossy(&template),
                e.0,
                e
            ),
        )));
    }
    // TODO: review safety and add proper comment.
    let result = unsafe { libc::close(file_descriptor) };
    if result < 0 {
        let e = errno::errno();
        return Err(crate::Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!(
                "Unable to close temporary file. template: {:?}, Error {}: {}",
                String::from_utf8_lossy(&template),
                e.0,
                e
            ),
        )));
    }
    Ok(template)
}

impl MacroImplementation for MkstempMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let buffer;
        (buffer, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        match mkstemp(buffer) {
            Ok(pathname) => stdout.write_all(&pathname)?,
            Err(error) => {
                write!(stderr, "Error evaluating `mkstemp` macro: {}", error)?;
                state.exit_error = true;
            }
        }
        Ok(state)
    }
}

/// Exit from the m4 utility. If the first argument is specified, it shall be the exit code. If no
/// argument is specified, the exit code shall be zero. It shall be an error to specify an argument
/// containing any non-numeric characters. If the first argument is zero or no argument is
/// specified, and an error has previously occurred (for example, a file operand that could not be
/// opened), it is unspecified whether the exit status is zero or non-zero.
struct M4exitMacro;

impl MacroImplementation for M4exitMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        if let Some(first_arg) = m.args.into_iter().next() {
            let buffer;
            (buffer, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
            let (_, exit_code) = nom::combinator::all_consuming(parse_index)(&buffer)?;
            let exit_code: i32 =
                i32::try_from(exit_code).map_err(|e| crate::Error::Parsing(e.to_string()))?;
            if exit_code == 0 && state.exit_error {
                return Err(crate::Error::Exit(1));
            } else {
                return Err(crate::Error::Exit(exit_code));
            }
        }

        if state.exit_error {
            return Err(crate::Error::Exit(1));
        } else {
            return Err(crate::Error::Exit(0));
        }
    }
}

/// The first argument shall be processed when `EOF` is reached. If the `m4wrap` macro is used multiple
/// times, the arguments specified shall be processed in the order in which the `m4wrap` macros were
/// processed. The behavior is unspecified if `m4wrap` is not immediately followed by a
/// `<left-parenthesis>`.
struct M4wrapMacro;

impl MacroImplementation for M4wrapMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        state.m4wrap.push(first_arg_text);
        Ok(state)
    }
}

/// The `syscmd` macro shall interpret its first argument as a shell command line. The defining text
/// shall be the string result of that command. The string result shall not be rescanned for macros
/// while setting the defining text. No output redirection shall be performed by the m4 utility. The
/// exit status value from the command can be retrieved using the sysval macro. The behavior is
/// unspecified if syscmd is not immediately followed by a `<left-parenthesis>`.
struct SyscmdMacro;

fn system(command: &[u8]) -> Result<ExitStatus> {
    let command = OsStr::from_bytes(command);
    // TODO(security): check security of this, for shell injection? It seems to be what the GNU m4
    // does in https://github.com/tar-mirror/gnu-m4/blob/master/src/builtin.c#L953
    let status = std::process::Command::new("sh")
        .arg("-c")
        .arg(command)
        .status()?;

    Ok(status)
}

impl MacroImplementation for SyscmdMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let first_arg = m
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::NotEnoughArguments)?;
        let first_arg_text;
        (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
        let status = system(&first_arg_text)?;
        state.last_syscmd_status = Some(status);
        Ok(state)
    }
}

/// The defining text of the `sysval` macro shall be the exit value of the utility last invoked by the
/// [`SyscmdMacro`] (as a string).
struct SysvalMacro;

impl MacroImplementation for SysvalMacro {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        _m: Macro,
    ) -> Result<State> {
        if let Some(status) = state.last_syscmd_status {
            match status.code() {
                Some(code) => write!(stdout, "{code}")?,
                None => write!(
                    stderr,
                    "Last syscmd exited without exit code (process terminated by signal)"
                )?,
            }
        }
        Ok(state)
    }
}

/// The m4 utility maintains nine temporary buffers, numbered 1 to 9, inclusive. When the last of
/// the input has been processed, any output that has been placed in these buffers shall be written
/// to standard output in buffer-numerical order. The `divert` macro shall divert future output to the
/// buffer specified by its argument. Specifying no argument or an argument of 0 shall resume the
/// normal output process. Output diverted to a stream with a negative number shall be discarded.
/// Behavior is implementation-defined if a stream number larger than 9 is specified. It shall be an
/// error to specify an argument containing any non-numeric characters.
struct DivertMacro;

impl MacroImplementation for DivertMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let divert_number = if let Some(first_arg) = m.args.into_iter().next() {
            let first_arg_text;
            (first_arg_text, state) = evaluate_to_buffer(state, first_arg.symbols, stderr, true)?;
            if first_arg_text.is_empty() {
                write!(
                    stderr,
                    "WARNING: treating empty first argument of `divert` macro as 0"
                )?;
                0
            } else {
                let (_, divert_number) =
                    nom::combinator::all_consuming(parse_index)(&first_arg_text)?;
                if divert_number > 9 {
                    return Err(crate::Error::Parsing(format!(
                        "Error parsing first argument for `divert` macro with value {divert_number}, should be between 0 and 9"
                    )));
                }
                divert_number
            }
        } else {
            0
        };
        state.divert_number = divert_number;
        Ok(state)
    }
}

/// The defining text of the divnum macro shall be the number of the current output stream as a string.
struct DivnumMacro;

impl MacroImplementation for DivnumMacro {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        _stderr: &mut dyn Write,
        _m: Macro,
    ) -> Result<State> {
        write!(stdout, "{}", state.divert_number)?;
        Ok(state)
    }
}

/// The undivert macro shall cause immediate output of any text in temporary buffers named as
/// arguments, or all temporary buffers if no arguments are specified. Buffers can be undiverted
/// into other temporary buffers. Undiverting shall discard the contents of the temporary buffer.
/// The behavior is unspecified if an argument contains any non-numeric characters.
struct UndivertMacro;

impl MacroImplementation for UndivertMacro {
    fn evaluate(
        &self,
        mut state: State,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        m: Macro,
    ) -> Result<State> {
        let undivert_buffers: Vec<usize> = if m.args.is_empty() {
            (1..=9).into_iter().collect()
        } else {
            let mut undivert_buffers: Vec<usize> = Vec::new();
            for arg in m.args.into_iter() {
                let arg_text;
                (arg_text, state) = evaluate_to_buffer(state, arg.symbols, stderr, true)?;
                let (_, buffer_number) = nom::combinator::all_consuming(parse_index)(&arg_text)?;
                if buffer_number < 1 || buffer_number > 9 {
                    return Err(crate::Error::Parsing(format!(
                        "Unexpected buffer number: {buffer_number}. Needs to be from 1 to 9"
                    )));
                }
                undivert_buffers.push(buffer_number);
            }

            undivert_buffers
        };

        for buffer_number in undivert_buffers {
            let mut b = state.divert_buffers[buffer_number - 1].0.borrow_mut();
            stdout.write_all(&b)?;
            b.clear();
        }
        Ok(state)
    }
}

struct FileMacro;

impl MacroImplementation for FileMacro {
    fn evaluate(
        &self,
        state: State,
        stdout: &mut dyn Write,
        _stderr: &mut dyn Write,
        _m: Macro,
    ) -> Result<State> {
        if state.file.is_empty() {
            stdout.write_all(b"stdin")?;
        } else {
            stdout.write_all(
                state
                    .file
                    .last()
                    .expect("At least one file")
                    .as_os_str()
                    .as_bytes(),
            )?;
        }
        Ok(state)
    }
}

fn evaluate_to_buffer(
    mut state: State,
    symbols: Vec<Symbol>,
    stderr: &mut dyn Write,
    unwrap_quotes: bool,
) -> Result<(Vec<u8>, State)> {
    let mut buffer = Vec::new();
    for symbol in symbols {
        state = evaluate(state, symbol, &mut buffer, stderr, unwrap_quotes)?;
    }
    Ok((buffer, state))
}

pub(crate) trait Evaluator {
    fn evaluate(
        &self,
        state: State,
        symbol: Symbol,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        unwrap_quotes: bool,
    ) -> Result<State>;
}

impl<T> Evaluator for T
where
    T: Fn(State, Symbol, &mut dyn Write, &mut dyn Write, bool) -> Result<State>,
{
    fn evaluate(
        &self,
        state: State,
        symbol: Symbol,
        stdout: &mut dyn Write,
        stderr: &mut dyn Write,
        unwrap_quotes: bool,
    ) -> Result<State> {
        self(state, symbol, stdout, stderr, unwrap_quotes)
    }
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
    stderr: &mut dyn Write,
    unwrap_quotes: bool,
) -> Result<State> {
    let symbol_debug = format!("{symbol:?}");
    log::debug!("evaluate() EVALUATING {symbol_debug}");
    let mut root_symbol = Some(symbol);
    // We should never be evaluating symbols when dnl is enabled
    debug_assert!(!state.parse_config.dnl);

    let mut first = true;
    let mut buffer = Vec::new();
    let mut last = false;
    loop {
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

        // TODO(performance): if this is the last we could write directly to stdout
        for symbol in symbols {
            match symbol {
                Symbol::Comment(comment) => new_buffer.write_all(comment)?,
                Symbol::Text(text) => new_buffer.write_all(text)?,
                Symbol::Quoted(quoted) => {
                    log::debug!("evaluate() writing quoted {quoted:?}");
                    if last && unwrap_quotes {
                        new_buffer.write_all(quoted.contents)?;
                    } else {
                        new_buffer.write_all(quoted.all)?;
                    }
                }
                Symbol::Macro(mut m) => {
                    let mut arg_buffer: Vec<u8> = Vec::new();
                    if !m.args.is_empty() {
                        // Here we need to expand the macros which may form additional arguments which
                        // were not picked up in the initial round of parsing.
                        let args_length = m.args.len();
                        for (i, arg) in m.args.into_iter().enumerate() {
                            for symbol in arg.symbols {
                                // Important that we don't evaluate quotes.
                                // x(y)
                                //   ^ should be evaluated
                                // x(`a,b`)
                                //   ^^^^^ should not be evaluated
                                state = evaluate(state, symbol, &mut arg_buffer, stderr, false)?;
                            }
                            if i < args_length - 1 {
                                arg_buffer.push(b',');
                            }
                        }
                        log::debug!(
                            "evaluate() arg_buffer {:?}",
                            String::from_utf8_lossy(&arg_buffer)
                        );
                        // Necessary for the streaming args parser to terminate.
                        arg_buffer.push(b')');
                        let (remaining, args) =
                            lexer::parse_macro_args(&state.parse_config)(&arg_buffer)?;
                        debug_assert_eq!(remaining, b")");
                        log::debug!("evaluate() updated args {args:?}");
                        m.args = args;
                    }

                    let name = m.name.to_string();
                    log::debug!("evaluate() evaluating macro {name:?}");
                    let definition = state
                        .macro_definitions
                        .get(&m.name)
                        .and_then(|e| e.last().cloned())
                        .expect("There should always be a definition for a parsed macro");
                    state =
                        definition
                            .implementation
                            .evaluate(state, &mut new_buffer, stderr, m)?;
                    log::debug!(
                        "evaluate() finished evaluating macro {name:?}, new_buffer: {:?}",
                        String::from_utf8_lossy(&new_buffer)
                    );
                }
                Symbol::Newline => new_buffer.write_all(b"\n")?,
                Symbol::Eof => {}
            }
        }
        if last {
            log::debug!(
                "evaluate() FINISHED {symbol_debug}: new_buffer: {:?}",
                String::from_utf8_lossy(&new_buffer)
            );
            stdout.write_all(&mut new_buffer)?;
            break;
        }
        first = false;
        buffer = new_buffer;
    }

    Ok(state)
}
