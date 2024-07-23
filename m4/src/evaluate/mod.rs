use std::cell::RefCell;
use std::ffi::{OsStr, OsString};
use std::io::Read;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::process::ExitStatus;
use std::{collections::HashMap, io::Write, rc::Rc};

use nom::error::{ContextError, FromExternalError};
use nom::IResult;
use output::{DivertBufferNumber, Output, OutputRef};

use crate::error::{Error, ErrorKind, Result, ResultExt};
use crate::eval_macro::{self, parse_integer};
use crate::lexer::{
    is_alpha, is_alphnumeric, is_space, MacroName, MacroParseConfig, ParseConfig,
    DEFAULT_QUOTE_CLOSE_TAG, DEFAULT_QUOTE_OPEN_TAG,
};
use crate::EOF;

mod output;

const AT_LEAST_ONE_MACRO_DEFINITION_EXPECT: &str =
    "There should always be at least one macro definition";

#[derive(Default)]
pub struct Trace {
    all: bool,
    exclude: Vec<MacroName>,
    include: Vec<MacroName>,
}

impl Trace {
    fn trace(
        &self,
        stack: &[StackFrame],
        current_frame: &StackFrame,
        stderr: &mut dyn Write,
    ) -> crate::Result<()> {
        let name = &current_frame.definition.parse_config.name;
        let level = stack.len() + 1;
        if (self.all && !self.exclude.contains(name)) || (!self.all && self.include.contains(name))
        {
            writeln!(stderr, "m4trace: -{level}- {name}")?;
        }
        Ok(())
    }
}

pub struct State {
    pub macro_definitions: HashMap<MacroName, Vec<Rc<MacroDefinition>>>,
    pub parse_config: ParseConfig,
    /// Whether the process should exit with an error once processing has completed.
    pub exit_error: bool,
    /// See [`M4wrapMacro`].
    pub m4wrap: Vec<Vec<u8>>,
    pub last_syscmd_status: Option<ExitStatus>,
    pub output: OutputState,
    pub input: InputStateRef,
    pub trace: Trace,
}

#[derive(Default)]
pub struct OutputState {
    pub output: OutputRef,
    pub stack: Vec<StackFrame>,
}

impl OutputState {
    /// Write either to output, or to the buffer for the macro arg currently being parsed.
    pub fn write_all(&mut self, buf: &[u8]) -> crate::Result<()> {
        if self.stack.is_empty() {
            log::trace!("Writing to output: {}", String::from_utf8_lossy(buf));
            self.output.write_all(buf)?;
        } else {
            log::trace!(
                "Writing to macro arg in stack: {:?}",
                String::from_utf8_lossy(buf)
            );

            let frame = self.stack.last_mut().expect("Stack not empty");
            let arg_buffer = if let Some(arg_buffer) = frame.args.last_mut() {
                arg_buffer
            } else {
                frame.args.push(Vec::new());
                frame.args.last_mut().expect("At least one arg")
            };

            arg_buffer.extend(buf);
        }
        Ok(())
    }
}

impl State {
    pub fn try_new(
        stdout: Rc<RefCell<dyn Write>>,
        input: Vec<Input>,
        line_synchronization: bool,
    ) -> crate::Result<Self> {
        let input_state = InputStateRef::new(InputState::new(line_synchronization));
        for i in input {
            input_state.input_push(i, &mut *stdout.borrow_mut())?;
        }
        Ok(Self {
            output: OutputState {
                output: Output::new(stdout, input_state.clone()).into_ref(),
                ..OutputState::default()
            },
            input: input_state,
            ..Self::default()
        })
    }
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
                .iter()
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
            output: OutputState::default(),
            input: InputStateRef::default(),
            trace: Trace::default(),
        }
    }
}

pub struct StackFrame {
    parenthesis_level: usize,
    args: Vec<Vec<u8>>,
    definition: Rc<MacroDefinition>,
}

impl StackFrame {
    pub fn new(parenthesis_level: usize, definition: Rc<MacroDefinition>) -> Self {
        Self {
            parenthesis_level,
            args: Vec::new(),
            definition,
        }
    }
}

#[derive(Default)]
pub struct InputState {
    line_synchronization: bool,
    input: Vec<Input>,
}

impl InputState {
    pub fn new(line_synchronization: bool) -> Self {
        Self {
            line_synchronization,
            input: Vec::new(),
        }
    }

    pub fn input_push(
        &mut self,
        mut input: Input,
        syncline_output: &mut dyn Write,
    ) -> std::io::Result<()> {
        if self.line_synchronization {
            input.emit_syncline(syncline_output, false)?;
        }

        self.input.push(input);

        Ok(())
    }

    pub fn input_pop(&mut self) -> Option<Input> {
        self.input.pop()
    }

    /// Get the next character to be parsed. First it tries to get one from the pushback buffer,
    /// otherwise it gets one from the input file.
    pub fn get_next_character(&mut self) -> crate::error::Result<u8> {
        let input = self.input.last_mut().unwrap();
        if let Some(c) = input.pushback_buffer.pop() {
            return Ok(c);
        }
        Ok(input.get_next_character()?)
    }

    pub fn pushback_character(&mut self, c: u8) {
        self.input.last_mut().unwrap().pushback_buffer.push(c);
    }

    pub fn pushback_string(&mut self, s: &[u8]) {
        let pushback_buffer = &mut self.input.last_mut().unwrap().pushback_buffer;
        for c in s.iter().rev() {
            pushback_buffer.push(*c);
        }
    }

    /// Fetch new characters attempting to match them all to token. If any character doesn't match
    /// the token, then all the characters previously fetched are placed onto the pushback buffer.
    ///
    /// **NOTE:** Will panic if token is not length of at least 1.
    ///
    /// * `c` - First character of input which is already available.
    /// * `token` - Token to match against.
    pub fn look_ahead(&mut self, mut c: u8, token: &[u8]) -> crate::error::Result<bool> {
        if c == EOF || c != token[0] {
            return Ok(false);
        }

        let mut i = 1;
        while i < token.len() {
            c = self.get_next_character()?;
            if c == EOF || c != token[i] {
                loop {
                    self.pushback_character(token[i]);
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
                return Ok(false);
            }

            i += 1;
        }

        Ok(true)
    }

    fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        self.input
            .last_mut()
            .expect("at least one input")
            .emit_syncline(output, check_line_numbers)
    }
}

#[derive(Clone, Default)]
pub struct InputStateRef(Rc<RefCell<InputState>>);

impl InputStateRef {
    pub fn new(input_state: InputState) -> Self {
        Self(Rc::new(RefCell::new(input_state)))
    }

    pub fn input_pop(&self) -> Option<Input> {
        self.0.borrow_mut().input_pop()
    }

    pub fn input_push(&self, input: Input, syncline_output: &mut dyn Write) -> std::io::Result<()> {
        self.0.borrow_mut().input_push(input, syncline_output)
    }

    pub fn input_len(&self) -> usize {
        self.0.borrow().input.len()
    }

    pub fn get_next_character(&self) -> crate::error::Result<u8> {
        self.0.borrow_mut().get_next_character()
    }

    pub fn pushback_character(&self, c: u8) {
        self.0.borrow_mut().pushback_character(c)
    }

    pub fn pushback_string(&self, s: &[u8]) {
        self.0.borrow_mut().pushback_string(s)
    }

    pub fn look_ahead(&self, c: u8, token: &[u8]) -> crate::error::Result<bool> {
        self.0.borrow_mut().look_ahead(c, token)
    }

    fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        self.0
            .borrow_mut()
            .emit_syncline(output, check_line_numbers)
    }

    fn sync_lines(&self) -> bool {
        self.0.borrow().line_synchronization
    }
}

pub struct Input {
    input: InputRead,
    pub pushback_buffer: Vec<u8>,
    pub line_number: usize,
    pub syncline_line_number: usize,
}

impl Input {
    pub fn new(input: InputRead) -> Self {
        Self {
            input,
            pushback_buffer: Vec::new(),
            line_number: 1,
            syncline_line_number: 0,
        }
    }

    fn get_next_character(&mut self) -> std::io::Result<u8> {
        let mut buf: [u8; 1] = [0; 1];
        let n = match &mut self.input {
            InputRead::File { file, .. } => file.read(&mut buf),
            InputRead::Stdin(s) => s.read(&mut buf),
        }?;

        if n == 0 {
            return Ok(EOF);
        }

        let c = buf[0];

        if c == b'\n' {
            self.line_number += 1;
        }

        Ok(c)
    }

    fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        let name = match &self.input {
            InputRead::File { path, .. } => path.as_os_str().as_encoded_bytes(),
            InputRead::Stdin(_) => b"stdin",
        };

        log::debug!(
            "Input::emit_syncline(): {} {check_line_numbers}",
            String::from_utf8_lossy(name)
        );
        if check_line_numbers {
            log::debug!(
                "Input::emit_syncline(): syncline_line_number:{},line_number:{}",
                self.syncline_line_number,
                self.line_number
            );
            self.syncline_line_number += 1;
            if self.syncline_line_number == self.line_number {
                return Ok(());
            }
        }

        output.write_all(b"#line ")?;
        write!(output, "{}", self.line_number)?;
        output.write_all(b" \"")?;
        output.write_all(&name)?;
        output.write_all(b"\"\n")?;

        self.syncline_line_number = self.line_number;
        Ok(())
    }
}

#[derive(Debug)]
pub enum InputRead {
    File { file: std::fs::File, path: PathBuf },
    Stdin(std::io::Stdin),
}

/// [`Symbol`] writing output to `writer`, and returns a [`ParseConfig`] which has been modified
/// during the process of evaluation (for example a new macro was defined, or changequote).
///
/// Arguments:
/// - `unwrap_quotes` - Whether to unwrap quotes during evaluation.
/// - `all_inputs` - Whether to process all the inputs in `state.input` (`true`), or just the top of
///   the stack (`false`).
pub(crate) fn process_streaming(
    mut state: State,
    stderr: &mut dyn Write,
) -> crate::error::Result<State> {
    let mut token: Vec<u8> = Vec::new();

    // TODO: rename these to something sensible.
    let mut l: u8 = 0;
    let mut t: u8;

    'main_loop: loop {
        t = state.input.get_next_character()?;
        // Strip quotes
        if state
            .input
            .look_ahead(t, &state.parse_config.quote_open_tag)?
        {
            log::trace!("Stripping quotes");
            let mut quotation_level: usize = 1;

            'inside_quote: loop {
                l = state.input.get_next_character()?;
                if state
                    .input
                    .look_ahead(l, &state.parse_config.quote_close_tag)?
                {
                    quotation_level -= 1;
                    if quotation_level > 0 {
                        // Encountered closing quote within the quote, so we output it.
                        state
                            .output
                            .write_all(&state.parse_config.quote_close_tag)?;
                    }
                } else if state
                    .input
                    .look_ahead(l, &state.parse_config.quote_open_tag)?
                {
                    quotation_level += 1;
                    state.output.write_all(&state.parse_config.quote_open_tag)?;
                } else if l == EOF {
                    return Err(crate::Error::new(crate::ErrorKind::UnclosedQuote));
                } else if quotation_level > 0 {
                    log::trace!(
                        "Writing quoted content to output: {:?}",
                        String::from_utf8_lossy(&[l])
                    );
                    state.output.write_all(&[l])?;
                }

                if quotation_level == 0 {
                    log::trace!("Finished stripping quotes");
                    break 'inside_quote;
                }
            }
        } else if state.output.stack.is_empty() // Parse comments
            && state.parse_config.comment_enabled && state
                .input
                .look_ahead(t, &state.parse_config.comment_open_tag)?
        {
            state
                .output
                .write_all(&state.parse_config.comment_open_tag)?;

            'inside_comment: loop {
                t = state.input.get_next_character()?;
                if state
                    .input
                    .look_ahead(t, &state.parse_config.comment_close_tag)?
                {
                    state
                        .output
                        .write_all(&state.parse_config.comment_close_tag)?;
                    break 'inside_comment;
                }
                if t == EOF {
                    break 'inside_comment;
                }
                state.output.write_all(&[t])?;
            }
        } else if t == b'_' || is_alpha(t) {
            // Possibly a macro to be evaluated.
            let definition = parse_macro(&mut state, t, &mut token)?;
            if definition.is_some() {
                l = state.input.get_next_character()?;
                state.input.pushback_character(l);
            }

            // Check to see whether it's currently defined macro or it needs some arguments but
            // there's no open bracket.
            if definition.is_none()
                || (l != b'(' && (definition.as_ref().unwrap().parse_config.min_args > 0))
            {
                state.output.write_all(&token)?;
            } else {
                let definition = definition.unwrap();

                let frame = StackFrame::new(0, definition.clone());

                if l == b'(' {
                    state.output.stack.push(frame);
                } else {
                    state = definition.implementation.evaluate(state, stderr, frame)?;
                }
            }
        } else if t == EOF {
            if state.input.input_len() == 1 {
                if !state.output.stack.is_empty() {
                    return Err(Error::new(ErrorKind::UnclosedParenthesis));
                }
                break 'main_loop;
            }
            state.input.input_pop();
            log::debug!("EOF synclines");
            if state.input.sync_lines() {
                state
                    .input
                    .emit_syncline(&mut *state.output.output.stdout().borrow_mut(), false)?;
            }
            continue 'main_loop;
        } else if state.output.stack.is_empty() {
            // not in a macro
            state.output.write_all(&[t])?;
        } else {
            match t {
                b'(' => {
                    if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                        state.output.write_all(&[t])?;
                    }
                    'skip_whitespace: loop {
                        l = state.input.get_next_character()?;
                        if is_space(l) {
                            if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                                state.output.write_all(&[l])?;
                            }
                        } else {
                            break 'skip_whitespace;
                        }
                    }
                    state.input.pushback_character(l);
                    state.output.stack.last_mut().unwrap().parenthesis_level += 1;
                }
                b')' => {
                    state.output.stack.last_mut().unwrap().parenthesis_level -= 1;
                    if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                        state.output.write_all(&[t])?;
                    } else {
                        // end of argument list
                        let frame = state.output.stack.pop().unwrap();
                        state = frame
                            .definition
                            .clone()
                            .implementation
                            .evaluate(state, stderr, frame)?;
                    }
                }
                b',' => {
                    if state.output.stack.last().unwrap().parenthesis_level == 1 {
                        // Skip spaces after comma
                        loop {
                            l = state.input.get_next_character()?;
                            if !is_space(l) {
                                break;
                            }
                        }
                        state.input.pushback_character(l);
                        let args = &mut state.output.stack.last_mut().unwrap().args;
                        if args.is_empty() {
                            args.push(Vec::new());
                        }
                        args.push(Vec::new());
                    } else {
                        state.output.write_all(&[t])?;
                    }
                }
                _ => {
                    // Output comment
                    if state
                        .input
                        .look_ahead(t, &state.parse_config.comment_open_tag)?
                    {
                        state
                            .output
                            .write_all(&state.parse_config.comment_open_tag)?;
                        'comment: loop {
                            t = state.input.get_next_character()?;
                            if t == EOF {
                                // TODO: should this break the main loop instead? What happens with multiple inputs?
                                break 'comment;
                            }
                            if state
                                .input
                                .look_ahead(t, &state.parse_config.comment_close_tag)?
                            {
                                state
                                    .output
                                    .write_all(&state.parse_config.comment_close_tag)?;
                                break 'comment;
                            }
                            state.output.write_all(&[t])?;
                        }
                    } else {
                        state.output.write_all(&[t])?;
                    }
                }
            }
        }
        // TODO
    }

    state.output.output.divert(0)?;
    state.output.output.undivert_all()?;

    for wrap in &state.m4wrap {
        state.output.write_all(wrap)?;
    }

    Ok(state)
}

/// Attempt to parse `state.input` as a macro name, into `token`. If it is a current macro name in
/// `state.macro_definitions`, then it will return `Some` of [`MacroDefinition`].
fn parse_macro(
    state: &mut State,
    mut c: u8,
    token: &mut Vec<u8>,
) -> crate::Result<Option<Rc<MacroDefinition>>> {
    token.clear();
    token.push(c);

    loop {
        c = state.input.get_next_character()?;
        if !(is_alphnumeric(c) || c == b'_') {
            break;
        }
        token.push(c)
    }
    if c != EOF {
        state.input.pushback_character(c);
    }

    Ok(state
        .macro_definitions
        .get(&MacroName::try_from_slice(token).expect("valid macro name"))
        .map(|v| v.last())
        .unwrap_or_default()
        .cloned())
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
                stderr: &mut dyn Write,
                f: StackFrame,
            ) -> Result<State> {
                state.trace.trace(&state.output.stack, &f, stderr)?;
                match self {
                    $(Self::$variant_name(d) => d.evaluate(state, stderr, f)),*,
                    Self::UserDefined(d) => d.evaluate(state, stderr, f),
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
        Changecom(ChangecomMacro),
        Changequote(ChangequoteMacro),
        Decr(DecrMacro),
        Define(DefineMacro),
        Defn(DefnMacro),
        Divert(DivertMacro),
        Divnum(DivnumMacro),
        Dnl(DnlMacro),
        Dumpdef(DumpdefMacro),
        Errprint(ErrprintMacro),
        Eval(EvalMacro),
        File(FileMacro),
        Ifdef(IfdefMacro),
        Ifelse(IfelseMacro),
        Include(IncludeMacro),
        Incr(IncrMacro),
        Index(IndexMacro),
        Len(LenMacro),
        M4exit(M4exitMacro),
        M4wrap(M4wrapMacro),
        Maketemp(MkstempMacro),
        Mkstemp(MkstempMacro),
        Popdef(PopdefMacro),
        Pushdef(PushdefMacro),
        Shift(ShiftMacro),
        Sinclude(SincludeMacro),
        Substr(SubstrMacro),
        Syscmd(SyscmdMacro),
        Sysval(SysvalMacro),
        Traceoff(TraceoffMacro),
        Traceon(TraceonMacro),
        Translit(TranslitMacro),
        Undefine(UndefineMacro),
        Undivert(UndivertMacro),
    }
);

impl AsRef<[u8]> for BuiltinMacro {
    fn as_ref(&self) -> &'static [u8] {
        use BuiltinMacro::*;
        match self {
            Changecom => b"changecom",
            Changequote => b"changequote",
            Decr => b"decr",
            Define => b"define",
            Defn => b"defn",
            Divert => b"divert",
            Divnum => b"divnum",
            Dnl => b"dnl",
            Dumpdef => b"dumpdef",
            Errprint => b"errprint",
            Eval => b"eval",
            File => b"__file__",
            Ifdef => b"ifdef",
            Ifelse => b"ifelse",
            Include => b"include",
            Incr => b"incr",
            Index => b"index",
            Len => b"len",
            M4exit => b"m4exit",
            M4wrap => b"m4wrap",
            Maketemp => b"maketemp",
            Mkstemp => b"mkstemp",
            Popdef => b"popdef",
            Pushdef => b"pushdef",
            Shift => b"shift",
            Sinclude => b"sinclude",
            Substr => b"substr",
            Syscmd => b"syscmd",
            Sysval => b"sysval",
            Traceoff => b"traceoff",
            Traceon => b"traceon",
            Translit => b"translit",
            Undefine => b"undefine",
            Undivert => b"undivert",
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
            Changecom => 0,
            Changequote => 0,
            Decr => 1,
            Define => 1,
            Defn => 1,
            Divert => 0,
            Divnum => 0,
            Dnl => 0,
            Dumpdef => 1,
            Errprint => 1,
            Eval => 1,
            File => 0,
            Ifdef => 1,
            Ifelse => 1,
            Include => 1,
            Incr => 1,
            Index => 1,
            Len => 1,
            M4exit => 0,
            M4wrap => 1,
            Maketemp => 1,
            Mkstemp => 1,
            Popdef => 1,
            Pushdef => 1,
            Shift => 1,
            Sinclude => 1,
            Substr => 1,
            Syscmd => 1,
            Sysval => 0,
            Traceoff => 0,
            Traceon => 0,
            Translit => 1,
            Undefine => 1,
            Undivert => 0,
        }
    }

    pub fn parse_config(&self) -> MacroParseConfig {
        MacroParseConfig {
            name: self.name(),
            min_args: self.min_args(),
        }
    }
}

pub(crate) struct MacroDefinition {
    pub parse_config: MacroParseConfig,
    // TODO: improve performance with enum dispatch
    implementation: MacroDefinitionImplementation,
}

impl MacroDefinition {
    pub fn new_user_defined(name: MacroName, definition: Vec<u8>) -> Self {
        Self {
            parse_config: MacroParseConfig { name, min_args: 0 },
            implementation: MacroDefinitionImplementation::UserDefined(UserDefinedMacro {
                definition,
            }),
        }
    }
}

impl std::fmt::Debug for MacroDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MacroDefinition")
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

trait MacroImplementation {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State>;
}

/// The dnl macro shall cause m4 to discard all input characters up to and including the next
/// `<newline>`.
struct DnlMacro;

impl MacroImplementation for DnlMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _f: StackFrame) -> Result<State> {
        let mut c: u8;
        loop {
            c = state.input.get_next_character()?;
            if c == b'\n' || c == EOF {
                break;
            }
        }
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
        state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<(State, Option<MacroDefinition>)> {
        let mut args = frame.args.into_iter();
        let name = if let Some(name_bytes) = args.next() {
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
        let definition = if let Some(definition) = args.next() {
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
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderr, frame)?;
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
        Ok(state)
    }
}

/// The pushdef macro shall be equivalent to the define macro with the exception that it shall
/// preserve any current definition for future retrieval using the popdef macro. The behavior is
/// unspecified if pushdef is not immediately followed by a `<left-parenthesis>`.
struct PushdefMacro;

impl MacroImplementation for PushdefMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let (mut state, definition) = DefineMacro::define(state, stderr, frame)?;
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
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if let Some(arg) = frame.args.into_iter().next() {
            if let Ok(name) = MacroName::try_from_slice(&arg) {
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
                }
            }
        }
        Ok(state)
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
pub struct UserDefinedMacro {
    pub definition: Vec<u8>,
}

impl MacroImplementation for UserDefinedMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        log::debug!(
            "UserDefinedMacro::evaluate() evaluating {:?}: definition:{:?} args:{:?}",
            frame.definition.parse_config.name.to_string(),
            String::from_utf8_lossy(&self.definition),
            frame
                .args
                .iter()
                .map(|a| String::from_utf8_lossy(a))
                .collect::<Vec<_>>(),
        );

        if self.definition.is_empty() {
            return Ok(state);
        }

        let mut i = self.definition.len() - 1;
        loop {
            if i == 0 || self.definition[i - 1] != b'$' {
                state.input.pushback_character(self.definition[i]);
            } else {
                let t = self.definition[i];
                match t {
                    b'#' => state
                        .input
                        .pushback_string(frame.args.len().to_string().as_bytes()),
                    b'0' => state
                        .input
                        .pushback_string(&frame.definition.parse_config.name.0),
                    b'1'..=b'9' => {
                        let arg_index = (t - b'1') as usize;
                        if arg_index < frame.args.len() {
                            state.input.pushback_string(&frame.args[arg_index]);
                        }
                    }
                    b'*' => {
                        for (arg_index, arg) in frame.args.iter().enumerate().rev() {
                            state.input.pushback_string(arg);
                            if arg_index > 0 {
                                state.input.pushback_character(b',');
                            }
                        }
                    }
                    b'@' => {
                        for (arg_index, arg) in frame.args.iter().enumerate().rev() {
                            state
                                .input
                                .pushback_string(&state.parse_config.quote_close_tag);
                            state.input.pushback_string(arg);
                            state
                                .input
                                .pushback_string(&state.parse_config.quote_open_tag);
                            if arg_index > 0 {
                                state.input.pushback_character(b',');
                            }
                        }
                    }
                    _ => {
                        state.input.pushback_character(t);
                        state.input.pushback_character(b'$');
                    }
                }
                // TODO: this might be able to skip an iteration with i==1?
                if i == 0 {
                    break;
                }
                i -= 1;
            }
            if i == 0 {
                break;
            }
            i -= 1;
        }

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
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if let Some(arg) = frame.args.into_iter().next() {
            if let Ok(name) = MacroName::try_from_slice(&arg) {
                state.macro_definitions.remove(&name);
            }
        }
        Ok(state)
    }
}

/// The defining text of the defn macro shall be the quoted definition (using the current quoting
/// strings) of its arguments. The behavior is unspecified if defn is not immediately followed by a
/// `<left-parenthesis>`.
struct DefnMacro;

impl MacroImplementation for DefnMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        if let Some(definitions) = state
            .macro_definitions
            .get(&MacroName::try_from_slice(&first_arg)?)
        {
            let definition = definitions
                .last()
                .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT);
            if let MacroDefinitionImplementation::UserDefined(definition) =
                &definition.implementation
            {
                state
                    .input
                    .pushback_string(&state.parse_config.quote_close_tag);
                state.input.pushback_string(&definition.definition);
                state
                    .input
                    .pushback_string(&state.parse_config.quote_open_tag);
            }
        }
        Ok(state)
    }
}

struct ErrprintMacro;

impl MacroImplementation for ErrprintMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let args_len = frame.args.len();
        for (i, arg) in frame.args.into_iter().enumerate() {
            stderr.write_all(&arg)?;
            if i < (args_len - 1) {
                stderr.write_all(b" ")?;
            }
        }
        Ok(state)
    }
}

/// The defining text for the include macro shall be the contents of the file named by the first
/// argument. It shall be an error if the file cannot be read. The behavior is unspecified if
/// include is not immediately followed by a `<left-parenthesis>`.
struct IncludeMacro;

impl IncludeMacro {
    fn get_file_path(frame: StackFrame, state: State) -> Result<(Option<PathBuf>, State)> {
        if let Some(arg) = frame.args.into_iter().next() {
            let path = PathBuf::from(OsString::from_vec(arg));
            Ok((Some(path), state))
        } else {
            Ok((None, state))
        }
    }

    fn include_impl(path: PathBuf, state: State) -> crate::error::Result<State> {
        let file = std::fs::File::open(&path)
            .map_err(crate::Error::from)
            .add_context(|| format!("Error opening file {path:?}"))?;
        state.input.input_push(
            Input::new(InputRead::File { file, path }),
            &mut *state.output.output.stdout().borrow_mut(),
        )?;
        Ok(state)
    }
}

impl MacroImplementation for IncludeMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let path;
        (path, state) = Self::get_file_path(frame, state)?;
        if let Some(path) = path {
            state = Self::include_impl(path, state)?;
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
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let path;
        (path, state) = IncludeMacro::get_file_path(frame, state)?;
        if let Some(path) = path {
            if path.is_file() {
                state = IncludeMacro::include_impl(path, state)?;
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
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let args_len = frame.args.len();

        if args_len == 0 {
            state.parse_config.comment_enabled = false;
            log::trace!("ChangecomMacro::evaluate() reset to default");
            return Ok(state);
        }

        let mut args = frame.args.into_iter();
        let open_tag = args.next().expect("1 argument should be present");
        if !open_tag.is_empty() {
            log::trace!(
                "ChangecomMacro::evaluate() comment_open_tag set to {:?}",
                String::from_utf8_lossy(&open_tag)
            );
            state.parse_config.comment_enabled = true;
            state.parse_config.comment_open_tag = open_tag;
        }

        if args_len >= 2 {
            let close_tag = args.next().expect("2 arguments should be present");
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
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        match frame.args.len() {
            0 => {
                DEFAULT_QUOTE_OPEN_TAG.clone_into(&mut state.parse_config.quote_open_tag);
                DEFAULT_QUOTE_CLOSE_TAG.clone_into(&mut state.parse_config.quote_close_tag);
            }
            1 => {}
            args_len @ 2.. => {
                if args_len > 2 {
                    stderr
                        .write_all(b"Warning: excess arguments to builtin `changequote' ignored")?;
                }
                let mut args = frame.args.into_iter();
                let open_tag = args.next().expect("2 arguments should be present");
                let close_tag = args.next().expect("2 arguments should be present");
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
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        log::debug!("IncrMacro::evaluate() {}", state.debug_macro_definitions());
        if let Some(first) = frame.args.into_iter().next() {
            let (remaining, mut number) = eval_macro::padded(eval_macro::parse_integer)(&first)?;
            if !remaining.is_empty() {
                return Err(
                    crate::Error::new(crate::ErrorKind::Parsing).add_context(format!(
                        "Error parsing number from {first:?}, remaining input: {remaining:?}"
                    )),
                );
            }
            number += 1;
            state.input.pushback_string(number.to_string().as_bytes());
        }
        Ok(state)
    }
}

/// The defining text of the decr macro shall be its first argument decremented by 1. It shall be
/// an error to specify an argument containing any non-numeric characters. The behavior is
/// unspecified if decr is not immediately followed by a `<left-parenthesis>`.
struct DecrMacro;

impl MacroImplementation for DecrMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        log::debug!("DecrMacro::evaluate() {}", state.debug_macro_definitions());
        if let Some(first) = frame.args.into_iter().next() {
            let (remaining, mut number) = eval_macro::padded(eval_macro::parse_integer)(&first)?;
            if !remaining.is_empty() {
                return Err(
                    crate::Error::new(crate::ErrorKind::Parsing).add_context(format!(
                        "Error parsing number from {first:?}, remaining input: {remaining:?}"
                    )),
                );
            }
            number -= 1;
            state.input.pushback_string(number.to_string().as_bytes());
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
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args_len = frame.args.len();
        if args_len < 3 {
            write!(stderr, "Too few arguments to builtin `ifelse'")?;
            return Ok(state);
        }

        let mut args = frame.args.into_iter();
        let mut i = 0;
        loop {
            // TODO: there's a bug here, when we originally parsed this as a set of macro args we
            // didn't evaluate it as we parsed it.
            let arg_0 = args.next().expect("at least 3 args");
            let arg_1 = args.next().expect("at least 3 args");
            if arg_0 == arg_1 {
                log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 2);
                let arg = args.next().expect("at least 3 args");
                state.input.pushback_string(&arg);
                return Ok(state);
            } else {
                match args_len {
                    0..=2 => panic!("at least 3 args"),
                    3 => return Ok(state),
                    4 | 5 => {
                        args.next();
                        log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 3);
                        let arg = args.next().expect("at least 4 args");
                        state.input.pushback_string(&arg);
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
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let second_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let name = MacroName::try_from_slice(&first_arg).ok();
        if name
            .map(|name| state.macro_definitions.contains_key(&name))
            .unwrap_or(false)
        {
            state.input.pushback_string(&second_arg);
        } else if let Some(third_arg) = args.next() {
            state.input.pushback_string(&third_arg);
        }
        log::debug!("IfdefMacro::evaluate() finished");
        Ok(state)
    }
}

/// The defining text for the shift macro shall be a comma-separated list of its arguments except
/// the first one. Each argument shall be quoted using the current quoting strings. The behavior is
/// unspecified if shift is not immediately followed by a `<left-parenthesis>`.
struct ShiftMacro;

impl MacroImplementation for ShiftMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        if frame.args.len() > 1 {
            let args = &frame.args[1..];
            for (i, arg) in args.iter().enumerate().rev() {
                state.input.pushback_string(arg);
                if i != 0 {
                    state.input.pushback_character(b',');
                }
            }
        }
        Ok(state)
    }
}

struct EvalMacro;

impl MacroImplementation for EvalMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        let (_, output) = nom::combinator::all_consuming(nom::combinator::complete(
            eval_macro::parse_and_evaluate,
        ))(&first_arg)?;
        state.input.pushback_string(output.to_string().as_bytes());
        Ok(state)
    }
}

/// The defining text of the len macro shall be the length (as a string) of the first argument. The
/// behavior is unspecified if len is not immediately followed by a `<left-parenthesis>`.
struct LenMacro;

impl MacroImplementation for LenMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        state
            .input
            .pushback_string(first_arg.len().to_string().as_bytes());
        Ok(state)
    }
}

/// The defining text of the index macro shall be the first character position (as a string) in the
/// first argument where a string matching the second argument begins (zero origin), or -1 if the
/// second argument does not occur. The behavior is unspecified if index is not immediately followed
/// by a `<left-parenthesis>`.
struct IndexMacro;

impl MacroImplementation for IndexMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            None => {
                stderr.write_all(b"Warning too few arguments for index macro")?;
                state.input.pushback_character(b'0');
                return Ok(state);
            }
        };

        let index = first_arg
            .windows(second_arg.len())
            .position(|window| window == second_arg);
        match index {
            Some(index) => {
                state.input.pushback_string(index.to_string().as_bytes());
            }
            None => {
                state.input.pushback_string(b"-1");
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
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let mut output_buffer = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        log::debug!(
            "TranslitMacro::evaluate() transliterating {:?}",
            String::from_utf8_lossy(&output_buffer)
        );

        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            None => {
                stderr.write_all(b"Warning too few arguments for index macro")?;
                state.input.pushback_character(b'0');
                return Ok(state);
            }
        };

        let third_arg = args.next().unwrap_or_default();

        for (i, source_char) in second_arg.into_iter().enumerate() {
            if let Some(replacement_char) = third_arg.get(i) {
                for current_char in output_buffer.iter_mut() {
                    if *current_char == source_char {
                        *current_char = *replacement_char;
                    }
                }
            } else {
                output_buffer.retain(|current_char| *current_char != source_char);
            }
        }

        state.input.pushback_string(&output_buffer);
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
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        if first_arg.is_empty() {
            return Ok(state);
        }

        let start_index = if let Some(second_arg) = args.next() {
            let (_, i) = nom::combinator::all_consuming(parse_index)(&second_arg)?;
            i
        } else {
            0
        };

        if start_index > (first_arg.len() - 1) {
            return Ok(state);
        }

        let out = if let Some(third_arg) = args.next() {
            let (_, number_of_chars) = nom::combinator::all_consuming(parse_index)(&third_arg)?;

            if number_of_chars > 0 {
                let end_index = usize::min(start_index + number_of_chars, first_arg.len());
                &first_arg[start_index..end_index]
            } else {
                return Ok(state);
            }
        } else {
            &first_arg[start_index..]
        };

        state.input.pushback_string(out);
        Ok(state)
    }
}

/// The dumpdef macro shall write the defined text to standard error for each of the macros
/// specified as arguments, or, if no arguments are specified, for all macros.
struct DumpdefMacro;

impl MacroImplementation for DumpdefMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
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
        if frame.args.is_empty() {
            for (name, definitions) in state.macro_definitions.iter() {
                dumpdef(
                    stderr,
                    name,
                    definitions
                        .last()
                        .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT),
                )?;
                stderr.write_all(b"\n")?;
            }
        }

        for arg in frame.args.into_iter() {
            let name = MacroName::try_from_slice(&arg)?;
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
        return Err(crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Unable to create temporary file, template should be greater than 6 characters long. template: {:?}", String::from_utf8_lossy(&template))
        )));
    }
    if &template[(template.len() - 6)..] != b"XXXXXX" {
        return Err(crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::new(
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
        return Err(
            crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "Unable to create temporary file. template: {:?}, Error {}: {}",
                    String::from_utf8_lossy(&template),
                    e.0,
                    e
                ),
            )),
        );
    }
    // TODO: review safety and add proper comment.
    let result = unsafe { libc::close(file_descriptor) };
    if result < 0 {
        let e = errno::errno();
        return Err(
            crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "Unable to close temporary file. template: {:?}, Error {}: {}",
                    String::from_utf8_lossy(&template),
                    e.0,
                    e
                ),
            )),
        );
    }
    Ok(template)
}

impl MacroImplementation for MkstempMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        match mkstemp(first_arg) {
            Ok(pathname) => state.input.pushback_string(&pathname),
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
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        if let Some(first_arg) = frame.args.into_iter().next() {
            let (_, exit_code) = nom::combinator::all_consuming(parse_index)(&first_arg)?;
            let exit_code: i32 = i32::try_from(exit_code).map_err(|e| {
                crate::Error::new(crate::ErrorKind::Parsing).add_context(e.to_string())
            })?;
            if exit_code == 0 && state.exit_error {
                return Err(crate::Error::new(crate::ErrorKind::Exit(1)));
            } else {
                return Err(crate::Error::new(crate::ErrorKind::Exit(exit_code)));
            }
        }

        if state.exit_error {
            Err(crate::Error::new(crate::ErrorKind::Exit(1)))
        } else {
            Err(crate::Error::new(crate::ErrorKind::Exit(0)))
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
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        state.m4wrap.push(first_arg);
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
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let status = system(&first_arg)?;
        state.last_syscmd_status = Some(status);
        Ok(state)
    }
}

/// The defining text of the `sysval` macro shall be the exit value of the utility last invoked by the
/// [`SyscmdMacro`] (as a string).
struct SysvalMacro;

impl MacroImplementation for SysvalMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        if let Some(status) = state.last_syscmd_status {
            match status.code() {
                Some(code) => state.input.pushback_string(code.to_string().as_bytes()),
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
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let divert_number = if let Some(first_arg) = frame.args.into_iter().next() {
            if first_arg.is_empty() {
                write!(
                    stderr,
                    "WARNING: treating empty first argument of `divert` macro as 0"
                )?;
                0
            } else {
                let (_, divert_number) = nom::combinator::all_consuming(parse_integer)(&first_arg)?;
                if divert_number > 9 {
                    return Err(crate::Error::new(crate::ErrorKind::Parsing).add_context(format!(
                        "Error parsing first argument for `divert` macro with value {divert_number}, should be between 0 and 9"
                    )));
                }
                divert_number
            }
        } else {
            0
        };
        state.output.output.divert(divert_number)?;
        Ok(state)
    }
}

/// The defining text of the divnum macro shall be the number of the current output stream as a string.
struct DivnumMacro;

impl MacroImplementation for DivnumMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        state
            .input
            .pushback_string(state.output.output.divert_number().to_string().as_bytes());
        Ok(state)
    }
}

/// The undivert macro shall cause immediate output of any text in temporary buffers named as
/// arguments, or all temporary buffers if no arguments are specified. Buffers can be undiverted
/// into other temporary buffers. Undiverting shall discard the contents of the temporary buffer.
/// The behavior is unspecified if an argument contains any non-numeric characters.
struct UndivertMacro;

// TODO: rewrite to be more performant and remove the need to parse as `str`.
fn parse_index(input: &[u8]) -> IResult<&[u8], usize> {
    log::trace!("parse_index() {}", String::from_utf8_lossy(input));
    let (remaining, found) = nom::bytes::complete::take_while(|c: u8| c.is_ascii_digit())(input)?;
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

impl MacroImplementation for UndivertMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let undivert_buffers: Vec<DivertBufferNumber> = if frame.args.is_empty() {
            (1..=9)
                .map(DivertBufferNumber::try_from)
                .collect::<Result<Vec<_>>>()?
        } else {
            let mut undivert_buffers: Vec<DivertBufferNumber> = Vec::new();
            for arg in frame.args.into_iter() {
                let (_, buffer_number) = nom::combinator::all_consuming(parse_index)(&arg)?;
                match DivertBufferNumber::try_from(buffer_number) {
                    Ok(n) => undivert_buffers.push(n),
                    Err(error) => write!(stderr, "WARNING: {error}")?,
                }
            }

            undivert_buffers
        };

        for buffer_number in undivert_buffers {
            state.output.output.undivert(buffer_number)?;
        }
        Ok(state)
    }
}

struct FileMacro;

impl MacroImplementation for FileMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        let name = match &state
            .input
            .0
            .borrow()
            .input
            .last()
            .expect("At least one input")
            .input
        {
            InputRead::File { path, .. } => path.as_os_str().as_encoded_bytes().to_vec(),
            InputRead::Stdin(_) => b"stdin".to_vec(),
        };

        state.input.pushback_string(&name);
        Ok(state)
    }
}

struct TraceoffMacro;

impl MacroImplementation for TraceoffMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            state.trace = Trace::default();
        } else {
            for arg in frame.args {
                let exclude = MacroName::try_from_slice(&arg)?;
                state.trace.include.retain(|include| include != &exclude);
                state.trace.exclude.push(exclude);
            }
        }

        Ok(state)
    }
}

struct TraceonMacro;

impl MacroImplementation for TraceonMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            state.trace.all = true;
            state.trace.exclude.clear();
        } else {
            for arg in frame.args {
                let include = MacroName::try_from_slice(&arg)?;
                state.trace.exclude.retain(|exclude| exclude != &include);
                state.trace.include.push(include);
            }
        }
        Ok(state)
    }
}
