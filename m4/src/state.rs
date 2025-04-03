use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::process::ExitStatus;
use std::rc::Rc;

use crate::input::{Input, InputState, InputStateRef};
use crate::lexer::{is_alphnumeric, MacroName, ParseConfig};
use crate::macros::trace::Trace;
use crate::macros::{BuiltinMacro, MacroDefinition};
use crate::output::{Output, OutputState};
use crate::EOF;

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

    /// Attempt to parse `state.input` as a macro name, into `token`. If it is a current macro name in
    /// `state.macro_definitions`, then it will return `Some` of [`MacroDefinition`].
    pub fn parse_macro(
        &mut self,
        mut c: u8,
        token: &mut Vec<u8>,
    ) -> crate::Result<Option<Rc<MacroDefinition>>> {
        token.clear();
        token.push(c);

        loop {
            c = self.input.get_next_character()?;
            if !(is_alphnumeric(c) || c == b'_') {
                break;
            }
            token.push(c)
        }
        if c != EOF {
            self.input.pushback_character(c);
        }

        Ok(self
            .macro_definitions
            .get(&MacroName::try_from_slice(token).expect("valid macro name"))
            .map(|v| v.last())
            .unwrap_or_default()
            .cloned())
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
    pub parenthesis_level: usize,
    pub args: Vec<Vec<u8>>,
    pub definition: Rc<MacroDefinition>,
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
