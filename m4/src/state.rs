//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{cell::RefCell, collections::HashMap, io::Write, process::ExitStatus, rc::Rc};

use crate::{
    input::{Input, InputState, InputStateRef, Location},
    lexer::{is_alphnumeric, MacroName, ParseConfig},
    macros::{trace::Trace, BuiltinMacro, MacroDefinition},
    output::{Output, OutputState},
    EOF,
};

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
    /// The call site of the built-in being run, if one is. See
    /// [`State::location`].
    pub call_site: Option<Location>,
    /// `-E`: any diagnostic makes the final exit status a failure.
    pub fatal_warnings: bool,
}

impl State {
    pub fn try_new(
        stdout: Rc<RefCell<dyn Write>>,
        input: Vec<Input>,
        line_synchronization: bool,
    ) -> crate::Result<Self> {
        let input_state = InputStateRef::new(InputState::new(line_synchronization));
        for i in input {
            input_state.input_push(i);
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
            .get(token.as_slice())
            .map(|v| v.last())
            .unwrap_or_default()
            .cloned())
    }

    /// Emit a non-fatal warning to stderr in the GNU `m4:<file>:<line>: Warning:`
    /// form. Warnings do not affect the exit status (see [`State::emit_error`]).
    pub fn emit_warning(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> std::io::Result<()> {
        self.diagnose(stderr, format_args!("Warning: {args}"))
    }

    /// Emit a non-fatal diagnostic that GNU m4 words without the `Warning:`
    /// tag, in the `m4:<file>:<line>:` form. Like a warning, it does not
    /// affect the exit status.
    pub fn emit_message(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> std::io::Result<()> {
        self.diagnose(stderr, args)
    }

    /// Emit a recoverable error diagnostic to stderr (GNU `m4:<file>:<line>:`
    /// form) and flag the run so its final exit status is non-zero, without
    /// aborting further processing.
    pub fn emit_error(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> std::io::Result<()> {
        self.diagnose(stderr, args)?;
        self.exit_error = true;
        Ok(())
    }

    /// Where a diagnostic happens: the line a macro call began on while a
    /// built-in runs, as GNU m4 reports it (and `__line__` expands to),
    /// otherwise the current input position.
    pub fn location(&self) -> Location {
        match &self.call_site {
            Some(location) => location.clone(),
            None => self.input.current_location(),
        }
    }

    /// Write `m4:<file>:<line>: <args>` and apply `--fatal-warnings`: every
    /// diagnostic then makes the final exit status a failure.
    fn diagnose(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> std::io::Result<()> {
        let (name, line) = self.location();
        writeln!(
            stderr,
            "m4:{}:{}: {}",
            String::from_utf8_lossy(&name),
            line,
            args
        )?;
        if self.fatal_warnings {
            self.exit_error = true;
        }
        Ok(())
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

/// Text macros defined at startup, as GNU m4 defines them, so that input can
/// test which m4 it runs under: m4sugar refuses to run without `__gnu__`.
const PREDEFINED_TEXT_MACROS: &[&[u8]] = &[b"__gnu__", b"__unix__"];

impl Default for State {
    fn default() -> Self {
        let builtins = BuiltinMacro::enumerate().iter().map(|builtin| {
            let parse_config = builtin.parse_config();
            (
                parse_config.name.clone(),
                vec![Rc::new(MacroDefinition {
                    parse_config,
                    implementation: builtin.implementation(),
                })],
            )
        });
        let predefined = PREDEFINED_TEXT_MACROS.iter().map(|name| {
            let name = MacroName(name.to_vec());
            (
                name.clone(),
                vec![Rc::new(MacroDefinition::new_user_defined(name, Vec::new()))],
            )
        });
        Self {
            macro_definitions: builtins.chain(predefined).collect(),
            parse_config: ParseConfig::default(),
            exit_error: false,
            m4wrap: Vec::new(),
            last_syscmd_status: None,
            output: OutputState::default(),
            input: InputStateRef::default(),
            trace: Trace::default(),
            call_site: None,
            fatal_warnings: false,
        }
    }
}

pub struct StackFrame {
    pub parenthesis_level: usize,
    pub args: Vec<Vec<u8>>,
    pub definition: Rc<MacroDefinition>,
    /// Where the macro's name was read: GNU m4 reports a call, and runs it,
    /// at the line it began on rather than the line its arguments ended on.
    pub location: Location,
}

impl StackFrame {
    pub fn new(
        parenthesis_level: usize,
        definition: Rc<MacroDefinition>,
        location: Location,
    ) -> Self {
        Self {
            parenthesis_level,
            args: Vec::new(),
            definition,
            location,
        }
    }
}
