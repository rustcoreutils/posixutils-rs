//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    cell::RefCell,
    collections::HashMap,
    io::Write,
    path::{Path, PathBuf},
    process::ExitStatus,
    rc::Rc,
};

use crate::{
    input::{Input, InputState, InputStateRef, Location},
    lexer::{is_name_byte, MacroName, ParseConfig},
    macros::{trace::Trace, BuiltinMacro, MacroDefinition},
    output::{Output, OutputState},
    EOF,
};

/// FNV-1a, for the macro table: it is searched for every name read, the
/// keys are short, and nothing about them is adversarial, so the default
/// SipHash's resistance to collision attacks only costs time.
#[derive(Default)]
pub struct FnvHasher(u64);

impl std::hash::Hasher for FnvHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        let mut hash = if self.0 == 0 {
            0xcbf2_9ce4_8422_2325
        } else {
            self.0
        };
        for &b in bytes {
            hash ^= u64::from(b);
            hash = hash.wrapping_mul(0x0100_0000_01b3);
        }
        self.0 = hash;
    }
}

pub type FastHash = std::hash::BuildHasherDefault<FnvHasher>;

pub struct State {
    pub macro_definitions: HashMap<MacroName, Vec<Rc<MacroDefinition>>, FastHash>,
    pub parse_config: ParseConfig,
    /// Whether the process should exit with an error once processing has completed.
    pub exit_error: bool,
    /// See [`M4wrapMacro`].
    pub m4wrap: Vec<(Vec<u8>, Location)>,
    pub last_syscmd_status: Option<ExitStatus>,
    pub output: OutputState,
    pub input: InputStateRef,
    pub trace: Trace,
    /// The call site of the built-in being run, if one is. See
    /// [`State::location`].
    pub call_site: Option<Location>,
    /// `-E` given once: any diagnostic makes the final exit status a failure;
    /// twice: the first one stops m4.
    pub fatal_warnings: u8,
    /// `-I` directories, then `M4PATH`. See [`State::open_input`].
    pub include_dirs: Vec<PathBuf>,
    /// GNU m4 debug flags and stream.
    pub debug: crate::debug::Debug,
    /// Compiled `regexp`/`patsubst` patterns: m4sugar uses a few, often.
    pub regex_cache: HashMap<Vec<u8>, Rc<regex::bytes::Regex>>,
    /// `-L`. See [`State::check_nesting`].
    pub nesting_limit: usize,
    /// `POSIXLY_CORRECT` is set in the environment: where GNU m4 1.4, the
    /// default, departs from POSIX in a way autotools depends on, do as
    /// POSIX says instead.
    pub posixly_correct: bool,
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
            // The rest of the name in runs; a byte at a time only where a run
            // ends, at the end of a piece of pushed-back text, since a name
            // continues across it.
            if self.input.take_run(|c| !is_name_byte(c), token)? > 0 {
                continue;
            }
            c = self.input.get_next_character()?;
            if !is_name_byte(c) {
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
    ) -> crate::Result<()> {
        self.diagnose(stderr, format_args!("Warning: {args}"))
    }

    /// Emit a non-fatal diagnostic that GNU m4 words without the `Warning:`
    /// tag, in the `m4:<file>:<line>:` form. Like a warning, it does not
    /// affect the exit status.
    pub fn emit_message(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> crate::Result<()> {
        self.diagnose(stderr, args)
    }

    /// Emit a recoverable error diagnostic to stderr (GNU `m4:<file>:<line>:`
    /// form) and flag the run so its final exit status is non-zero, without
    /// aborting further processing.
    pub fn emit_error(
        &mut self,
        stderr: &mut dyn Write,
        args: std::fmt::Arguments<'_>,
    ) -> crate::Result<()> {
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
    ) -> crate::Result<()> {
        let (name, line) = self.location();
        writeln!(
            stderr,
            "m4:{}:{}: {}",
            String::from_utf8_lossy(&name),
            line,
            args
        )?;
        match self.fatal_warnings {
            0 => {}
            1 => self.exit_error = true,
            _ => return Err(crate::Error::new(crate::ErrorKind::Exit(1))),
        }
        Ok(())
    }

    /// Open the input file `path`: as named, or else, unless it is absolute,
    /// in each `-I` directory in turn. Returns the name it was found under,
    /// which is what diagnostics and `__file__` then report.
    pub fn open_input(&self, path: &Path) -> std::io::Result<(std::fs::File, PathBuf)> {
        let first_error = match std::fs::File::open(path) {
            Ok(file) => return Ok((file, path.to_path_buf())),
            Err(error) => error,
        };
        if !path.is_absolute() {
            for dir in &self.include_dirs {
                let candidate = dir.join(path);
                if let Ok(file) = std::fs::File::open(&candidate) {
                    return Ok((file, candidate));
                }
            }
        }
        Err(first_error)
    }

    /// Apply the command-line options that shape the initial state.
    pub fn configure(&mut self, args: &crate::Args, stderr: &mut dyn Write) -> crate::Result<()> {
        if let Some(flags) = &args.debug_flags {
            match crate::debug::parse_flags(flags) {
                Some(flags) => self.debug.flags = flags,
                None => writeln!(
                    stderr,
                    "m4: bad debug flags: `{}'",
                    String::from_utf8_lossy(flags)
                )?,
            }
        }
        if let Some(Some(name)) = &args.debugfile {
            match crate::debug::Sink::open(name) {
                Ok(sink) => self.debug.sink = sink,
                Err(error) => writeln!(
                    stderr,
                    "m4: cannot set debug file `{}': {}",
                    String::from_utf8_lossy(name),
                    plib::diag::io_error_text(&error)
                )?,
            }
        }
        self.trace.traced.extend(args.trace.iter().cloned());
        self.include_dirs = args.include_dirs.clone();
        // GNU m4 searches `M4PATH` after the `-I` directories.
        if let Some(path) = std::env::var_os("M4PATH") {
            self.include_dirs
                .extend(std::env::split_paths(&path).filter(|dir| !dir.as_os_str().is_empty()));
        }
        self.fatal_warnings = args.fatal_warnings;
        self.nesting_limit = args.nesting_limit;
        if args.prefix_builtins {
            for builtin in BuiltinMacro::enumerate() {
                self.macro_definitions.remove(builtin.as_ref());
                let mut name = b"m4_".to_vec();
                name.extend_from_slice(builtin.as_ref());
                let name = MacroName(name);
                let definition = Rc::new(MacroDefinition::new_builtin(name.clone(), *builtin));
                self.macro_definitions.insert(name, vec![definition]);
            }
        }
        Ok(())
    }

    /// Diagnose an error m4 cannot continue past, in GNU m4's
    /// `m4:<file>:<line>: ERROR: ...` form, and give the error that stops it
    /// with exit status 1.
    pub fn fatal(&mut self, stderr: &mut dyn Write, args: std::fmt::Arguments<'_>) -> crate::Error {
        match self.diagnose(stderr, format_args!("ERROR: {args}")) {
            Err(error) => error,
            Ok(()) => crate::Error::new(crate::ErrorKind::Exit(1)),
        }
    }

    /// Diagnose a macro call nested deeper than `-L` allows, and stop: GNU
    /// m4 treats it as fatal, since the input is almost certainly recursing
    /// without end.
    pub fn check_nesting(&mut self, stderr: &mut dyn Write, depth: usize) -> crate::Result<()> {
        if self.nesting_limit == 0 || depth <= self.nesting_limit {
            return Ok(());
        }
        let limit = self.nesting_limit;
        self.diagnose(
            stderr,
            format_args!("recursion limit of {limit} exceeded, use -L<N> to change it"),
        )?;
        Err(crate::Error::new(crate::ErrorKind::Exit(1)))
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
            fatal_warnings: 0,
            include_dirs: Vec::new(),
            nesting_limit: 0,
            debug: crate::debug::Debug::default(),
            regex_cache: HashMap::new(),
            posixly_correct: std::env::var_os("POSIXLY_CORRECT").is_some(),
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
    /// The arguments that are built-in tokens rather than text, by index
    /// (their text is empty). See [`crate::output::OutputState::write_builtin`].
    pub builtin_args: Vec<(usize, BuiltinMacro)>,
}

impl StackFrame {
    /// The built-in token argument `index` is, if it is one.
    pub fn builtin_arg(&self, index: usize) -> Option<BuiltinMacro> {
        self.builtin_args
            .iter()
            .find(|(i, _)| *i == index)
            .map(|(_, builtin)| *builtin)
    }

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
            builtin_args: Vec::new(),
        }
    }
}
