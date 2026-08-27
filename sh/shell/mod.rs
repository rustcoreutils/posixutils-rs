//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::set::SetOptions;
use crate::builtin::trap::TrapAction;
use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, BuiltinUtility, SpecialBuiltinUtility,
};
use crate::cli::terminal::Terminal;
use crate::jobs::{JobManager, JobState};
use crate::nonempty::NonEmpty;
use crate::os::errno::Errno;
use crate::os::signals::{kill, Signal, SignalManager};
use crate::os::{
    close, dup2, dup_cloexec, exec, find_command, fork, getpgid, getpgrp, is_process_in_foreground,
    pipe, setpgid, tcsetpgrp, waitpid, ExecError, ForkResult, OsError, OsResult, Pid, WaitStatus,
};
use crate::parse::command::{
    Assignment, CaseItem, Command, CommandType, CompleteCommand, CompoundCommand, Conjunction,
    FunctionDefinition, If, LogicalOp, Name, Pipeline, Redirection, SimpleCommand,
};
use crate::parse::command_parser::{is_valid_name, CommandParser};
use crate::parse::word::WordPair;
use crate::parse::word_parser::parse_word;
use crate::parse::{AliasTable, ParserError};
use crate::shell::environment::{CannotModifyReadonly, Environment, Value};
use crate::shell::history::{initialize_history_from_system, write_history_to_file, History};
use crate::shell::opened_files::OpenedFiles;
use crate::shstr::{ShStr, ShString};
use crate::wordexp::{
    expand_declaration_operand, expand_word, expand_word_to_string, word_to_pattern,
};
use gettextrs::gettext;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::os::fd::{AsRawFd, IntoRawFd};
use std::path::Path;
use std::rc::Rc;
use std::{env, io};

/// Default capacity for cached command locations HashMap
const DEFAULT_COMMAND_CACHE_CAPACITY: usize = 64;

pub mod environment;
pub mod history;
pub mod opened_files;

#[derive(Clone, Debug)]
pub enum CommandExecutionError {
    RedirectionError(String),
    VariableAssignmentError(CannotModifyReadonly),
    ExpansionError(String),
    CommandNotFound(String),
    OsError(OsError),
    ParseError(ParserError),
}

impl From<OsError> for CommandExecutionError {
    fn from(value: OsError) -> Self {
        Self::OsError(value)
    }
}

impl From<CannotModifyReadonly> for CommandExecutionError {
    fn from(value: CannotModifyReadonly) -> Self {
        CommandExecutionError::VariableAssignmentError(value)
    }
}

impl Display for CommandExecutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandExecutionError::RedirectionError(err) => {
                writeln!(f, "{err}")
            }
            CommandExecutionError::VariableAssignmentError(err) => {
                writeln!(f, "{err}")
            }
            CommandExecutionError::ExpansionError(err) => {
                writeln!(f, "{err}")
            }
            CommandExecutionError::CommandNotFound(command_name) => {
                writeln!(f, "sh: '{command_name}' {}", gettext("not found"))
            }
            CommandExecutionError::OsError(err) => {
                writeln!(f, "{err}")
            }
            CommandExecutionError::ParseError(err) => {
                writeln!(
                    f,
                    "sh: {} {}: {}",
                    gettext("parsing error at line"),
                    err.lineno,
                    err.message
                )
            }
        }
    }
}

type CommandExecutionResult<T> = Result<T, CommandExecutionError>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ControlFlowState {
    Break(u32),
    Continue(u32),
    Return,
    None,
}

impl ControlFlowState {
    fn go_to_outer_loop(&mut self) {
        match *self {
            ControlFlowState::Break(1) => *self = ControlFlowState::None,
            ControlFlowState::Break(n) => {
                assert_ne!(n, 0);
                *self = ControlFlowState::Break(n - 1)
            }
            ControlFlowState::Continue(1) => *self = ControlFlowState::None,
            ControlFlowState::Continue(n) => {
                assert_ne!(n, 0);
                *self = ControlFlowState::Continue(n - 1);
            }
            _ => {}
        }
    }
}

pub enum ScriptExecutionError {
    ParsingError(ParserError),
    IoError(std::io::Error),
}

pub fn execute_file_as_script(shell: &mut Shell, path: &Path) -> Result<i32, ScriptExecutionError> {
    let mut file = File::options()
        .read(true)
        .open(path)
        .map_err(ScriptExecutionError::IoError)?;

    let mut source = String::new();
    file.read_to_string(&mut source)
        .map_err(ScriptExecutionError::IoError)?;

    let lineno = shell.last_lineno;
    shell.last_lineno = 0;
    let execution_result = shell.execute_program(source.as_bytes());
    shell.last_lineno = lineno;
    execution_result.map_err(ScriptExecutionError::ParsingError)
}

#[derive(Clone)]
pub struct Shell {
    pub environment: Environment,
    pub program_name: ShString,
    pub positional_parameters: Vec<ShString>,
    pub opened_files: OpenedFiles,
    pub functions: HashMap<Name, Rc<CompoundCommand>>,
    pub last_pipeline_exit_status: i32,
    pub last_command_substitution_status: i32,
    pub shell_pid: i32,
    pub current_directory: OsString,
    pub set_options: SetOptions,
    pub alias_table: AliasTable,
    pub control_flow_state: ControlFlowState,
    pub loop_depth: u32,
    pub function_call_depth: u32,
    pub dot_script_depth: u32,
    pub is_interactive: bool,
    pub last_lineno: u32,
    pub exit_action: TrapAction,
    pub signal_manager: SignalManager,
    pub background_jobs: JobManager,
    pub history: History,
    pub umask: u32,
    pub saved_command_locations: HashMap<Vec<u8>, OsString>,
    pub is_subshell: bool,
    pub last_pipeline_command: String,
    pub terminal: Terminal,
    /// `getopts` keeps `OPTIND` a plain integer; the within-argument position
    /// for bundled options (`-abc`) is tracked here, as `(optind_we_wrote,
    /// option_index)`. If `OPTIND` differs from `optind_we_wrote` on entry the
    /// application reset it and the option index restarts at 0.
    pub getopts_state: (usize, usize),
    /// Time of the last mail check and the last-seen mtime of each mail file.
    pub mail_check: MailCheck,
    /// `$!`: the process id of the most recent asynchronous command. It stays
    /// set once the job has been waited for and removed from the job table.
    pub last_background_pid: Option<Pid>,
}

#[derive(Default, Clone)]
pub struct MailCheck {
    last_check: Option<std::time::Instant>,
    mtimes: HashMap<String, Option<std::time::SystemTime>>,
}

/// Splits a `MAILPATH` entry into its pathname and optional `%message`, with a
/// backslash escaping the following character (so `\%` is a literal `%`).
fn split_mailpath_entry(entry: &str) -> (String, Option<String>) {
    let mut path = String::new();
    let mut chars = entry.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                if let Some(next) = chars.next() {
                    path.push(next);
                }
            }
            '%' => return (path, Some(chars.collect())),
            _ => path.push(c),
        }
    }
    (path, None)
}

/// Whether the command being built is a POSIX 2.9.1 declaration utility.
///
/// `export` and `readonly` are, and so is `command` once its own options are
/// past and the utility it invokes turns out to be one of them — which is only
/// known after that operand has been expanded, hence `Pending`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum DeclarationUtility {
    No,
    Pending,
    Yes,
}

impl DeclarationUtility {
    /// Classifies the command name, as an already-expanded field.
    fn from_name(name: &ShStr) -> Self {
        match name.as_bytes() {
            b"export" | b"readonly" => Self::Yes,
            b"command" => Self::Pending,
            _ => Self::No,
        }
    }

    /// Refines a `Pending` verdict with `command`'s next expanded word.
    fn resolve(self, word: &ShStr) -> Self {
        match self {
            Self::Pending if word.starts_with(b"-") => Self::Pending,
            Self::Pending => Self::from_name(word),
            other => other,
        }
    }
}

/// True for a word of the form `name=…`, whose value a declaration utility
/// expands as if it were an assignment.
fn is_assignment_shaped(word: &[u8]) -> bool {
    // A name is restricted to the portable character set, so the bytes before
    // the first `=` must be valid text to be one at all.
    match word.iter().position(|&b| b == b'=') {
        Some(pos) => std::str::from_utf8(&word[..pos]).is_ok_and(is_valid_name),
        None => false,
    }
}

impl Shell {
    /// POSIX 2.12: a subshell starts with every trap the parent *caught* reset
    /// to its default action; ignored traps stay ignored. That covers the EXIT
    /// trap too, which is held separately from the signal traps.
    fn become_subshell(&mut self) {
        self.signal_manager.reset();
        self.exit_action = TrapAction::Default;
        self.is_subshell = true;
    }

    fn eprint(&self, message: &str) {
        self.opened_files.write_err(message);
    }

    pub fn exit(&mut self, code: i32) -> ! {
        // Run the EXIT trap exactly once: take it out before executing so that
        // an `exit` invoked from within the trap action terminates immediately
        // (POSIX) instead of recursing forever.
        let exit_action = std::mem::replace(&mut self.exit_action, TrapAction::Default);
        self.execute_action(exit_action);
        if self.is_interactive && !self.is_subshell {
            write_history_to_file(&self.history, &self.environment);
        }
        // POSIX 2.15: `exit n` for 0 <= n <= 255 exits *normally* with status
        // n. The shell must not re-raise the signal a `128+signo` status would
        // stand for -- that would dump core and, from a subshell, signal the
        // parent. dash and bash both exit normally here.
        std::process::exit(code);
    }

    pub fn wait_child_process(&mut self, child_pid: Pid) -> OsResult<i32> {
        self.wait_child_process_result(child_pid)
            .map(|(status, _)| status)
    }

    /// Waits for `child_pid`, returning its status and whether it actually
    /// terminated. A stopped child also ends the wait, but it is still alive
    /// and must stay in the job table.
    pub fn wait_child_process_result(&mut self, child_pid: Pid) -> OsResult<(i32, bool)> {
        loop {
            // Block. Signal handlers are installed without SA_RESTART and write
            // to a self-pipe, so a signal interrupts the wait with EINTR and the
            // trap runs below; polling with WNOHANG and sleeping instead cost a
            // full tick per foreground command.
            match waitpid(child_pid, false, true)? {
                WaitStatus::Interrupted => {
                    self.handle_async_events();
                }
                WaitStatus::Exited { exit_status } => return Ok((exit_status, true)),
                WaitStatus::Signaled { signal, .. } => return Ok((signal.exit_status(), true)),
                WaitStatus::Stopped { signal } => {
                    // Only register the job if it is not one already, or the
                    // table would gain a duplicate under a new number.
                    if !self.background_jobs.mark_stopped_by_pid(child_pid) {
                        self.background_jobs.add_job(
                            child_pid,
                            self.last_pipeline_command.clone(),
                            JobState::Stopped,
                        );
                    }
                    return Ok((signal.exit_status(), false));
                }
                WaitStatus::StillAlive => {
                    // Only reachable for a status this shell does not ask for
                    // (WIFCONTINUED); the child is still running, so wait again.
                    self.handle_async_events();
                }
            }
        }
    }

    pub fn handle_async_events(&mut self) {
        self.process_signals();
        // Reap terminated background jobs whether or not job control is on:
        // without this a non-interactive `cmd &` loop leaves one zombie per
        // iteration. Only the *reporting* of state changes is a job-control
        // feature.
        if let Err(err) = self.background_jobs.update_jobs() {
            self.eprint(&format!("sh: error updating background jobs ({err})\n"));
        }
        if self.set_options.monitor && self.set_options.notify {
            self.background_jobs
                .write_report(|job| self.opened_files.write_err(job.to_string_short()));
        }
        if !self.set_options.monitor {
            self.background_jobs.collect_terminated_jobs();
        }
    }

    pub fn assign_global<V: Into<ShString>>(
        &mut self,
        name: String,
        value: V,
    ) -> Result<&mut Value, CannotModifyReadonly> {
        // Changing PATH invalidates the remembered command locations (hash).
        if name == "PATH" {
            self.saved_command_locations.clear();
        }
        // inspect does not work in this case
        #[allow(clippy::manual_inspect)]
        self.environment.set_global(name, value).map(|val| {
            val.export_or(self.set_options.allexport);
            val
        })
    }

    pub fn execute_action(&mut self, action: TrapAction) {
        if let TrapAction::Commands(commands) = action {
            let last_pipeline_exit_status_before_trap = self.last_pipeline_exit_status;
            if let Err(err) = self.execute_program(commands.as_bytes()) {
                eprintln!("sh: error parsing action: {}", err.message);
            }
            self.last_pipeline_exit_status = last_pipeline_exit_status_before_trap;
        }
    }

    pub fn process_signals(&mut self) {
        while let Some(action) = self.signal_manager.get_pending_action().cloned() {
            self.execute_action(action)
        }
    }

    fn handle_error(&mut self, err: CommandExecutionError) -> i32 {
        self.eprint(&err.to_string());
        match err {
            CommandExecutionError::CommandNotFound(_) => 127,
            CommandExecutionError::OsError(_) => self.exit(1),
            // POSIX §2.8.1: an expansion error or a variable-assignment error
            // shall cause a non-interactive shell to exit.
            CommandExecutionError::ExpansionError(_)
            | CommandExecutionError::VariableAssignmentError(_)
                if !self.is_interactive =>
            {
                self.exit(1)
            }
            _ => 1,
        }
    }

    /// Replaces the shell process with `command`. Returns only when the exec
    /// failed, giving the diagnostic and the status POSIX prescribes for it;
    /// the caller decides whether to exit (see `exec_and_exit`).
    pub fn try_exec(
        &mut self,
        command: OsString,
        args: &[ShString],
        opened_files: &OpenedFiles,
    ) -> (String, i32) {
        let saved_signals = self.signal_manager.clone();
        self.signal_manager.reset();
        let failure =
            match exec(command.clone(), args, opened_files, &self.environment).unwrap_err() {
                ExecError::OsError(err) => (format!("{err}\n"), 126),
                ExecError::InteriorNul => (
                    "sh: command, argument or environment entry contains a NUL\n".to_string(),
                    126,
                ),
                ExecError::CannotExecute(errno) => {
                    if errno == Errno::ENOEXEC {
                        match execute_file_as_script(self, Path::new(&command)) {
                            Ok(status) => self.exit(status),
                            Err(ScriptExecutionError::ParsingError(err)) => {
                                self.eprint(&format!(
                                    "sh: parsing error ({}): {}\n",
                                    err.lineno, err.message
                                ));
                                self.exit(2)
                            }
                            Err(ScriptExecutionError::IoError(_)) => {
                                // fallthrough to the default error
                            }
                        }
                    }
                    (
                        format!(
                            "sh: failed to execute {} ({})\n",
                            command.to_string_lossy(),
                            errno
                        ),
                        126,
                    )
                }
            };
        // the process was not replaced after all, so the shell keeps its traps
        self.signal_manager.restore(saved_signals);
        failure
    }

    pub fn exec(&mut self, command: OsString, args: &[ShString], opened_files: &OpenedFiles) -> ! {
        let (message, status) = self.try_exec(command, args, opened_files);
        self.eprint(&message);
        self.exit(status)
    }

    pub fn fork_and_exec(
        &mut self,
        command: OsString,
        args: &[ShString],
        opened_files: &OpenedFiles,
    ) -> OsResult<i32> {
        match fork()? {
            ForkResult::Child => {
                self.become_subshell();
                self.exec(command, args, opened_files)
            }
            ForkResult::Parent { child } => {
                let status = self.wait_child_process(child);
                // A background job may have finished while this one ran.
                // `update_jobs` is a no-op when nothing is tracked.
                self.handle_async_events();
                status
            }
        }
    }

    /// Expands and applies each assignment in turn, returning the expanded
    /// values for `set -x`.
    ///
    /// Expansion and assignment must interleave: POSIX processes the
    /// assignments of a simple command left to right, and an earlier one is
    /// visible to a later one, so `a=1 b=$a` sets `b` to `1`.
    fn assign_globals(
        &mut self,
        assignments: &[Assignment],
        export: bool,
    ) -> CommandExecutionResult<Vec<(Name, ShString)>> {
        let mut expanded = Vec::with_capacity(assignments.len());
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value.word, true, self)?;
            self.assign_global(assignment.name.to_string(), word_str.clone())?
                .export_or(export);
            expanded.push((assignment.name.clone(), word_str));
        }
        Ok(expanded)
    }

    fn assign_locals(
        &mut self,
        assignments: &[Assignment],
    ) -> CommandExecutionResult<Vec<(Name, ShString)>> {
        let mut expanded = Vec::with_capacity(assignments.len());
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value.word, true, self)?;
            self.environment
                .set(assignment.name.to_string(), word_str.clone())?;
            expanded.push((assignment.name.clone(), word_str));
        }
        Ok(expanded)
    }

    fn exec_special_builtin(
        &mut self,
        simple_command: &SimpleCommand,
        args: &[ShString],
        special_builtin_utility: &dyn SpecialBuiltinUtility,
    ) -> CommandExecutionResult<i32> {
        // the standard does not specify if the variables should have the export attribute.
        // Bash exports them, we do the same here (neither sh, nor zsh do it though)
        self.assign_globals(&simple_command.assignments, true)?;
        let mut opened_files = self.opened_files.clone();
        if let Err(err) = opened_files.redirect(&simple_command.redirections, self) {
            if !self.is_interactive {
                self.eprint(&err.to_string());
                self.exit(1)
            }
            return Err(err);
        }
        match special_builtin_utility.exec(args, self, &mut opened_files) {
            Ok(status) => Ok(status),
            Err(err) => {
                opened_files.write_err(format!("{err}\n"));
                if !self.is_interactive {
                    self.exit(1)
                }
                Ok(1)
            }
        }
    }

    fn exec_function(
        &mut self,
        simple_command: &SimpleCommand,
        expanded_words: &[ShString],
        function_body: &CompoundCommand,
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        self.environment.push_scope();

        // The scope must be popped even when the assignment fails, or the
        // variable stays set and, because `Environment::exported` exports every
        // local scope, leaks into every later child.
        if let Err(err) = self.assign_locals(&simple_command.assignments) {
            self.environment.pop_scope();
            return Err(err);
        }

        // The redirections are applied by `interpret_compound_command` below;
        // applying them here as well opened every one of them twice.
        let mut args = expanded_words[1..].to_vec();
        std::mem::swap(&mut args, &mut self.positional_parameters);

        // A `break`/`continue` inside the function body must not escape into a
        // loop in the caller (POSIX), so the loop nesting starts fresh here.
        let saved_loop_depth = std::mem::take(&mut self.loop_depth);
        // LINENO is restored after the call so the caller's line numbering is
        // unaffected by the function body.
        let saved_lineno = self.last_lineno;
        self.function_call_depth += 1;
        let result = self.interpret_compound_command(
            function_body,
            &simple_command.redirections,
            ignore_errexit,
        );

        if self.control_flow_state == ControlFlowState::Return {
            self.control_flow_state = ControlFlowState::None;
        }
        self.function_call_depth -= 1;
        self.loop_depth = saved_loop_depth;
        self.last_lineno = saved_lineno;
        std::mem::swap(&mut args, &mut self.positional_parameters);
        self.environment.pop_scope();
        result
    }

    fn exec_builtin_utility(
        &mut self,
        simple_command: &SimpleCommand,
        args: &[ShString],
        builtin_utility: &dyn BuiltinUtility,
    ) -> CommandExecutionResult<i32> {
        let mut opened_files = self.opened_files.clone();
        opened_files.redirect(&simple_command.redirections, self)?;

        // The prefix assignments live in a scope of their own, which must be
        // popped even when the assignment fails: `Environment::exported`
        // exports every local scope, so a leaked one would put the variable in
        // the environment of every later child.
        self.environment.push_scope();
        let result =
            self.assign_locals(&simple_command.assignments).map(|_| {
                match builtin_utility.exec(args, self, &mut opened_files) {
                    Ok(status) => status,
                    Err(err) => {
                        opened_files.write_err(format!("{err}\n"));
                        1
                    }
                }
            });
        self.environment.pop_scope();
        result
    }

    /// `set -x`: report the command about to be executed, preceded by PS4.
    /// Variable assignments are shown with their *expanded* values and precede
    /// the command words; redirections are not shown. This matches dash.
    fn trace(&mut self, assignments: &[(Name, ShString)], expanded_words: &[ShString]) {
        if assignments.is_empty() && expanded_words.is_empty() {
            return;
        }
        let ps4 = self.get_ps4();
        self.eprint(&ps4);
        let mut separator = "";
        for (name, value) in assignments {
            self.eprint(separator);
            self.eprint(&format!("{name}={}", value.display()));
            separator = " ";
        }
        for expanded_word in expanded_words {
            self.eprint(separator);
            self.eprint(&expanded_word.display().to_string());
            separator = " ";
        }
        self.eprint("\n");
    }

    pub fn find_command(
        &mut self,
        command_name: &ShStr,
        default_path: &str,
        remember_location: bool,
    ) -> Option<OsString> {
        if let Some(command) = self.saved_command_locations.get(command_name.as_bytes()) {
            return Some(command.clone());
        }
        let path = self
            .environment
            .get_str_value("PATH")
            .unwrap_or(default_path);
        if let Some(command) = find_command(command_name, path) {
            if remember_location {
                self.saved_command_locations
                    .insert(command_name.as_bytes().to_vec(), command.clone());
            }
            Some(command)
        } else {
            None
        }
    }

    fn interpret_simple_command(
        &mut self,
        simple_command: &SimpleCommand,
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let mut expanded_words = Vec::new();
        // reset
        self.last_command_substitution_status = 0;
        // POSIX 2.9.1: for a declaration utility, operands that look like
        // assignments are expanded as assignments (tilde expansion after `=`
        // and after each `:`) and are not field-split or globbed. The command
        // name decides this, so the first word is expanded on its own.
        let mut declaration_utility = DeclarationUtility::No;
        for (index, word_pair) in simple_command.words.iter().enumerate() {
            if declaration_utility == DeclarationUtility::Yes
                && index > 0
                && is_assignment_shaped(word_pair.as_string.as_bytes())
            {
                expanded_words.push(expand_declaration_operand(&word_pair.word, self)?);
                continue;
            }
            let fields = expand_word(&word_pair.word, false, self)?;
            if let Some(first) = fields.first() {
                declaration_utility = if index == 0 {
                    DeclarationUtility::from_name(first)
                } else {
                    declaration_utility.resolve(first)
                };
            }
            expanded_words.extend(fields);
        }
        if expanded_words.is_empty() {
            // No command to run: the assignments affect the shell itself, and
            // `set -x` reports them with their expanded values.
            let assigned = self.assign_globals(&simple_command.assignments, false)?;
            if self.set_options.xtrace {
                self.trace(&assigned, &expanded_words);
            }
            if !simple_command.redirections.is_empty() {
                let mut opened_files = self.opened_files.clone();
                opened_files.redirect(&simple_command.redirections, self)?;
            }
            return Ok(self.last_command_substitution_status);
        }

        if self.set_options.xtrace {
            // Any variable assignments are reported by the branches below,
            // which is where they are expanded.
            self.trace(&[], &expanded_words);
        }

        let command_name = expanded_words[0].display().to_string();
        if let Some(special_builtin_utility) = get_special_builtin_utility(&command_name) {
            self.exec_special_builtin(
                simple_command,
                &expanded_words[1..],
                special_builtin_utility,
            )
        } else if let Some(function_body) = self.functions.get(command_name.as_str()).cloned() {
            self.exec_function(
                simple_command,
                &expanded_words,
                &function_body,
                ignore_errexit,
            )
        } else if let Some(builtin_utility) = get_builtin_utility(&command_name) {
            self.exec_builtin_utility(simple_command, &expanded_words[1..], builtin_utility)
        } else {
            let command = self
                .find_command(&expanded_words[0], "", self.set_options.hashall)
                .ok_or(CommandExecutionError::CommandNotFound(
                    expanded_words[0].display().to_string(),
                ))?;

            self.environment.push_scope();
            let result = self
                .assign_locals(&simple_command.assignments)
                .and_then(|_| {
                    let mut opened_files = self.opened_files.clone();
                    opened_files.redirect(&simple_command.redirections, self)?;
                    self.fork_and_exec(command, &expanded_words, &opened_files)
                        .map_err(|err| err.into())
                });
            self.environment.pop_scope();
            result
        }
    }

    fn interpret_for_clause(
        &mut self,
        iter_var: Name,
        iter_words: &[WordPair],
        body: &CompleteCommand,
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let mut result = 0;
        self.loop_depth += 1;
        'outer: for word_pair in iter_words {
            let items = expand_word(&word_pair.word, false, self)?;
            for item in items {
                self.assign_global(iter_var.to_string(), item)?;
                result = self.interpret(body, ignore_errexit);
                match self.control_flow_state {
                    ControlFlowState::Break(_) => {
                        self.control_flow_state.go_to_outer_loop();
                        break 'outer;
                    }
                    ControlFlowState::Continue(n) => {
                        self.control_flow_state.go_to_outer_loop();
                        if n > 1 {
                            break 'outer;
                        } else {
                            continue 'outer;
                        }
                    }
                    ControlFlowState::Return => {
                        break 'outer;
                    }
                    _ => {}
                }
            }
        }
        self.loop_depth -= 1;
        Ok(result)
    }

    fn interpret_case_clause(
        &mut self,
        arg: &WordPair,
        cases: &[CaseItem],
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let arg = expand_word_to_string(&arg.word, false, self)?;
        for (index, case) in cases.iter().enumerate() {
            let mut matched = false;
            for pattern in &case.pattern {
                let pattern = word_to_pattern(&pattern.word, self)?;
                if pattern.matches(arg.as_bytes()) {
                    matched = true;
                    break;
                }
            }
            if !matched {
                continue;
            }
            let mut result = match &case.body {
                Some(body) => self.interpret(body, ignore_errexit),
                None => 0,
            };
            // `;&` falls through: execute subsequent items' bodies without
            // pattern matching, stopping at a `;;` item, the end, or once a
            // break/continue/return is pending.
            let mut idx = index;
            while cases[idx].fallthrough
                && idx + 1 < cases.len()
                && self.control_flow_state == ControlFlowState::None
            {
                idx += 1;
                result = match &cases[idx].body {
                    Some(body) => self.interpret(body, ignore_errexit),
                    None => 0,
                };
            }
            return Ok(result);
        }
        Ok(0)
    }

    fn interpret_if_clause(
        &mut self,
        if_chain: &NonEmpty<If>,
        else_body: &Option<CompleteCommand>,
        ignore_errexit: bool,
    ) -> i32 {
        for if_ in if_chain {
            if self.interpret(&if_.condition, true) == 0 {
                return self.interpret(&if_.body, ignore_errexit);
            }
        }
        if let Some(else_body) = else_body {
            self.interpret(else_body, ignore_errexit)
        } else {
            0
        }
    }

    fn interpret_loop_clause(
        &mut self,
        condition: &CompleteCommand,
        body: &CompleteCommand,
        continue_if_zero: bool,
        ignore_errexit: bool,
    ) -> i32 {
        // POSIX: a loop's exit status is that of the last command executed in
        // its body, or zero if the body never ran.
        let mut status = 0;
        loop {
            let condition = self.interpret(condition, true);
            if (condition == 0 && !continue_if_zero) || (condition != 0 && continue_if_zero) {
                break;
            }
            self.loop_depth += 1;
            status = self.interpret(body, ignore_errexit);
            self.loop_depth -= 1;
            match self.control_flow_state {
                ControlFlowState::Break(_) => {
                    self.control_flow_state.go_to_outer_loop();
                    break;
                }
                ControlFlowState::Continue(n) => {
                    self.control_flow_state.go_to_outer_loop();
                    if n > 1 {
                        break;
                    } else {
                        continue;
                    }
                }
                ControlFlowState::Return => {
                    break;
                }
                _ => {}
            }
        }
        status
    }

    fn interpret_subshell(&mut self, commands: &CompleteCommand) -> CommandExecutionResult<i32> {
        match fork()? {
            ForkResult::Child => {
                self.become_subshell();
                let status = self.interpret(commands, false);
                self.exit(status);
            }
            ForkResult::Parent { child } => {
                self.wait_child_process(child).map_err(|err| err.into())
            }
        }
    }

    pub fn interpret_compound_command(
        &mut self,
        compound_command: &CompoundCommand,
        redirections: &[Redirection],
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let mut prev_opened_files = self.opened_files.clone();
        prev_opened_files.redirect(redirections, self)?;
        std::mem::swap(&mut self.opened_files, &mut prev_opened_files);
        let result = match compound_command {
            CompoundCommand::BraceGroup(command) => Ok(self.interpret(command, ignore_errexit)),
            CompoundCommand::Subshell(commands) => self.interpret_subshell(commands),
            CompoundCommand::ForClause {
                iter_var,
                words,
                body,
            } => self.interpret_for_clause(iter_var.clone(), words, body, ignore_errexit),
            CompoundCommand::CaseClause { arg, cases } => {
                self.interpret_case_clause(arg, cases, ignore_errexit)
            }
            CompoundCommand::IfClause {
                if_chain,
                else_body,
            } => Ok(self.interpret_if_clause(if_chain, else_body, ignore_errexit)),
            CompoundCommand::WhileClause { condition, body } => {
                Ok(self.interpret_loop_clause(condition, body, true, ignore_errexit))
            }
            CompoundCommand::UntilClause { condition, body } => {
                Ok(self.interpret_loop_clause(condition, body, false, ignore_errexit))
            }
        };
        std::mem::swap(&mut self.opened_files, &mut prev_opened_files);
        result
    }

    fn define_function(&mut self, definition: &FunctionDefinition) {
        self.functions
            .insert(definition.name.clone(), definition.body.clone());
    }

    fn interpret_command(&mut self, command: &Command, ignore_errexit: bool) -> i32 {
        let lineno_var = self.environment.set_global_forced(
            "LINENO".to_string(),
            ShString::from(command.lineno.to_string()),
        );
        if lineno_var.readonly {
            self.opened_files
                .write_err("sh: setting LINENO to readonly has no effect");
            lineno_var.readonly = false;
        }
        let execution_result = match &command.type_ {
            CommandType::SimpleCommand(simple_command) => {
                self.interpret_simple_command(simple_command, ignore_errexit)
            }
            CommandType::CompoundCommand {
                command,
                redirections,
            } => self.interpret_compound_command(command, redirections, ignore_errexit),
            CommandType::FunctionDefinition(function) => {
                self.define_function(function);
                Ok(0)
            }
        };

        match execution_result {
            Ok(result) => result,
            Err(err) => self.handle_error(err),
        }
    }

    fn interpret_pipeline(&mut self, pipeline: &Pipeline, ignore_errexit: bool) -> OsResult<i32> {
        self.last_pipeline_command = pipeline.to_string();
        let pipeline_exit_status;
        if pipeline.commands.len() == 1 {
            let command = pipeline.commands.first();
            pipeline_exit_status = self.interpret_command(command, ignore_errexit);
        } else {
            match fork()? {
                ForkResult::Child => {
                    self.become_subshell();
                    // this should never fail as both arguments are valid
                    setpgid(0, 0).expect("failed to create new process group");
                    let pipeline_pgid = getpgrp();
                    // wait for the parent process to put the subshell in the foreground
                    if let Err(err) = kill(0, Some(Signal::SigStop)) {
                        self.eprint(&format!("sh: internal call to kill failed ({err})"));
                        self.exit(1);
                    }

                    // Keep a copy of the real stdin: after the last command has
                    // run, fd 0 still holds the final pipe's read end, and the
                    // writers upstream would never see EPIPE while this process
                    // waits for them (`yes | head` would hang forever). It is
                    // close-on-exec so the pipeline's utilities never see it,
                    // and an `OwnedFd` so the error paths below cannot leak it.
                    let saved_stdin = dup_cloexec(libc::STDIN_FILENO).ok();
                    let mut current_stdin = libc::STDIN_FILENO;
                    let mut head_pids = Vec::new();
                    for command in pipeline.commands.head() {
                        let (read_pipe, write_pipe) = pipe()?;
                        match fork()? {
                            ForkResult::Child => {
                                // should never fail as `pipeline_pgid` is a valid process group
                                setpgid(0, pipeline_pgid)
                                    .expect("failed to set process group for pipeline subcommand");
                                drop(read_pipe);
                                let write_stdout = write_pipe.into_raw_fd();
                                dup2(current_stdin, libc::STDIN_FILENO)?;
                                dup2(write_stdout, libc::STDOUT_FILENO)?;
                                // fds 0 and 1 now hold both ends; the extra
                                // copies must go before the command runs, or
                                // it inherits descriptors it never asked for
                                // (and upstream writers never see EPIPE).
                                if current_stdin != libc::STDIN_FILENO {
                                    close(current_stdin)?;
                                }
                                if write_stdout != libc::STDOUT_FILENO {
                                    close(write_stdout)?;
                                }
                                let return_status = self.interpret_command(command, false);
                                self.exit(return_status);
                            }
                            ForkResult::Parent { child } => {
                                head_pids.push(child);
                                if current_stdin != libc::STDIN_FILENO {
                                    close(current_stdin)?;
                                }
                                current_stdin = read_pipe.into_raw_fd();
                            }
                        }
                    }
                    dup2(current_stdin, libc::STDIN_FILENO)?;
                    if current_stdin != libc::STDIN_FILENO {
                        close(current_stdin)?;
                    }
                    let return_status = self.interpret_command(pipeline.commands.last(), false);
                    // Drop the pipe's read end from fd 0 as well, so that the
                    // upstream writers can be signalled and reaped below.
                    if let Some(saved_stdin) = saved_stdin {
                        dup2(saved_stdin.as_raw_fd(), libc::STDIN_FILENO)?;
                    }
                    // Wait for every command in the pipeline to finish (POSIX
                    // requires it), reaping the head commands so they are not
                    // left running as orphans (e.g. `sleep 5 | true`).
                    let mut statuses: Vec<i32> = head_pids
                        .into_iter()
                        .map(|pid| self.wait_child_process(pid).unwrap_or(0))
                        .collect();
                    statuses.push(return_status);
                    // With `pipefail`, the pipeline status is that of the
                    // rightmost command that exited non-zero (else 0); otherwise
                    // it is the status of the last command only.
                    let exit_status = if self.set_options.pipefail {
                        statuses
                            .iter()
                            .rev()
                            .find(|&&s| s != 0)
                            .copied()
                            .unwrap_or(0)
                    } else {
                        return_status
                    };
                    self.exit(exit_status);
                }
                ForkResult::Parent { child } => {
                    loop {
                        // Blocking; a signal interrupts with EINTR and is
                        // handled below rather than being polled for.
                        match waitpid(child, false, true)? {
                            WaitStatus::Exited { .. } => {
                                // the only way this happened is if there was an error before going
                                // the child went to sleep
                                return Ok(1);
                            }
                            WaitStatus::Signaled { .. } => {
                                self.eprint("sh: unsynchronised pipeline was terminated by another process\n");
                                return Ok(1);
                            }
                            WaitStatus::Stopped { .. } => {
                                if is_process_in_foreground() {
                                    // should never fail as child is a valid process id and
                                    // in the same session as the current shell
                                    let child_gpid = getpgid(child)
                                        .expect("failed to get process id of child process");
                                    // should never fail as stdin is a valid file descriptor and
                                    // child gpid is valid and in the same session
                                    tcsetpgrp(io::stdin().as_raw_fd(), child_gpid)
                                        .expect("failed to set pipeline in foreground");
                                    kill(child, Some(Signal::SigCont))
                                        .expect("failed to start pipeline");
                                    pipeline_exit_status = self.wait_child_process(child)?;
                                    // should never fail
                                    tcsetpgrp(io::stdin().as_raw_fd(), getpgrp())
                                        .expect("failed to reset foreground process");
                                    break;
                                } else {
                                    kill(child, Some(Signal::SigCont))
                                        .expect("failed start pipeline");
                                    pipeline_exit_status = self.wait_child_process(child)?;
                                    break;
                                }
                            }
                            WaitStatus::StillAlive | WaitStatus::Interrupted => {
                                self.handle_async_events();
                            }
                        }
                    }
                }
            }
        }
        self.last_pipeline_exit_status = if pipeline.negate_status {
            (pipeline_exit_status == 0) as i32
        } else {
            if pipeline_exit_status != 0 && !ignore_errexit && self.set_options.errexit {
                self.exit(pipeline_exit_status)
            }
            pipeline_exit_status
        };
        Ok(self.last_pipeline_exit_status)
    }

    fn interpret_and_or_list(
        &mut self,
        list: &NonEmpty<(Pipeline, LogicalOp)>,
        ignore_errexit: bool,
    ) -> i32 {
        let mut status = 0;
        for i in 0..list.len() {
            let (pipeline, _) = &list[i];
            // The first pipeline always runs. Each later one runs only if the
            // operator joining it to the previous pipeline is satisfied by the
            // status so far; otherwise it is skipped and the status carries
            // through. Skipping one element at a time is what makes
            // `false && a && b` run neither a nor b.
            if i > 0 {
                let run = match list[i - 1].1 {
                    LogicalOp::And => status == 0,
                    LogicalOp::Or => status != 0,
                    LogicalOp::None => true,
                };
                if !run {
                    continue;
                }
            }
            // POSIX 2.11: `set -e` exempts every command of an AND-OR list
            // except the last, since the earlier ones' failure is what the
            // operators are testing.
            let ignore_errexit = i != list.len() - 1 || ignore_errexit;
            status = match self.interpret_pipeline(pipeline, ignore_errexit) {
                Ok(status) => status,
                Err(err) => {
                    self.eprint(&format!("{err}\n"));
                    self.exit(1)
                }
            };
            if self.control_flow_state != ControlFlowState::None {
                return status;
            }
        }
        status
    }

    fn interpret_conjunction(&mut self, conjunction: &Conjunction, ignore_errexit: bool) -> i32 {
        if conjunction.is_async {
            match fork() {
                Ok(ForkResult::Child) => {
                    self.become_subshell();
                    // should never fail
                    setpgid(0, 0).expect("failed to create process group for background job");
                    if !self.set_options.monitor {
                        // POSIX 2.11: without job control, an asynchronous list
                        // ignores SIGINT and SIGQUIT and reads from /dev/null,
                        // so it neither competes for the terminal nor dies with
                        // the foreground job.
                        self.signal_manager
                            .set_action(Signal::SigInt, TrapAction::Ignore);
                        self.signal_manager
                            .set_action(Signal::SigQuit, TrapAction::Ignore);
                        self.opened_files.redirect_stdin_to_dev_null();
                    }
                    let status = self.interpret_and_or_list(&conjunction.elements, false);
                    self.exit(status);
                }
                Ok(ForkResult::Parent { child }) => {
                    self.last_background_pid = Some(child);
                    self.background_jobs
                        .add_job(child, conjunction.to_string(), JobState::Running);
                    // Reap whatever has already finished, so a loop that starts
                    // background jobs does not accumulate a zombie per
                    // iteration.
                    self.handle_async_events();
                    0
                }
                Err(_) => {
                    self.eprint("sh: failed to create background job\n");
                    if !self.is_interactive {
                        self.exit(1);
                    } else {
                        1
                    }
                }
            }
        } else {
            self.interpret_and_or_list(&conjunction.elements, ignore_errexit)
        }
    }

    fn interpret(&mut self, command: &CompleteCommand, ignore_errexit: bool) -> i32 {
        let mut status = 0;
        for conjunction in &command.commands {
            status = self.interpret_conjunction(conjunction, ignore_errexit);
            if self.control_flow_state != ControlFlowState::None {
                return status;
            }
        }
        status
    }

    pub fn execute_in_subshell(&mut self, program: &[u8]) -> CommandExecutionResult<ShString> {
        let (read_pipe, write_pipe) = pipe()?;
        match fork()? {
            ForkResult::Child => {
                self.become_subshell();
                drop(read_pipe);
                dup2(write_pipe.as_raw_fd(), libc::STDOUT_FILENO)?;
                self.execute_program(program)
                    .map_err(CommandExecutionError::ParseError)?;
                self.exit(self.last_pipeline_exit_status);
            }
            ForkResult::Parent { child } => {
                drop(write_pipe);
                // Drain the pipe *before* waiting: a child whose output exceeds
                // the pipe buffer blocks in write() until it is read, so
                // waiting first would deadlock.
                let mut bytes = Vec::new();
                File::from(read_pipe)
                    .read_to_end(&mut bytes)
                    .map_err(|err| {
                        CommandExecutionError::ExpansionError(format!(
                            "command substitution: {err}"
                        ))
                    })?;
                // POSIX 2.9.1: when a command consists only of substitutions,
                // its exit status is that of the last one, so the status has
                // to be recorded rather than discarded.
                // Blocking, so EINTR must be retried like every other wait
                // site: handlers are installed without SA_RESTART, and folding
                // `Interrupted` into 0 would report success for whatever the
                // child actually did and leave it unreaped.
                self.last_command_substitution_status = loop {
                    match waitpid(child, false, false)? {
                        WaitStatus::Exited { exit_status } => break exit_status,
                        WaitStatus::Signaled { signal } => break signal.exit_status(),
                        WaitStatus::Interrupted | WaitStatus::StillAlive => {
                            self.handle_async_events()
                        }
                        WaitStatus::Stopped { .. } => break 0,
                    }
                };
                // POSIX leaves NUL bytes in command output unspecified; drop
                // them (as bash does) so they cannot reach the C-string paths.
                // Everything else is kept exactly as the command wrote it — the
                // output of a command substitution is a value, not text.
                bytes.retain(|&b| b != 0);
                while bytes.last() == Some(&b'\n') {
                    bytes.pop();
                }
                Ok(ShString::from(bytes))
            }
        }
    }

    pub fn execute_program(&mut self, program: &[u8]) -> Result<i32, ParserError> {
        if self.set_options.verbose {
            self.eprint(&String::from_utf8_lossy(program))
        }
        if let Err(err) = self.background_jobs.update_jobs() {
            self.eprint(&format!("sh: error updating background jobs ({err})"));
        }
        self.process_signals();
        let mut parser = CommandParser::new(program, self.last_lineno)?;
        let mut result = 0;
        loop {
            let command = parser.parse_next_command(&self.alias_table)?;
            if !self.is_interactive && self.set_options.noexec {
                // `set -n` reads and parses without executing, so the whole
                // input must be parsed: returning here checked only the first
                // command and reported success for a script whose later
                // commands do not parse.
                if command.is_none() {
                    return Ok(result);
                }
                continue;
            }
            if let Some(command) = command {
                if self.is_interactive {
                    self.history.add_entry(command.to_string());
                }
                result = self.interpret(&command, false);
                if self.control_flow_state == ControlFlowState::Return {
                    self.control_flow_state = ControlFlowState::None;
                    return Ok(result);
                }
            } else {
                break;
            }
        }
        if self.set_options.monitor {
            self.background_jobs
                .write_report(|job| self.opened_files.write_err(job.to_string_short()));
        }
        self.background_jobs.collect_terminated_jobs();
        self.last_lineno = parser.lineno() - 1;
        Ok(result)
    }

    pub fn initialize_from_system(
        program_name: ShString,
        args: Vec<ShString>,
        mut set_options: SetOptions,
        is_interactive: bool,
    ) -> Shell {
        // `$-` reports `i` for an interactive shell; `set` cannot change it.
        set_options.interactive = is_interactive;
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let mut environment =
            // `vars()` panics inside libstd on an entry that is not valid
            // UTF-8; `vars_os()` cannot. A *name* is restricted to the portable
            // character set, so one that is not text cannot be addressed as a
            // shell variable — but POSIX still requires passing it on to
            // children, so it is kept aside rather than dropped.
            Environment::from(std::env::vars_os().filter_map(|(k, v)| {
                k.into_string()
                    .ok()
                    .map(|k| (k, Value::new_exported(ShString::from(v))))
            }));
        let ppid = unsafe { libc::getppid() };
        environment.set_global_forced("PPID".to_string(), ppid.to_string());
        environment.set_global_if_unset("IFS", " \t\n");
        environment.set_global_if_unset("PS1", "\\$ ");
        environment.set_global_if_unset("PS2", "> ");
        environment.set_global_if_unset("PS4", "+ ");
        environment.set_global_if_unset("OPTIND", "1");
        let history = initialize_history_from_system(&environment);
        let current_directory = match env::current_dir() {
            Ok(path) => path.into_os_string(),
            Err(err) => {
                eprintln!(
                    "sh: failed to determine the current working directory ({})",
                    err
                );
                std::process::exit(1);
            }
        };
        // POSIX: the shell sets and exports PWD to the current directory.
        environment
            .set_global_forced(
                "PWD".to_string(),
                current_directory.to_string_lossy().into_owned(),
            )
            .export = true;
        Shell {
            environment,
            program_name,
            positional_parameters: args,
            shell_pid: unsafe { libc::getpid() },
            current_directory,
            history,
            set_options,
            is_interactive,
            signal_manager: SignalManager::new(is_interactive),
            ..Default::default()
        }
    }

    /// Checks the mailbox file(s) named by `MAILPATH` (or `MAIL`) and returns
    /// any notifications to write before the next prompt, throttled by the
    /// `MAILCHECK` interval (default 600s; 0 = every prompt). `MAILPATH` takes
    /// precedence over `MAIL`.
    pub fn check_mail(&mut self) -> Vec<String> {
        let interval: u64 = self
            .environment
            .get_str_value("MAILCHECK")
            .and_then(|v| v.parse().ok())
            .unwrap_or(600);
        let entries: Vec<(String, Option<String>)> =
            if let Some(mailpath) = self.environment.get_str_value("MAILPATH") {
                mailpath.split(':').map(split_mailpath_entry).collect()
            } else if let Some(mail) = self.environment.get_str_value("MAIL") {
                vec![(mail.to_string(), None)]
            } else {
                return Vec::new();
            };
        if interval > 0 {
            if let Some(last) = self.mail_check.last_check {
                if last.elapsed().as_secs() < interval {
                    return Vec::new();
                }
            }
        }
        self.mail_check.last_check = Some(std::time::Instant::now());
        let mut messages = Vec::new();
        for (path, message) in entries {
            let mtime = std::fs::metadata(&path)
                .ok()
                .and_then(|m| m.modified().ok());
            let first_seen = !self.mail_check.mtimes.contains_key(&path);
            let prev = self.mail_check.mtimes.get(&path).copied().flatten();
            // notify when the file is created or its mtime advances (but never
            // on the first observation, which just establishes the baseline)
            let notify = !first_seen
                && match (prev, mtime) {
                    (Some(p), Some(m)) => m > p,
                    (None, Some(_)) => true,
                    _ => false,
                };
            self.mail_check.mtimes.insert(path, mtime);
            if notify {
                messages.push(message.unwrap_or_else(|| "you have mail".to_string()));
            }
        }
        messages
    }

    pub fn get_var_and_expand(&mut self, var: &str, default_if_err: &str) -> String {
        // PS1/PS2/PS4 are written to the terminal, so a lossy view is right.
        let var = self.environment.get_str_value(var).unwrap_or_default();
        match parse_word(var.as_bytes(), 0, false) {
            Ok(word) => match expand_word_to_string(&word, false, self) {
                Ok(str) => str.display().to_string(),
                Err(err) => {
                    self.handle_error(err);
                    default_if_err.to_string()
                }
            },
            Err(err) => {
                eprintln!("sh: error parsing contents of {var}: {}", err.message);
                if !self.is_interactive {
                    self.exit(1)
                }
                default_if_err.to_string()
            }
        }
    }

    pub fn get_ps1(&mut self) -> String {
        // The standard specifies that only parameter expansion should be performed,
        // but other shells also do all the other forms of substitution. Since its easier
        // and basically an extension to the standard, we do the same here.
        self.get_var_and_expand("PS1", "\\$ ")
    }

    pub fn get_ps2(&mut self) -> String {
        self.get_var_and_expand("PS2", "> ")
    }

    pub fn get_ps4(&mut self) -> String {
        self.get_var_and_expand("PS4", "+ ")
    }
}

impl Default for Shell {
    fn default() -> Self {
        Shell {
            environment: Environment::from([("IFS".to_string(), Value::new(" \t\n".to_string()))]),
            program_name: ShString::from("sh"),
            positional_parameters: Vec::default(),
            opened_files: OpenedFiles::default(),
            functions: HashMap::default(),
            last_pipeline_exit_status: 0,
            last_command_substitution_status: 0,
            shell_pid: 0,
            current_directory: OsString::from("/"),
            set_options: SetOptions::default(),
            alias_table: AliasTable::default(),
            control_flow_state: ControlFlowState::None,
            loop_depth: 0,
            function_call_depth: 0,
            dot_script_depth: 0,
            is_interactive: false,
            last_lineno: 0,
            exit_action: TrapAction::Default,
            signal_manager: SignalManager::new(false),
            background_jobs: JobManager::default(),
            history: History::new(32767),
            // Stored as the complement: the permission bits a new file may
            // keep. Seeded from the inherited process mask rather than assumed.
            umask: !crate::os::get_umask() & 0o777,
            saved_command_locations: HashMap::with_capacity(DEFAULT_COMMAND_CACHE_CAPACITY),
            is_subshell: false,
            last_pipeline_command: String::new(),
            getopts_state: (0, 0),
            mail_check: MailCheck::default(),
            terminal: Terminal::default(),
            last_background_pid: None,
        }
    }
}
