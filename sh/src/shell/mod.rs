use crate::builtin::set::SetOptions;
use crate::builtin::trap::TrapAction;
use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, BuiltinUtility, SpecialBuiltinUtility,
};
use crate::jobs::{JobManager, JobState};
use crate::nonempty::NonEmpty;
use crate::parse::command::{
    Assignment, CaseItem, Command, CommandType, CompleteCommand, CompoundCommand, Conjunction,
    FunctionDefinition, If, LogicalOp, Name, Pipeline, Redirection, SimpleCommand,
};
use crate::parse::command_parser::CommandParser;
use crate::parse::word::WordPair;
use crate::parse::word_parser::parse_word;
use crate::parse::{AliasTable, ParserError};
use crate::shell::environment::{CannotModifyReadonly, Environment, Value};
use crate::shell::history::{initialize_history_from_system, History};
use crate::shell::opened_files::OpenedFiles;
use crate::signals::SignalManager;
use crate::utils::{
    close, dup2, exec, find_command, fork, is_process_in_foreground, pipe, signal_to_exit_status,
    waitpid, ExecError, OsError, OsResult,
};
use crate::wordexp::{expand_word, expand_word_to_string, word_to_pattern};
use nix::errno::Errno;
use nix::libc;
use nix::sys::wait::{WaitPidFlag, WaitStatus};
use nix::unistd::{getpgrp, getpid, getppid, setpgid, ForkResult, Pid};
use std::collections::HashMap;
use std::ffi::{CString, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::{read_to_string, Read};
use std::os::fd::{AsFd, AsRawFd, IntoRawFd};
use std::path::Path;
use std::rc::Rc;
use std::time::Duration;

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
                writeln!(f, "sh: '{command_name}' not found")
            }
            CommandExecutionError::OsError(err) => {
                writeln!(f, "{err}")
            }
            CommandExecutionError::ParseError(err) => {
                writeln!(
                    f,
                    "sh: parsing error at line {}: {}",
                    err.lineno, err.message
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
    let execution_result = shell.execute_program(&source);
    shell.last_lineno = lineno;
    execution_result.map_err(ScriptExecutionError::ParsingError)
}

#[derive(Clone)]
pub struct Shell {
    pub environment: Environment,
    pub program_name: String,
    pub positional_parameters: Vec<String>,
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
    pub saved_command_locations: HashMap<String, OsString>,
    pub is_subshell: bool,
    pub last_pipeline_command: String,
}

impl Shell {
    fn become_subshell(&mut self) {
        self.signal_manager.reset();
        self.is_subshell = true;
    }

    fn eprint(&self, message: &str) {
        self.opened_files.write_err(message);
    }

    pub fn exit(&mut self, code: i32) -> ! {
        self.execute_action(self.exit_action.clone());
        std::process::exit(code);
    }

    pub fn wait_child_process(&mut self, child_pid: Pid) -> OsResult<i32> {
        loop {
            match waitpid(
                child_pid,
                Some(WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED),
            )? {
                WaitStatus::Exited(_, status) => return Ok(status),
                WaitStatus::Signaled(_, signal, _) => return Ok(signal_to_exit_status(signal)),
                WaitStatus::Stopped(_, signal) => {
                    self.background_jobs.add_job(
                        child_pid,
                        self.last_pipeline_command.clone(),
                        JobState::Stopped,
                    );
                    return Ok(signal_to_exit_status(signal));
                }
                WaitStatus::StillAlive => {
                    self.update_global_state();
                    std::thread::sleep(Duration::from_millis(16));
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn update_global_state(&mut self) {
        self.process_signals();
        if self.set_options.monitor {
            if let Err(err) = self.background_jobs.update_jobs() {
                self.eprint(&format!("sh: error updating background jobs ({err})\n"));
            }
            if self.set_options.notify {
                self.background_jobs
                    .write_report(|job| self.opened_files.write_err(job.to_string_short()));
            }
        }
    }

    pub fn assign_global(
        &mut self,
        name: String,
        value: String,
    ) -> Result<&mut Value, CannotModifyReadonly> {
        self.environment.set_global(name, value).map(|val| {
            val.export_or(self.set_options.allexport);
            val
        })
    }

    pub fn execute_action(&mut self, action: TrapAction) {
        if let TrapAction::Commands(commands) = action {
            let last_pipeline_exit_status_before_trap = self.last_pipeline_exit_status;
            match self.execute_program(&commands) {
                Err(err) => {
                    eprintln!("sh: error parsing action: {}", err.message);
                }
                Ok(_) => {}
            }
            self.last_pipeline_exit_status = last_pipeline_exit_status_before_trap;
        }
    }

    pub fn process_signals(&mut self) {
        while let Some(action) = self.signal_manager.get_pending_action() {
            self.execute_action(action.clone())
        }
    }

    fn handle_error(&mut self, err: CommandExecutionError) -> i32 {
        self.eprint(&err.to_string());
        match err {
            CommandExecutionError::CommandNotFound(_) => 127,
            CommandExecutionError::OsError(_) => self.exit(1),
            _ => 1,
        }
    }

    pub fn exec(
        &mut self,
        command: OsString,
        args: &[String],
        opened_files: &OpenedFiles,
    ) -> OsResult<i32> {
        match fork()? {
            ForkResult::Child => {
                self.signal_manager.reset();
                let flags =
                    nix::fcntl::fcntl(libc::STDIN_FILENO, nix::fcntl::FcntlArg::F_GETFL).unwrap();
                let new_flags = flags & !nix::fcntl::OFlag::O_NONBLOCK.bits();
                nix::fcntl::fcntl(
                    libc::STDIN_FILENO,
                    nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::from_bits_truncate(new_flags)),
                )
                .unwrap();
                match exec(command.clone(), args, &opened_files, &self.environment).unwrap_err() {
                    ExecError::OsError(err) => {
                        self.eprint(&format!("{err}\n"));
                        self.exit(1)
                    }
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
                        self.eprint(&format!(
                            "sh: failed to execute {}\n",
                            command.to_string_lossy()
                        ));
                        self.exit(126);
                    }
                }
            }
            ForkResult::Parent { child } => self.wait_child_process(child),
        }
    }

    fn assign_globals(
        &mut self,
        assignments: &[Assignment],
        export: bool,
    ) -> CommandExecutionResult<()> {
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value.word, true, self)?;
            self.assign_global(assignment.name.to_string(), word_str)?
                .export_or(export);
        }
        Ok(())
    }

    fn assign_locals(&mut self, assignments: &[Assignment]) -> CommandExecutionResult<()> {
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value.word, true, self)?;
            self.environment
                .set(assignment.name.to_string(), word_str)?;
        }
        Ok(())
    }

    fn exec_special_builtin(
        &mut self,
        simple_command: &SimpleCommand,
        args: &[String],
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
        expanded_words: &[String],
        function_body: &CompoundCommand,
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        self.environment.push_scope();

        self.assign_locals(&simple_command.assignments)?;

        let mut previous_opened_files = self.opened_files.clone();
        previous_opened_files.redirect(&simple_command.redirections, self)?;
        std::mem::swap(&mut self.opened_files, &mut previous_opened_files);

        let mut args = expanded_words[1..].to_vec();
        std::mem::swap(&mut args, &mut self.positional_parameters);

        self.function_call_depth += 1;
        let result = self.interpret_compound_command(
            &function_body,
            &simple_command.redirections,
            ignore_errexit,
        );

        if self.control_flow_state == ControlFlowState::Return {
            self.control_flow_state = ControlFlowState::None;
        }
        self.function_call_depth -= 1;
        std::mem::swap(&mut args, &mut self.positional_parameters);
        std::mem::swap(&mut self.opened_files, &mut previous_opened_files);
        self.environment.pop_scope();
        result
    }

    fn exec_builtin_utility(
        &mut self,
        simple_command: &SimpleCommand,
        args: &[String],
        builtin_utility: &dyn BuiltinUtility,
    ) -> CommandExecutionResult<i32> {
        let mut opened_files = self.opened_files.clone();
        opened_files.redirect(&simple_command.redirections, self)?;

        self.environment.push_scope();
        self.assign_locals(&simple_command.assignments)?;
        let status = match builtin_utility.exec(args, self, &mut opened_files) {
            Ok(status) => status,
            Err(err) => {
                opened_files.write_err(format!("{err}\n"));
                1
            }
        };
        self.environment.pop_scope();
        Ok(status)
    }

    fn trace(&mut self, expanded_words: &[String]) {
        let ps4 = self.get_ps4();
        self.eprint(&ps4);
        for expanded_word in &expanded_words[..expanded_words.len() - 1] {
            self.eprint(expanded_word);
            self.eprint(" ");
        }
        if let Some(expanded_word) = expanded_words.last() {
            self.eprint(expanded_word);
        }
        self.eprint("\n");
    }

    pub fn find_command(
        &mut self,
        command_name: &str,
        default_path: &str,
        remember_location: bool,
    ) -> Option<OsString> {
        if let Some(command) = self.saved_command_locations.get(command_name) {
            return Some(command.clone());
        }
        let path = self
            .environment
            .get_str_value("PATH")
            .unwrap_or(default_path);
        if let Some(command) = find_command(command_name, path) {
            if remember_location {
                self.saved_command_locations
                    .insert(command_name.to_string(), command.clone());
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
        for word_pair in &simple_command.words {
            expanded_words.extend(expand_word(&word_pair.word, false, self)?);
        }
        if self.set_options.xtrace {
            self.trace(&expanded_words);
        }
        if expanded_words.is_empty() {
            // no commands to execute, perform assignments and redirections
            self.assign_globals(&simple_command.assignments, false)?;
            if !simple_command.redirections.is_empty() {
                let mut subshell = self.clone();
                subshell
                    .opened_files
                    .redirect(&simple_command.redirections, self)?;
                (&simple_command.redirections, &mut subshell);
            }
            return Ok(self.last_command_substitution_status);
        }

        if let Some(special_builtin_utility) = get_special_builtin_utility(&expanded_words[0]) {
            self.exec_special_builtin(
                &simple_command,
                &expanded_words[1..],
                special_builtin_utility,
            )
        } else if let Some(function_body) = self.functions.get(expanded_words[0].as_str()).cloned()
        {
            self.exec_function(
                &simple_command,
                &mut expanded_words,
                &function_body,
                ignore_errexit,
            )
        } else if let Some(builtin_utility) = get_builtin_utility(&expanded_words[0]) {
            self.exec_builtin_utility(&simple_command, &expanded_words[1..], builtin_utility)
        } else {
            let command = self
                .find_command(&expanded_words[0], "", self.set_options.hashall)
                .ok_or(CommandExecutionError::CommandNotFound(
                    expanded_words[0].to_string(),
                ))?;

            self.environment.push_scope();
            self.assign_locals(&simple_command.assignments)?;
            let mut opened_files = self.opened_files.clone();
            opened_files.redirect(&simple_command.redirections, self)?;
            let result = self
                .exec(command, &expanded_words, &opened_files)
                .map_err(|err| err.into());
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
        let arg_cstr = CString::new(arg).expect("invalid pattern");
        for case in cases {
            for pattern in &case.pattern {
                let pattern = word_to_pattern(&pattern.word, self)?;
                if pattern.matches(&arg_cstr) {
                    return Ok(self.interpret(&case.body, ignore_errexit));
                }
            }
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
        let status = 0;
        loop {
            let condition = self.interpret(condition, true);
            if (condition == 0 && !continue_if_zero) || (condition != 0 && continue_if_zero) {
                break;
            }
            self.loop_depth += 1;
            self.interpret(body, ignore_errexit);
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
        let lineno_var = self
            .environment
            .set_global_forced("LINENO".to_string(), command.lineno.to_string());
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
                    setpgid(Pid::from_raw(0), Pid::from_raw(0))
                        .expect("failed to create new process group for pipeline");
                    let pipeline_pgid = getpgrp();

                    let mut current_stdin = libc::STDIN_FILENO;
                    for command in pipeline.commands.head() {
                        let (read_pipe, write_pipe) = pipe()?;
                        match fork()? {
                            ForkResult::Child => {
                                setpgid(Pid::from_raw(0), pipeline_pgid)
                                    .expect("failed to set pipeline pgid");
                                drop(read_pipe);
                                dup2(current_stdin, libc::STDIN_FILENO)?;
                                dup2(write_pipe.as_raw_fd(), libc::STDOUT_FILENO)?;
                                let return_status = self.interpret_command(command, false);
                                if current_stdin != libc::STDIN_FILENO {
                                    close(current_stdin)?;
                                }
                                self.exit(return_status);
                            }
                            ForkResult::Parent { .. } => {
                                if current_stdin != libc::STDIN_FILENO {
                                    close(current_stdin)?;
                                }
                                current_stdin = read_pipe.into_raw_fd();
                            }
                        }
                    }
                    dup2(current_stdin, libc::STDIN_FILENO)?;
                    let return_status = self.interpret_command(pipeline.commands.last(), false);
                    close(current_stdin)?;
                    self.exit(return_status);
                }
                ForkResult::Parent { child } => {
                    if is_process_in_foreground() {
                        nix::unistd::tcsetpgrp(io::stdin().as_fd(), child).unwrap();
                        pipeline_exit_status = self.wait_child_process(child)?;
                        nix::unistd::tcsetpgrp(io::stdin().as_fd(), getpgrp()).unwrap();
                    } else {
                        pipeline_exit_status = self.wait_child_process(child)?;
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
        let mut i = 0;
        while i < list.len() {
            let (pipeline, op) = &list[i];
            let ignore_errexit = i == list.len() - 1 && ignore_errexit;
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
            if status != 0 && *op == LogicalOp::And {
                // false && other ... -> skip other
                i += 1;
            } else if status == 0 && *op == LogicalOp::Or {
                // true || other ... -> skip other
                i += 1;
            }
            i += 1;
        }
        status
    }

    fn interpret_conjunction(&mut self, conjunction: &Conjunction, ignore_errexit: bool) -> i32 {
        if conjunction.is_async {
            match fork() {
                Ok(ForkResult::Child) => {
                    self.become_subshell();
                    // should never fail
                    setpgid(Pid::from_raw(0), Pid::from_raw(0))
                        .expect("failed to create process group for background job");
                    let status = self.interpret_and_or_list(&conjunction.elements, false);
                    self.exit(status);
                }
                Ok(ForkResult::Parent { child }) => {
                    self.background_jobs
                        .add_job(child, conjunction.to_string(), JobState::Running);
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

    pub fn execute_in_subshell(&mut self, program: &str) -> CommandExecutionResult<String> {
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
                match waitpid(child, None)? {
                    WaitStatus::Exited(_, _) | WaitStatus::Signaled(_, _, _) => {
                        let read_file = File::from(read_pipe);
                        let mut output = read_to_string(&read_file).unwrap();
                        let new_len = output.trim_end_matches('\n').len();
                        output.truncate(new_len);
                        Ok(output)
                    }
                    _ => todo!(),
                }
            }
        }
    }

    pub fn execute_program(&mut self, program: &str) -> Result<i32, ParserError> {
        if self.set_options.verbose {
            self.eprint(program)
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
                return Ok(result);
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
        self.background_jobs.cleanup_terminated_jobs();
        self.last_lineno = parser.lineno() - 1;
        Ok(result)
    }

    pub fn initialize_from_system(
        program_name: String,
        args: Vec<String>,
        set_options: SetOptions,
        is_interactive: bool,
    ) -> Shell {
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let mut environment = Environment::from(
            std::env::vars()
                .into_iter()
                .map(|(k, v)| (k, Value::new_exported(v))),
        );
        environment.set_global_forced("PPID".to_string(), getppid().to_string());
        environment.set_global_if_unset("IFS", " \t\n");
        environment.set_global_if_unset("PS1", "\\$ ");
        environment.set_global_if_unset("PS2", "> ");
        environment.set_global_if_unset("PS4", "+ ");
        environment.set_global_if_unset("OPTIND", "1");
        let history = initialize_history_from_system(&environment);
        Shell {
            environment,
            program_name,
            positional_parameters: args,
            shell_pid: getpid().as_raw(),
            // TODO: handle error
            current_directory: std::env::current_dir().unwrap().into_os_string(),
            history,
            set_options,
            is_interactive,
            ..Default::default()
        }
    }

    fn get_var_and_expand(&mut self, var: &str, default_if_err: &str) -> String {
        let var = self.environment.get_str_value(var).unwrap_or_default();
        match parse_word(var, 0, false) {
            Ok(word) => match expand_word_to_string(&word, false, self) {
                Ok(str) => str,
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
            program_name: "sh".to_string(),
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
            signal_manager: SignalManager::default(),
            background_jobs: JobManager::default(),
            history: History::new(32767),
            umask: !0o022 & 0o777,
            saved_command_locations: HashMap::new(),
            is_subshell: false,
            last_pipeline_command: String::new(),
        }
    }
}
