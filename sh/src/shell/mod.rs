use crate::builtin::set::SetOptions;
use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, BuiltinUtility, SpecialBuiltinUtility,
};
use crate::parse::command::{
    Assignment, CaseItem, Command, CommandType, CompleteCommand, CompoundCommand, Conjunction,
    FunctionDefinition, If, LogicalOp, Name, Pipeline, Redirection, SimpleCommand,
};
use crate::parse::command_parser::CommandParser;
use crate::parse::word::Word;
use crate::parse::word_parser::parse_word;
use crate::parse::{AliasTable, ParserError};
use crate::shell::environment::{CannotModifyReadonly, Environment, Value};
use crate::shell::opened_files::{OpenedFile, OpenedFiles};
use crate::utils::{
    close, dup2, exec, find_command, find_in_path, fork, pipe, waitpid, ExecError, OsError,
    OsResult,
};
use crate::wordexp::{expand_word, expand_word_to_string, word_to_pattern};
use nix::errno::Errno;
use nix::sys::wait::WaitStatus;
use nix::unistd::{execve, getpid, getppid, ForkResult, Pid};
use nix::{libc, NixPath};
use std::collections::HashMap;
use std::ffi::{CString, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{read_to_string, Read, Write};
use std::os::fd::{AsRawFd, IntoRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::OsStringExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub mod environment;
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
    pub most_recent_background_command_pid: Option<i32>,
    pub current_directory: OsString,
    pub set_options: SetOptions,
    pub alias_table: AliasTable,
    pub control_flow_state: ControlFlowState,
    pub loop_depth: u32,
    pub function_call_depth: u32,
    pub dot_script_depth: u32,
    pub is_interactive: bool,
    pub last_lineno: u32,
}

impl Shell {
    pub fn eprint(&self, message: &str) {
        self.opened_files.stderr().write_str(message);
    }

    pub fn assign(
        &mut self,
        name: String,
        value: Option<String>,
        add_export: bool,
        add_readonly: bool,
    ) -> Result<(), CannotModifyReadonly> {
        let export = self.set_options.allexport || add_export;
        self.environment.set(name, value, export, add_readonly)
    }

    fn handle_error(&self, err: CommandExecutionError) -> i32 {
        self.eprint(&err.to_string());
        match err {
            CommandExecutionError::CommandNotFound(_) => 127,
            CommandExecutionError::OsError(_) => std::process::exit(1),
            _ => 1,
        }
    }

    fn exec(&self, command: OsString, args: &[String]) -> OsResult<i32> {
        match fork()? {
            ForkResult::Child => match exec(command, args, &self.opened_files, &self.environment) {
                Err(ExecError::OsError(err)) => {
                    self.eprint(&format!("{err}\n"));
                    std::process::exit(1)
                }
                Err(ExecError::CannotExecute(_)) => std::process::exit(126),
                Ok(_) => unreachable!(),
            },
            ForkResult::Parent { child } => match waitpid(child)? {
                WaitStatus::Exited(_, status) => Ok(status),
                _ => todo!(),
            },
        }
    }

    fn perform_assignments(
        &mut self,
        assignments: &[Assignment],
        export: bool,
    ) -> CommandExecutionResult<()> {
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value, true, self)?;
            self.assign(assignment.name.to_string(), Some(word_str), export, false)?;
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
        self.perform_assignments(&simple_command.assignments, true)?;
        let mut opened_files = self.opened_files.clone();
        if let Err(err) = opened_files.redirect(&simple_command.redirections, self) {
            if !self.is_interactive {
                self.eprint(&err.to_string());
                std::process::exit(1)
            }
            return Err(err);
        }
        match special_builtin_utility.exec(args, self, &mut opened_files) {
            Ok(status) => Ok(status),
            Err(err) => {
                opened_files.stderr().write_str(&format!("{err}\n"));
                if !self.is_interactive {
                    std::process::exit(1)
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
        let mut args = expanded_words[1..].to_vec();
        // assignments affect the current environment and are marked for export,
        // same as special builtin utilities
        self.perform_assignments(&simple_command.assignments, true)?;
        let mut previous_opened_files = self.opened_files.clone();
        previous_opened_files.redirect(&simple_command.redirections, self)?;
        std::mem::swap(&mut self.opened_files, &mut previous_opened_files);
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
        let mut command_env = self.environment.clone();
        self.perform_assignments(&simple_command.assignments, false)?;
        std::mem::swap(&mut self.environment, &mut command_env);
        match builtin_utility.exec(args, self, &mut opened_files, command_env) {
            Ok(status) => Ok(status),
            Err(err) => {
                opened_files.stderr().write_str(format!("{err}\n"));
                Ok(1)
            }
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
        for word in &simple_command.words {
            expanded_words.extend(expand_word(word, false, self)?);
        }
        if expanded_words.is_empty() {
            // no commands to execute, perform assignments and redirections
            self.perform_assignments(&simple_command.assignments, false)?;
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
            let path = self.environment.get_str_value("PATH").unwrap_or_default();
            let command = if let Some(command) = find_command(&expanded_words[0], path) {
                command
            } else {
                return Err(CommandExecutionError::CommandNotFound(
                    expanded_words[0].to_string(),
                ));
            };

            let mut command_environment = self.clone();
            command_environment.perform_assignments(&simple_command.assignments, true)?;
            command_environment
                .opened_files
                .redirect(&simple_command.redirections, self)?;
            let arguments = expanded_words
                .iter()
                .map(|w| w.clone())
                .collect::<Vec<String>>();
            command_environment
                .exec(command, &arguments)
                .map_err(|err| err.into())
        }
    }

    fn interpret_for_clause(
        &mut self,
        iter_var: Name,
        iter_words: &[Word],
        body: &CompleteCommand,
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let mut result = 0;
        self.loop_depth += 1;
        'outer: for word in iter_words {
            let items = expand_word(word, false, self)?;
            for item in items {
                self.assign(iter_var.to_string(), Some(item), false, false)?;
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
        arg: &Word,
        cases: &[CaseItem],
        ignore_errexit: bool,
    ) -> CommandExecutionResult<i32> {
        let arg = expand_word_to_string(arg, false, self)?;
        let arg_cstr = CString::new(arg).expect("invalid pattern");
        for case in cases {
            for pattern in &case.pattern {
                let pattern = word_to_pattern(pattern, self)?;
                if pattern.matches(&arg_cstr) {
                    return Ok(self.interpret(&case.body, ignore_errexit));
                }
            }
        }
        Ok(0)
    }

    fn interpret_if_clause(
        &mut self,
        if_chain: &[If],
        else_body: &Option<CompleteCommand>,
        ignore_errexit: bool,
    ) -> i32 {
        assert!(!if_chain.is_empty(), "parsed if without else");
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
                std::process::exit(self.interpret(commands, false));
            }
            ForkResult::Parent { child } => match waitpid(child)? {
                WaitStatus::Exited(_, status) => Ok(status),
                _ => todo!(),
            },
        }
    }

    fn interpret_compound_command(
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
        self.environment
            .set_forced("LINENO", command.lineno.to_string());
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
        let pipeline_exit_status;
        if pipeline.commands.len() == 1 {
            let command = &pipeline.commands[0];
            pipeline_exit_status = self.interpret_command(command, ignore_errexit);
        } else {
            let mut current_stdin = libc::STDIN_FILENO;
            for command in pipeline.commands.iter().take(pipeline.commands.len() - 1) {
                let (read_pipe, write_pipe) = pipe()?;
                match fork()? {
                    ForkResult::Child => {
                        drop(read_pipe);
                        dup2(current_stdin, libc::STDIN_FILENO)?;
                        dup2(write_pipe.as_raw_fd(), libc::STDOUT_FILENO)?;
                        let return_status = self.interpret_command(command, false);
                        if current_stdin != libc::STDIN_FILENO {
                            close(current_stdin)?;
                        }
                        std::process::exit(return_status);
                    }
                    ForkResult::Parent { .. } => {
                        if current_stdin != libc::STDIN_FILENO {
                            close(current_stdin)?;
                        }
                        current_stdin = read_pipe.into_raw_fd();
                    }
                }
            }

            match fork()? {
                ForkResult::Child => {
                    dup2(current_stdin, libc::STDIN_FILENO)?;
                    let return_status =
                        self.interpret_command(pipeline.commands.last().unwrap(), false);
                    close(current_stdin)?;
                    std::process::exit(return_status);
                }
                ForkResult::Parent { child } => {
                    close(current_stdin)?;
                    match waitpid(child)? {
                        WaitStatus::Exited(_, status) => pipeline_exit_status = status,
                        _ => todo!(),
                    }
                }
            }
        }
        self.last_pipeline_exit_status = if pipeline.negate_status {
            (pipeline_exit_status == 0) as i32
        } else {
            if pipeline_exit_status != 0 && !ignore_errexit && self.set_options.errexit {
                std::process::exit(pipeline_exit_status)
            }
            pipeline_exit_status
        };
        Ok(self.last_pipeline_exit_status)
    }

    fn interpret_conjunction(&mut self, conjunction: &Conjunction, ignore_errexit: bool) -> i32 {
        let mut status = 0;
        let mut i = 0;
        while i < conjunction.elements.len() {
            let (pipeline, op) = &conjunction.elements[i];
            let ignore_errexit = i == conjunction.elements.len() - 1 && ignore_errexit;
            status = match self.interpret_pipeline(pipeline, ignore_errexit) {
                Ok(status) => status,
                Err(err) => {
                    self.eprint(&format!("{err}\n"));
                    std::process::exit(1)
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
                drop(read_pipe);
                dup2(write_pipe.as_raw_fd(), libc::STDOUT_FILENO)?;
                self.execute_program(program)
                    .map_err(CommandExecutionError::ParseError)?;
                std::process::exit(self.last_pipeline_exit_status);
            }
            ForkResult::Parent { child } => {
                drop(write_pipe);
                match waitpid(child)? {
                    WaitStatus::Exited(_, _) => {
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
        let mut parser = CommandParser::new(program, self.last_lineno)?;
        let mut result = 0;
        loop {
            let command = parser.parse_next_command(&self.alias_table)?;
            if !self.is_interactive && self.set_options.noexec {
                return Ok(result);
            }
            if let Some(command) = command {
                result = self.interpret(&command, false);
                if self.control_flow_state == ControlFlowState::Return {
                    self.control_flow_state = ControlFlowState::None;
                    return Ok(result);
                }
            } else {
                break;
            }
        }
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
        let environment = Environment::from(
            std::env::vars()
                .into_iter()
                .map(|(k, v)| (k, Value::new_exported(v))),
        );

        let mut shell = Shell {
            environment,
            program_name,
            positional_parameters: args,
            shell_pid: getpid().as_raw(),
            // TODO: handle error
            current_directory: std::env::current_dir().unwrap().into_os_string(),
            set_options,
            is_interactive,
            ..Default::default()
        };
        shell
            .assign(
                "PPID".to_string(),
                Some(getppid().to_string()),
                false,
                false,
            )
            .unwrap();
        shell
            .assign("IFS".to_string(), Some(" \t\n".to_string()), false, false)
            .unwrap();
        shell
            .assign("PS1".to_string(), Some("\\$ ".to_string()), false, false)
            .unwrap();
        shell
            .assign("PS2".to_string(), Some("> ".to_string()), false, false)
            .unwrap();
        shell
            .assign("PS4".to_string(), Some("+ ".to_string()), false, false)
            .unwrap();
        shell
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
                    std::process::exit(1)
                }
                default_if_err.to_string()
            }
        }
    }

    pub fn get_ps1(&mut self) -> String {
        self.get_var_and_expand("PS1", "\\$ ")
    }

    pub fn get_ps2(&mut self) -> String {
        self.get_var_and_expand("PS2", "> ")
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
            most_recent_background_command_pid: None,
            current_directory: OsString::from("/"),
            set_options: SetOptions::default(),
            alias_table: AliasTable::default(),
            control_flow_state: ControlFlowState::None,
            loop_depth: 0,
            function_call_depth: 0,
            dot_script_depth: 0,
            is_interactive: false,
            last_lineno: 0,
        }
    }
}
