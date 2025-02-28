use crate::builtin::set::SetOptions;
use crate::builtin::{get_builtin_utility, get_special_builtin_utility};
use crate::parse::command::{
    Assignment, Command, CompleteCommand, CompoundCommand, Conjunction, FunctionDefinition,
    IORedirectionKind, LogicalOp, Name, Pipeline, Redirection, RedirectionKind, SimpleCommand,
};
use crate::parse::command_parser::CommandParser;
use crate::parse::{AliasTable, ParseResult, ParserError};
use crate::wordexp::{expand_word, expand_word_to_string};
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{close, dup2, execve, fork, getpid, getppid, pipe, ForkResult};
use nix::{libc, NixPath};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ffi::{CString, OsString};
use std::os::fd::{AsRawFd, IntoRawFd};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone)]
pub struct VariableValue {
    /// `None` if `Variable` is unset
    pub value: Option<String>,
    pub export: bool,
    pub readonly: bool,
}

impl VariableValue {
    pub fn new_exported(value: String) -> Self {
        VariableValue {
            value: Some(value),
            export: true,
            readonly: false,
        }
    }

    pub fn new(value: String) -> Self {
        VariableValue {
            value: Some(value),
            export: false,
            readonly: false,
        }
    }

    pub fn is_null(&self) -> bool {
        self.value.as_ref().is_some_and(|v| v.is_empty())
    }
}

fn find_in_path(command: &str, env_path: &str) -> Option<String> {
    for path in env_path.split(':') {
        let mut command_path = PathBuf::from(path);
        command_path.push(command);
        if command_path.is_file() {
            return Some(command_path.into_os_string().to_string_lossy().into());
        }
    }
    None
}

pub type Environment = HashMap<String, VariableValue>;

#[derive(Clone, Debug)]
pub enum ExecutionError {
    ParserError(ParserError),
}

impl From<ParserError> for ExecutionError {
    fn from(value: ParserError) -> Self {
        Self::ParserError(value)
    }
}

#[derive(Clone)]
pub struct Shell {
    pub environment: Environment,
    pub program_name: String,
    pub positional_parameters: Vec<String>,
    pub opened_files: HashMap<u32, Rc<std::fs::File>>,
    pub functions: HashMap<Name, Rc<CompoundCommand>>,
    pub most_recent_pipeline_exit_status: i32,
    pub last_command_substitution_status: i32,
    pub shell_pid: i32,
    pub most_recent_background_command_pid: Option<i32>,
    pub current_directory: OsString,
    pub set_options: SetOptions,
    pub alias_table: AliasTable,
}

impl Shell {
    fn exec(&self, command: &str, args: &[String]) -> i32 {
        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                for (id, file) in &self.opened_files {
                    let dest = *id as i32;
                    let src = file.as_raw_fd();
                    dup2(src, dest).expect("TODO: handle dup2 error");
                }
                let command = CString::new(command).unwrap();
                let args = args
                    .iter()
                    .map(|s| CString::new(s.as_str()).unwrap())
                    .collect::<Vec<_>>();
                let env = self
                    .environment
                    .iter()
                    .filter_map(|(name, value)| {
                        if value.export {
                            // TODO: look into this unwrap
                            value
                                .value
                                .as_ref()
                                .map(|v| CString::new(format!("{name}={}", v)).unwrap())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<CString>>();
                // can never fail
                execve(&command, &args, &env).unwrap();
                unreachable!();
            }
            Ok(ForkResult::Parent { child }) => match waitpid(child, None) {
                Ok(WaitStatus::Exited(_, status)) => status,
                Err(_) => {
                    todo!("failed to wait for child process");
                }
                _ => todo!(),
            },
            Err(_) => {
                todo!("error: fork failed");
            }
        }
    }

    fn perform_redirections(&mut self, redirections: &[Redirection]) {
        for redir in redirections {
            match &redir.kind {
                RedirectionKind::IORedirection { kind, file } => {
                    // > the word that follows the redirection operator shall be subjected to tilde
                    // > expansion, parameter expansion, command substitution, arithmetic expansion,
                    // > and quote removal.
                    let path = expand_word_to_string(file, false, self);
                    // TODO: pathname expansion is not allowed if the shell is non-interactive,
                    // optional otherwise. Bash does implement this, maybe we should too.
                    match kind {
                        IORedirectionKind::RedirectOutput
                        | IORedirectionKind::RedirectOutputClobber
                        | IORedirectionKind::RedirectOuputAppend => {
                            // TODO: fail if noclobber is set and file exists and is a regular file

                            // TODO: fix unwrap
                            let file = if *kind == IORedirectionKind::RedirectOuputAppend {
                                std::fs::OpenOptions::new()
                                    .append(true)
                                    .create(true)
                                    .open(path)
                                    .unwrap()
                            } else {
                                std::fs::File::create(path).unwrap()
                            };
                            let source_fd =
                                redir.file_descriptor.unwrap_or(libc::STDOUT_FILENO as u32);
                            self.opened_files.insert(source_fd, Rc::new(file));
                        }
                        IORedirectionKind::DuplicateOutput => {}
                        IORedirectionKind::RedirectInput => {}
                        IORedirectionKind::DuplicateInput => {}
                        IORedirectionKind::OpenRW => {}
                    }
                }
                RedirectionKind::HereDocument { contents } => {}
            }
        }
    }

    fn perform_assignments(&mut self, assignments: &[Assignment], export: bool) {
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value, true, self);
            match self.environment.entry(assignment.name.to_string()) {
                Entry::Occupied(mut e) => {
                    if e.get().readonly {
                        todo!("error")
                    }
                    e.get_mut().value = Some(word_str);
                    e.get_mut().export = e.get_mut().export || export;
                }
                Entry::Vacant(e) => {
                    e.insert(VariableValue {
                        value: Some(word_str),
                        export,
                        readonly: false,
                    });
                }
            }
        }
    }

    fn interpret_simple_command(&mut self, simple_command: &SimpleCommand) -> i32 {
        let mut expanded_words = Vec::new();
        // reset
        self.last_command_substitution_status = 0;
        for word in &simple_command.words {
            expanded_words.extend(expand_word(word, false, self));
        }
        if expanded_words.is_empty() {
            // no commands to execute, perform assignments and redirections
            self.perform_assignments(&simple_command.assignments, false);
            if !simple_command.redirections.is_empty() {
                let mut subshell = self.clone();
                subshell.perform_redirections(&simple_command.redirections);
            }
            return self.last_command_substitution_status;
        }

        if expanded_words[0].contains('/') {
            let mut command_environment = self.clone();
            command_environment.perform_assignments(&simple_command.assignments, true);
            command_environment.perform_redirections(&simple_command.redirections);
            let command = &expanded_words[0];
            let arguments = expanded_words
                .iter()
                .map(|w| w.clone())
                .collect::<Vec<String>>();
            command_environment.exec(&command, &arguments)
        } else {
            if let Some(special_builtin_utility) = get_special_builtin_utility(&expanded_words[0]) {
                // the standard does not specify if the variables should have the export attribute.
                // Bash exports them, we do the same here (neither sh, nor zsh do it though)
                self.perform_assignments(&simple_command.assignments, true);
                return special_builtin_utility.exec(&expanded_words[1..], self);
            }

            if let Some(function_body) = self.functions.get(expanded_words[0].as_str()).cloned() {
                let mut args = expanded_words[1..].to_vec();
                std::mem::swap(&mut args, &mut self.positional_parameters);
                let result =
                    self.interpret_compound_command(&function_body, &simple_command.redirections);
                std::mem::swap(&mut args, &mut self.positional_parameters);
                return result;
            }

            if let Some(builtin_utility) = get_builtin_utility(&expanded_words[0]) {
                return builtin_utility.exec(&expanded_words[1..], self);
            }

            let mut command_environment = self.clone();
            command_environment.perform_assignments(&simple_command.assignments, true);
            command_environment.perform_redirections(&simple_command.redirections);
            // TODO: fix unwrap with proper error
            let path = self.get_variable_value("PATH").unwrap();
            if let Some(command) = find_in_path(&expanded_words[0], path) {
                let arguments = expanded_words
                    .iter()
                    .map(|w| w.clone())
                    .collect::<Vec<String>>();
                command_environment.exec(&command, &arguments)
            } else {
                eprintln!("{}: command not found", expanded_words[0]);
                127
            }
        }
    }

    fn interpret_compound_command(
        &mut self,
        compound_command: &CompoundCommand,
        redirections: &[Redirection],
    ) -> i32 {
        match compound_command {
            CompoundCommand::BraceGroup(command) => self.interpret(command),
            CompoundCommand::Subshell(_) => {
                todo!()
            }
            CompoundCommand::ForClause { .. } => {
                todo!()
            }
            CompoundCommand::CaseClause { .. } => {
                todo!()
            }
            CompoundCommand::IfClause { .. } => {
                todo!()
            }
            CompoundCommand::WhileClause { .. } => {
                todo!()
            }
            CompoundCommand::UntilClause { .. } => {
                todo!()
            }
        }
    }

    fn define_function(&mut self, definition: &FunctionDefinition) -> i32 {
        self.functions
            .insert(definition.name.clone(), definition.body.clone());
        0
    }

    fn interpret_command(&mut self, command: &Command) -> i32 {
        match command {
            Command::SimpleCommand(simple_command) => self.interpret_simple_command(simple_command),
            Command::CompoundCommand {
                command,
                redirections,
            } => self.interpret_compound_command(command, redirections),
            Command::FunctionDefinition(function) => self.define_function(function),
        }
    }

    fn interpret_pipeline(&mut self, pipeline: &Pipeline) -> i32 {
        let pipeline_exit_status;
        if pipeline.commands.len() == 1 {
            let command = &pipeline.commands[0];
            pipeline_exit_status = self.interpret_command(command);
        } else {
            let mut current_stdin = libc::STDIN_FILENO;
            for command in pipeline.commands.iter().take(pipeline.commands.len() - 1) {
                let (read_pipe, write_pipe) = pipe().unwrap();
                match unsafe { fork() } {
                    Ok(ForkResult::Child) => {
                        drop(read_pipe);
                        dup2(current_stdin, libc::STDIN_FILENO);
                        dup2(write_pipe.as_raw_fd(), libc::STDOUT_FILENO);
                        let return_status = self.interpret_command(command);
                        if current_stdin != libc::STDIN_FILENO {
                            close(current_stdin);
                        }
                        std::process::exit(return_status);
                    }
                    Ok(ForkResult::Parent { .. }) => {
                        if current_stdin != libc::STDIN_FILENO {
                            close(current_stdin);
                        }
                        current_stdin = read_pipe.into_raw_fd();
                    }
                    Err(_) => {
                        todo!("failed to fork")
                    }
                }
            }

            match unsafe { fork() } {
                Ok(ForkResult::Child) => {
                    dup2(current_stdin, libc::STDIN_FILENO).unwrap();
                    let return_status = self.interpret_command(pipeline.commands.last().unwrap());
                    close(current_stdin);
                    std::process::exit(return_status);
                }
                Ok(ForkResult::Parent { child }) => {
                    close(current_stdin);
                    match waitpid(child, None) {
                        Ok(WaitStatus::Exited(_, status)) => pipeline_exit_status = status,
                        Err(_) => {
                            todo!("failed to wait for child process");
                        }
                        _ => todo!(),
                    }
                }
                Err(_) => {
                    todo!("handle fork error")
                }
            }
        }
        self.most_recent_pipeline_exit_status = if pipeline.negate_status {
            (pipeline_exit_status == 0) as i32
        } else {
            pipeline_exit_status
        };
        self.most_recent_pipeline_exit_status
    }

    fn interpret_conjunction(&mut self, conjunction: &Conjunction) -> i32 {
        let mut status = 0;
        let mut i = 0;
        while i < conjunction.elements.len() {
            let (pipeline, op) = &conjunction.elements[i];
            status = self.interpret_pipeline(pipeline);
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

    fn interpret(&mut self, command: &CompleteCommand) -> i32 {
        let mut result = 0;
        for conjunction in &command.commands {
            result = self.interpret_conjunction(conjunction)
        }
        result
    }

    pub fn get_variable_value(&self, name: &str) -> Option<&str> {
        self.environment
            .get(name)
            .map(|var| var.value.as_ref().map(|v| v.as_str()))
            .flatten()
    }

    pub fn execute_program(&mut self, program: &str) -> Result<(), ExecutionError> {
        let mut parser = CommandParser::new(program)?;
        loop {
            let command = parser.parse_next_command(&self.alias_table)?;
            if let Some(command) = command {
                self.interpret(&command);
            } else {
                break;
            }
        }
        Ok(())
    }

    pub fn initialize_from_system(
        program_name: String,
        args: Vec<String>,
        set_options: SetOptions,
    ) -> Shell {
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let mut variables: Environment = std::env::vars()
            .into_iter()
            .map(|(k, v)| (k, VariableValue::new_exported(v)))
            .collect();
        variables.insert(
            "PPID".to_string(),
            VariableValue::new(getppid().to_string()),
        );
        variables.insert("IFS".to_string(), VariableValue::new(" \t\n".to_string()));
        variables.insert("PS1".to_string(), VariableValue::new("$ ".to_string()));
        variables.insert("PS2".to_string(), VariableValue::new("> ".to_string()));
        variables.insert("PS4".to_string(), VariableValue::new("+ ".to_string()));
        Shell {
            environment: variables,
            program_name,
            positional_parameters: args,
            shell_pid: getpid().as_raw(),
            // TODO: handle error
            current_directory: std::env::current_dir().unwrap().into_os_string(),
            set_options,
            ..Default::default()
        }
    }
}

impl Default for Shell {
    fn default() -> Self {
        Shell {
            environment: Environment::from([(
                "IFS".to_string(),
                VariableValue::new(" \t\n".to_string()),
            )]),
            program_name: "sh".to_string(),
            positional_parameters: Vec::default(),
            opened_files: HashMap::default(),
            functions: HashMap::default(),
            most_recent_pipeline_exit_status: 0,
            last_command_substitution_status: 0,
            shell_pid: 0,
            most_recent_background_command_pid: None,
            current_directory: OsString::from("/"),
            set_options: SetOptions::default(),
            alias_table: AliasTable::default(),
        }
    }
}
