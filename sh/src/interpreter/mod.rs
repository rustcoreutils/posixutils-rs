use crate::interpreter::set::{SetOptions, SetSpecialBuiltin};
use crate::interpreter::wordexp::{expand_word, expand_word_to_string};
use crate::parse::command::{
    Assignment, Command, CompleteCommand, CompoundCommand, Conjunction, IORedirectionKind,
    LogicalOp, Name, Pipeline, Program, Redirection, RedirectionKind, SimpleCommand,
};
use nix::libc;
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{close, dup2, execve, fork, getpid, getppid, pipe, ForkResult, Pid};
use std::collections::HashMap;
use std::ffi::{CString, OsString};
use std::os::fd::{AsRawFd, IntoRawFd};
use std::path::PathBuf;
use std::rc::Rc;

pub mod set;
mod wordexp;

trait BuiltinUtility {
    fn exec(&self, args: &[String], interpreter: &mut Interpreter) -> i32;
}

fn get_special_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "set" => Some(&SetSpecialBuiltin),
        _ => None,
    }
}

fn get_bultin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        _ => None,
    }
}

#[derive(Clone)]
struct Variable {
    value: String,
    export: bool,
}

impl Variable {
    fn new_exported(value: String) -> Self {
        Variable {
            value,
            export: true,
        }
    }

    fn new(value: String) -> Self {
        Variable {
            value,
            export: false,
        }
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

pub type Environment = HashMap<String, Variable>;

#[derive(Clone)]
pub struct Interpreter {
    environment: Environment,
    program_name: String,
    positional_parameters: Vec<String>,
    opened_files: HashMap<u32, Rc<std::fs::File>>,
    functions: HashMap<Name, Rc<CompoundCommand>>,
    most_recent_pipeline_exit_status: i32,
    last_command_substitution_status: i32,
    shell_pid: i32,
    most_recent_background_command_pid: Option<i32>,
    current_directory: OsString,
    set_options: SetOptions,
}

impl Interpreter {
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
                            Some(CString::new(format!("{name}={}", value.value)).unwrap())
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

    fn perform_assignments(&mut self, assignments: &[Assignment]) {
        for assignment in assignments {
            let word_str = expand_word_to_string(&assignment.value, true, self);
            self.environment
                .insert(assignment.name.to_string(), Variable::new(word_str));
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
            self.perform_assignments(&simple_command.assignments);
            if !simple_command.redirections.is_empty() {
                let mut subshell = self.clone();
                subshell.perform_redirections(&simple_command.redirections);
            }
            return self.last_command_substitution_status;
        }

        if expanded_words[0].contains('/') {
            let mut command_environment = self.clone();
            command_environment.perform_assignments(&simple_command.assignments);
            command_environment.perform_redirections(&simple_command.redirections);
            let command = &expanded_words[0];
            let arguments = expanded_words
                .iter()
                .map(|w| w.clone())
                .collect::<Vec<String>>();
            command_environment.exec(&command, &arguments)
        } else {
            if let Some(special_builtin_utility) = get_special_builtin_utility(&expanded_words[0]) {
                self.perform_assignments(&simple_command.assignments);
                return special_builtin_utility.exec(&expanded_words[1..], self);
            }

            if let Some(_function_body) = self.functions.get(expanded_words[0].as_str()) {
                self.perform_assignments(&simple_command.assignments);
                todo!()
            }

            if let Some(_builtin_utility) = get_bultin_utility(&expanded_words[0]) {
                todo!()
            }

            let mut command_environment = self.clone();
            command_environment.perform_assignments(&simple_command.assignments);
            command_environment.perform_redirections(&simple_command.redirections);
            if let Some(command) = find_in_path(
                &expanded_words[0],
                &self.environment.get("PATH").unwrap().value,
            ) {
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

    fn interpret_command(&mut self, command: &Command) -> i32 {
        match command {
            Command::SimpleCommand(simple_command) => self.interpret_simple_command(simple_command),
            Command::CompoundCommand { .. } => {
                todo!()
            }
            _ => todo!("not implemented"),
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
        for (pipeline, op) in &conjunction.elements {
            status = self.interpret_pipeline(pipeline);
            if status != 0 && *op == LogicalOp::And {
                break;
            } else if status == 0 && *op == LogicalOp::Or {
                break;
            }
        }
        status
    }

    pub fn interpret(&mut self, command: &CompleteCommand) {
        for conjunction in &command.commands {
            self.interpret_conjunction(conjunction);
        }
    }

    pub fn initialize_from_system(
        program_name: String,
        args: Vec<String>,
        set_options: SetOptions,
    ) -> Interpreter {
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let mut variables: Environment = std::env::vars()
            .into_iter()
            .map(|(k, v)| (k, Variable::new_exported(v)))
            .collect();
        variables.insert("PPID".to_string(), Variable::new(getppid().to_string()));
        variables.insert("IFS".to_string(), Variable::new(" \t\n".to_string()));
        variables.insert("PS1".to_string(), Variable::new("$ ".to_string()));
        variables.insert("PS2".to_string(), Variable::new("> ".to_string()));
        variables.insert("PS4".to_string(), Variable::new("+ ".to_string()));
        Interpreter {
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

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter {
            environment: Environment::from([(
                "IFS".to_string(),
                Variable::new(" \t\n".to_string()),
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
        }
    }
}
