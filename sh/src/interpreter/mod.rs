use crate::interpreter::wordexp::{expand_word, expand_word_to_string};
use crate::program::{
    Command, CompleteCommand, Conjunction, IORedirectionKind, LogicalOp, Pipeline, Program,
    Redirection, RedirectionKind, SimpleCommand,
};
use std::collections::HashMap;
use std::ffi::{c_char, CString, OsString};
use std::os::fd::{AsRawFd, IntoRawFd};
use std::rc::Rc;

mod wordexp;

fn is_portable_filename_character(c: char) -> bool {
    // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    c.is_ascii_alphanumeric() || "._-".contains(c)
}

fn is_ifs_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n'
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

pub type Environment = HashMap<String, Variable>;

#[derive(Clone)]
pub struct Interpreter {
    environment: Environment,
    opened_files: HashMap<u32, Rc<std::fs::File>>,
    most_recent_pipeline_status: i32,
    last_command_substitution_status: i32,
    shell_pid: i32,
    most_recent_background_command_pid: i32,
    current_directory: OsString,
}

impl Interpreter {
    fn exec(&self, command: &str, args: &[String]) -> i32 {
        let pid = unsafe { libc::fork() };
        if pid < 0 {
            todo!("error: fork failed")
        } else if pid == 0 {
            // child
            for (id, file) in &self.opened_files {
                let dest = *id as i32;
                let src = file.as_raw_fd();
                unsafe { libc::dup2(src, dest) };
            }

            let command = CString::new(command).unwrap();
            let args = args
                .iter()
                .map(|s| CString::new(s.as_str()).unwrap())
                .collect::<Vec<_>>();
            let args = std::iter::once(command.as_ptr())
                .chain(args.iter().map(|s| s.as_ptr()))
                .chain(std::iter::once(std::ptr::null() as *const c_char))
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
            let env = env
                .iter()
                .map(|s| s.as_ptr())
                .chain(std::iter::once(std::ptr::null()))
                .collect::<Vec<_>>();
            unsafe { libc::execve(command.as_ptr(), args.as_ptr(), env.as_ptr()) }
        } else {
            // parent
            let mut status = 0;
            let wait_result = unsafe { libc::waitpid(pid, &mut status, 0) };
            if wait_result != pid {
                panic!("failed to wait for child process");
            }
            libc::WEXITSTATUS(status)
        }
    }

    fn interpret_complete_command_to_string(&self, complete_command: &CompleteCommand) -> String {
        todo!()
    }

    fn perform_redirection(&mut self, redirection: &Redirection) {
        match &redirection.kind {
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
                        let source_fd = redirection
                            .file_descriptor
                            .unwrap_or(libc::STDOUT_FILENO as u32);
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

    fn interpret_simple_command(&mut self, simple_command: &SimpleCommand) -> i32 {
        // > 1. The words that are recognized as variable assignments or redirections according to
        // >    Shell Grammar Rules are saved for processing in steps 3 and 4.
        // > 2. The words that are not variable assignments or redirections shall be expanded. If
        // >    any fields remain following their expansion, the first field shall be considered
        // >    the command name and remaining fields are the arguments for the command.
        // > 4. Redirections shall be performed [..].
        // > 5. Each variable assignment shall be expanded for tilde expansion, parameter expansion,
        // >    command substitution, arithmetic expansion, and quote removal prior to assigning the
        // >    value.
        let mut expanded_words = Vec::new();
        for word in &simple_command.words {
            expanded_words.extend(expand_word(word, false, self));
        }
        if expanded_words.is_empty() {
            // > If there is no command name, any redirections shall be performed in a
            // > subshell environment;
            if !simple_command.redirections.is_empty() {
                let mut subshell = self.clone();
                for redirection in &simple_command.redirections {
                    subshell.perform_redirection(redirection);
                }
            }
            // > If no command name results, variable assignments shall affect the current
            // > execution environment.
            for assignment in &simple_command.assignments {
                let word_str = expand_word_to_string(&assignment.value, true, self);
                self.environment
                    .insert(assignment.name.to_string(), Variable::new(word_str));
            }
            // > If there is no command name, but the command contained a command substitution,
            // > the command shall complete with the exit status of the last command substitution
            // > performed.
            return self.last_command_substitution_status;
        }

        // TODO: consider all other cases specified in Command Search and Execution
        let mut command_environment = self.clone();
        for assignment in &simple_command.assignments {
            let word_str = expand_word_to_string(&assignment.value, true, self);
            command_environment.environment.insert(
                assignment.name.to_string(),
                Variable::new_exported(word_str),
            );
        }

        for redirection in &simple_command.redirections {
            command_environment.perform_redirection(redirection);
        }
        let command = expanded_words[0].clone().to_string();
        let arguments = expanded_words[1..]
            .iter()
            .map(|w| w.clone().to_string())
            .collect::<Vec<String>>();
        self.exec(&command, &arguments)
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
        if pipeline.commands.len() > 1 {
            todo!()
        }
        let command = &pipeline.commands[0];
        let status = self.interpret_command(command);
        if pipeline.negate_status {
            (status == 0) as i32
        } else {
            status
        }
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

    fn interpret_complete_command(&mut self, command: &CompleteCommand) {
        for conjunction in &command.commands {
            self.interpret_conjunction(conjunction);
        }
    }

    pub fn interpret(&mut self, program: Program) {
        for command in &program.commands {
            self.interpret_complete_command(command);
        }
    }

    pub fn initialize_from_system() -> Interpreter {
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let variables = std::env::vars()
            .into_iter()
            .map(|(k, v)| (k, Variable::new_exported(v)))
            .collect();
        let pid = unsafe { libc::getpid() };
        Interpreter {
            environment: variables,
            opened_files: HashMap::new(),
            most_recent_pipeline_status: 0,
            last_command_substitution_status: 0,
            shell_pid: pid,
            most_recent_background_command_pid: 0,
            // TODO: handle error
            current_directory: std::env::current_dir().unwrap().into_os_string(),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter {
            environment: Environment::default(),
            opened_files: HashMap::new(),
            most_recent_pipeline_status: 0,
            last_command_substitution_status: 0,
            shell_pid: 0,
            most_recent_background_command_pid: 0,
            current_directory: OsString::from("/"),
        }
    }
}
