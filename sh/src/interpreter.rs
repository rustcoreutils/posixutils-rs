use crate::program::{
    Command, CompleteCommand, CompoundCommand, Conjunction, IORedirectionKind, LogicalOp,
    Parameter, ParameterExpansion, Pipeline, Program, Redirection, RedirectionKind, SimpleCommand,
    SpecialParameter, Word, WordPart,
};
use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};
use std::os::fd::{AsRawFd, IntoRawFd, RawFd};
use std::rc::Rc;

trait SystemInterface {
    fn environment_variables(&self) -> HashMap<String, String>;
    /// # Panics
    /// Panics if `login_name` contains non-portable filename characters as defined in
    /// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    fn get_user_home(&self, login_name: &str) -> Option<String>;
    fn get_pid(&self) -> i32;

    // TODO: change args to something better
    fn exec(&self, command: &str, args: &[String], interpreter: &Interpreter) -> i32;
}

#[derive(Default)]
struct System {}

impl SystemInterface for System {
    fn environment_variables(&self) -> HashMap<String, String> {
        std::env::vars().collect()
    }

    fn get_user_home(&self, login_name: &str) -> Option<String> {
        // it cannot contain a null char as part of the method's contract
        let login_name = CString::new(login_name).unwrap();
        let passwd = unsafe { libc::getpwnam(login_name.as_ptr()) };
        if passwd == std::ptr::null_mut() {
            return None;
        }
        // this is safe, since the pointer is not null
        // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getpwnam.html
        let user_home_dir = unsafe { CStr::from_ptr((*passwd).pw_dir as *const c_char) };
        Some(user_home_dir.to_string_lossy().into_owned())
    }

    fn get_pid(&self) -> i32 {
        unsafe { libc::getpid() }
    }

    fn exec(&self, command: &str, args: &[String], interpreter: &Interpreter) -> i32 {
        let pid = unsafe { libc::fork() };
        if pid < 0 {
            todo!("error: fork failed")
        } else if pid == 0 {
            // child
            for (id, file) in &interpreter.opened_files {
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

            let env = interpreter
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
}

fn is_portable_filename_character(c: char) -> bool {
    // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    c.is_ascii_alphanumeric() || "._-".contains(c)
}

fn remove_prefix_pattern(mut parameter: Vec<u8>, pattern: CString, remove_largest: bool) -> String {
    let mut prefix_end = 0;
    parameter.push(b'\0');
    for i in 1..parameter.len() {
        let temp = parameter[i];
        parameter[i] = b'\0';
        let match_result = unsafe {
            libc::fnmatch(
                pattern.as_ptr(),
                parameter[..=i].as_ptr() as *const c_char,
                0,
            )
        };
        parameter[i] = temp;
        if match_result == 0 {
            prefix_end = i;
            if !remove_largest {
                break;
            }
        }
    }
    parameter.drain(..prefix_end);
    // remove '\0' at the end
    parameter.pop();
    // if this fails, the above code is wrong
    String::from_utf8(parameter).unwrap()
}

fn remove_suffix_pattern(mut parameter: Vec<u8>, pattern: CString, remove_largest: bool) -> String {
    parameter.push(b'\0');
    let mut suffix_end = parameter.len() - 1;
    for i in (0..parameter.len() - 2).rev() {
        let match_result = unsafe {
            libc::fnmatch(
                pattern.as_ptr(),
                parameter[i..].as_ptr() as *const c_char,
                0,
            )
        };
        if match_result == 0 {
            suffix_end = i;
            if !remove_largest {
                break;
            }
        }
    }
    parameter.truncate(suffix_end);
    // if this fails, the above code is wrong
    String::from_utf8(parameter).unwrap()
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

#[derive(Clone)]
pub struct Interpreter {
    environment: HashMap<String, Variable>,
    opened_files: HashMap<u32, Rc<std::fs::File>>,
    most_recent_pipeline_status: i32,
    last_command_substitution_status: i32,
    shell_pid: i32,
    most_recent_background_command_pid: i32,
    system: Rc<dyn SystemInterface>,
}

impl Interpreter {
    fn expand_home(&self, login_name: &str) -> String {
        if login_name.is_empty() {
            // > If the login name is null (that is, the tilde-prefix contains only the tilde),
            // > the tilde-prefix is replaced by the value of the variable HOME
            self.environment
                .get("HOME")
                .map(|v| v.value.clone())
                .unwrap_or_else(|| todo!("error: HOME not set"))
        } else {
            if !login_name.chars().all(is_portable_filename_character) {
                todo!("error: invalid character in login name")
            }
            self.system
                .get_user_home(login_name)
                .unwrap_or_else(|| todo!("error: login name not found"))
        }
    }

    /// performs tilde expansion on `unquoted_start`. Assumes that `unquoted_start` starts with
    /// `~`
    fn tilde_expansion_simple(&mut self, unquoted_start: &str, is_assignment: bool) -> String {
        if is_assignment {
            let mut result = String::with_capacity(unquoted_start.len());
            for sub in unquoted_start.split(':') {
                if sub.starts_with('~') {
                    let prefix_end = sub.find('/').unwrap_or(sub.len());
                    let login_name = &sub[1..prefix_end];
                    result += &self.expand_home(login_name);
                    result += &sub[prefix_end..];
                } else {
                    result += sub
                }
                result.push(':');
            }
            // removes last ':'
            result.pop();
            result
        } else {
            let prefix_end = unquoted_start.find('/').unwrap_or(unquoted_start.len());
            let login_name = &unquoted_start[1..prefix_end];
            let mut result = self.expand_home(login_name);
            result += &unquoted_start[prefix_end..];
            result
        }
    }

    fn tilde_expansion(&mut self, word: &mut Word, is_assignment: bool) {
        let unquoted_start = if let Some(WordPart::UnquotedLiteral(start)) = word.parts.first() {
            start.as_str()
        } else {
            return;
        };

        if is_assignment {
            // > In an assignment (see XBD Variable Assignment), multiple tilde-prefixes can be
            // > used: at the beginning of the word (that is, following the <equals-sign> of the
            // > assignment), following any unquoted <colon>, or both

            if unquoted_start.starts_with('~') {
                word.parts[0] =
                    WordPart::QuotedLiteral(self.tilde_expansion_simple(unquoted_start, true));
            }
            for i in 1..word.parts.len() {
                if let WordPart::UnquotedLiteral(lit) = &word.parts[i] {
                    if let Some(prefix_start) = lit.find(":~") {
                        word.parts[i] = WordPart::QuotedLiteral(
                            self.tilde_expansion_simple(&lit[prefix_start + 1..], true),
                        )
                    }
                }
            }
        } else {
            if !unquoted_start.starts_with('~') {
                return;
            }
            // > The pathname resulting from tilde expansion shall be treated as if
            // > quoted to prevent it being altered by field splitting and pathname expansion.
            word.parts[0] =
                WordPart::QuotedLiteral(self.tilde_expansion_simple(unquoted_start, false));
        }
    }

    fn expand_simple_parameter(&mut self, parameter: &Parameter) -> Option<String> {
        match parameter {
            Parameter::Number(_) => {
                todo!()
            }
            Parameter::Variable(var_name) => self
                .environment
                .get(var_name.as_ref())
                .map(|v| v.value.clone()),
            Parameter::Special(special_parameter) => match special_parameter {
                SpecialParameter::At => {
                    todo!()
                }
                SpecialParameter::Asterisk => {
                    todo!()
                }
                SpecialParameter::Hash => {
                    todo!()
                }
                SpecialParameter::QuestionMark => {
                    Some(self.most_recent_pipeline_status.to_string())
                }
                SpecialParameter::Minus => {
                    todo!()
                }
                SpecialParameter::Dollar => Some(self.shell_pid.to_string()),
                SpecialParameter::Bang => Some(self.most_recent_background_command_pid.to_string()),
                SpecialParameter::Zero => {
                    todo!()
                }
            },
        }
    }

    fn expand_parameter(&mut self, parameter_expansion: &ParameterExpansion) -> String {
        match parameter_expansion {
            ParameterExpansion::Simple(parameter) => {
                self.expand_simple_parameter(parameter).unwrap_or_default()
            }
            ParameterExpansion::UnsetUseDefault {
                parameter,
                word: default,
                default_on_null,
            } => {
                // > If parameter is unset [or null], the expansion of word (or an empty string if
                // > word is omitted) shall be substituted; otherwise, the value of parameter
                // > shall be substituted.
                match self.expand_simple_parameter(parameter) {
                    Some(value) if !default_on_null || !value.is_empty() => value,
                    _ => {
                        if let Some(default) = default {
                            self.expand_word_to_string(default, false)
                        } else {
                            String::new()
                        }
                    }
                }
            }
            ParameterExpansion::UnsetAssignDefault {
                parameter,
                word,
                assign_on_null,
            } => {
                // > If parameter is unset [or null], the expansion of word (or an empty string if
                // > word is omitted) shall be assigned to parameter. In all cases, the final value
                // > of parameter shall be substituted. Only variables, not positional parameters
                // > or special parameters, can be assigned in this way.
                match parameter {
                    Parameter::Number(_) | Parameter::Special(_) => {
                        todo!("error: cannot assign to positional argument or special parameter")
                    }
                    Parameter::Variable(variable_name) => {
                        let value = word
                            .as_ref()
                            .map(|w| self.expand_word_to_string(w, false))
                            .unwrap_or_default();
                        match self.environment.get_mut(variable_name.as_ref()) {
                            Some(variable) if *assign_on_null && variable.value.is_empty() => {
                                variable.value = value.clone();
                            }
                            None => {
                                self.environment.insert(
                                    variable_name.to_string(),
                                    Variable::new(value.clone()),
                                );
                            }
                            _ => {
                                // variable is set and not null
                            }
                        }
                        value
                    }
                }
            }
            ParameterExpansion::UnsetError {
                parameter,
                word,
                error_on_null,
            } => {
                // > If parameter is unset [or null], the expansion of word (or a message indicating
                // > it is unset if word is omitted) shall be written to standard error and the
                // > shell exits with a non-zero exit status. Otherwise, the value of parameter
                // > shall be substituted. An interactive shell need not exit.
                match self.expand_simple_parameter(parameter) {
                    Some(value) if !error_on_null || !value.is_empty() => value,
                    _ => {
                        if let Some(word) = word {
                            eprintln!("{}", self.expand_word_to_string(word, false));
                        } else if *error_on_null {
                            eprintln!("parameter is unset or null");
                        } else {
                            eprintln!("parameter is unset");
                        }
                        std::process::exit(1);
                    }
                }
            }
            ParameterExpansion::SetUseAlternative {
                parameter,
                word,
                substitute_null_with_word,
            } => {
                // > If parameter is unset [or null], null shall be substituted; otherwise, the
                // > expansion of word (or an empty string if word is omitted) shall be substituted.
                match self.expand_simple_parameter(parameter) {
                    Some(value) if *substitute_null_with_word || !value.is_empty() => {
                        self.expand_word_to_string(word.as_ref().unwrap(), false)
                    }
                    _ => String::new(),
                }
            }
            ParameterExpansion::StrLen(parameter) => {
                // > The length in characters of the value of parameter shall be substituted.
                // > If parameter is '*' or '@', the result of the expansion is unspecified.
                // > If parameter is unset and set -u is in effect, the expansion shall fail.
                if matches!(
                    parameter,
                    Parameter::Special(SpecialParameter::Asterisk)
                        | Parameter::Special(SpecialParameter::At)
                ) {
                    todo!("error: length of '*' or '@' is unspecified")
                }
                match self.expand_simple_parameter(parameter) {
                    Some(value) => value.len().to_string(),
                    None => {
                        todo!("fail if set -u is in effect")
                    }
                }
            }
            ParameterExpansion::RemovePattern {
                parameter,
                pattern,
                remove_prefix,
                remove_largest,
            } => {
                let param_str = self.expand_simple_parameter(parameter).unwrap_or_default();
                if param_str.is_empty() {
                    return String::new();
                }
                if let Some(pattern) = pattern {
                    // TODO: determine if this unwrap is safe
                    let pattern = CString::new(self.expand_word_to_string(pattern, false)).unwrap();
                    let bytes = param_str.into_bytes();
                    if *remove_prefix {
                        remove_prefix_pattern(bytes, pattern, *remove_largest)
                    } else {
                        remove_suffix_pattern(bytes, pattern, *remove_largest)
                    }
                } else {
                    param_str
                }
            }
        }
    }

    fn interpret_complete_command_to_string(&self, complete_command: &CompleteCommand) -> String {
        todo!()
    }

    fn split_fields(&self, expanded_word: String) -> Vec<String> {
        let ifs = self
            .environment
            .get("IFS")
            .map(|v| v.value.as_str())
            .unwrap_or(" \t\n");
        if ifs.is_empty() {
            return vec![expanded_word];
        }
        expanded_word
            .split(|c| ifs.contains(c))
            .map(str::to_string)
            .collect()
    }

    fn pathname_expansion(&self, pathname: String) -> String {
        pathname
    }

    /// performs:
    /// - tilde expansion
    /// - parameter expansion
    /// - command substitution
    /// - arithmetic expansion
    fn expand_word_simple(&mut self, word: &Word, is_assignment: bool) -> Word {
        let mut word = word.clone();
        self.tilde_expansion(&mut word, is_assignment);
        for part in &mut word.parts {
            match part {
                WordPart::ParameterExpansion { expansion, inside_double_quotes } => {
                    // > If a parameter expansion occurs inside double-quotes:
                    // > - Pathname expansion shall not be performed on the results of the
                    // >   expansion.
                    // > - Field splitting shall not be performed on the results of the expansion.
                    if *inside_double_quotes {
                        *part = WordPart::QuotedLiteral(self.expand_parameter(expansion))
                    } else {
                        *part = WordPart::UnquotedLiteral(self.expand_parameter(expansion))
                    }
                }
                WordPart::ArithmeticExpansion(_) => {
                    todo!()
                }
                WordPart::CommandSubstitution { command, inside_double_quotes } => {
                    // > If a command substitution occurs inside double-quotes, field splitting
                    // > and pathname expansion shall not be performed on the results of
                    // > the substitution.
                    if *inside_double_quotes {
                        *part = WordPart::QuotedLiteral(self.interpret_complete_command_to_string(command))
                    } else {
                        *part = WordPart::UnquotedLiteral(self.interpret_complete_command_to_string(command))
                    }
                }
                _ => {}
            }
        }
        word
    }

    /// performs:
    /// - tilde expansion
    /// - parameter expansion
    /// - command substitution
    /// - arithmetic expansion
    fn expand_word_to_string(&mut self, word: &Word, is_assignment: bool) -> String {
        let word = self.expand_word_simple(word, is_assignment);
        word.parts
            .into_iter()
            .filter_map(|p| match p {
                WordPart::UnquotedLiteral(s) => Some(s),
                WordPart::QuotedLiteral(s) => Some(s),
                _ => None,
            })
            .collect()
    }

    fn perform_redirection(&mut self, redirection: &Redirection) {
        match &redirection.kind {
            RedirectionKind::IORedirection { kind, file } => {
                // > the word that follows the redirection operator shall be subjected to tilde
                // > expansion, parameter expansion, command substitution, arithmetic expansion,
                // > and quote removal.
                let path = self.expand_word_to_string(file, false);
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
            let word_str = self.expand_word_to_string(word, false);
            let fields = self.split_fields(word_str);
            let expanded_word = fields
                .into_iter()
                .map(|f| self.pathname_expansion(f));
            expanded_words.extend(expanded_word);
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
                let word_str = self.expand_word_to_string(&assignment.value, true);
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
            let word_str = self.expand_word_to_string(&assignment.value, true);
            command_environment.environment.insert(
                assignment.name.to_string(),
                Variable::new_exported(word_str),
            );
        }

        for redirection in &simple_command.redirections {
            command_environment.perform_redirection(redirection);
        }
        self.system.exec(
            &expanded_words[0],
            &expanded_words[1..],
            &command_environment,
        )
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

    fn with_system<S: SystemInterface + 'static>(system: S) -> Self {
        let system = Rc::new(system);
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let variables = system
            .environment_variables()
            .into_iter()
            .map(|(k, v)| (k, Variable::new_exported(v)))
            .collect();
        Interpreter {
            environment: variables,
            opened_files: HashMap::new(),
            most_recent_pipeline_status: 0,
            last_command_substitution_status: 0,
            shell_pid: system.get_pid(),
            most_recent_background_command_pid: 0,
            system,
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::with_system(System::default())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::program::test_utils::unquoted_literal;

    #[rustfmt::skip]
    const TEST_ENV: &[(&'static str, &'static str)] = &[
        ("ENV", "test_env"),
        ("HOME", "/home/test_user"),
        ("PATH", "/usr/local/bin:/usr/bin:/bin"),
        ("USER", "test_user"),
        ("PATH", "/test_path0/:/test_path1/"),
        ("PWD", "/test_dir"),
        ("NULL", ""),
    ];

    struct TestSystem {
        env: HashMap<String, String>,
        user_homes: HashMap<String, String>,
        pid: i32,
    }

    impl TestSystem {
        fn add_environment_var(mut self, key: &str, value: &str) -> Self {
            self.env.insert(key.to_string(), value.to_string());
            self
        }
    }

    impl SystemInterface for TestSystem {
        fn environment_variables(&self) -> HashMap<String, String> {
            self.env.clone()
        }

        fn get_user_home(&self, login_name: &str) -> Option<String> {
            self.user_homes.get(login_name).cloned()
        }

        fn get_pid(&self) -> i32 {
            self.pid
        }

        fn exec(&self, command: &str, args: &[String], interpreter: &Interpreter) -> i32 {
            0
        }
    }

    impl Default for TestSystem {
        fn default() -> Self {
            let env = TEST_ENV
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect();
            let user_homes = [
                ("test_user".to_string(), "/home/test_user".to_string()),
                ("test_user2".to_string(), "/home/test_user2".to_string()),
            ]
                .into();
            TestSystem {
                env,
                user_homes,
                pid: 0,
            }
        }
    }

    #[test]
    fn expand_tilde() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_word_to_string(&unquoted_literal("~/"), false),
            "/home/test_user/".to_string()
        );
        assert_eq!(
            interpreter.expand_word_to_string(&unquoted_literal("~test_user/"), false),
            "/home/test_user/".to_string()
        );
        assert_eq!(
            interpreter.expand_word_to_string(&unquoted_literal("~test_user2"), false),
            "/home/test_user2".to_string()
        );
    }

    #[test]
    fn expand_tilde_in_assignments() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_word_to_string(&unquoted_literal("~/test1:~:~/test3"), true),
            "/home/test_user/test1:/home/test_user:/home/test_user/test3".to_string()
        );
        assert_eq!(
            interpreter.expand_word_to_string(
                &unquoted_literal("~test_user/test1:~test_user2/:~::"),
                true
            ),
            "/home/test_user/test1:/home/test_user2/:/home/test_user::"
        );
    }

    #[test]
    fn expand_simple_named_parameter() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_simple_parameter(&Parameter::Variable("HOME".into())),
            Some("/home/test_user".to_string())
        );
        assert_eq!(
            interpreter.expand_simple_parameter(&Parameter::Variable("PWD".into())),
            Some("/test_dir".to_string())
        );
    }

    #[test]
    fn expand_special_parameters() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_simple_parameter(&Parameter::Special(SpecialParameter::Dollar)),
            Some("0".to_string())
        );
        assert_eq!(
            interpreter.expand_simple_parameter(&Parameter::Special(SpecialParameter::Bang)),
            Some("0".to_string())
        );
    }

    #[test]
    fn unset_use_default_parameter_expansion() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable("HOME".into()),
                word: None,
                default_on_null: false,
            }),
            "/home/test_user".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("default")),
                default_on_null: false,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("default")),
                default_on_null: true,
            }),
            "default".to_string()
        );
    }

    #[test]
    fn unset_assign_default_parameter_expansion() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("value")),
                assign_on_null: false,
            }),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "unset_var".into()
            ))),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: None,
                assign_on_null: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "unset_var".into()
            ))),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("default")),
                assign_on_null: false,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "NULL".into()
            ))),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("default")),
                assign_on_null: true,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "NULL".into()
            ))),
            "default".to_string()
        );
    }

    #[test]
    fn set_use_alternative_parameter_expansion() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("HOME".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "word".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: true,
            }),
            "word".to_string()
        );
    }

    #[test]
    fn string_length_parameter_expansion() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::StrLen(Parameter::Variable(
                "HOME".into()
            ))),
            "15".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::StrLen(Parameter::Variable(
                "PWD".into()
            ))),
            "9".to_string()
        );
    }

    #[test]
    fn remove_smallest_suffix() {
        let mut interpreter =
            Interpreter::with_system(TestSystem::default().add_environment_var("TEST", "aabbc"));
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: Some(unquoted_literal("test_user")),
                remove_largest: false,
                remove_prefix: false,
            }),
            "/home/".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("TEST".into()),
                pattern: Some(unquoted_literal("a*c")),
                remove_largest: false,
                remove_prefix: false,
            }),
            "a".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("NULL".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: false,
                remove_prefix: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("UNDEFINED".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: false,
                remove_prefix: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: None,
                remove_largest: false,
                remove_prefix: false,
            }),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_suffix() {
        let mut interpreter =
            Interpreter::with_system(TestSystem::default().add_environment_var("TEST", "aabbc"));
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: Some(unquoted_literal("test_user")),
                remove_largest: true,
                remove_prefix: false,
            }),
            "/home/".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("TEST".into()),
                pattern: Some(unquoted_literal("a*c")),
                remove_largest: true,
                remove_prefix: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("NULL".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: true,
                remove_prefix: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("UNDEFINED".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: true,
                remove_prefix: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: None,
                remove_largest: true,
                remove_prefix: false,
            }),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_smallest_prefix() {
        let mut interpreter =
            Interpreter::with_system(TestSystem::default().add_environment_var("TEST", "abbcc"));
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: Some(unquoted_literal("/home/")),
                remove_largest: false,
                remove_prefix: true,
            }),
            "test_user".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("TEST".into()),
                pattern: Some(unquoted_literal("a*c")),
                remove_largest: false,
                remove_prefix: true,
            }),
            "c".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("NULL".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: false,
                remove_prefix: true,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("UNDEFINED".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: false,
                remove_prefix: true,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: None,
                remove_largest: false,
                remove_prefix: true,
            }),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_prefix() {
        let mut interpreter =
            Interpreter::with_system(TestSystem::default().add_environment_var("TEST", "abbcc"));
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: Some(unquoted_literal("/home/")),
                remove_largest: true,
                remove_prefix: true,
            }),
            "test_user".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("TEST".into()),
                pattern: Some(unquoted_literal("a*c")),
                remove_largest: true,
                remove_prefix: true,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("NULL".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: true,
                remove_prefix: true,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("UNDEFINED".into()),
                pattern: Some(unquoted_literal("anything")),
                remove_largest: true,
                remove_prefix: true,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_parameter(&ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable("HOME".into()),
                pattern: None,
                remove_largest: true,
                remove_prefix: true,
            }),
            "/home/test_user".to_string()
        );
    }
}
