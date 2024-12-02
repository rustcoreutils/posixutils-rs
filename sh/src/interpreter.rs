use crate::program::{
    Command, CompleteCommand, CompoundCommand, Conjunction, LogicalOp, Parameter,
    ParameterExpansion, Pipeline, Program, SimpleCommand, SpecialParameter, Word, WordPart,
};
use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};
use std::process::Stdio;

trait SystemInterface {
    fn environment_variables(&self) -> HashMap<String, String>;
    /// # Panics
    /// Panics if `login_name` contains non-portable filename characters as defined in
    /// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    fn get_user_home(&self, login_name: &str) -> Option<String>;
    fn get_pid(&self) -> i32;
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
}

fn is_portable_filename_character(c: char) -> bool {
    // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    c.is_ascii_alphanumeric() || "._-".contains(c)
}

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

pub struct Interpreter {
    variables: HashMap<String, Variable>,
    most_recent_pipeline_status: i32,
    shell_pid: i32,
    most_recent_background_command_pid: i32,
    system: Box<dyn SystemInterface>,
}

impl Interpreter {
    fn expand_home(&self, login_name: &str) -> String {
        if login_name.is_empty() {
            // > If the login name is null (that is, the tilde-prefix contains only the tilde),
            // > the tilde-prefix is replaced by the value of the variable HOME
            self.variables
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
                .variables
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

    fn expand_complex_parameter(&mut self, parameter_expansion: &ParameterExpansion) -> String {
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
                            self.expand_word(default, false)
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
                            .map(|w| self.expand_word(w, false))
                            .unwrap_or_default();
                        match self.variables.get_mut(variable_name.as_ref()) {
                            Some(variable) if *assign_on_null && variable.value.is_empty() => {
                                variable.value = value.clone();
                            }
                            None => {
                                self.variables.insert(
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
                            eprintln!("{}", self.expand_word(word, false));
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
                        self.expand_word(word.as_ref().unwrap(), false)
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
            ParameterExpansion::RemoveSmallestSuffix(_, _) => {
                todo!()
            }
            ParameterExpansion::RemoveLargestSuffix(_, _) => {
                todo!()
            }
            ParameterExpansion::RemoveSmallestPrefix(_, _) => {
                todo!()
            }
            ParameterExpansion::RemoveLargestPrefix(_, _) => {
                todo!()
            }
        }
    }

    fn parameter_expansion(&mut self, word: &mut Word) {
        // TODO:
        // > If a parameter expansion occurs inside double-quotes:
        // > - Pathname expansion shall not be performed on the results of the expansion.
        // > - Field splitting shall not be performed on the results of the expansion.
        for part in &mut word.parts {
            if let WordPart::ParameterExpansion(expansion) = part {
                *part = WordPart::UnquotedLiteral(self.expand_complex_parameter(expansion));
            }
        }
    }

    fn expand_word(&mut self, word: &Word, is_assignment: bool) -> String {
        let mut word = word.clone();
        self.tilde_expansion(&mut word, is_assignment);
        self.parameter_expansion(&mut word);
        word.parts
            .into_iter()
            .filter_map(|p| match p {
                WordPart::UnquotedLiteral(s) => Some(s),
                WordPart::QuotedLiteral(s) => Some(s),
                _ => None,
            })
            .collect()
    }

    fn interpret_simple_command(&mut self, simple_command: &SimpleCommand) -> i32 {
        todo!()
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
        todo!()
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
        let system = Box::new(system);
        // > If a variable is initialized from the environment, it shall be marked for
        // > export immediately
        let variables = system
            .environment_variables()
            .into_iter()
            .map(|(k, v)| (k, Variable::new_exported(v)))
            .collect();
        Interpreter {
            variables,
            most_recent_pipeline_status: 0,
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
            interpreter.expand_word(&unquoted_literal("~/"), false),
            "/home/test_user/".to_string()
        );
        assert_eq!(
            interpreter.expand_word(&unquoted_literal("~test_user/"), false),
            "/home/test_user/".to_string()
        );
        assert_eq!(
            interpreter.expand_word(&unquoted_literal("~test_user2"), false),
            "/home/test_user2".to_string()
        );
    }

    #[test]
    fn expand_tilde_in_assignments() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_word(&unquoted_literal("~/test1:~:~/test3"), true),
            "/home/test_user/test1:/home/test_user:/home/test_user/test3".to_string()
        );
        assert_eq!(
            interpreter.expand_word(&unquoted_literal("~test_user/test1:~test_user2/:~::"), true),
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
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable("HOME".into()),
                word: None,
                default_on_null: false,
            }),
            "/home/test_user".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("default")),
                default_on_null: false,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetUseDefault {
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
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("value")),
                assign_on_null: false,
            }),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "unset_var".into()
            ))),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("unset_var".into()),
                word: None,
                assign_on_null: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "unset_var".into()
            ))),
            "value".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("default")),
                assign_on_null: false,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "NULL".into()
            ))),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("default")),
                assign_on_null: true,
            }),
            "default".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::Simple(Parameter::Variable(
                "NULL".into()
            ))),
            "default".to_string()
        );
    }

    #[test]
    fn set_use_alternative_parameter_expansion() {
        let mut interpreter = Interpreter::with_system(TestSystem::default());
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("HOME".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "word".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("unset_var".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable("NULL".into()),
                word: Some(unquoted_literal("word")),
                substitute_null_with_word: false,
            }),
            "".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::SetUseAlternative {
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
            interpreter.expand_complex_parameter(&ParameterExpansion::StrLen(Parameter::Variable(
                "HOME".into()
            ))),
            "15".to_string()
        );
        assert_eq!(
            interpreter.expand_complex_parameter(&ParameterExpansion::StrLen(Parameter::Variable(
                "PWD".into()
            ))),
            "9".to_string()
        );
    }
}
