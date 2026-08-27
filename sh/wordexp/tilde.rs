//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::command_parser::is_valid_name;
use crate::parse::word::{Word, WordPart};
use crate::shell::environment::Environment;
use crate::shstr::ShString;
use std::ffi::{c_char, CStr, CString};

fn is_portable_filename_character(c: char) -> bool {
    // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
    c.is_ascii_alphanumeric() || "._-".contains(c)
}

trait UsersHomeDirs {
    fn get_user_home(&self, login_name: &str) -> Option<String>;
}

struct DefaultUsersHomeDirs;

impl UsersHomeDirs for DefaultUsersHomeDirs {
    /// `login_name` has to be a valid login name
    fn get_user_home(&self, login_name: &str) -> Option<String> {
        // it cannot contain a null char as part of the method's contract
        let login_name = CString::new(login_name).unwrap();
        let passwd = unsafe { libc::getpwnam(login_name.as_ptr()) };
        if passwd.is_null() {
            return None;
        }
        // this is safe, since the pointer is not null
        // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getpwnam.html
        let user_home_dir = unsafe { CStr::from_ptr((*passwd).pw_dir as *const c_char) };
        Some(String::from_utf8(user_home_dir.to_bytes().to_vec()).unwrap())
    }
}

fn expand_home(
    login_name: &str,
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<String, String> {
    if login_name.is_empty() {
        // POSIX leaves `~` with HOME unset unspecified; like dash, leave the
        // tilde literal rather than failing the whole expansion.
        Ok(env
            .get_str_value("HOME")
            .map(|s| s.to_string())
            .unwrap_or_else(|| "~".to_string()))
    } else {
        if !login_name.chars().all(is_portable_filename_character) {
            return Err(format!("sh: invalid user '{login_name}'"));
        }
        user_home
            .get_user_home(login_name)
            .ok_or(format!("sh: invalid user '{login_name}'"))
    }
}

/// Expands `~` at the start of `value` and after each unquoted `:` in it, as
/// required for the value of an assignment.
fn expand_assignment_value(
    value: &[u8],
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<ShString, String> {
    let mut result = ShString::new();
    for (i, sub) in value.split(|&b| b == b':').enumerate() {
        if i > 0 {
            result.push_bytes(b":");
        }
        if let Some(rest) = sub.strip_prefix(b"~") {
            let prefix_end = rest.iter().position(|&b| b == b'/').unwrap_or(rest.len());
            // A login name is text; one that is not cannot name a user.
            let name = std::str::from_utf8(&rest[..prefix_end]).unwrap_or("");
            result.push_bytes(expand_home(name, env, user_home)?);
            result.push_bytes(&rest[prefix_end..]);
        } else {
            result.push_bytes(sub);
        }
    }
    Ok(result)
}

/// Expands a leading `~`, as required for an ordinary word. Assumes `word`
/// starts with `~`.
fn expand_word_tilde(
    word: &[u8],
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<ShString, String> {
    let prefix_end = word.iter().position(|&b| b == b'/').unwrap_or(word.len());
    let name = std::str::from_utf8(&word[1..prefix_end]).unwrap_or("");
    let mut result = ShString::from(expand_home(name, env, user_home)?);
    result.push_bytes(&word[prefix_end..]);
    Ok(result)
}

/// Where a word appears, which decides how much tilde expansion it gets.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TildeMode {
    /// An ordinary word: only a leading `~` expands.
    Word,
    /// The value of an assignment: a leading `~` and a `~` after each unquoted
    /// `:` expand.
    AssignmentValue,
    /// A `name=value` operand of a declaration utility (POSIX 2.9.1): like
    /// `AssignmentValue`, applied to the text after the first `=`.
    DeclarationOperand,
}

fn expand_tilde_with_custom_users_home_dirs(
    word: &mut Word,
    mode: TildeMode,
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<(), String> {
    let unquoted_start = if let Some(WordPart::UnquotedLiteral(start)) = word.parts.first() {
        start.clone()
    } else {
        return Ok(());
    };

    if mode == TildeMode::Word {
        if !unquoted_start.starts_with(b"~") {
            return Ok(());
        }
        // > The pathname resulting from tilde expansion shall be treated as if
        // > quoted to prevent it being altered by field splitting and pathname expansion.
        word.parts[0] =
            WordPart::QuotedLiteral(expand_word_tilde(&unquoted_start, env, user_home)?);
        return Ok(());
    }

    // The part of the first literal that is an assignment value, and whatever
    // precedes it (the `name=` of a declaration utility operand).
    let (prefix, value): (&[u8], &[u8]) = if mode == TildeMode::DeclarationOperand {
        match unquoted_start.iter().position(|&b| b == b'=') {
            Some(pos) if std::str::from_utf8(&unquoted_start[..pos]).is_ok_and(is_valid_name) => {
                (&unquoted_start[..pos + 1], &unquoted_start[pos + 1..])
            }
            // not `name=value` after all, so nothing here is an assignment
            _ => return Ok(()),
        }
    } else {
        (b"", unquoted_start.as_bytes())
    };
    let expanded = expand_assignment_value(value, env, user_home)?;
    if expanded.as_bytes() != value {
        let mut replacement = ShString::from(prefix);
        replacement.push_bytes(&expanded);
        word.parts[0] = WordPart::QuotedLiteral(replacement);
    }
    for i in 1..word.parts.len() {
        if let WordPart::UnquotedLiteral(lit) = &word.parts[i] {
            if let Some(prefix_start) = lit.windows(2).position(|w| w == b":~") {
                let expanded = expand_assignment_value(&lit[prefix_start + 1..], env, user_home)?;
                let mut replacement = ShString::from(&lit[..=prefix_start]);
                replacement.push_bytes(&expanded);
                word.parts[i] = WordPart::QuotedLiteral(replacement);
            }
        }
    }
    Ok(())
}

pub fn tilde_expansion(word: &mut Word, mode: TildeMode, env: &Environment) -> Result<(), String> {
    expand_tilde_with_custom_users_home_dirs(word, mode, env, &DefaultUsersHomeDirs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::word::test_utils::{quoted_literal, unquoted_literal};
    use crate::shell::environment::Value;
    use std::collections::HashMap;

    #[derive(Default)]

    struct TestUsersHomeDirs {
        users_home_dirs: HashMap<String, String>,
    }

    impl UsersHomeDirs for TestUsersHomeDirs {
        fn get_user_home(&self, login_name: &str) -> Option<String> {
            self.users_home_dirs.get(login_name).cloned()
        }
    }

    fn expand_tilde(
        word_str: &str,
        mode: TildeMode,
        env_home: &str,
        users_home_dirs: TestUsersHomeDirs,
    ) -> Word {
        let env = Environment::from([("HOME".to_string(), Value::new(env_home.to_string()))]);
        let mut word = unquoted_literal(word_str);
        expand_tilde_with_custom_users_home_dirs(&mut word, mode, &env, &users_home_dirs)
            .expect("expansion failure");
        word
    }

    #[test]
    fn expand_tilde_from_env() {
        assert_eq!(
            expand_tilde(
                "~",
                TildeMode::Word,
                "test_home",
                TestUsersHomeDirs::default()
            ),
            quoted_literal("test_home")
        );
    }

    #[test]
    fn expand_tilde_from_user_home_dir() {
        let users_home_dirs = TestUsersHomeDirs {
            users_home_dirs: [("test_user".to_string(), "test_home".to_string())].into(),
        };
        assert_eq!(
            expand_tilde("~test_user", TildeMode::Word, "test_home", users_home_dirs),
            quoted_literal("test_home")
        );
    }

    #[test]
    fn expand_tilde_in_assignments() {
        assert_eq!(
            expand_tilde(
                "~/test1:~:~/test3",
                TildeMode::AssignmentValue,
                "/home/test_user",
                TestUsersHomeDirs::default()
            ),
            quoted_literal("/home/test_user/test1:/home/test_user:/home/test_user/test3")
        );
        assert_eq!(
            expand_tilde(
                "~/test1:~:~/test3",
                TildeMode::AssignmentValue,
                "/home/test_user",
                TestUsersHomeDirs {
                    users_home_dirs: [
                        ("test_user".to_string(), "/home/test_user".to_string()),
                        ("test_user2".to_string(), "/home/test_user2".to_string())
                    ]
                    .into(),
                },
            ),
            quoted_literal("/home/test_user/test1:/home/test_user:/home/test_user/test3")
        );
    }
}
