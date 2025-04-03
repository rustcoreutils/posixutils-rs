//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{c_char, CStr, CString};

use nix::libc;

use crate::parse::word::{Word, WordPart};
use crate::shell::environment::Environment;

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
        env.get_str_value("HOME")
            .map(|s| s.to_string())
            .ok_or("sh: failed to expand ~, variable HOME is unset".to_string())
    } else {
        if !login_name.chars().all(is_portable_filename_character) {
            return Err(format!("sh: invalid user '{login_name}'"));
        }
        user_home
            .get_user_home(login_name)
            .ok_or(format!("sh: invalid user '{login_name}'"))
    }
}

/// performs tilde expansion on `unquoted_start`. Assumes that `unquoted_start` starts with
/// `~`
fn tilde_expansion_simple(
    unquoted_start: &str,
    is_assignment: bool,
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<String, String> {
    if is_assignment {
        let mut result = String::with_capacity(unquoted_start.len());
        for sub in unquoted_start.split(':') {
            if sub.starts_with('~') {
                let prefix_end = sub.find('/').unwrap_or(sub.len());
                let login_name = &sub[1..prefix_end];
                result += &expand_home(login_name, env, user_home)?;
                result += &sub[prefix_end..];
            } else {
                result += sub
            }
            result.push(':');
        }
        // removes last ':'
        result.pop();
        Ok(result)
    } else {
        let prefix_end = unquoted_start.find('/').unwrap_or(unquoted_start.len());
        let login_name = &unquoted_start[1..prefix_end];
        let mut result = expand_home(login_name, env, user_home)?;
        result += &unquoted_start[prefix_end..];
        Ok(result)
    }
}

fn expand_tilde_with_custom_users_home_dirs(
    word: &mut Word,
    is_assignment: bool,
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Result<(), String> {
    let unquoted_start = if let Some(WordPart::UnquotedLiteral(start)) = word.parts.first() {
        start.as_str()
    } else {
        return Ok(());
    };

    if is_assignment {
        if unquoted_start.starts_with('~') {
            word.parts[0] = WordPart::QuotedLiteral(tilde_expansion_simple(
                unquoted_start,
                true,
                env,
                user_home,
            )?);
        }
        for i in 1..word.parts.len() {
            if let WordPart::UnquotedLiteral(lit) = &word.parts[i] {
                if let Some(prefix_start) = lit.find(":~") {
                    word.parts[i] = WordPart::QuotedLiteral(tilde_expansion_simple(
                        &lit[prefix_start + 1..],
                        true,
                        env,
                        user_home,
                    )?)
                }
            }
        }
    } else {
        if !unquoted_start.starts_with('~') {
            return Ok(());
        }
        // > The pathname resulting from tilde expansion shall be treated as if
        // > quoted to prevent it being altered by field splitting and pathname expansion.
        word.parts[0] = WordPart::QuotedLiteral(tilde_expansion_simple(
            unquoted_start,
            false,
            env,
            user_home,
        )?);
    }
    Ok(())
}

pub fn tilde_expansion(
    word: &mut Word,
    is_assignment: bool,
    env: &Environment,
) -> Result<(), String> {
    expand_tilde_with_custom_users_home_dirs(word, is_assignment, env, &DefaultUsersHomeDirs)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::parse::word::test_utils::{quoted_literal, unquoted_literal};
    use crate::shell::environment::Value;

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
        is_assignment: bool,
        env_home: &str,
        users_home_dirs: TestUsersHomeDirs,
    ) -> Word {
        let env = Environment::from([("HOME".to_string(), Value::new(env_home.to_string()))]);
        let mut word = unquoted_literal(word_str);
        expand_tilde_with_custom_users_home_dirs(&mut word, is_assignment, &env, &users_home_dirs)
            .expect("expansion failure");
        word
    }

    #[test]
    fn expand_tilde_from_env() {
        assert_eq!(
            expand_tilde("~", false, "test_home", TestUsersHomeDirs::default()),
            quoted_literal("test_home")
        );
    }

    #[test]
    fn expand_tilde_from_user_home_dir() {
        let users_home_dirs = TestUsersHomeDirs {
            users_home_dirs: [("test_user".to_string(), "test_home".to_string())].into(),
        };
        assert_eq!(
            expand_tilde("~test_user", false, "test_home", users_home_dirs),
            quoted_literal("test_home")
        );
    }

    #[test]
    fn expand_tilde_in_assignments() {
        assert_eq!(
            expand_tilde(
                "~/test1:~:~/test3",
                true,
                "/home/test_user",
                TestUsersHomeDirs::default()
            ),
            quoted_literal("/home/test_user/test1:/home/test_user:/home/test_user/test3")
        );
        assert_eq!(
            expand_tilde(
                "~/test1:~:~/test3",
                true,
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
