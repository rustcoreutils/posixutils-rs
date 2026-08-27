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
    fn get_user_home(&self, login_name: &str) -> Option<ShString>;
}

struct DefaultUsersHomeDirs;

impl UsersHomeDirs for DefaultUsersHomeDirs {
    fn get_user_home(&self, login_name: &str) -> Option<ShString> {
        let login_name = CString::new(login_name).ok()?;
        let passwd = unsafe { libc::getpwnam(login_name.as_ptr()) };
        if passwd.is_null() {
            return None;
        }
        // this is safe, since the pointer is not null
        // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getpwnam.html
        let user_home_dir = unsafe { CStr::from_ptr((*passwd).pw_dir as *const c_char) };
        Some(ShString::from(user_home_dir.to_bytes()))
    }
}

/// Expands the login name of a tilde-prefix to a home directory. `None` means
/// the name is not one the system recognizes, which POSIX leaves undefined and
/// which dash and bash both answer by leaving the tilde-prefix alone.
fn expand_home(
    login_name: &[u8],
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Option<ShString> {
    if login_name.is_empty() {
        // POSIX leaves `~` with HOME unset unspecified; like dash, leave the
        // tilde literal rather than failing the whole expansion.
        return Some(
            env.get_value("HOME")
                .map(ShString::from)
                .unwrap_or_else(|| ShString::from("~")),
        );
    }
    // A login name is drawn from the portable filename character set, so it is
    // both text and ASCII; anything else cannot name a user.
    let login_name = std::str::from_utf8(login_name).ok()?;
    if !login_name.chars().all(is_portable_filename_character) {
        return None;
    }
    user_home.get_user_home(login_name)
}

/// Expands `~` at the start of `value` and after each unquoted `:` in it, as
/// required for the value of an assignment. `continues` says whether more of
/// the same word follows in another part, which leaves a trailing tilde-prefix
/// unterminated -- see [`prefix_is_unquoted`].
fn expand_assignment_value(
    value: &[u8],
    continues: bool,
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> ShString {
    let subs = value.split(|&b| b == b':').collect::<Vec<_>>();
    let last = subs.len() - 1;
    let mut result = ShString::new();
    for (i, sub) in subs.into_iter().enumerate() {
        if i > 0 {
            result.push_bytes(b":");
        }
        // Here a tilde-prefix ends at a `:` as well as at a `/`, so only the
        // final field can run past the end of this word part.
        let unterminated = i == last && continues && !sub.contains(&b'/');
        match sub.strip_prefix(b"~").filter(|_| !unterminated) {
            Some(rest) => {
                let prefix_end = rest.iter().position(|&b| b == b'/').unwrap_or(rest.len());
                match expand_home(&rest[..prefix_end], env, user_home) {
                    Some(home) => {
                        result.push_bytes(&home);
                        result.push_bytes(&rest[prefix_end..]);
                    }
                    None => result.push_bytes(sub),
                }
            }
            None => result.push_bytes(sub),
        }
    }
    result
}

/// Expands a leading `~`, as required for an ordinary word. Assumes `word`
/// starts with `~`. `None` leaves the word alone.
fn expand_word_tilde(
    word: &[u8],
    env: &Environment,
    user_home: &dyn UsersHomeDirs,
) -> Option<ShString> {
    let prefix_end = word.iter().position(|&b| b == b'/').unwrap_or(word.len());
    let mut result = expand_home(&word[1..prefix_end], env, user_home)?;
    result.push_bytes(&word[prefix_end..]);
    Some(result)
}

/// Whether the tilde-prefix opening `first` is made up entirely of unquoted
/// characters, which POSIX 2.6.1 requires:
///
/// > If any of the characters in the tilde-prefix are quoted, none of the
/// > characters in the tilde-prefix shall be treated as a tilde-prefix.
///
/// The prefix ends at the first unquoted `/`, so one with no `/` in `first`
/// runs to the end of the word -- and any further word part is quoted or is an
/// expansion, either of which disqualifies it. `~"root"` and `~$u` are both
/// left alone by dash and bash for this reason.
fn prefix_is_unquoted(first: &[u8], rest_of_word: &[WordPart]) -> bool {
    first.contains(&b'/') || rest_of_word.is_empty()
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
) {
    let unquoted_start = if let Some(WordPart::UnquotedLiteral(start)) = word.parts.first() {
        start.clone()
    } else {
        return;
    };

    if mode == TildeMode::Word {
        if !unquoted_start.starts_with(b"~")
            || !prefix_is_unquoted(&unquoted_start, &word.parts[1..])
        {
            return;
        }
        // > The pathname resulting from tilde expansion shall be treated as if
        // > quoted to prevent it being altered by field splitting and pathname expansion.
        if let Some(expanded) = expand_word_tilde(&unquoted_start, env, user_home) {
            word.parts[0] = WordPart::QuotedLiteral(expanded);
        }
        return;
    }

    // The part of the first literal that is an assignment value, and whatever
    // precedes it (the `name=` of a declaration utility operand).
    let (prefix, value): (&[u8], &[u8]) = if mode == TildeMode::DeclarationOperand {
        match unquoted_start.iter().position(|&b| b == b'=') {
            Some(pos) if std::str::from_utf8(&unquoted_start[..pos]).is_ok_and(is_valid_name) => {
                (&unquoted_start[..pos + 1], &unquoted_start[pos + 1..])
            }
            // not `name=value` after all, so nothing here is an assignment
            _ => return,
        }
    } else {
        (b"", unquoted_start.as_bytes())
    };
    let expanded = expand_assignment_value(value, word.parts.len() > 1, env, user_home);
    if expanded.as_bytes() != value {
        let mut replacement = ShString::from(prefix);
        replacement.push_bytes(&expanded);
        word.parts[0] = WordPart::QuotedLiteral(replacement);
    }
    for i in 1..word.parts.len() {
        if let WordPart::UnquotedLiteral(lit) = &word.parts[i] {
            if let Some(prefix_start) = lit.windows(2).position(|w| w == b":~") {
                let expanded = expand_assignment_value(
                    &lit[prefix_start + 1..],
                    i + 1 < word.parts.len(),
                    env,
                    user_home,
                );
                let mut replacement = ShString::from(&lit[..=prefix_start]);
                replacement.push_bytes(&expanded);
                word.parts[i] = WordPart::QuotedLiteral(replacement);
            }
        }
    }
}

pub fn tilde_expansion(word: &mut Word, mode: TildeMode, env: &Environment) {
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
        fn get_user_home(&self, login_name: &str) -> Option<ShString> {
            self.users_home_dirs.get(login_name).map(ShString::from)
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
        expand_tilde_with_custom_users_home_dirs(&mut word, mode, &env, &users_home_dirs);
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
