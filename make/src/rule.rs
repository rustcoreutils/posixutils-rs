//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod config;
pub mod prerequisite;
pub mod recipe;
pub mod target;

use crate::{
    config::Config as GlobalConfig,
    error_code::ErrorCode::{self, *},
    parser::Rule as ParsedRule,
    signal_handler, Macro, DEFAULT_SHELL, DEFAULT_SHELL_VAR,
};
use config::Config;
use gettextrs::gettext;
use prerequisite::Prerequisite;
use recipe::config::Config as RecipeConfig;
use recipe::Recipe;
use std::collections::VecDeque;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::{
    collections::HashMap,
    fs::{File, FileTimes},
    process::{self, Command},
    sync::{Arc, LazyLock, Mutex},
    time::SystemTime,
};
use target::Target;

type LazyArcMutex<T> = LazyLock<Arc<Mutex<T>>>;

/// State about the target whose recipe is currently running, used by the signal
/// handler to decide whether to delete a partially built target on interrupt.
#[derive(Debug, Clone)]
pub struct InterruptInfo {
    pub target: String,
    pub precious: bool,
    pub phony: bool,
    /// The target's modification time before its recipe started, so the handler
    /// can tell whether the interrupted recipe actually changed the file.
    pub original_mtime: Option<SystemTime>,
}

pub static INTERRUPT_FLAG: LazyArcMutex<Option<InterruptInfo>> =
    LazyLock::new(|| Arc::new(Mutex::new(None)));

/// Set when a non-ignored recipe error is swallowed under `-k` (keep going) so
/// that `make` can report the failure and exit nonzero even though the build
/// loop continued with the remaining, independent targets.
pub static KEEP_GOING_ERROR: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    /// The targets of the rule
    targets: Vec<Target>,
    /// The prerequisites of the rule
    prerequisites: Vec<Prerequisite>,
    /// The recipe of the rule
    recipes: Vec<Recipe>,

    pub config: Config,
}

impl Rule {
    pub fn targets(&self) -> impl Iterator<Item = &Target> {
        self.targets.iter()
    }

    pub fn prerequisites(&self) -> impl Iterator<Item = &Prerequisite> {
        self.prerequisites.iter()
    }

    pub fn recipes(&self) -> impl Iterator<Item = &Recipe> {
        self.recipes.iter()
    }

    /// Runs an inference rule for a specific target (not a CWD scan).
    ///
    /// This is used when POSIX requires applying an inference rule to a specific
    /// target that has no commands of its own. The internal macros ($<, $*, etc.)
    /// are substituted based on the target name and the inference rule's suffixes.
    pub fn run_for_target(
        &self,
        global_config: &GlobalConfig,
        macros: &[Macro],
        target: &Target,
        up_to_date: bool,
        newer: &[String],
    ) -> Result<(), ErrorCode> {
        // For an inference rule applied to a specific target, compute the
        // input/output pair from the target name and the rule's suffixes.
        let files = if let Some(Target::Inference { from, to, .. }) = self.targets().next() {
            let target_name = target.as_ref();
            if to.is_empty() {
                // Single-suffix rule (`.s2:`): build `target` from `target.s2`.
                let input = PathBuf::from(format!("{}.{}", target_name, from));
                vec![(input, PathBuf::from(target_name))]
            } else {
                let expected_suffix = format!(".{}", to);
                if let Some(stem) = target_name.strip_suffix(&expected_suffix) {
                    let input = PathBuf::from(format!("{}.{}", stem, from));
                    let output = PathBuf::from(target_name);
                    vec![(input, output)]
                } else {
                    vec![(PathBuf::from(""), PathBuf::from(""))]
                }
            }
        } else {
            vec![(PathBuf::from(""), PathBuf::from(""))]
        };

        self.run_with_files(global_config, macros, target, up_to_date, files, newer)
    }

    /// Runs the rule with the global config and macros passed in.
    ///
    /// Returns `Ok` on success and `Err` on any errors while running the rule.
    pub fn run(
        &self,
        global_config: &GlobalConfig,
        macros: &[Macro],
        target: &Target,
        up_to_date: bool,
        newer: &[String],
    ) -> Result<(), ErrorCode> {
        let files = match target {
            Target::Inference { from, to, .. } => find_files_with_extension(from)?
                .into_iter()
                .map(|input| {
                    let mut output = input.clone();
                    output.set_extension(to);
                    (input, output)
                })
                .collect::<Vec<_>>(),
            _ => {
                vec![(PathBuf::from(""), PathBuf::from(""))]
            }
        };

        self.run_with_files(global_config, macros, target, up_to_date, files, newer)
    }

    /// Internal helper: runs the rule's recipes for the given input/output file pairs.
    fn run_with_files(
        &self,
        global_config: &GlobalConfig,
        macros: &[Macro],
        target: &Target,
        up_to_date: bool,
        files: Vec<(PathBuf, PathBuf)>,
        newer: &[String],
    ) -> Result<(), ErrorCode> {
        let GlobalConfig {
            ignore: global_ignore,
            dry_run: global_dry_run,
            silent: global_silent,
            touch: global_touch,
            env_macros: global_env_macros,
            quit: global_quit,
            clear: _,
            print: global_print,
            keep_going: global_keep_going,
            terminate: global_terminate,
            precious: global_precious,
            jobs: _,
            not_parallel: _,
            suffixes: _,
            rules: _,
        } = *global_config;
        let Config {
            ignore: rule_ignore,
            silent: rule_silent,
            precious: rule_precious,
            phony: rule_phony,
        } = self.config;

        // Capture the target's modification time once, before any recipe line
        // runs, so the signal handler can tell whether an interrupted recipe
        // changed the (possibly newly created) target.
        let original_mtime = std::fs::metadata(target.as_ref())
            .ok()
            .and_then(|m| m.modified().ok());

        for inout in files {
            for recipe in self.recipes() {
                let RecipeConfig {
                    ignore: recipe_ignore,
                    silent: recipe_silent,
                    force_run: recipe_force_run,
                } = recipe.config;

                let ignore = global_ignore || rule_ignore || recipe_ignore;
                let dry_run = global_dry_run;
                let silent = global_silent || rule_silent || recipe_silent;
                let force_run = recipe_force_run;
                let touch = global_touch;
                let env_macros = global_env_macros;
                let quit = global_quit;
                let print = global_print;
                let precious = global_precious || rule_precious;
                let keep_going = global_keep_going;
                let terminate = global_terminate;

                *INTERRUPT_FLAG.lock().unwrap() = Some(InterruptInfo {
                    target: target.as_ref().to_string(),
                    precious,
                    phony: rule_phony,
                    original_mtime,
                });

                // POSIX: make catches signals unless -n, -p, or -q is set (those
                // take the default action). -i is not an exemption.
                if !dry_run && !print && !quit {
                    signal_handler::register_signals();
                }

                // POSIX: a recipe line prefixed with `+`, or one containing the
                // `$(MAKE)`/`${MAKE}` macro, is executed even under -n, -t, or
                // -q (so recursive sub-makes still run).
                let raw = recipe.inner();
                let is_recursive = raw.contains("$(MAKE)") || raw.contains("${MAKE}");
                let always_run = force_run || is_recursive;

                if !always_run {
                    // -n flag
                    if dry_run {
                        println!("{}", recipe);
                        continue;
                    }

                    // -t flag
                    if touch {
                        continue;
                    }
                    // -q flag
                    if quit {
                        if up_to_date {
                            process::exit(0);
                        } else {
                            process::exit(1);
                        }
                    }
                }

                // -s flag
                if !silent {
                    println!("{}", recipe);
                }

                // POSIX: the recipe shell comes from the `SHELL` macro
                // (default /bin/sh); the `SHELL` *environment variable* shall
                // not be used and shall not be modified by the macro.
                let shell = macros
                    .iter()
                    .find(|(name, _)| name == DEFAULT_SHELL_VAR)
                    .map(|(_, value)| value.clone())
                    .unwrap_or_else(|| DEFAULT_SHELL.to_string());
                let mut command = Command::new(shell);

                self.init_env(env_macros, &mut command, macros);
                let recipe = self.substitute_internal_macros(target, recipe, &inout, newer);
                // POSIX: when errors are not being ignored, the shell -e option
                // shall also be in effect, so the recipe aborts on the first
                // failing command.
                if ignore {
                    command.args(["-c", recipe.as_ref()]);
                } else {
                    command.args(["-e", "-c", recipe.as_ref()]);
                }

                let status = match command.status() {
                    Ok(status) => status,
                    Err(err) => {
                        if ignore {
                            continue;
                        } else {
                            return Err(IoError(err.kind()));
                        }
                    }
                };
                if !status.success() && !ignore {
                    // -S and -k flags
                    if !terminate && keep_going {
                        eprintln!(
                            "make: {}",
                            ExecutionError {
                                exit_code: status.code(),
                            }
                        );
                        KEEP_GOING_ERROR.store(true, std::sync::atomic::Ordering::Relaxed);
                        break;
                    } else {
                        return Err(ExecutionError {
                            exit_code: status.code(),
                        });
                    }
                }
            }

            let silent = global_silent || rule_silent;
            let touch = global_touch;

            // -t flag
            if touch {
                if !silent {
                    println!("{} {target}", gettext("touch"));
                }
                let file = File::create(target.as_ref())?;
                file.set_times(FileTimes::new().set_modified(SystemTime::now()))?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Expand the internal macro identified by `sigil` (one of
    /// `@ % ? < * ^ +`), optionally taking the directory (`D`) or filename
    /// (`F`) part of each element.
    fn expand_internal_macro(
        &self,
        sigil: char,
        modifier: Option<char>,
        target: &Target,
        files: &(PathBuf, PathBuf),
        newer: &[String],
    ) -> String {
        // `$@` is the target name; for an `archive(member)` target `$%` is the
        // member and `$@` is the archive name.
        let target_name = target.as_ref();
        let archive_name = target_name.split('(').next().unwrap_or(target_name);
        let member = target_name
            .split_once('(')
            .map(|(_, m)| m.strip_suffix(')').unwrap_or(m))
            .unwrap_or("");

        let all_prereqs: Vec<String> = self
            .prerequisites()
            .map(|p| p.as_ref().to_string())
            .collect();

        let base: Vec<String> = match sigil {
            '@' => vec![archive_name.to_string()],
            '%' => vec![member.to_string()],
            '?' => newer.to_vec(),
            '^' => {
                // All prerequisites, duplicates removed, order preserved.
                let mut seen = std::collections::HashSet::new();
                all_prereqs
                    .into_iter()
                    .filter(|p| seen.insert(p.clone()))
                    .collect()
            }
            '+' => all_prereqs,
            '<' => vec![files.0.to_string_lossy().into_owned()],
            '*' => vec![files.1.to_string_lossy().into_owned()],
            _ => return String::new(),
        };

        let parts: Vec<String> = match modifier {
            Some('D') => base.iter().map(|p| dir_part(p)).collect(),
            Some('F') => base.iter().map(|p| file_part(p)).collect(),
            _ => base,
        };
        parts.join(" ")
    }

    fn substitute_internal_macros(
        &self,
        target: &Target,
        recipe: &Recipe,
        files: &(PathBuf, PathBuf),
        newer: &[String],
    ) -> Recipe {
        const SIGILS: [char; 7] = ['@', '%', '?', '<', '*', '^', '+'];
        let recipe = recipe.inner();
        let mut stream = recipe.chars().peekable();
        let mut result = String::new();

        while let Some(ch) = stream.next() {
            if ch != '$' {
                result.push(ch);
                continue;
            }

            match stream.peek().copied() {
                Some('$') => {
                    stream.next();
                    result.push('$');
                }
                // Two-character form, e.g. `$@`, `$^`, `$?`.
                Some(c) if SIGILS.contains(&c) => {
                    stream.next();
                    result.push_str(&self.expand_internal_macro(c, None, target, files, newer));
                }
                // Bracketed form, e.g. `$(@)`, `$(@D)`, `$(?F)`.
                Some(open @ ('(' | '{')) => {
                    let close = if open == '(' { ')' } else { '}' };
                    let mut inner = String::new();
                    stream.next();
                    while let Some(&c) = stream.peek() {
                        if c == close {
                            stream.next();
                            break;
                        }
                        inner.push(c);
                        stream.next();
                    }
                    let mut chars = inner.chars();
                    match chars.next() {
                        Some(sigil) if SIGILS.contains(&sigil) => {
                            let modifier = chars.next().filter(|m| matches!(m, 'D' | 'F'));
                            result.push_str(
                                &self.expand_internal_macro(sigil, modifier, target, files, newer),
                            );
                        }
                        // The special `MAKE` macro expands to the make program.
                        _ if inner == "MAKE" => result.push_str(&make_program()),
                        // Not an internal macro: re-emit verbatim.
                        _ => {
                            result.push('$');
                            result.push(open);
                            result.push_str(&inner);
                            result.push(close);
                        }
                    }
                }
                // A stray `$` (or `$` at end of line): emit it literally.
                _ => result.push('$'),
            }
        }

        Recipe::new(result)
    }

    /// A helper function to initialize env vars for shell commands.
    fn init_env(&self, env_macros: bool, command: &mut Command, variables: &[Macro]) {
        let mut macros: HashMap<String, String> = variables.iter().cloned().collect();

        // POSIX: the `SHELL` macro shall not modify the `SHELL` environment
        // variable seen by recipes, so never export it. The child still
        // inherits the real `SHELL` from this process's environment.
        macros.remove(DEFAULT_SHELL_VAR);

        if env_macros {
            let env_vars: HashMap<String, String> = std::env::vars().collect();
            macros.extend(env_vars);
        }
        command.envs(macros);
    }
}

impl From<ParsedRule> for Rule {
    fn from(parsed: ParsedRule) -> Self {
        let config = Config::default();
        Self::from((parsed, config))
    }
}

impl From<(ParsedRule, Config)> for Rule {
    fn from((parsed, config): (ParsedRule, Config)) -> Self {
        let targets = parsed.targets().map(Target::new).collect();
        let prerequisites = parsed.prerequisites().map(Prerequisite::new).collect();
        let recipes = parsed.recipes().map(Recipe::new).collect();
        Rule {
            targets,
            prerequisites,
            recipes,
            config,
        }
    }
}

fn find_files_with_extension(ext: &str) -> Result<Vec<PathBuf>, ErrorCode> {
    use std::{env, fs};

    let mut result = vec![];
    let Ok(current) = env::current_dir() else {
        Err(IoError(ErrorKind::PermissionDenied))?
    };
    let mut dirs_to_walk = VecDeque::new();
    dirs_to_walk.push_back(current);

    while let Some(path) = dirs_to_walk.pop_front() {
        let files = fs::read_dir(path)?;
        for file in files.filter_map(Result::ok) {
            let Ok(metadata) = file.metadata() else {
                continue;
            };

            if metadata.is_file() {
                if let Some(e) = file.path().extension() {
                    if ext == e {
                        result.push(file.path());
                    }
                }
            }
        }
    }

    Ok(result)
}

/// The make program to substitute for the `$(MAKE)` macro: this executable's
/// path if known, else the bare name `make`.
fn make_program() -> String {
    std::env::current_exe()
        .ok()
        .and_then(|p| p.to_str().map(String::from))
        .unwrap_or_else(|| "make".to_string())
}

/// The directory part of a path (the prefix without a trailing slash). For a
/// path with no slash the directory part is `.`; for a root-anchored name the
/// directory part is `/`.
fn dir_part(path: &str) -> String {
    match path.rfind('/') {
        Some(0) => "/".to_string(),
        Some(i) => path[..i].to_string(),
        None => ".".to_string(),
    }
}

/// The filename part of a path (everything after the last slash).
fn file_part(path: &str) -> String {
    match path.rfind('/') {
        Some(i) => path[i + 1..].to_string(),
        None => path.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{dir_part, file_part};

    #[test]
    fn dir_and_file_parts() {
        // No directory component -> "." and the whole name (audit #15 D/F).
        assert_eq!(dir_part("obj.o"), ".");
        assert_eq!(file_part("obj.o"), "obj.o");
        // Nested path.
        assert_eq!(dir_part("sub/dir/obj.o"), "sub/dir");
        assert_eq!(file_part("sub/dir/obj.o"), "obj.o");
        // Root-anchored.
        assert_eq!(dir_part("/obj.o"), "/");
        assert_eq!(file_part("/obj.o"), "obj.o");
    }
}
