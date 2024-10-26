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
    parser::{Rule as ParsedRule, VariableDefinition},
    signal_handler, DEFAULT_SHELL, DEFAULT_SHELL_VAR,
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
    env,
    fs::{File, FileTimes},
    process::{self, Command},
    sync::{Arc, LazyLock, Mutex},
    time::SystemTime,
};
use target::Target;

type LazyArcMutex<T> = LazyLock<Arc<Mutex<T>>>;

pub static INTERRUPT_FLAG: LazyArcMutex<Option<(String, bool)>> =
    LazyLock::new(|| Arc::new(Mutex::new(None)));

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

    /// Runs the rule with the global config and macros passed in.
    ///
    /// Returns `Ok` on success and `Err` on any errors while running the rule.
    pub fn run(
        &self,
        global_config: &GlobalConfig,
        macros: &[VariableDefinition],
        target: &Target,
        up_to_date: bool,
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
            rules: _,
        } = *global_config;
        let Config {
            ignore: rule_ignore,
            silent: rule_silent,
            precious: rule_precious,
            phony: _,
        } = self.config;

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

                *INTERRUPT_FLAG.lock().unwrap() = Some((target.as_ref().to_string(), precious));

                if !ignore || print || quit || dry_run {
                    signal_handler::register_signals();
                }

                if !force_run {
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

                let mut command = Command::new(
                    env::var(DEFAULT_SHELL_VAR)
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or(DEFAULT_SHELL),
                );

                self.init_env(env_macros, &mut command, macros);
                let recipe =
                    self.substitute_internal_macros(target, recipe, &inout, self.prerequisites());
                command.args(["-c", recipe.as_ref()]);

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

    fn substitute_internal_macros<'a>(
        &self,
        target: &Target,
        recipe: &Recipe,
        files: &(PathBuf, PathBuf),
        mut prereqs: impl Iterator<Item = &'a Prerequisite>,
    ) -> Recipe {
        let recipe = recipe.inner();
        let mut stream = recipe.chars();
        let mut result = String::new();

        while let Some(ch) = stream.next() {
            if ch != '$' {
                result.push(ch);
                continue;
            }

            match stream.next() {
                Some('@') => {
                    if let Some(s) = target.as_ref().split('(').next() {
                        result.push_str(s)
                    }
                }
                Some('%') => {
                    if let Some(body) = target.as_ref().split('(').nth(1) {
                        result.push_str(body.strip_suffix(')').unwrap_or(body))
                    }
                }
                Some('?') => {
                    (&mut prereqs)
                        .map(|x| x.as_ref())
                        .for_each(|x| result.push_str(x));
                }
                Some('$') => result.push('$'),
                Some('<') => result.push_str(files.0.to_str().unwrap()),
                Some('*') => result.push_str(files.1.to_str().unwrap()),
                Some(_) => break,
                None => {
                    eprintln!("Unexpected `$` at the end of the rule!")
                }
            }
        }

        Recipe::new(result)
    }

    /// A helper function to initialize env vars for shell commands.
    fn init_env(&self, env_macros: bool, command: &mut Command, variables: &[VariableDefinition]) {
        let mut macros: HashMap<String, String> = variables
            .iter()
            .map(|v| {
                (
                    v.name().unwrap_or_default(),
                    v.raw_value().unwrap_or_default(),
                )
            })
            .collect();

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
