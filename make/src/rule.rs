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

use std::{
    env,
    fs::{File, FileTimes},
    process::Command,
    time::SystemTime,
};

use crate::{
    config::Config as GlobalConfig,
    error_code::ErrorCode::{self, *},
    DEFAULT_SHELL, DEFAULT_SHELL_VAR,
};
use config::Config;
use makefile_lossless::{Rule as ParsedRule, VariableDefinition};
use prerequisite::Prerequisite;
use recipe::config::Config as RecipeConfig;
use recipe::Recipe;
use target::Target;

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
    ) -> Result<(), ErrorCode> {
        let GlobalConfig {
            ignore: global_ignore,
            dry_run: global_dry_run,
            silent: global_silent,
            touch: global_touch,
        } = *global_config;
        let Config {
            ignore: rule_ignore,
            silent: rule_silent,
        } = self.config;

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
            self.init_env(&mut command, macros);
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
                return Err(ExecutionError {
                    exit_code: status.code(),
                });
            }
        }

        let silent = global_silent || rule_silent;
        let touch = global_touch;

        // -t flag
        if touch {
            if !silent {
                println!("touch {target}");
            }
            let file = File::create(target.as_ref())?;
            file.set_times(FileTimes::new().set_modified(SystemTime::now()))?;
            return Ok(());
        }

        Ok(())
    }

    /// A helper function to initialize env vars for shell commands.
    fn init_env(&self, command: &mut Command, variables: &[VariableDefinition]) {
        command.envs(variables.iter().map(|v| {
            (
                v.name().unwrap_or_default(),
                v.raw_value().unwrap_or_default(),
            )
        }));
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
