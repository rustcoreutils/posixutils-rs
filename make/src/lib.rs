//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod config;
pub mod error_code;
pub mod parser;
pub mod rule;
pub mod signal_handler;
pub mod special_target;

use std::{
    collections::HashSet,
    fs::{self},
    time::SystemTime,
};

use parser::{Makefile, VariableDefinition};

use crate::special_target::InferenceTarget;
use config::Config;
use error_code::ErrorCode::{self, *};
use rule::{prerequisite::Prerequisite, target::Target, Rule};
use special_target::SpecialTarget;

/// The default shell variable name.
const DEFAULT_SHELL_VAR: &str = "SHELL";

/// The default shell to use for running recipes. Linux and MacOS
const DEFAULT_SHELL: &str = "/bin/sh";

/// Represents the make utility with its data and configuration.
///
/// The only way to create a Make is from a Makefile and a Config.
pub struct Make {
    macros: Vec<VariableDefinition>,
    /// Target rules (non-special, non-inference).
    /// Invariant: inference rules are never stored here, so `first_target()`
    /// always returns a valid default target per POSIX.
    rules: Vec<Rule>,
    /// Inference rules (e.g. `.c.o:`, `.txt.out:`).
    inference_rules: Vec<Rule>,
    default_rule: Option<Rule>, // .DEFAULT
    pub config: Config,
}

impl Make {
    /// Retrieves the rule that has the given target.
    ///
    /// # Returns
    ///
    /// - Some(rule) if a rule with the target exists.
    /// - None if no rule with the target exists.
    fn rule_by_target_name(&self, target: impl AsRef<str>) -> Option<&Rule> {
        self.rules
            .iter()
            .find(|rule| rule.targets().any(|t| t.as_ref() == target.as_ref()))
    }

    pub fn first_target(&self) -> Result<&Target, ErrorCode> {
        // Per POSIX: "the first target that make encounters that is not a special
        // target or an inference rule shall be used."
        // If there are no non-special, non-inference targets, fall back to the
        // first inference rule (which will scan CWD for matching files).
        let rule = self
            .rules
            .first()
            .or_else(|| self.inference_rules.first())
            .ok_or(NoTarget { target: None })?;
        rule.targets().next().ok_or(NoTarget { target: None })
    }

    /// Finds a matching inference rule for the given target name.
    ///
    /// Per POSIX: the suffix of the target (.s1) is compared to .SUFFIXES.
    /// If found, inference rules are searched for the first .s2.s1 rule whose
    /// prerequisite file ($*.s2) exists.
    fn find_inference_rule(&self, name: &str) -> Option<&Rule> {
        let suffixes = self.config.rules.get(".SUFFIXES")?;

        // Find the target's suffix (.s1)
        let target_suffix = suffixes
            .iter()
            .filter(|s| name.ends_with(s.as_str()))
            .max_by_key(|s| s.len())?;

        let stem = &name[..name.len() - target_suffix.len()];

        // Search inference rules for .s2.s1 where $*.s2 exists
        for rule in &self.inference_rules {
            let Some(rule_target) = rule.targets().next() else {
                continue;
            };
            if let Target::Inference { from, to, .. } = rule_target {
                let expected_suffix = format!(".{}", to);
                if expected_suffix == *target_suffix {
                    let prereq_path = format!("{}.{}", stem, from);
                    if std::path::Path::new(&prereq_path).exists() {
                        return Some(rule);
                    }
                }
            }
        }
        None
    }

    /// Builds the target with the given name.
    ///
    /// # Returns
    /// - Ok(true) if the target was built.
    /// - Ok(false) if the target was already up to date.
    /// - Err(_) if any errors occur.
    pub fn build_target(&self, name: impl AsRef<str>) -> Result<bool, ErrorCode> {
        // Search both regular rules and inference rules
        let rule = match self.rule_by_target_name(&name) {
            Some(rule) => rule,
            None => match self
                .inference_rules
                .iter()
                .find(|rule| rule.targets().any(|t| t.as_ref() == name.as_ref()))
            {
                Some(rule) => rule,
                None => {
                    // Per POSIX: "If a target exists and there is neither a target rule
                    // nor an inference rule for the target, the target shall be considered
                    // up-to-date."
                    if get_modified_time(&name).is_some() {
                        return Ok(false);
                    }
                    // No rule and file doesn't exist - try .DEFAULT or fail
                    match &self.default_rule {
                        Some(rule) => rule,
                        None => {
                            return Err(NoTarget {
                                target: Some(name.as_ref().to_string()),
                            })
                        }
                    }
                }
            },
        };
        let target = Target::new(name.as_ref());

        self.run_rule_with_prerequisites(rule, &target)
    }

    /// Runs the given rule.
    ///
    /// # Returns
    /// - Ok(true) if the rule was run.
    /// - Ok(false) if the rule was already up to date.
    /// - Err(_) if any errors occur.
    fn run_rule_with_prerequisites(&self, rule: &Rule, target: &Target) -> Result<bool, ErrorCode> {
        if self.are_prerequisites_recursive(target) {
            return Err(RecursivePrerequisite {
                origin: target.to_string(),
            });
        }

        let newer_prerequisites = self.get_newer_prerequisites(target);
        let mut up_to_date = newer_prerequisites.is_empty() && get_modified_time(target).is_some();
        if rule.config.phony {
            up_to_date = false;
        }

        if up_to_date {
            return Ok(false);
        }

        for prerequisite in &newer_prerequisites {
            self.build_target(prerequisite)?;
        }

        // Per POSIX: "When no target rule with commands is found to update a
        // target, the inference rules shall be checked."  If the matched target
        // rule has no recipes, look for a matching inference rule and run it
        // for this specific target instead.
        if rule.recipes().count() == 0 {
            if let Some(inference_rule) = self.find_inference_rule(target.as_ref()) {
                inference_rule.run_for_target(&self.config, &self.macros, target, up_to_date)?;
                return Ok(true);
            }
        }

        rule.run(&self.config, &self.macros, target, up_to_date)?;

        Ok(true)
    }

    /// Retrieves the prerequisites of the target that are newer than the target.
    /// Recursively checks the prerequisites of the prerequisites.
    /// Returns an empty vector if the target does not exist (or it's a file).
    fn get_newer_prerequisites(&self, target: impl AsRef<str>) -> Vec<&Prerequisite> {
        let Some(target_rule) = self.rule_by_target_name(&target) else {
            return vec![];
        };
        let target_modified = get_modified_time(target);

        let prerequisites = target_rule.prerequisites();

        if let Some(target_modified) = target_modified {
            prerequisites
                .filter(|prerequisite| {
                    let Some(pre_modified) = get_modified_time(prerequisite) else {
                        return true;
                    };

                    !self.get_newer_prerequisites(prerequisite).is_empty()
                        || pre_modified > target_modified
                })
                .collect()
        } else {
            prerequisites.collect()
        }
    }

    /// Checks if the target has recursive prerequisites.
    /// Returns true if the target has recursive prerequisites.
    fn are_prerequisites_recursive(&self, target: impl AsRef<str>) -> bool {
        let mut visited = HashSet::from([target.as_ref()]);
        let mut stack = HashSet::from([target.as_ref()]);

        self._are_prerequisites_recursive(target.as_ref(), &mut visited, &mut stack)
    }

    /// A helper function to check if the target has recursive prerequisites.
    /// Uses DFS to check for recursive prerequisites.
    fn _are_prerequisites_recursive(
        &self,
        target: &str,
        visited: &mut HashSet<&str>,
        stack: &mut HashSet<&str>,
    ) -> bool {
        let Some(rule) = self.rule_by_target_name(target) else {
            return false;
        };

        let prerequisites = rule.prerequisites();

        for prerequisite in prerequisites {
            if (!visited.contains(prerequisite.as_ref())
                && self._are_prerequisites_recursive(prerequisite.as_ref(), visited, stack))
                || stack.contains(prerequisite.as_ref())
            {
                return true;
            }
        }

        stack.remove(target);
        false
    }
}

impl TryFrom<(Makefile, Config)> for Make {
    type Error = ErrorCode;

    fn try_from((makefile, config): (Makefile, Config)) -> Result<Self, Self::Error> {
        // Two-pass classification: .SUFFIXES must be processed before inference
        // rule classification so that user-defined suffixes (especially with -r)
        // are available when determining whether a rule like `.txt.out:` is an
        // inference rule.

        let mut suffixes_rules = vec![];
        let mut remaining_parsed_rules = vec![];

        // Pass 1: Separate .SUFFIXES rules from everything else and process
        // them immediately so config.rules[".SUFFIXES"] is populated.
        for parsed_rule in makefile.rules() {
            let rule = Rule::from(parsed_rule);
            let Some(target) = rule.targets().next() else {
                return Err(NoTarget { target: None });
            };
            if let Ok(SpecialTarget::Suffixes) = SpecialTarget::try_from(target.clone()) {
                suffixes_rules.push(rule);
            } else {
                remaining_parsed_rules.push(rule);
            }
        }

        // Build the Make struct early so we can process .SUFFIXES via the
        // normal special_target::process path (which writes to make.config).
        let mut make = Self {
            rules: vec![],
            inference_rules: vec![],
            macros: makefile.variable_definitions().collect(),
            default_rule: None,
            config,
        };

        for rule in suffixes_rules {
            special_target::process(rule, &mut make)?;
        }

        // Pass 2: Classify remaining rules.  Now make.config.rules[".SUFFIXES"]
        // contains both built-in (unless -r) and user-defined suffixes.
        let mut special_rules = vec![];

        for rule in remaining_parsed_rules {
            let Some(target) = rule.targets().next() else {
                return Err(NoTarget { target: None });
            };

            if SpecialTarget::try_from(target.clone()).is_ok() {
                special_rules.push(rule);
            } else if InferenceTarget::try_from((target.clone(), make.config.clone())).is_ok() {
                make.inference_rules.push(rule);
            } else {
                make.rules.push(rule);
            }
        }

        for rule in special_rules {
            special_target::process(rule, &mut make)?;
        }

        Ok(make)
    }
}

/// Retrieves the modified time of the file at the given path.
fn get_modified_time(path: impl AsRef<str>) -> Option<SystemTime> {
    fs::metadata(path.as_ref())
        .ok()
        .and_then(|meta| meta.modified().ok())
}
