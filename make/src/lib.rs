//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
    time::{Duration, SystemTime},
};

use parser::Makefile;

/// An owned macro definition `(name, value)`. Owning the data (rather than
/// holding a rowan AST node) keeps `Make` `Send`/`Sync` for parallel builds.
pub type Macro = (String, String);

use crate::special_target::InferenceTarget;
use config::Config;
use error_code::ErrorCode::{self, *};
use rule::{prerequisite::Prerequisite, target::Target, Rule};
use special_target::SpecialTarget;

/// The default shell variable name.
const DEFAULT_SHELL_VAR: &str = "SHELL";

/// The default shell to use for running recipes. Linux and MacOS
const DEFAULT_SHELL: &str = "/bin/sh";

/// The `.WAIT` special target, used as a prerequisite-list barrier.
const WAIT_TARGET: &str = ".WAIT";

/// A pool of build tokens bounding how many targets are updated concurrently
/// under `-j`. Acquisition is non-blocking: a caller that cannot get a token
/// builds the target inline instead, which keeps the recursive build
/// deadlock-free (the inline path always makes progress).
struct TokenPool {
    available: std::sync::Mutex<usize>,
}

impl TokenPool {
    fn new(tokens: usize) -> Self {
        TokenPool {
            available: std::sync::Mutex::new(tokens),
        }
    }

    fn try_acquire(&self) -> bool {
        let mut available = self.available.lock().unwrap();
        if *available > 0 {
            *available -= 1;
            true
        } else {
            false
        }
    }

    fn release(&self) {
        *self.available.lock().unwrap() += 1;
    }
}

/// Represents the make utility with its data and configuration.
///
/// The only way to create a Make is from a Makefile and a Config.
pub struct Make {
    macros: Vec<Macro>,
    /// Target rules (non-special, non-inference).
    /// Invariant: inference rules are never stored here, so `first_target()`
    /// always returns a valid default target per POSIX.
    rules: Vec<Rule>,
    /// Inference rules (e.g. `.c.o:`, `.txt.out:`).
    inference_rules: Vec<Rule>,
    default_rule: Option<Rule>, // .DEFAULT
    /// Token pool bounding concurrent target updates under `-j` (maxjobs - 1
    /// tokens; the inline build needs no token).
    pool: TokenPool,
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
        let suffixes = &self.config.suffixes;

        // Double-suffix: the target has a known suffix `.s1`; find a `.s2.s1`
        // rule whose prerequisite `$*.s2` exists.
        if let Some(target_suffix) = suffixes
            .iter()
            .filter(|s| name.ends_with(s.as_str()))
            .max_by_key(|s| s.len())
        {
            let stem = &name[..name.len() - target_suffix.len()];
            for rule in &self.inference_rules {
                if let Some(Target::Inference { from, to, .. }) = rule.targets().next() {
                    if !to.is_empty() && format!(".{}", to) == *target_suffix {
                        let prereq_path = format!("{}.{}", stem, from);
                        if std::path::Path::new(&prereq_path).exists() {
                            return Some(rule);
                        }
                    }
                }
            }
        }

        // Single-suffix: the target has no suffix; find a `.s2` (single-suffix)
        // rule whose prerequisite `<name>.s2` exists.
        for rule in &self.inference_rules {
            if let Some(Target::Inference { from, to, .. }) = rule.targets().next() {
                if to.is_empty() {
                    let prereq_path = format!("{}.{}", name, from);
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
                    // No target rule named `name`: try to infer one (single- or
                    // double-suffix) from an existing prerequisite file.
                    if let Some(inference_rule) = self.find_inference_rule(name.as_ref()) {
                        let target = Target::new(name.as_ref());
                        inference_rule.run_for_target(
                            &self.config,
                            &self.macros,
                            &target,
                            false,
                            &[],
                        )?;
                        return Ok(true);
                    }
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

        self.build_prerequisites(&newer_prerequisites)?;

        // `$?` expands to the prerequisites newer than the target (the `.WAIT`
        // barrier markers are not real prerequisites).
        let newer: Vec<String> = newer_prerequisites
            .iter()
            .map(|p| p.as_ref().to_string())
            .filter(|p| p != WAIT_TARGET)
            .collect();

        // Per POSIX: "When no target rule with commands is found to update a
        // target, the inference rules shall be checked."  If the matched target
        // rule has no recipes, look for a matching inference rule and run it
        // for this specific target instead.
        if rule.recipes().count() == 0 {
            if let Some(inference_rule) = self.find_inference_rule(target.as_ref()) {
                inference_rule.run_for_target(
                    &self.config,
                    &self.macros,
                    target,
                    up_to_date,
                    &newer,
                )?;
                return Ok(true);
            }
        }

        rule.run(&self.config, &self.macros, target, up_to_date, &newer)?;

        Ok(true)
    }

    /// Builds a target's prerequisites, honoring `.WAIT` barriers and, under
    /// `-j`, building independent prerequisites concurrently.
    ///
    /// `.WAIT` prerequisites split the list into segments that must be built in
    /// order: every prerequisite to the left of a `.WAIT` is brought up to date
    /// before any to its right. Within a segment, prerequisites are independent.
    fn build_prerequisites(&self, prerequisites: &[&Prerequisite]) -> Result<(), ErrorCode> {
        let parallel = self.config.jobs > 1
            && !self.config.not_parallel
            && !self.config.dry_run
            && !self.config.quit
            && !self.config.touch;

        let mut segment: Vec<&str> = Vec::new();
        for prerequisite in prerequisites {
            let name = prerequisite.as_ref();
            if name == WAIT_TARGET {
                // Barrier: finish the current segment before continuing.
                self.build_segment(&segment, parallel)?;
                segment.clear();
            } else {
                segment.push(name);
            }
        }
        self.build_segment(&segment, parallel)
    }

    /// Builds one segment of independent prerequisites, in parallel when `-j`
    /// allows it. Parallelism is bounded by the token pool; a prerequisite that
    /// cannot obtain a token is built inline so the build always progresses.
    fn build_segment(&self, names: &[&str], parallel: bool) -> Result<(), ErrorCode> {
        if !parallel || names.len() <= 1 {
            for name in names {
                self.build_target(name)?;
            }
            return Ok(());
        }

        let errors: std::sync::Mutex<Vec<ErrorCode>> = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|scope| {
            let mut handles = Vec::new();
            let mut inline: Vec<&str> = Vec::new();
            // Spawn a worker for each prerequisite that can obtain a token; the
            // rest are built inline in this thread. Spawning first (before any
            // inline build) is what lets the work actually overlap.
            for &name in names {
                if self.pool.try_acquire() {
                    let errors = &errors;
                    handles.push(scope.spawn(move || {
                        let result = self.build_target(name);
                        self.pool.release();
                        if let Err(err) = result {
                            errors.lock().unwrap().push(err);
                        }
                    }));
                } else {
                    inline.push(name);
                }
            }
            for name in inline {
                if let Err(err) = self.build_target(name) {
                    errors.lock().unwrap().push(err);
                }
            }
            for handle in handles {
                let _ = handle.join();
            }
        });

        // Report the first error (if any); recipe-level `-k` handling already
        // happens inside the recipe runner via the KEEP_GOING_ERROR flag.
        match errors.into_inner().unwrap().into_iter().next() {
            Some(err) => Err(err),
            None => Ok(()),
        }
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
        let pool = TokenPool::new(config.jobs.saturating_sub(1));
        let mut make = Self {
            rules: vec![],
            inference_rules: vec![],
            macros: makefile
                .variable_definitions()
                .map(|v| {
                    (
                        v.name().unwrap_or_default(),
                        v.raw_value().unwrap_or_default(),
                    )
                })
                .collect(),
            default_rule: None,
            pool,
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
    let path = path.as_ref();
    // An `archive(member)` target's timestamp is the member's stored mtime
    // inside the `ar` archive, not a file on disk.
    if let Some((archive, member)) = parse_archive_target(path) {
        return archive_member_mtime(archive, member);
    }
    fs::metadata(path)
        .ok()
        .and_then(|meta| meta.modified().ok())
}

/// Splits an `archive(member)` target into its `(archive, member)` parts.
fn parse_archive_target(s: &str) -> Option<(&str, &str)> {
    let s = s.strip_suffix(')')?;
    let open = s.find('(')?;
    let (archive, member) = (&s[..open], &s[open + 1..]);
    if archive.is_empty() || member.is_empty() {
        return None;
    }
    Some((archive, member))
}

/// Reads the stored modification time of `member` inside the `ar` archive at
/// `archive_path`. Supports the common System V / GNU short-name format (member
/// names terminated by `/`). Returns `None` if the archive or member is absent
/// or the header cannot be parsed.
fn archive_member_mtime(archive_path: &str, member: &str) -> Option<SystemTime> {
    const MAGIC: &[u8] = b"!<arch>\n";
    const HEADER_LEN: usize = 60;

    let data = fs::read(archive_path).ok()?;
    if !data.starts_with(MAGIC) {
        return None;
    }

    let mut pos = MAGIC.len();
    while pos + HEADER_LEN <= data.len() {
        let header = &data[pos..pos + HEADER_LEN];
        let name = std::str::from_utf8(&header[0..16]).ok()?.trim_end();
        let name = name.strip_suffix('/').unwrap_or(name);
        let mtime = std::str::from_utf8(&header[16..28]).ok()?.trim();
        let size: usize = std::str::from_utf8(&header[48..58])
            .ok()?
            .trim()
            .parse()
            .ok()?;

        if name == member {
            let secs: u64 = mtime.parse().ok()?;
            return Some(SystemTime::UNIX_EPOCH + Duration::from_secs(secs));
        }

        // Member data follows the header, padded to an even boundary.
        pos += HEADER_LEN + size + (size & 1);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_archive_target_splits() {
        assert_eq!(
            parse_archive_target("libfoo.a(member.o)"),
            Some(("libfoo.a", "member.o"))
        );
        assert_eq!(parse_archive_target("plain.o"), None);
        assert_eq!(parse_archive_target("libfoo.a()"), None);
        assert_eq!(parse_archive_target("(member.o)"), None);
    }

    #[test]
    fn archive_member_mtime_reads_header() {
        // Build a minimal `ar` archive containing one member `m.o` (4 bytes)
        // with mtime 1234567890, and confirm the stored time is read back.
        let mut ar = Vec::new();
        ar.extend_from_slice(b"!<arch>\n");
        let header = format!(
            "{:<16}{:<12}{:<6}{:<6}{:<8}{:<10}`\n",
            "m.o/", "1234567890", "0", "0", "100644", "4"
        );
        ar.extend_from_slice(header.as_bytes());
        ar.extend_from_slice(b"data");

        let path = std::env::temp_dir().join("posixutils_make_ar_member_test.a");
        fs::write(&path, &ar).unwrap();
        let path_str = path.to_str().unwrap();

        let expected = SystemTime::UNIX_EPOCH + Duration::from_secs(1234567890);
        assert_eq!(archive_member_mtime(path_str, "m.o"), Some(expected));
        assert_eq!(archive_member_mtime(path_str, "absent.o"), None);

        let _ = fs::remove_file(&path);
    }
}
