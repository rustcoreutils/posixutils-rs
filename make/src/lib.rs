//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod attributes;
pub mod config;
pub mod error_code;
pub mod graph;
pub mod parser;
pub mod rule;
pub mod signal_handler;
pub mod special_target;
mod stat_cache;

use std::os::unix::fs::PermissionsExt;
use std::{
    fs::{self},
    time::{Duration, SystemTime},
};

use parser::preprocessor::{self, MacroSource, MacroTable};
use parser::{Makefile, TargetVar, VPathEntry};
use std::collections::HashMap;

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

/// How many pattern rules may be chained to make a prerequisite of a
/// pattern rule (GNU make chains them; POSIX allows it as an extension).
const MAX_PATTERN_CHAIN: usize = 2;

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
    /// The same macros with the source of each: what command lines are
    /// expanded against, and what target-specific assignments are applied to.
    table: MacroTable,
    /// Target-specific macro assignments, by target, in the order read.
    target_vars: HashMap<String, Vec<TargetVar>>,
    /// Pattern-specific ones (a `%` target), with the pattern, in order.
    pattern_vars: Vec<(String, TargetVar)>,
    /// Every name a target rule has as a target or a prerequisite: what
    /// GNU make says "ought to exist" when it picks a pattern rule.
    mentioned: std::collections::HashSet<String>,
    /// The modification times of targets without commands, as they were
    /// before their prerequisites were made; see `prerequisite_mtime`.
    frozen_mtimes: std::sync::Mutex<HashMap<String, Option<SystemTime>>>,
    /// Targets whose commands ran in this run: known by the name they are
    /// written under from then on, never by one `VPATH` found.
    remade: std::sync::Mutex<std::collections::HashSet<String>>,
    /// `stat` results since the last command ran.
    stats: stat_cache::StatCache,
    /// Targets already known to reach no dependency cycle.
    acyclic: std::sync::Mutex<std::collections::HashSet<String>>,
    /// Target rules (non-special, non-inference).
    /// Invariant: inference rules are never stored here, so `first_target()`
    /// always returns a valid default target per POSIX.
    rules: Vec<Rule>,
    /// Where each target name appears in `rules`: the positions of every
    /// rule with that name among its targets, in `rules` order. Looking a
    /// target up by scanning `rules` made a makefile of n targets O(n^2) to
    /// read and to walk.
    rule_index: HashMap<String, Vec<usize>>,
    /// The positions of the `%` pattern rules in `rules`, in order.
    pattern_rules: Vec<usize>,
    /// Inference rules (e.g. `.c.o:`, `.txt.out:`).
    inference_rules: Vec<Rule>,
    default_rule: Option<Rule>, // .DEFAULT
    /// The recipe that retrieves a source file from SCCS. Built-in unless the
    /// makefile supplies its own `.SCCS_GET`.
    sccs_get: Option<Rule>,
    /// Token pool bounding concurrent target updates under `-j` (maxjobs - 1
    /// tokens; the inline build needs no token).
    pool: TokenPool,
    /// Which targets are built, building, or failed. Gives single-build under
    /// `-j` and memoization across repeated visits.
    ledger: graph::Ledger,
    /// `vpath` search paths, in declaration order.
    vpaths: Vec<VPathEntry>,
    /// Macros an `export` directive put in the recipe environment.
    exports: Vec<String>,
    /// What the attribute special targets say about each target.
    attributes: attributes::Attributes,
    pub config: Config,
}

impl graph::Edges for Make {
    /// The prerequisites the cycle check must see.
    ///
    /// A named rule is not the only way a target acquires prerequisites: a `%`
    /// pattern rule contributes them once instantiated for a concrete name,
    /// and an inference rule contributes the source it derives from its
    /// suffixes. Looking only at named rules made `%.a: %.a` invisible to
    /// `find_cycle`, so the self-edge went undetected and the build deadlocked
    /// on it (audit #65); the same hole let `.c.o:` plus `.o.c:` build `foo.o`
    /// from `foo.c` from `foo.o`, where the ledger blocked the one thread on a
    /// target it was itself in the middle of building (audit #80).
    ///
    /// The order below mirrors `build_target_uncached`, so the edges reported
    /// are the edges that will actually be walked.
    fn prerequisites_of(&self, target: &str) -> Vec<String> {
        let named = self.rule_by_target_name(target);
        if named.is_some_and(Rule::is_double_colon) {
            return self
                .double_colon_rules(target)
                .into_iter()
                .flat_map(|rule| edge_names(rule.prerequisites().chain(rule.order_only())))
                .collect();
        }
        if let Some(rule) = named {
            if let Some(pattern) = self.pattern_rule_for_commandless(rule, target) {
                let merged = pattern.with_prerequisites_of(rule);
                return edge_names(merged.prerequisites().chain(merged.order_only()));
            }
        }
        let mut edges = match named {
            Some(rule) => edge_names(rule.prerequisites().chain(rule.order_only())),
            None => match self.find_pattern_rule(target) {
                Some(rule) => return edge_names(rule.prerequisites().chain(rule.order_only())),
                None => Vec::new(),
            },
        };
        // A rule carrying commands is the one that runs; only a commandless
        // one (or none at all) falls through to inference.
        if named.is_some_and(|rule| rule.recipes().count() > 0) {
            return edges;
        }
        let source = self
            .find_inference_rule(target)
            .and_then(|rule| inference_source(rule, &Target::new(target)));
        if let Some(source) = source {
            if !edges.contains(&source) {
                edges.push(source);
            }
        }
        edges
    }
}

/// Prerequisite names as graph edges. `.WAIT` is a barrier marker, not a target.
fn edge_names<'a>(prerequisites: impl Iterator<Item = &'a Prerequisite>) -> Vec<String> {
    prerequisites
        .map(|p| p.as_ref().to_string())
        .filter(|p| p != WAIT_TARGET)
        .collect()
}

impl Make {
    /// Retrieves the rule that has the given target.
    ///
    /// # Returns
    ///
    /// - Some(rule) if a rule with the target exists.
    /// - None if no rule with the target exists.
    fn rule_by_target_name(&self, target: impl AsRef<str>) -> Option<&Rule> {
        self.rules_named(target.as_ref())
            .find(|rule| !rule.is_pattern())
    }

    /// Every rule with `name` among its targets, in the order filed.
    fn rules_named<'a>(&'a self, name: &str) -> impl Iterator<Item = &'a Rule> + 'a {
        let at: &[usize] = self.rule_index.get(name).map_or(&[], Vec::as_slice);
        at.iter().map(|&i| &self.rules[i])
    }

    /// The `%` pattern rules, in the order filed.
    fn pattern_rules(&self) -> impl Iterator<Item = &Rule> {
        self.pattern_rules.iter().map(|&i| &self.rules[i])
    }

    /// File `rule` at the end of `rules`, and index it.
    fn push_rule(&mut self, rule: Rule) {
        let at = self.rules.len();
        for target in rule.targets() {
            let positions = self
                .rule_index
                .entry(target.as_ref().to_string())
                .or_default();
            // A target named twice in one rule is still one rule.
            if positions.last() != Some(&at) {
                positions.push(at);
            }
        }
        if rule.is_pattern() {
            self.pattern_rules.push(at);
        }
        self.rules.push(rule);
    }

    /// Every macro definition in force, in first-definition order.
    pub fn macros(&self) -> &[Macro] {
        &self.macros
    }

    /// The complete set of macro definitions and target descriptions, in
    /// makefile syntax (POSIX 105395 mandates the content and leaves the format
    /// unspecified).
    ///
    /// Rendering it as makefile text rather than a bespoke format means the
    /// dump round-trips: `make -p` output is itself a makefile.
    pub fn database(&self) -> String {
        let mut out = String::new();
        self.write_macros(&mut out);
        self.write_section(&mut out, "rules", self.rules.iter());
        self.write_section(&mut out, "inference rules", self.inference_rules.iter());
        self.write_section(&mut out, "default rule", self.default_rule.iter());
        self.write_section(&mut out, "sccs get", self.sccs_get.iter());
        self.write_vpaths(&mut out);
        out
    }

    fn write_macros(&self, out: &mut String) {
        if self.macros.is_empty() {
            return;
        }
        out.push_str("# macros\n");
        for (name, value) in &self.macros {
            // No trailing blank on an empty value, so the dump stays clean to
            // read back.
            match value.is_empty() {
                true => out.push_str(&format!("{name} =\n")),
                false => out.push_str(&format!("{name} = {value}\n")),
            }
        }
        out.push('\n');
    }

    fn write_section<'a>(
        &self,
        out: &mut String,
        title: &str,
        rules: impl Iterator<Item = &'a Rule>,
    ) {
        let rendered: Vec<String> = rules.map(|rule| rule.to_string()).collect();
        if rendered.is_empty() {
            return;
        }
        out.push_str(&format!("# {title}\n"));
        for rule in rendered {
            out.push_str(&rule);
        }
        out.push('\n');
    }

    fn write_vpaths(&self, out: &mut String) {
        if self.vpaths.is_empty() {
            return;
        }
        out.push_str("# search paths\n");
        for entry in &self.vpaths {
            out.push_str(&format!(
                "vpath {} {}\n",
                entry.pattern,
                entry.dirs.join(" ")
            ));
        }
        out.push('\n');
    }

    /// Directories named by the `VPATH` macro, in search order.
    ///
    /// POSIX has no `VPATH`, but it is the conventional way to keep sources in
    /// one tree and build in another. Both the `:`-separated and
    /// blank-separated spellings are accepted, as GNU does.
    fn vpath_dirs(&self) -> Vec<&str> {
        self.macros
            .iter()
            .find(|(name, _)| name == "VPATH")
            .map(|(_, value)| {
                value
                    .split([':', ' ', '\t'])
                    .filter(|d| !d.is_empty())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Resolve a prerequisite through `VPATH`.
    ///
    /// A name that exists as written, or that no `VPATH` directory supplies, is
    /// returned unchanged, so this is transparent when `VPATH` is unset. An
    /// absolute name is never searched for, but a relative one with a
    /// directory part is, as in GNU make: automake's subdir-objects builds
    /// out of tree compile `common/force.o` from `$(srcdir)/common/force.c`
    /// (dpkg, gmp, libffi, xz-utils and others).
    ///
    /// A target whose commands ran is the file they made here, as in GNU
    /// make: `$<` then names `parse.c`, not the stale `$(srcdir)/parse.c`.
    pub fn resolve_vpath(&self, name: &str) -> String {
        if name.starts_with('/')
            || self.stats.exists(name)
            || self.remade.lock().unwrap().contains(name)
        {
            return name.to_string();
        }
        // A `vpath` pattern is more specific than the blanket `VPATH` macro, so
        // it is consulted first; the first matching pattern wins, as GNU does.
        if let Some(found) = self.search_vpath_patterns(name) {
            return found;
        }
        self.search_dirs(&self.vpath_dirs(), name)
            .unwrap_or_else(|| name.to_string())
    }

    /// Look `name` up in the directories of the first `vpath` pattern it
    /// matches.
    fn search_vpath_patterns(&self, name: &str) -> Option<String> {
        for entry in &self.vpaths {
            if rule::pattern_stem(&entry.pattern, name).is_none() {
                continue;
            }
            let dirs: Vec<&str> = entry.dirs.iter().map(String::as_str).collect();
            if let Some(found) = self.search_dirs(&dirs, name) {
                return Some(found);
            }
        }
        None
    }

    /// Find a `%` pattern rule matching `name` and instantiate it for that
    /// target.
    ///
    /// Pattern rules are not POSIX -- the standard has only suffix inference
    /// rules -- but they are how real makefiles express the same thing. As in
    /// GNU make, a rule applies only if each of its prerequisites exists
    /// (`VPATH` searched) or ought to exist, being named by some rule; the
    /// first such rule in file order wins. Failing that, the first whose
    /// missing prerequisites another pattern or inference rule can make.
    /// libzstd has `$(D)/%.o : %.c` and `$(D)/%.o : %.S`, and which one
    /// builds an object depends on which source there is.
    fn find_pattern_rule(&self, name: &str) -> Option<Rule> {
        self.find_pattern_rule_within(name, MAX_PATTERN_CHAIN)
    }

    fn find_pattern_rule_within(&self, name: &str, depth: usize) -> Option<Rule> {
        let candidates: Vec<Rule> = self
            .pattern_rules()
            .filter_map(|rule| rule.instantiate_pattern(name))
            .collect();
        let needs = |rule: &Rule| -> Vec<String> {
            rule.prerequisites()
                .chain(rule.order_only())
                .map(|p| p.as_ref().to_string())
                .filter(|p| p != WAIT_TARGET)
                .collect()
        };
        if let Some(found) = candidates
            .iter()
            .find(|rule| needs(rule).iter().all(|p| self.exists_or_ought_to(p)))
        {
            return Some(found.clone());
        }
        if depth == 0 {
            return None;
        }
        candidates.into_iter().find(|rule| {
            needs(rule).iter().all(|p| {
                self.exists_or_ought_to(p)
                    || self.find_inference_rule(p).is_some()
                    || self.find_pattern_rule_within(p, depth - 1).is_some()
            })
        })
    }

    /// Whether `name` exists, here or through `VPATH`, or ought to exist:
    /// some target rule names it as its target or as a prerequisite (GNU
    /// make's term).
    fn exists_or_ought_to(&self, name: &str) -> bool {
        self.mentioned.contains(name) || self.stats.exists(&self.resolve_vpath(name))
    }

    pub fn first_target(&self) -> Result<&Target, ErrorCode> {
        // POSIX 105428: "the first target that make encounters that is not a
        // special target or an inference rule shall be used."
        //
        // A dot-target such as `.config` is neither a usable default nor an
        // inference rule, and a pattern rule is a template rather than a
        // target, so both are skipped -- matching GNU, which builds the first
        // ordinary target. Falling back to an inference rule (audit #N6) would
        // send a bare `make` scanning the working directory instead.
        self.rules
            .iter()
            .filter(|rule| !rule.is_pattern())
            .filter_map(|rule| rule.targets().next())
            .find(|target| {
                let name = target.as_ref();
                !name.starts_with('.') || name.contains('/')
            })
            .ok_or(NoTargets)
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
            // POSIX 105920/105930: "The order in which the suffixes are
            // specified defines the order in which the inference rules ... are
            // used", so the search iterates `.SUFFIXES`, not the order the
            // rules happen to appear in the makefile (audit #47).
            for source_suffix in suffixes {
                let from_name = source_suffix.trim_start_matches('.');
                let hit = self.inference_rules.iter().find(|rule| {
                    matches!(rule.targets().next(),
                        Some(Target::Inference { from, to, .. })
                            if !to.is_empty()
                                && format!(".{to}") == *target_suffix
                                && from == from_name)
                });
                if let Some(rule) = hit {
                    let prereq_path = self.resolve_vpath(&suffix_source_name(stem, source_suffix));
                    if self.exists_or_ought_to(&prereq_path) {
                        return Some(rule);
                    }
                }
            }
        }

        // Single-suffix: the target has no suffix; find a `.s2` (single-suffix)
        // rule whose prerequisite `<name>.s2` exists. Same ordering rule.
        for source_suffix in suffixes {
            let from_name = source_suffix.trim_start_matches('.');
            let hit = self.inference_rules.iter().find(|rule| {
                matches!(rule.targets().next(),
                    Some(Target::Inference { from, to, .. })
                        if to.is_empty() && from == from_name)
            });
            if let Some(rule) = hit {
                let prereq_path = self.resolve_vpath(&suffix_source_name(name, source_suffix));
                if self.exists_or_ought_to(&prereq_path) {
                    return Some(rule);
                }
            }
        }
        None
    }

    /// Builds the target with the given name.
    ///
    /// The ledger makes this idempotent: a target reachable by several paths is
    /// built once, its outcome replayed to every later visit, and under `-j` a
    /// second thread arriving mid-build waits rather than running the recipe a
    /// second time (audit #29, #31).
    ///
    /// # Returns
    /// - Ok(true) if the target was built.
    /// - Ok(false) if the target was already up to date.
    /// - Err(_) if any errors occur.
    pub fn build_target(&self, name: impl AsRef<str>) -> Result<bool, ErrorCode> {
        self.build_target_in(name.as_ref(), &[])
    }

    /// Build `name` as a prerequisite of a target whose target-specific
    /// assignments are `inherited` (GNU make): they are in force for this
    /// target's commands too, under its own.
    ///
    /// A target reached by several paths is built once, in the context of
    /// the first, as in GNU make.
    fn build_target_in(&self, name: &str, inherited: &[TargetVar]) -> Result<bool, ErrorCode> {
        let name = &rule::prerequisite::without_dot_slash(name.to_string());
        match self.ledger.claim(name) {
            graph::Claim::Done(outcome) => return outcome.into_result(),
            graph::Claim::Build => {}
        }
        let result = self.build_target_uncached(name, inherited);
        self.ledger.finish(name, &result);
        result
    }

    fn build_target_uncached(
        &self,
        name: impl AsRef<str>,
        inherited: &[TargetVar],
    ) -> Result<bool, ErrorCode> {
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
                    // A file `VPATH` finds elsewhere is known by the name it
                    // was found under, and that is the name an implicit rule
                    // must match, as in GNU make. binutils' bfd has
                    // `doc/%.texi: doc/%.stamp` for the texinfo files it
                    // generates, and `doc/bfd.texi`, a source in
                    // `$(srcdir)/doc`, is not one of them: matched by the
                    // name it is written under, the rule "remade" it.
                    let found = self.resolve_vpath(name.as_ref());
                    if found != name.as_ref()
                        && self.find_pattern_rule(&found).is_none()
                        && self.find_inference_rule(&found).is_none()
                    {
                        return Ok(false);
                    }
                    // A `%.o: %.c` pattern rule, instantiated for this target.
                    // Pattern rules are tried before suffix inference: a
                    // makefile that writes `%.o: %.c` means it, and must not
                    // lose to the built-in `.c.o`. GNU orders them the same way.
                    //
                    // Both this and the inference case below go through
                    // `run_rule_with_prerequisites` rather than calling the
                    // runner directly. Calling it directly skipped the staleness
                    // check, so every build recompiled (audit #63, #64); skipped
                    // the cycle check (#65); and reported every prerequisite in
                    // `$?` rather than the newer ones.
                    if let Some(rule) = self.find_pattern_rule(name.as_ref()) {
                        let target = Target::new(name.as_ref());
                        return self.run_rule_with_prerequisites(
                            &rule,
                            &target,
                            Dispatch::Explicit,
                            inherited,
                        );
                    }
                    // No target rule named `name`: try to infer one (single- or
                    // double-suffix) from an existing prerequisite file.
                    if let Some(inference_rule) = self.find_inference_rule(name.as_ref()) {
                        let target = Target::new(name.as_ref());
                        return self.run_rule_with_prerequisites(
                            inference_rule,
                            &target,
                            Dispatch::Inference,
                            inherited,
                        );
                    }
                    // XSI: a source file with an SCCS history is retrieved
                    // rather than declared missing, and a stale one is
                    // refreshed before it is called up to date (audit #61).
                    if self.retrieve_from_sccs(name.as_ref())? {
                        return Ok(true);
                    }
                    // POSIX 105672: a prerequisite of `.PHONY` is a target
                    // itself, so one with no rule of its own is a target
                    // with no commands, and `.DEFAULT` is not for it. It is
                    // always out of date, so it is made, by doing nothing,
                    // and its dependents are remade after it, as in GNU
                    // make. binutils' top-level Makefile declares
                    // `.PHONY: clean-stage1` with no rule, under `do-clean:
                    // clean-stage1`.
                    if self.attributes.is_phony(name.as_ref()) {
                        return Ok(true);
                    }
                    // Per POSIX: "If a target exists and there is neither a target rule
                    // nor an inference rule for the target, the target shall be considered
                    // up-to-date."  Resolved through VPATH: a file supplied by a
                    // search directory exists (audit #66).
                    if self
                        .stats
                        .modified(&self.resolve_vpath(name.as_ref()))
                        .is_some()
                    {
                        return Ok(false);
                    }
                    // No rule and file doesn't exist - try .DEFAULT or fail
                    match &self.default_rule {
                        Some(rule) => rule,
                        None => {
                            return Err(NoRule {
                                target: name.as_ref().to_string(),
                                needed_by: None,
                            })
                        }
                    }
                }
            },
        };
        let target = Target::new(name.as_ref());

        if rule.is_double_colon() {
            return self.run_double_colon_rules(&target, inherited);
        }
        if let Some(pattern) = self.pattern_rule_for_commandless(rule, name.as_ref()) {
            let merged = pattern.with_prerequisites_of(rule);
            return self.run_rule_with_prerequisites(
                &merged,
                &target,
                Dispatch::Explicit,
                inherited,
            );
        }
        self.run_rule_with_prerequisites(rule, &target, Dispatch::Explicit, inherited)
    }

    /// The pattern rule that supplies the commands of `rule`, a target rule
    /// for `name` that has none (GNU make). libzstd's default target is
    /// `lib-release:`, made by `%-release : %`. A phony target is not
    /// searched for, as in GNU make.
    fn pattern_rule_for_commandless(&self, rule: &Rule, name: &str) -> Option<Rule> {
        let named = self.rule_by_target_name(name).is_some();
        if !named || rule.recipes().count() > 0 || self.attributes.is_phony(name) {
            return None;
        }
        self.find_pattern_rule(name)
    }

    /// The prerequisites that decide whether `target` is out of date.
    ///
    /// A named rule lists them, and an instantiated pattern rule has already had
    /// the stem substituted in. An inference rule lists none: its input is
    /// derived from its suffixes and the concrete target name, which is why
    /// nothing could compare timestamps for it before (audit #63).
    fn effective_prerequisites(
        &self,
        rule: &Rule,
        target: &Target,
        dispatch: Dispatch,
    ) -> Vec<String> {
        let listed: Vec<String> = rule
            .prerequisites()
            .map(|p| p.as_ref().to_string())
            .collect();
        if dispatch == Dispatch::Explicit {
            return listed;
        }
        match inference_source(rule, target) {
            Some(source) => {
                let mut all = vec![source];
                all.extend(listed);
                all
            }
            None => listed,
        }
    }

    /// Which of `prerequisites` are newer than `target`, or do not exist yet.
    ///
    /// Names are resolved through `VPATH` first: a prerequisite supplied by a
    /// search directory exists, and treating it as missing is what made
    /// `VPATH` useless for a prerequisite with no rule of its own (audit #66).
    fn newer_prerequisites(&self, target: &str, prerequisites: &[String]) -> Vec<String> {
        let Some(target_modified) = self.stats.modified(&self.resolve_vpath(target)) else {
            // The target does not exist, so everything it needs counts.
            return prerequisites.to_vec();
        };
        prerequisites
            .iter()
            .filter(|name| *name != WAIT_TARGET)
            .filter(|name| match self.prerequisite_mtime(name) {
                Some(modified) => modified > target_modified,
                None => true,
            })
            .cloned()
            .collect()
    }

    /// The modification time a prerequisite is judged by.
    ///
    /// For a target without commands it is the time it had before its own
    /// prerequisites were made: no command of its own ran, so as far as make
    /// knows it did not change, as in GNU make, which does not look again.
    /// automake relies on it -- `$(srcdir)/version.texi:
    /// $(srcdir)/stamp-vti` has no commands, the stamp's commands rewrite
    /// version.texi, and the manual made from it is not remade, so a build
    /// from a release needs no makeinfo.
    fn prerequisite_mtime(&self, name: &str) -> Option<SystemTime> {
        if let Some(frozen) = self.frozen_mtimes.lock().unwrap().get(name) {
            return *frozen;
        }
        self.stats.modified(&self.resolve_vpath(name))
    }

    /// Runs a rule after bringing its prerequisites up to date.
    ///
    /// # Returns
    /// - Ok(true) if the rule was run.
    /// - Ok(false) if the rule was already up to date.
    /// - Err(_) if any errors occur.
    fn run_rule_with_prerequisites(
        &self,
        rule: &Rule,
        target: &Target,
        dispatch: Dispatch,
        inherited: &[TargetVar],
    ) -> Result<bool, ErrorCode> {
        let cycle =
            graph::find_cycle_beyond(self, target.as_ref(), &mut self.acyclic.lock().unwrap());
        if let Some(origin) = cycle {
            return Err(RecursivePrerequisite { origin });
        }
        let scope = self.scope_for(target.as_ref(), inherited);
        let mtime_before = self.stats.modified(&self.resolve_vpath(target.as_ref()));

        let mut prerequisites = self.effective_prerequisites(rule, target, dispatch);

        // POSIX 105929: "When no target rule with commands is found to update a
        // target, the inference rules shall be checked." The rule that will
        // actually run is the one that decides staleness, so its source joins
        // the prerequisite list here, before the comparison. Looking it up only
        // *after* the up-to-date test meant the standard `foo.o: foo.h` plus
        // `.c.o:` idiom never saw `foo.c` at all, and a changed source never
        // recompiled (audit #83).
        let inferred = match rule.recipes().count() {
            0 if !rule.is_double_colon() => self.find_inference_rule(target.as_ref()),
            _ => None,
        };
        if let Some(source) = inferred.and_then(|rule| inference_source(rule, target)) {
            if !prerequisites.contains(&source) {
                prerequisites.insert(0, source);
            }
        }

        // Bring every prerequisite up to date before judging staleness. One that
        // was actually rebuilt makes this target stale whatever the timestamps
        // say, and one that is merely older must still be visited -- it may be
        // phony, or stale against its own sources.
        let rebuilt = self
            .build_prerequisites(&prerequisites, &scope)
            .map_err(|err| needed_by(err, target.as_ref()))?;

        // Order-only prerequisites (GNU make) are brought up to date too, but
        // whether they were remade, or are newer, says nothing about this
        // target. A name that is also a normal prerequisite is a normal one.
        let order_only: Vec<String> = rule
            .order_only()
            .map(|p| p.as_ref().to_string())
            .filter(|name| !prerequisites.contains(name))
            .collect();
        self.build_prerequisites(&order_only, &scope)
            .map_err(|err| needed_by(err, target.as_ref()))?;

        // `$?` is the prerequisites newer than the target.
        let newer = self.newer_prerequisites(target.as_ref(), &prerequisites);
        let exists = self
            .stats
            .modified(&self.resolve_vpath(target.as_ref()))
            .is_some();
        let phony = self.attributes.is_phony(target.as_ref());
        // A double-colon rule with no prerequisites always runs (GNU make).
        let always = rule.is_double_colon() && prerequisites.is_empty();
        let up_to_date = !rebuilt && newer.is_empty() && exists && !phony && !always;

        if up_to_date {
            return Ok(false);
        }

        if let Some(inference_rule) = inferred {
            let inference_rule = inference_rule.with_prerequisites(&prerequisites);
            self.run_inference(&inference_rule, target, up_to_date, &newer, &scope)?;
            self.mark_remade(target);
            return Ok(self.changed_since(target, mtime_before));
        }

        // An inference rule derives `$<` from its suffixes rather than from a
        // prerequisite list, so it must go through `run_for_target`; handing it
        // to `run` would leave `$<` empty.
        if dispatch == Dispatch::Inference {
            let rule = rule.with_prerequisites(&prerequisites);
            self.run_inference(&rule, target, up_to_date, &newer, &scope)?;
            self.mark_remade(target);
            return Ok(self.changed_since(target, mtime_before));
        }

        // A rule without commands remakes nothing itself, so it counts as
        // remade for its dependents only if its file does not exist, and they
        // judge it by the time it had before (GNU make; see
        // `prerequisite_mtime`).
        if rule.recipes().count() == 0 {
            self.frozen_mtimes
                .lock()
                .unwrap()
                .insert(target.as_ref().to_string(), mtime_before);
            return Ok(mtime_before.is_none());
        }

        self.with_env(&scope, |env| rule.run(env, target, up_to_date, &newer))?;
        self.mark_remade(target);
        Ok(self.changed_since(target, mtime_before))
    }

    /// Record that `target`'s commands ran; see `resolve_vpath`.
    fn mark_remade(&self, target: &Target) {
        self.remade
            .lock()
            .unwrap()
            .insert(target.as_ref().to_string());
    }

    /// Whether `target`, whose commands just ran, counts as remade for the
    /// targets that depend on it: it does not exist (a phony target), its
    /// file changed, or under -n, where nothing is run and everything that
    /// would have been is taken as remade (GNU make).
    ///
    /// A target whose commands left it alone is not: automake's
    /// `config.h: stamp-h1` runs `@test -f $@ || ...` whenever the stamp is
    /// newer, and every object that includes config.h was compiled again on
    /// every run.
    fn changed_since(&self, target: &Target, before: Option<SystemTime>) -> bool {
        if self.config.dry_run {
            return true;
        }
        let after = self.stats.modified(&self.resolve_vpath(target.as_ref()));
        after.is_none() || after != before
    }

    /// Register the built-in inference rules, skipping any the makefile already
    /// defines. Their commands are expanded when they run, like any others.
    fn seed_builtin_rules(&mut self) -> Result<(), ErrorCode> {
        let text = BUILTIN_RULES;
        let parsed = parser::parse(text).map_err(|constraint| ParserError { constraint })?;
        for parsed_rule in parsed.rules() {
            let rule = Rule::from(parsed_rule.clone());
            let Some(name) = rule.targets().next().map(Target::to_string) else {
                continue;
            };
            let already = self
                .inference_rules
                .iter()
                .any(|r| r.targets().any(|t| t.as_ref() == name));
            if !already {
                self.inference_rules.push(rule);
            }
        }
        Ok(())
    }

    /// Retrieve `name` from SCCS if it has a history file and needs one.
    ///
    /// POSIX 105699: an `SCCS/s.name` beside a target makes make compare the
    /// two timestamps and run the `.SCCS_GET` commands when the target is
    /// missing or older. Returns whether anything was retrieved.
    fn retrieve_from_sccs(&self, name: &str) -> Result<bool, ErrorCode> {
        let Some(rule) = self.sccs_get.as_ref() else {
            return Ok(false);
        };
        let Some(history) = sccs_history(name, &self.stats) else {
            return Ok(false);
        };
        if !needs_sccs_get(name, &history) {
            return Ok(false);
        }
        let target = Target::new(name);
        self.with_env(&[], |env| rule.run(env, &target, false, &[]))?;
        Ok(true)
    }

    /// Register the built-in `.SCCS_GET` recipe, unless the makefile gave one.
    fn seed_sccs_get(&mut self) -> Result<(), ErrorCode> {
        if self.sccs_get.is_some() {
            return Ok(());
        }
        let text = SCCS_GET_RULE;
        let parsed = parser::parse(text).map_err(|constraint| ParserError { constraint })?;
        self.sccs_get = parsed.rules().next().map(|r| Rule::from(r.clone()));
        Ok(())
    }

    /// Assemble the invariants a recipe runs against, and hand them to `f`.
    ///
    /// The `VPATH` resolver borrows `self`, so it cannot outlive the call; a
    /// callback keeps that borrow local instead of asking every caller to
    /// spell the closure out.
    ///
    /// The macros are the makefile's with `scope` -- the target-specific
    /// assignments in force for the target -- applied in order. Without any,
    /// the makefile's own table is used as it is.
    fn with_env<R>(&self, scope: &[TargetVar], f: impl FnOnce(&rule::Env<'_>) -> R) -> R {
        let resolve = |name: &str| self.resolve_vpath(name);
        let table = match scope.is_empty() {
            true => std::borrow::Cow::Borrowed(&self.table),
            false => {
                let mut table = self.table.clone();
                for var in scope {
                    table.apply(var, &self.table);
                }
                std::borrow::Cow::Owned(table)
            }
        };
        let mut exports = self.exports.clone();
        exports.extend(scope.iter().filter(|v| v.exported).map(|v| v.name.clone()));
        let result = f(&rule::Env {
            config: &self.config,
            table: &table,
            exports: &exports,
            resolve: &resolve,
            attributes: &self.attributes,
        });
        // Commands may have run, failed or not, and changed any file.
        self.stats.invalidate();
        result
    }

    /// The target-specific assignments in force for `target`: those it
    /// inherits from the target it is made for, then those of each `%`
    /// pattern it matches, then its own -- each overriding the ones before,
    /// as in GNU make.
    fn scope_for(&self, target: &str, inherited: &[TargetVar]) -> Vec<TargetVar> {
        let mut scope = inherited.to_vec();
        for (pattern, var) in &self.pattern_vars {
            if rule::pattern_stem(pattern, target).is_some() {
                scope.push(var.clone());
            }
        }
        if let Some(own) = self.target_vars.get(target) {
            scope.extend(own.iter().cloned());
        }
        scope
    }

    fn run_inference(
        &self,
        rule: &Rule,
        target: &Target,
        up_to_date: bool,
        newer: &[String],
        scope: &[TargetVar],
    ) -> Result<(), ErrorCode> {
        self.with_env(scope, |env| {
            rule.run_for_target(env, target, up_to_date, newer)
        })
    }

    /// Builds a target's prerequisites, honoring `.WAIT` barriers and, under
    /// `-j`, building independent prerequisites concurrently.
    ///
    /// `.WAIT` prerequisites split the list into segments that must be built in
    /// order: every prerequisite to the left of a `.WAIT` is brought up to date
    /// before any to its right. Within a segment, prerequisites are independent.
    fn build_prerequisites(
        &self,
        prerequisites: &[String],
        scope: &[TargetVar],
    ) -> Result<bool, ErrorCode> {
        let parallel = self.config.jobs > 1
            && !self.config.not_parallel
            && !self.config.dry_run
            && !self.config.quit
            && !self.config.touch;

        let mut updated = false;
        let mut segment: Vec<&str> = Vec::new();
        for prerequisite in prerequisites {
            let name = prerequisite.as_str();
            if name == WAIT_TARGET {
                // Barrier: finish the current segment before continuing.
                updated |= self.build_segment(&segment, parallel, scope)?;
                segment.clear();
            } else {
                segment.push(name);
            }
        }
        updated |= self.build_segment(&segment, parallel, scope)?;
        Ok(updated)
    }

    /// Builds one segment of independent prerequisites, in parallel when `-j`
    /// allows it. Parallelism is bounded by the token pool; a prerequisite that
    /// cannot obtain a token is built inline so the build always progresses.
    fn build_segment(
        &self,
        names: &[&str],
        parallel: bool,
        scope: &[TargetVar],
    ) -> Result<bool, ErrorCode> {
        if !parallel || names.len() <= 1 {
            // Under -k a failed prerequisite must not stop its *siblings*; the
            // first error is still returned, so the dependent target is skipped.
            let mut first_error = None;
            let mut updated = false;
            for name in names {
                match self.build_target_in(name, scope) {
                    Ok(built) => updated |= built,
                    Err(err) => {
                        if !self.config.keep_going || self.config.terminate {
                            return Err(err);
                        }
                        first_error.get_or_insert(err);
                    }
                }
            }
            return match first_error {
                Some(err) => Err(err),
                None => Ok(updated),
            };
        }

        let updated = std::sync::atomic::AtomicBool::new(false);
        let errors: std::sync::Mutex<Vec<ErrorCode>> = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|threads| {
            let mut handles = Vec::new();
            let mut inline: Vec<&str> = Vec::new();
            // Spawn a worker for each prerequisite that can obtain a token; the
            // rest are built inline in this thread. Spawning first (before any
            // inline build) is what lets the work actually overlap.
            for &name in names {
                if self.pool.try_acquire() {
                    let errors = &errors;
                    let updated = &updated;
                    handles.push(threads.spawn(move || {
                        let result = self.build_target_in(name, scope);
                        self.pool.release();
                        match result {
                            Ok(true) => updated.store(true, std::sync::atomic::Ordering::Relaxed),
                            Ok(false) => {}
                            Err(err) => errors.lock().unwrap().push(err),
                        }
                    }));
                } else {
                    inline.push(name);
                }
            }
            for name in inline {
                match self.build_target_in(name, scope) {
                    Ok(true) => updated.store(true, std::sync::atomic::Ordering::Relaxed),
                    Ok(false) => {}
                    Err(err) => errors.lock().unwrap().push(err),
                }
            }
            for handle in handles {
                let _ = handle.join();
            }
        });

        // Report the first error (if any).
        match errors.into_inner().unwrap().into_iter().next() {
            Some(err) => Err(err),
            None => Ok(updated.into_inner()),
        }
    }

    /// File a single-target rule.
    ///
    /// A second ordinary rule for a target is folded into the first
    /// (POSIX 105653). A double-colon rule never is: each is independent,
    /// with its own prerequisites and commands, and they run in the order
    /// written. GNU make refuses a target that has both kinds, as here.
    fn add_target_rule(&mut self, rule: Rule) -> Result<(), ErrorCode> {
        let Some(name) = rule.targets().next().map(|t| t.to_string()) else {
            return Ok(());
        };
        // Pattern rules with one target pattern are alternatives, not one
        // rule: libzstd has `$(D)/%.o : %.c` and `$(D)/%.o : %.S`, and the
        // one used is the one whose prerequisites exist. One with the same
        // prerequisites as an earlier one replaces it, as in GNU make.
        if rule.is_pattern() {
            let same = |r: &Rule| {
                r.targets().eq(rule.targets()) && r.prerequisites().eq(rule.prerequisites())
            };
            let found = self
                .pattern_rules
                .iter()
                .copied()
                .find(|&at| same(&self.rules[at]));
            match found {
                Some(at) => self.rules[at] = rule,
                None => self.push_rule(rule),
            }
            return Ok(());
        }
        let existing = self
            .rule_index
            .get(&name)
            .and_then(|at| at.first().copied())
            .map(|at| &mut self.rules[at]);
        match existing {
            Some(existing) if existing.is_double_colon() != rule.is_double_colon() => {
                Err(MixedColons { target: name })
            }
            Some(existing) if !rule.is_double_colon() => {
                existing.absorb(rule);
                Ok(())
            }
            _ => {
                self.push_rule(rule);
                Ok(())
            }
        }
    }

    /// Every `::` rule for `name`, in the order written.
    fn double_colon_rules(&self, name: &str) -> Vec<&Rule> {
        self.rules_named(name)
            .filter(|rule| rule.is_double_colon())
            .collect()
    }

    /// Run each double-colon rule for `target` in turn.
    ///
    /// Each brings its own prerequisites up to date and runs its commands
    /// when one of them is newer than the target, or always when it has none
    /// (GNU make). Reports whether any rule ran.
    fn run_double_colon_rules(
        &self,
        target: &Target,
        inherited: &[TargetVar],
    ) -> Result<bool, ErrorCode> {
        let mut ran = false;
        for rule in self.double_colon_rules(target.as_ref()) {
            ran |= self.run_rule_with_prerequisites(rule, target, Dispatch::Explicit, inherited)?;
        }
        Ok(ran)
    }

    /// Whether some rule can make `name`: a target rule, a pattern rule or an
    /// inference rule.
    fn has_rule_for(&self, name: &str) -> bool {
        self.rule_by_target_name(name).is_some()
            || self.find_pattern_rule(name).is_some()
            || self.find_inference_rule(name).is_some()
    }

    /// Bring every included file up to date, after the makefiles are read.
    ///
    /// POSIX 105587-105600 ("delayed remaking"): once the makefiles have been
    /// read, make uses any rule for an included file to create it or bring it
    /// up to date, and the new contents are then used. Returns whether any
    /// included file changed, in which case the caller reads the makefiles
    /// again from the start.
    ///
    /// The recipes run even under `-n`, `-q` and `-t`: what is being made is
    /// the makefile itself, which those options cannot answer for without
    /// reading. GNU make does the same.
    ///
    /// A file is reported as changed only if its modification time did, so a
    /// rule that runs and makes nothing -- `$(DEPFILES):` with no commands --
    /// cannot restart the read forever.
    pub fn remake_includes(&mut self, includes: &[parser::Include]) -> Result<bool, ErrorCode> {
        let saved = (self.config.dry_run, self.config.quit, self.config.touch);
        (self.config.dry_run, self.config.quit, self.config.touch) = (false, false, false);
        let result = self.remake_each_include(includes);
        (self.config.dry_run, self.config.quit, self.config.touch) = saved;
        result
    }

    fn remake_each_include(&self, includes: &[parser::Include]) -> Result<bool, ErrorCode> {
        let mut changed = false;
        for include in includes {
            let path = rule::prerequisite::without_dot_slash(include.path.clone());
            let path = path.as_str();
            let before = self.stats.modified(path);
            if !self.has_rule_for(path) {
                if before.is_none() && !include.optional {
                    return Err(missing_include(path));
                }
                continue;
            }
            match self.build_target(path) {
                Ok(_) => {}
                // GNU: a `-include` that cannot be made is passed over.
                Err(_) if include.optional => continue,
                Err(err) => return Err(err),
            }
            let after = self.stats.modified(path);
            if after.is_none() && !include.optional {
                return Err(missing_include(path));
            }
            changed |= after != before;
        }
        Ok(changed)
    }
}

/// Name `parent` as the target a missing prerequisite was needed by, unless
/// a target nearer to it is named already: GNU make's `No rule to make target
/// 'X', needed by 'Y'` names the immediate dependent.
fn needed_by(err: ErrorCode, parent: &str) -> ErrorCode {
    match err {
        NoRule {
            target,
            needed_by: None,
        } => NoRule {
            target,
            needed_by: Some(parent.to_string()),
        },
        other => other,
    }
}

/// The error for an `include` whose file neither exists nor can be made.
fn missing_include(path: &str) -> ErrorCode {
    let reason = std::io::Error::from_raw_os_error(libc::ENOENT);
    let message = preprocessor::PreprocError::IncludeFailed {
        path: path.to_string(),
        reason: reason.to_string(),
    };
    ParserError {
        constraint: parser::ParseError(vec![message.to_string()]),
    }
}

/// How the rule about to run was selected.
///
/// This cannot be read off the rule: `.config:` parses as `Target::Inference`
/// syntactically while being an ordinary target rule (audit #38). Only the
/// classification done at construction knows, so the caller says.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Dispatch {
    /// A named rule, an instantiated pattern rule, or `.DEFAULT`.
    Explicit,
    /// A suffix inference rule, whose input comes from its suffixes.
    Inference,
}

/// The file a source suffix names, for a given stem.
///
/// Ordinarily that is just the two glued together: `foo` + `.c` is `foo.c`.
///
/// XSI (POSIX 105941): a `~` suffix "refers to an SCCS file in the current
/// directory", so `.c~` applied to `foo` names `s.foo.c` -- the history file --
/// and not a file literally called `foo.c~`. The `s.` is a *prefix* where make
/// otherwise thinks only in suffixes, which is the whole reason the `~`
/// spelling exists.
///
/// Distinct from `.SCCS_GET` retrieval, which POSIX 105699 specifies against
/// `SCCS/s.source_file`; see [`sccs_history`].
pub(crate) fn suffix_source_name(stem: &str, source_suffix: &str) -> String {
    let Some(base) = source_suffix.strip_suffix('~') else {
        return format!("{stem}{source_suffix}");
    };
    // The `s.` attaches to the basename, so a stem with a directory keeps it.
    let (dir, file) = match stem.rfind('/') {
        Some(at) => (&stem[..=at], &stem[at + 1..]),
        None => ("", stem),
    };
    format!("{dir}s.{file}{base}")
}

/// The file an inference rule reads to make `target`.
///
/// `.c.o` applied to `foo.o` reads `foo.c`; a single-suffix `.c` applied to
/// `foo` reads `foo.c`; an XSI `.c~.o` applied to `foo.o` reads `s.foo.c`.
fn inference_source(rule: &Rule, target: &Target) -> Option<String> {
    let Some(Target::Inference { from, to, .. }) = rule.targets().next() else {
        return None;
    };
    let name = target.as_ref();
    if to.is_empty() {
        return Some(suffix_source_name(name, &format!(".{from}")));
    }
    let stem = name.strip_suffix(&format!(".{to}"))?;
    Some(suffix_source_name(stem, &format!(".{from}")))
}

/// Return `name` found under one of `dirs`, if any.
impl Make {
    fn search_dirs(&self, dirs: &[&str], name: &str) -> Option<String> {
        dirs.iter()
            .map(|dir| format!("{dir}/{name}"))
            .find(|candidate| self.stats.exists(candidate))
    }
}

/// The default macros and inference rules POSIX requires make to start with.
///
/// These existed only as display strings in the `-p` dump table, so `make f.o`
/// with an `f.c` present reported "no target" -- every makefile that leans on
/// the built-in `.c.o` rule, which is most of them, could not build.
///
/// Written as makefile text so it goes through the same reader as anything
/// else. `-r` (`config.clear`) suppresses it, as POSIX requires.
///
/// POSIX 106035 lets an implementation "provide additional macros and
/// rules", and two of GNU make's are here because sysvinit's Makefile
/// cannot build without them: `.o:` links a program from its objects
/// (`init: init.o init_utmp.o runlevellog.o` has no commands), and `.c.o`
/// passes `$(CPPFLAGS)`, which sysvinit sets per object
/// (`init.o: CPPFLAGS += $(SELINUX_DEF)`). With `CPPFLAGS` empty, `.c.o`
/// gives what POSIX's gives.
///
/// `.c:` links, as GNU make's `%: %.c` does, every prerequisite (`$^`)
/// with `$(CPPFLAGS)` and `$(LDLIBS)`: libselinux links each of its utils
/// from its source with `override LDLIBS += -lselinux`, and libzstd's
/// `fuzzer:` has no commands, only its objects and sources. With one
/// prerequisite and both macros empty it runs what POSIX's runs.
const BUILTIN_RULES: &str = r#"
.o:
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@
.c:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@
.sh:
	cp $< $@
	chmod a+x $@
.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
.y.o:
	$(YACC) $(YFLAGS) $<
	$(CC) $(CFLAGS) -c y.tab.c
	rm -f y.tab.c
	mv y.tab.o $@
.l.o:
	$(LEX) $(LFLAGS) $<
	$(CC) $(CFLAGS) -c lex.yy.c
	rm -f lex.yy.c
	mv lex.yy.o $@
.y.c:
	$(YACC) $(YFLAGS) $<
	mv y.tab.c $@
.l.c:
	$(LEX) $(LFLAGS) $<
	mv lex.yy.c $@
.c.a:
	$(CC) -c $(CFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o
.c~:
	$(GET) $(GFLAGS) -p $< > $*.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $*.c
.sh~:
	$(GET) $(GFLAGS) -p $< > $*.sh
	cp $*.sh $@
	chmod a+x $@
.c~.o:
	$(GET) $(GFLAGS) -p $< > $*.c
	$(CC) $(CFLAGS) -c $*.c
.y~.o:
	$(GET) $(GFLAGS) -p $< > $*.y
	$(YACC) $(YFLAGS) $*.y
	$(CC) $(CFLAGS) -c y.tab.c
	rm -f y.tab.c
	mv y.tab.o $@
.l~.o:
	$(GET) $(GFLAGS) -p $< > $*.l
	$(LEX) $(LFLAGS) $*.l
	$(CC) $(CFLAGS) -c lex.yy.c
	rm -f lex.yy.c
	mv lex.yy.o $@
.y~.c:
	$(GET) $(GFLAGS) -p $< > $*.y
	$(YACC) $(YFLAGS) $*.y
	mv y.tab.c $@
.l~.c:
	$(GET) $(GFLAGS) -p $< > $*.l
	$(LEX) $(LFLAGS) $*.l
	mv lex.yy.c $@
"#;

/// The default `.SCCS_GET` recipe (POSIX 106038). `sccs` is taken from `PATH`,
/// which is what the spec's own default rule names; this repo's `sccs/` build
/// serves if it is installed there.
const SCCS_GET_RULE: &str = ".SCCS_GET:\n\tsccs $(SCCSFLAGS) get $(SCCSGETFLAGS) $@\n";

/// The macros those rules refer to. A makefile definition overrides them, so
/// they are seeded first and only kept where the makefile is silent.
///
/// The last five are GNU make's, which libzstd's Makefiles compile, link
/// and clean with (`$(COMPILE.c) $(DEPFLAGS) $(OUTPUT_OPTION) $<`,
/// `$(LINK.c) $^ -o $@`, `$(RM)`); POSIX 106035 allows additional macros.
const BUILTIN_MACROS: [(&str, &str); 17] = [
    ("CC", "c17"),
    ("CFLAGS", "-O 1"),
    ("AR", "ar"),
    ("ARFLAGS", "-rv"),
    ("YACC", "yacc"),
    ("YFLAGS", ""),
    ("LEX", "lex"),
    ("LFLAGS", ""),
    ("GET", "get"),
    ("GFLAGS", ""),
    ("SCCSFLAGS", ""),
    ("SCCSGETFLAGS", "-s"),
    ("COMPILE.c", "$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c"),
    (
        "COMPILE.S",
        "$(CC) $(ASFLAGS) $(CPPFLAGS) $(TARGET_MACH) -c",
    ),
    (
        "LINK.c",
        "$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    ),
    ("OUTPUT_OPTION", "-o $@"),
    ("RM", "rm -f"),
];

/// The macros POSIX's built-in inference rules refer to (source 4).
///
/// Handed to the parser before the makefile is read, so a makefile can expand
/// them; `Make` then receives them like any other definition.
pub fn builtin_macros() -> Vec<Macro> {
    BUILTIN_MACROS
        .iter()
        .map(|(name, value)| (name.to_string(), value.to_string()))
        .collect()
}

impl TryFrom<(Makefile, Config)> for Make {
    type Error = ErrorCode;

    fn try_from((makefile, config): (Makefile, Config)) -> Result<Self, Self::Error> {
        // Two-pass classification: .SUFFIXES must be processed before inference
        // rule classification so that user-defined suffixes (especially with -r)
        // are available when determining whether a rule like `.txt.out:` is an
        // inference rule.

        let table = makefile.table().clone();
        let (target_vars, pattern_vars) = index_target_vars(makefile.target_vars());
        let (parsed_rules, macros, vpaths, exports) = makefile.into_parts();

        let mut suffixes_rules = vec![];
        let mut remaining_parsed_rules = vec![];

        // Pass 1: Separate .SUFFIXES rules from everything else and process
        // them immediately so config.rules[".SUFFIXES"] is populated.
        for parsed_rule in parsed_rules {
            let rule = Rule::from(parsed_rule);
            let Some(target) = rule.targets().next() else {
                return Err(NoTargets);
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
            rule_index: HashMap::new(),
            pattern_rules: Vec::new(),
            inference_rules: vec![],
            macros,
            table,
            target_vars,
            pattern_vars,
            mentioned: Default::default(),
            frozen_mtimes: Default::default(),
            remade: Default::default(),
            stats: Default::default(),
            acyclic: Default::default(),
            default_rule: None,
            sccs_get: None,
            pool,
            ledger: graph::Ledger::new(),
            vpaths,
            exports,
            attributes: attributes::Attributes::default(),
            config,
        };

        // A fallback for callers that build a `Make` without going through the
        // parser: the `make` binary seeds these before the makefile is read
        // (see `builtin_macros`), so by here they are already present and this
        // adds nothing. It stays because `Make` is a library type and must not
        // depend on its caller having done that.
        if !make.config.clear {
            for (name, value) in BUILTIN_MACROS {
                if !make.macros.iter().any(|(n, _)| n == name) {
                    make.macros.push((name.to_string(), value.to_string()));
                    make.table
                        .seed(name.to_string(), value.to_string(), MacroSource::BuiltIn);
                }
            }
        }

        for rule in suffixes_rules {
            special_target::process(rule, &mut make)?;
        }

        // Pass 2: Classify remaining rules.  Now make.config.rules[".SUFFIXES"]
        // contains both built-in (unless -r) and user-defined suffixes.
        let mut special_rules = vec![];

        for rule in remaining_parsed_rules {
            let Some(target) = rule.targets().next() else {
                return Err(NoTargets);
            };

            if SpecialTarget::try_from(target.clone()).is_ok() {
                special_rules.push(rule);
            } else if let Ok(inference) =
                InferenceTarget::try_from((target.clone(), make.config.clone()))
            {
                // The suffixes as the known ones split the name, which only
                // the suffix list can say for `.c.depends.o`.
                make.inference_rules
                    .push(rule.with_suffixes(inference.from(), inference.to().unwrap_or("")));
            } else {
                // POSIX 105653: several rules may name the same target, with
                // prerequisites accumulating. The old lookup returned the first
                // match and silently dropped the rest (audit #30).
                //
                // Each target of a multi-target rule is folded in separately.
                // POSIX 105643 makes the target side a `<blank>`-separated
                // list of targets, each a target in its own right, so `a b: c`
                // is `a: c` and `b: c`. Absorbing a later `a: extra` into the
                // rule the two shared handed
                // `extra` to `b` as well -- the same leak `.IGNORE: a` had
                // before attributes were keyed by target (audit #84).
                for rule in rule.split_targets() {
                    make.add_target_rule(rule)?;
                }
            }
        }

        for rule in special_rules {
            special_target::process(rule, &mut make)?;
        }

        make.mentioned = make
            .rules
            .iter()
            .filter(|rule| !rule.is_pattern())
            .flat_map(|rule| {
                let targets = rule.targets().map(|t| t.as_ref().to_string());
                let prerequisites = rule
                    .prerequisites()
                    .chain(rule.order_only())
                    .map(|p| p.as_ref().to_string());
                targets.chain(prerequisites).collect::<Vec<_>>()
            })
            .collect();

        // Built-in inference rules come last, so a rule of the same name in
        // the makefile is found first.
        if !make.config.clear {
            make.seed_builtin_rules()?;
            make.seed_sccs_get()?;
        }

        Ok(make)
    }
}

/// Target-specific macro assignments indexed by target, and the
/// pattern-specific ones (a `%` target) with their patterns, each in the
/// order read.
type TargetVarIndex = (HashMap<String, Vec<TargetVar>>, Vec<(String, TargetVar)>);

fn index_target_vars(vars: &[TargetVar]) -> TargetVarIndex {
    let mut by_target: HashMap<String, Vec<TargetVar>> = HashMap::new();
    let mut by_pattern = Vec::new();
    for var in vars {
        for target in &var.targets {
            match target.contains('%') {
                true => by_pattern.push((target.clone(), var.clone())),
                false => by_target
                    .entry(target.clone())
                    .or_default()
                    .push(var.clone()),
            }
        }
    }
    (by_target, by_pattern)
}

/// The SCCS history file for `name`, if there is one.
///
/// POSIX names it `SCCS/s.source_file`, beside the target rather than beside
/// the makefile, so a target with a directory part looks in that directory.
///
/// When that finds nothing, the XSI `PROJECTDIR` (105471-82) names "a
/// directory to be used to search for SCCS files **not found in the current
/// directory**" -- a fallback, not a replacement, which is why it is tried
/// second and why a local `SCCS/s.file` still wins. The search there is made
/// "in the directory SCCS in the identified directory", singular, so the
/// target's own directory part does not reappear under it.
///
/// This does not touch the `~`-suffix rules' `s.foo.c` form in
/// `suffix_source_name`: 105481-82 scopes `PROJECTDIR` to "files with a
/// component named SCCS", and that form has none.
fn sccs_history(name: &str, stats: &stat_cache::StatCache) -> Option<String> {
    let (dir, file) = match name.rfind('/') {
        Some(at) => (&name[..=at], &name[at + 1..]),
        None => ("", name),
    };
    let path = format!("{dir}SCCS/s.{file}");
    if stats.exists(&path) {
        return Some(path);
    }

    let project = plib::projectdir::from_env()?;
    let path = plib::projectdir::sccs_dir(&project).join(format!("s.{file}"));
    let path = path.to_string_lossy().into_owned();
    stats.exists(&path).then_some(path)
}

/// Whether `target` should be retrieved from its SCCS history.
///
/// POSIX 105701: retrieve when the target is missing or the history is newer.
/// POSIX 105704: "if the target is writable by anyone, make shall not retrieve
/// a new version" -- a checked-out, editable file is left as it is.
fn needs_sccs_get(target: &str, history: &str) -> bool {
    let Ok(metadata) = fs::metadata(target) else {
        return true;
    };
    if metadata.permissions().mode() & 0o222 != 0 {
        return false;
    }
    match (get_modified_time(history), get_modified_time(target)) {
        (Some(history), Some(target)) => history > target,
        _ => true,
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
pub(crate) fn parse_archive_target(s: &str) -> Option<(&str, &str)> {
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
pub(crate) fn archive_member_mtime(archive_path: &str, member: &str) -> Option<SystemTime> {
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

        let path = plib::testing::TempFile::new("member_test.a", &ar);
        let path_str = path.to_str().unwrap();

        let expected = SystemTime::UNIX_EPOCH + Duration::from_secs(1234567890);
        assert_eq!(archive_member_mtime(path_str, "m.o"), Some(expected));
        assert_eq!(archive_member_mtime(path_str, "absent.o"), None);
    }
}
