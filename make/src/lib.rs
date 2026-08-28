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
pub mod graph;
pub mod parser;
pub mod rule;
pub mod signal_handler;
pub mod special_target;

use std::{
    fs::{self},
    time::{Duration, SystemTime},
};

use parser::{Makefile, VPathEntry};

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
    /// Which targets are built, building, or failed. Gives single-build under
    /// `-j` and memoization across repeated visits.
    ledger: graph::Ledger,
    /// `vpath` search paths, in declaration order.
    vpaths: Vec<VPathEntry>,
    pub config: Config,
}

impl graph::Edges for Make {
    /// The prerequisites the cycle check must see.
    ///
    /// A named rule is not the only way a target acquires prerequisites: a `%`
    /// pattern rule contributes them once instantiated for a concrete name.
    /// Looking only at named rules made `%.a: %.a` invisible to `find_cycle`,
    /// so the self-edge went undetected and the build deadlocked on it
    /// (audit #65).
    fn prerequisites_of(&self, target: &str) -> Vec<String> {
        let named = self
            .rule_by_target_name(target)
            .map(|rule| edge_names(rule.prerequisites()));
        if let Some(edges) = named {
            return edges;
        }
        match self.find_pattern_rule(target) {
            Some(rule) => edge_names(rule.prerequisites()),
            None => Vec::new(),
        }
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
        self.rules
            .iter()
            .filter(|rule| !rule.is_pattern())
            .find(|rule| rule.targets().any(|t| t.as_ref() == target.as_ref()))
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
    /// returned unchanged, so this is transparent when `VPATH` is unset.
    pub fn resolve_vpath(&self, name: &str) -> String {
        if name.contains('/') || std::path::Path::new(name).exists() {
            return name.to_string();
        }
        // A `vpath` pattern is more specific than the blanket `VPATH` macro, so
        // it is consulted first; the first matching pattern wins, as GNU does.
        if let Some(found) = self.search_vpath_patterns(name) {
            return found;
        }
        search_dirs(&self.vpath_dirs(), name).unwrap_or_else(|| name.to_string())
    }

    /// Look `name` up in the directories of the first `vpath` pattern it
    /// matches.
    fn search_vpath_patterns(&self, name: &str) -> Option<String> {
        for entry in &self.vpaths {
            if rule::pattern_stem(&entry.pattern, name).is_none() {
                continue;
            }
            let dirs: Vec<&str> = entry.dirs.iter().map(String::as_str).collect();
            if let Some(found) = search_dirs(&dirs, name) {
                return Some(found);
            }
        }
        None
    }

    /// Find a `%` pattern rule matching `name` and instantiate it for that
    /// target. The first matching pattern in file order wins, as GNU does.
    ///
    /// Pattern rules are not POSIX -- the standard has only suffix inference
    /// rules -- but they are how real makefiles express the same thing.
    fn find_pattern_rule(&self, name: &str) -> Option<Rule> {
        self.rules
            .iter()
            .filter(|rule| rule.is_pattern())
            .find_map(|rule| rule.instantiate_pattern(name))
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
            .ok_or(NoTarget { target: None })
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
                    let prereq_path = self.resolve_vpath(&format!("{stem}{source_suffix}"));
                    if std::path::Path::new(&prereq_path).exists() {
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
                let prereq_path = self.resolve_vpath(&format!("{name}{source_suffix}"));
                if std::path::Path::new(&prereq_path).exists() {
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
        match self.ledger.claim(name.as_ref()) {
            graph::Claim::Done(outcome) => return outcome.into_result(),
            graph::Claim::Build => {}
        }
        let result = self.build_target_uncached(name.as_ref());
        self.ledger.finish(name.as_ref(), &result);
        result
    }

    fn build_target_uncached(&self, name: impl AsRef<str>) -> Result<bool, ErrorCode> {
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
                    // A `%.o: %.c` pattern rule, instantiated for this target.
                    // Pattern rules are tried before suffix inference: a
                    // makefile that writes `%.o: %.c` means it, and must not
                    // lose to the built-in `.c.o`. GNU orders them the same way.
                    if let Some(rule) = self.find_pattern_rule(name.as_ref()) {
                        // A pattern whose prerequisite matches the same pattern
                        // makes the target its own prerequisite; without this
                        // the build blocks on the ledger forever (audit #65).
                        if let Some(origin) = graph::find_cycle(self, name.as_ref()) {
                            return Err(RecursivePrerequisite { origin });
                        }
                        let target = Target::new(name.as_ref());
                        for prerequisite in rule.prerequisites() {
                            self.build_target(prerequisite.as_ref())?;
                        }
                        let newer: Vec<String> = rule
                            .prerequisites()
                            .map(|p| p.as_ref().to_string())
                            .collect();
                        rule.run(&self.config, &self.macros, &target, false, &newer)?;
                        return Ok(true);
                    }
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
                            &|name| self.resolve_vpath(name),
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
        if let Some(origin) = graph::find_cycle(self, target.as_ref()) {
            return Err(RecursivePrerequisite { origin });
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
                    &|name| self.resolve_vpath(name),
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
            // Under -k a failed prerequisite must not stop its *siblings*; the
            // first error is still returned, so the dependent target is skipped.
            let mut first_error = None;
            for name in names {
                if let Err(err) = self.build_target(name) {
                    if !self.config.keep_going || self.config.terminate {
                        return Err(err);
                    }
                    first_error.get_or_insert(err);
                }
            }
            return match first_error {
                Some(err) => Err(err),
                None => Ok(()),
            };
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
                    let resolved = self.resolve_vpath(prerequisite.as_ref());
                    let Some(pre_modified) = get_modified_time(&resolved) else {
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
}

/// Return `name` found under one of `dirs`, if any.
fn search_dirs(dirs: &[&str], name: &str) -> Option<String> {
    dirs.iter()
        .map(|dir| format!("{dir}/{name}"))
        .find(|candidate| std::path::Path::new(candidate).exists())
}

/// The default macros and inference rules POSIX requires make to start with.
///
/// These existed only as display strings in the `-p` dump table, so `make f.o`
/// with an `f.c` present reported "no target" -- every makefile that leans on
/// the built-in `.c.o` rule, which is most of them, could not build.
///
/// Written as makefile text so it goes through the same reader as anything
/// else. `-r` (`config.clear`) suppresses it, as POSIX requires.
const BUILTIN_RULES: &str = r#"
.c:
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<
.sh:
	cp $< $@
	chmod a+x $@
.c.o:
	$(CC) $(CFLAGS) -c $<
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
"#;

/// The macros those rules refer to. A makefile definition overrides them, so
/// they are seeded first and only kept where the makefile is silent.
const BUILTIN_MACROS: [(&str, &str); 8] = [
    ("CC", "c17"),
    ("CFLAGS", "-O 1"),
    ("AR", "ar"),
    ("ARFLAGS", "-rv"),
    ("YACC", "yacc"),
    ("YFLAGS", ""),
    ("LEX", "lex"),
    ("LFLAGS", ""),
];

impl TryFrom<(Makefile, Config)> for Make {
    type Error = ErrorCode;

    fn try_from((makefile, config): (Makefile, Config)) -> Result<Self, Self::Error> {
        // Two-pass classification: .SUFFIXES must be processed before inference
        // rule classification so that user-defined suffixes (especially with -r)
        // are available when determining whether a rule like `.txt.out:` is an
        // inference rule.

        let (parsed_rules, macros, vpaths) = makefile.into_parts();

        let mut suffixes_rules = vec![];
        let mut remaining_parsed_rules = vec![];

        // Pass 1: Separate .SUFFIXES rules from everything else and process
        // them immediately so config.rules[".SUFFIXES"] is populated.
        for parsed_rule in parsed_rules {
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
            macros,
            default_rule: None,
            pool,
            ledger: graph::Ledger::new(),
            vpaths,
            config,
        };

        // Seed the built-in macros the default rules refer to, without
        // overriding anything the makefile defined.
        if !make.config.clear {
            for (name, value) in BUILTIN_MACROS {
                if !make.macros.iter().any(|(n, _)| n == name) {
                    make.macros.push((name.to_string(), value.to_string()));
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
                return Err(NoTarget { target: None });
            };

            if SpecialTarget::try_from(target.clone()).is_ok() {
                special_rules.push(rule);
            } else if InferenceTarget::try_from((target.clone(), make.config.clone())).is_ok() {
                make.inference_rules.push(rule);
            } else {
                // POSIX 105653: several rules may name the same target, with
                // prerequisites accumulating. The old lookup returned the first
                // match and silently dropped the rest (audit #30).
                let name = target.to_string();
                match make
                    .rules
                    .iter_mut()
                    .find(|existing| existing.targets().any(|t| t.as_ref() == name))
                {
                    Some(existing) => existing.absorb(rule),
                    None => make.rules.push(rule),
                }
            }
        }

        for rule in special_rules {
            special_target::process(rule, &mut make)?;
        }

        // Built-in inference rules come last, so a rule of the same name in
        // the makefile is found first.
        if !make.config.clear {
            // The built-in recipes reference $(CC), $(CFLAGS) and friends, so
            // they must be expanded against the macros the makefile defined
            // plus the defaults it did not. Emitting those as immediate
            // definitions ahead of the rules runs the whole thing through the
            // ordinary reader.
            let mut builtin = String::new();
            for (name, value) in &make.macros {
                builtin.push_str(&format!("{name} ::= {value}\n"));
            }
            builtin.push_str(&BUILTIN_RULES.replace("\\t", "\t"));
            if let Ok(parsed) = builtin.parse::<Makefile>() {
                let (rules, _, _) = parsed.into_parts();
                for parsed_rule in rules {
                    let rule = Rule::from(parsed_rule);
                    let Some(target) = rule.targets().next() else {
                        continue;
                    };
                    let name = target.to_string();
                    let already = make
                        .inference_rules
                        .iter()
                        .any(|r| r.targets().any(|t| t.as_ref() == name));
                    if !already {
                        make.inference_rules.push(rule);
                    }
                }
            }
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
