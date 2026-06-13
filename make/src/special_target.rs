//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;
use std::collections::BTreeSet;

use crate::{
    error_code::ErrorCode,
    rule::{target::Target, Rule},
    Make,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecialTarget {
    Default,
    Ignore,
    Posix,
    Phony,
    Precious,
    SccsGet,
    Silent,
    Suffixes,
    Wait,
    NotParallel,
}
use crate::config::Config;
use gettextrs::gettext;
use SpecialTarget::*;

impl SpecialTarget {
    // could be automated with `strum`
    pub const COUNT: usize = 10;
    pub const VARIANTS: [Self; Self::COUNT] = [
        Default,
        Ignore,
        Posix,
        Precious,
        SccsGet,
        Silent,
        Suffixes,
        Phony,
        Wait,
        NotParallel,
    ];
}

impl AsRef<str> for SpecialTarget {
    fn as_ref(&self) -> &'static str {
        match self {
            Default => ".DEFAULT",
            Ignore => ".IGNORE",
            Posix => ".POSIX",
            Precious => ".PRECIOUS",
            SccsGet => ".SCCS_GET",
            Silent => ".SILENT",
            Suffixes => ".SUFFIXES",
            Phony => ".PHONY",
            Wait => ".WAIT",
            NotParallel => ".NOTPARALLEL",
        }
    }
}

impl fmt::Display for SpecialTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[derive(Debug)]
pub struct InferenceTarget {
    from: String,
    to: Option<String>,
}

impl InferenceTarget {
    pub fn from(&self) -> &str {
        self.from.as_ref()
    }

    pub fn to(&self) -> Option<&str> {
        self.to.as_deref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    MustNotHavePrerequisites,
    MustNotHaveRecipes,
    MustHaveRecipes,

    NotSupported(SpecialTarget),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            MustNotHavePrerequisites => {
                write!(
                    f,
                    "{}",
                    gettext("the special target must not have prerequisites"),
                )
            }
            MustNotHaveRecipes => {
                write!(f, "{}", gettext("the special target must not have recipes"))
            }
            MustHaveRecipes => {
                write!(f, "{}", gettext("the special target must have recipes"))
            }
            NotSupported(target) => {
                write!(
                    f,
                    "{}: '{}'",
                    gettext("the special target is not supported"),
                    target,
                )
            }
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct ParseError;
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", gettext("parse error"))
    }
}

impl TryFrom<Target> for SpecialTarget {
    type Error = ParseError;

    fn try_from(target: Target) -> Result<Self, Self::Error> {
        for variant in Self::VARIANTS {
            if target.as_ref() == variant.as_ref() {
                return Ok(variant);
            }
        }
        Err(ParseError)
    }
}

impl TryFrom<(Target, Config)> for InferenceTarget {
    type Error = ParseError;

    fn try_from((target, config): (Target, Config)) -> Result<Self, Self::Error> {
        let suffixes = &config.suffixes;
        let source = target.to_string();

        let from = suffixes
            .iter()
            .filter_map(|x| source.strip_prefix(x.as_str()))
            .next()
            .ok_or(ParseError)?
            .to_string();
        let to = suffixes
            .iter()
            .filter_map(|x| source.strip_prefix(x.as_str()))
            .next()
            .map(|x| x.to_string());

        Ok(Self { from, to })
    }
}

pub struct Processor<'make> {
    rule: Rule,
    make: &'make mut Make,
}

pub fn process(rule: Rule, make: &mut Make) -> Result<(), ErrorCode> {
    let Some(target) = rule.targets().next().cloned() else {
        return Err(ErrorCode::NoTarget { target: None });
    };

    let this = Processor { rule, make };

    let Ok(target) = SpecialTarget::try_from(target) else {
        // not an error, ignore
        return Ok(());
    };

    match target {
        Default => this.process_default(),
        Posix => this.process_posix(),
        Ignore => this.process_ignore(),
        Silent => this.process_silent(),
        Suffixes => this.process_suffixes(),
        Phony => this.process_phony(),
        Precious => this.process_precious(),
        SccsGet => this.process_sccs_get(),
        Wait => this.process_wait(),
        NotParallel => this.process_not_parallel(),
    }
    .map_err(|err| ErrorCode::SpecialTargetConstraintNotFulfilled {
        target: target.to_string(),
        constraint: err,
    })
}

/// This impl block contains modifiers for special targets
impl Processor<'_> {
    /// - Additive: multiple special targets can be specified in the same makefile and the effects are
    ///   cumulative.
    fn additive(&mut self, f: impl FnMut(&mut Rule) + Clone) {
        for prerequisite in self.rule.prerequisites() {
            self.make
                .rules
                .iter_mut()
                .chain(self.make.inference_rules.iter_mut())
                .filter(|r| r.targets().any(|t| t.as_ref() == prerequisite.as_ref()))
                .for_each(f.clone());
        }
    }

    /// - Global: the special target applies to all rules in the makefile if no prerequisites are
    ///   specified.
    fn global(&mut self, f: impl FnMut(&mut Rule) + Clone) {
        if self.rule.prerequisites().count() == 0 {
            self.make
                .rules
                .iter_mut()
                .chain(self.make.inference_rules.iter_mut())
                .for_each(f);
        }
    }
}

/// This impl block contains constraint validations for special targets
impl Processor<'_> {
    fn without_prerequisites(&self) -> Result<(), Error> {
        if self.rule.prerequisites().count() > 0 {
            return Err(Error::MustNotHavePrerequisites);
        }
        Ok(())
    }

    fn without_recipes(&self) -> Result<(), Error> {
        if self.rule.recipes().count() > 0 {
            return Err(Error::MustNotHaveRecipes);
        }
        Ok(())
    }

    fn with_recipes(&self) -> Result<(), Error> {
        if self.rule.recipes().count() == 0 {
            return Err(Error::MustHaveRecipes);
        }
        Ok(())
    }
}

/// This impl block contains processing logic for special targets
impl Processor<'_> {
    fn process_default(self) -> Result<(), Error> {
        // POSIX: `.DEFAULT` is specified with commands but without prerequisites.
        self.without_prerequisites()?;
        self.with_recipes()?;

        self.make.default_rule.replace(self.rule);

        Ok(())
    }

    /// `.POSIX` requests strictly conformant behavior. POSIX requires it to be
    /// specified without prerequisites or commands; beyond validating that, we
    /// simply accept it (the implementation already follows POSIX semantics).
    fn process_posix(self) -> Result<(), Error> {
        self.without_prerequisites()?;
        self.without_recipes()?;

        Ok(())
    }

    fn process_ignore(mut self) -> Result<(), Error> {
        self.without_recipes()?;

        let what_to_do = |rule: &mut Rule| rule.config.ignore = true;
        self.additive(what_to_do);
        self.global(what_to_do);

        Ok(())
    }

    fn process_silent(mut self) -> Result<(), Error> {
        self.without_recipes()?;

        let what_to_do = |rule: &mut Rule| rule.config.silent = true;
        self.additive(what_to_do);
        self.global(what_to_do);

        Ok(())
    }

    /// Per POSIX: a `.SUFFIXES` with no prerequisites clears the suffix list; a
    /// later `.SUFFIXES` with prerequisites appends to it (preserving order).
    fn process_suffixes(self) -> Result<(), Error> {
        let new_suffixes: Vec<String> = self
            .rule
            .prerequisites()
            .map(|suffix| suffix.as_ref().to_string())
            .collect();

        if new_suffixes.is_empty() {
            self.make.config.clear_suffixes();
        } else {
            for suffix in new_suffixes {
                self.make.config.add_suffix(&suffix);
            }
        }

        Ok(())
    }
    fn process_phony(mut self) -> Result<(), Error> {
        // POSIX: subsequent occurrences add to the list, so extend rather than
        // replace the stored set.
        let names = self
            .rule
            .prerequisites()
            .map(|suffix| suffix.as_ref().to_string());
        self.make
            .config
            .rules
            .entry(Phony.as_ref().to_string())
            .or_default()
            .extend(names);

        let what_to_do = |rule: &mut Rule| rule.config.phony = true;
        self.additive(what_to_do);
        self.global(what_to_do);

        Ok(())
    }

    fn process_precious(mut self) -> Result<(), Error> {
        let precious_names: Vec<String> = self
            .rule
            .prerequisites()
            .map(|val| val.as_ref().to_string())
            .collect();

        // POSIX: with no prerequisites, `.PRECIOUS` protects *all* targets, so
        // set the global flag the signal handler consults.
        if precious_names.is_empty() {
            self.make.config.precious = true;
        }

        let what_to_do = |rule: &mut Rule| rule.config.precious = true;

        self.additive(what_to_do);
        self.global(what_to_do);

        // POSIX: subsequent occurrences add to the list.
        self.make
            .config
            .rules
            .entry(Precious.as_ref().to_string())
            .or_default()
            .extend(precious_names);
        Ok(())
    }
    /// `.WAIT` as a target has no effect; it must have no prerequisites or
    /// commands. Its meaning as a prerequisite is handled during the build.
    fn process_wait(self) -> Result<(), Error> {
        self.without_prerequisites()?;
        self.without_recipes()?;
        Ok(())
    }

    /// `.NOTPARALLEL` forces make to update one target at a time regardless of
    /// `-j`. It must have no prerequisites or commands.
    fn process_not_parallel(self) -> Result<(), Error> {
        self.without_prerequisites()?;
        self.without_recipes()?;
        self.make.config.not_parallel = true;
        Ok(())
    }

    fn process_sccs_get(self) -> Result<(), Error> {
        self.without_prerequisites()?;

        let sccs_set = self
            .rule
            .recipes()
            .map(|val| val.as_ref().to_string())
            .collect::<BTreeSet<String>>();

        self.make
            .config
            .rules
            .insert(SccsGet.as_ref().to_string(), sccs_set);

        Ok(())
    }
}
