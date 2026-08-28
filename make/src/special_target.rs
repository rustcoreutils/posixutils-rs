//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;

use crate::{
    attributes::Flags,
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

    /// A dot-target is an inference rule only if it *parses* as `.s1` or
    /// `.s1.s2` and its suffixes are known.
    ///
    /// The previous version asked whether the name merely started with a known
    /// suffix, which filed `.config:` as an inference rule for `.c` and made a
    /// bare `make` build the wrong target (audit #38). It also computed `from`
    /// and `to` from the same expression, so both were the same string.
    fn try_from((target, config): (Target, Config)) -> Result<Self, Self::Error> {
        let Target::Inference { from, to, .. } = Target::new(target.to_string()) else {
            return Err(ParseError);
        };
        let known = |suffix: &str| {
            config
                .suffixes
                .iter()
                .any(|s| s.trim_start_matches('.') == suffix)
        };
        if !known(&from) {
            return Err(ParseError);
        }
        if to.is_empty() {
            return Ok(Self { from, to: None });
        }
        if !known(&to) {
            return Err(ParseError);
        }
        Ok(Self { from, to: Some(to) })
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
    /// The targets this special target names.
    ///
    /// POSIX: "prerequisites of this special target are targets themselves".
    fn named_targets(&self) -> Vec<String> {
        self.rule
            .prerequisites()
            .map(|p| p.as_ref().to_string())
            .collect()
    }

    /// Set an attribute on each named target.
    ///
    /// Per target, never per rule: one rule may name several targets, and
    /// `.IGNORE: a` on `a b: dep` must leave `b` alone (audit #78). Marking a
    /// name that has no rule at all is harmless -- an inference rule may
    /// supply it later.
    fn mark_each(&mut self, set: impl Fn(&mut Flags)) {
        for target in self.named_targets() {
            self.make.attributes.mark(&target, &set);
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

    /// POSIX 105663: with no prerequisites, "make shall behave as if the -i
    /// option had been specified" -- a global option, not an attribute on
    /// every target.
    fn process_ignore(mut self) -> Result<(), Error> {
        self.without_recipes()?;

        if self.named_targets().is_empty() {
            self.make.config.ignore = true;
            return Ok(());
        }
        self.mark_each(|flags| flags.ignore = true);

        Ok(())
    }

    /// Likewise `-s` for a `.SILENT` with no prerequisites.
    fn process_silent(mut self) -> Result<(), Error> {
        self.without_recipes()?;

        if self.named_targets().is_empty() {
            self.make.config.silent = true;
            return Ok(());
        }
        self.mark_each(|flags| flags.silent = true);

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
    /// POSIX 105677: "a `.PHONY` special target with no prerequisites shall be
    /// ignored". It used to fall through to the global modifier and mark every
    /// rule phony, so nothing in the makefile was ever up to date (audit #77).
    ///
    /// Subsequent occurrences add to the list, which is inherent: each marks
    /// the targets it names and nothing clears a flag.
    fn process_phony(mut self) -> Result<(), Error> {
        self.mark_each(|flags| flags.phony = true);

        Ok(())
    }

    /// POSIX 105689: with no prerequisites, "all targets in the makefile shall
    /// be treated as if specified with `.PRECIOUS`", which is a global flag.
    fn process_precious(mut self) -> Result<(), Error> {
        if self.named_targets().is_empty() {
            self.make.config.precious = true;
            return Ok(());
        }
        self.mark_each(|flags| flags.precious = true);

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

    /// `.SCCS_GET` names the recipe for retrieving a source file from SCCS.
    ///
    /// Accepted and validated, but inert: this make performs no SCCS retrieval,
    /// so there is nothing to run the recipe from. Recorded as audit #61 rather
    /// than left looking implemented — its recipe used to be stored in the `-p`
    /// mirror table and read by nothing, which made the gap invisible.
    fn process_sccs_get(self) -> Result<(), Error> {
        self.without_prerequisites()?;

        Ok(())
    }
}
