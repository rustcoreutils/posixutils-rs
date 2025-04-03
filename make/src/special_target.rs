//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;
use std::collections::BTreeSet;

use crate::error_code::ErrorCode;
use crate::rule::target::Target;
use crate::rule::Rule;
use crate::Make;

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
}
use crate::config::Config;
use gettextrs::gettext;
use SpecialTarget::*;

impl SpecialTarget {
    // could be automated with `strum`
    pub const COUNT: usize = 8;
    pub const VARIANTS: [Self; Self::COUNT] = [
        Default, Ignore, Posix, Precious, SccsGet, Silent, Suffixes, Phony,
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
        let map = &BTreeSet::new();
        let suffixes = config.rules.get(".SUFFIXES").unwrap_or(map);
        let source = target.to_string();

        let from = suffixes
            .iter()
            .filter_map(|x| source.strip_prefix(x))
            .next()
            .ok_or(ParseError)?
            .to_string();
        let to = suffixes
            .iter()
            .filter_map(|x| source.strip_prefix(x))
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
        Ignore => this.process_ignore(),
        Silent => this.process_silent(),
        Suffixes => this.process_suffixes(),
        Phony => this.process_phony(),
        Precious => this.process_precious(),
        SccsGet => this.process_sccs_get(),
        unsupported => Err(Error::NotSupported(unsupported)),
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
                .filter(|r| r.targets().any(|t| t.as_ref() == prerequisite.as_ref()))
                .for_each(f.clone());
        }
    }

    /// - Global: the special target applies to all rules in the makefile if no prerequisites are
    ///   specified.
    fn global(&mut self, f: impl FnMut(&mut Rule) + Clone) {
        if self.rule.prerequisites().count() == 0 {
            self.make.rules.iter_mut().for_each(f);
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
}

/// This impl block contains processing logic for special targets
impl Processor<'_> {
    fn process_default(self) -> Result<(), Error> {
        self.without_prerequisites()?;

        self.make.default_rule.replace(self.rule);

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

    fn process_suffixes(self) -> Result<(), Error> {
        let suffixes_key = Suffixes.as_ref();
        let suffixes_set = self
            .rule
            .prerequisites()
            .map(|suffix| suffix.as_ref().to_string())
            .collect::<BTreeSet<String>>();

        self.make
            .config
            .rules
            .insert(suffixes_key.to_string(), suffixes_set);

        Ok(())
    }
    fn process_phony(mut self) -> Result<(), Error> {
        let suffixes_set = self
            .rule
            .prerequisites()
            .map(|suffix| suffix.as_ref().to_string())
            .collect::<BTreeSet<String>>();

        self.make
            .config
            .rules
            .insert(Phony.as_ref().to_string(), suffixes_set);

        let what_to_do = |rule: &mut Rule| rule.config.phony = true;
        self.additive(what_to_do);
        self.global(what_to_do);

        Ok(())
    }

    fn process_precious(mut self) -> Result<(), Error> {
        let precious_set = self
            .rule
            .prerequisites()
            .map(|val| val.as_ref().to_string())
            .collect::<BTreeSet<String>>();

        let what_to_do = |rule: &mut Rule| rule.config.precious = true;

        self.additive(what_to_do);
        self.global(what_to_do);

        self.make
            .config
            .rules
            .insert(Precious.as_ref().to_string(), precious_set);
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
