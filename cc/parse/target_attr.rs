//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((target("...")))` and `target_clones("...", ...)` in the
// parser: their arguments checked and diagnosed as gcc 13 diagnoses them,
// and what they ask of a function read into its `FunctionAttrs`. The
// strings themselves are read by `crate::target_attr`.
//

use super::attribute::{AttributeArg, AttributeList, ATTRIBUTE_WARNING};
use crate::diag;
use crate::target::{IsaRequest, Os, Target};
use crate::target_attr::{
    parse_target, parse_target_clones, ClonesIssue, TargetClones, TargetIssue,
};
use crate::token::lexer::Position;

/// The string arguments of a `target` or `target_clones` attribute, or
/// `None` when one is not a string.
fn string_args(args: &[AttributeArg]) -> Option<Vec<&str>> {
    args.iter()
        .map(|a| match a {
            AttributeArg::String(s) => Some(s.as_str()),
            _ => None,
        })
        .collect()
}

/// Every item of a `target_clones` attribute: each string argument split
/// at its commas, as gcc reads them.
fn clone_items(strings: &[&str]) -> Vec<String> {
    strings
        .iter()
        .flat_map(|s| s.split(','))
        .map(str::to_string)
        .collect()
}

/// A warning in the `-Wattributes` group.
fn attr_warning(pos: Position, template: &str, args: &[&str]) {
    diag::group_warning_args(ATTRIBUTE_WARNING, pos, template, args);
}

fn report_target_issue(pos: Position, issue: &TargetIssue) {
    match issue {
        TargetIssue::Empty => attr_warning(pos, "empty string in attribute '{0}'", &["target"]),
        TargetIssue::Unknown(item) => diag::error_args(
            pos,
            "attribute '{0}' argument '{1}' is unknown",
            &["target", item],
        ),
        TargetIssue::BadCpu { option, value } => diag::error_args(
            pos,
            "bad value '{0}' for 'target(\"{1}\")' attribute",
            &[value, option],
        ),
        TargetIssue::BadOptionValue(item) => diag::error_args(
            pos,
            "attribute value '{0}' is unknown in '{1}' attribute",
            &[item, "target"],
        ),
        // `target` only permits an ISA: ignoring one c17 does not generate
        // leaves plain C as it was, and a body that really used it fails on
        // its own. libzstd's tests build such functions under -Werror.
        TargetIssue::BeyondCeiling(_) => {}
        TargetIssue::Unsupported(item) => attr_warning(
            pos,
            "'{0}' in 'target' attribute is not supported and is ignored",
            &[item],
        ),
        TargetIssue::NotValid(item) => diag::error_args(
            pos,
            "pragma or attribute 'target(\"{0}\")' is not valid",
            &[item],
        ),
    }
}

fn report_clones_issue(pos: Position, issue: &ClonesIssue) {
    match issue {
        ClonesIssue::Single => {
            attr_warning(pos, "single '{0}' attribute is ignored", &["target_clones"])
        }
        ClonesIssue::NoDefault => diag::error_args(pos, "'{0}' target was not set", &["default"]),
        ClonesIssue::MultipleDefault => {
            diag::error_args(pos, "multiple '{0}' targets were set", &["default"])
        }
        ClonesIssue::Unknown(item) => diag::error_args(
            pos,
            "attribute '{0}' argument '{1}' is unknown",
            &["target_clone", item],
        ),
        ClonesIssue::Negated(item) => diag::error_args(
            pos,
            "ISA '{0}' is not supported in 'target' attribute, use 'arch=' syntax",
            &[item],
        ),
        ClonesIssue::Dropped(item) => attr_warning(
            pos,
            "'target_clones' version '{0}' is beyond c17's SSE4.2 ceiling and is not built",
            &[item],
        ),
        ClonesIssue::NotValid(item) => diag::error_args(
            pos,
            "pragma or attribute 'target(\"{0}\")' is not valid",
            &[item],
        ),
        ClonesIssue::NoDispatcher => diag::error(
            pos,
            &gettextrs::gettext("target does not support function version dispatcher"),
        ),
    }
}

/// Check the arguments of a `target` (`clones` false) or `target_clones`
/// attribute, diagnosing what gcc diagnoses. `false` drops the attribute:
/// one gcc refuses asks for nothing c17 could honour.
pub(super) fn check_target_args(
    clones: bool,
    args: &[AttributeArg],
    pos: Position,
    target: &Target,
) -> bool {
    let name = if clones { "target_clones" } else { "target" };
    let Some(strings) = string_args(args).filter(|s| !s.is_empty()) else {
        diag::error_args(pos, "attribute '{0}' argument is not a string", &[name]);
        return false;
    };
    if clones {
        let (_, issues) = parse_target_clones(&clone_items(&strings), target.arch);
        for issue in &issues {
            report_clones_issue(pos, issue);
        }
        !issues.iter().any(ClonesIssue::is_error)
    } else {
        let mut ok = true;
        for text in strings {
            let (_, issues) = parse_target(text, target.arch);
            for issue in &issues {
                report_target_issue(pos, issue);
            }
            ok &= !issues.iter().any(TargetIssue::is_error);
        }
        ok
    }
}

impl AttributeList {
    /// What a `target` attribute in this list asks of the function's ISA.
    /// Its arguments were checked when it was parsed.
    pub(super) fn target_request(&self, target: &Target) -> Option<IsaRequest> {
        let attr = self.find("target")?;
        let strings = string_args(&attr.args)?;
        let mut request = IsaRequest::default();
        for text in strings {
            let (part, _) = parse_target(text, target.arch);
            request.extend(part);
        }
        Some(request)
    }

    /// The versions a `target_clones` attribute in this list asks for, when
    /// there are any to dispatch between. Mach-O has no indirect functions,
    /// so there the function is its `default` version alone.
    pub(super) fn target_clones(&self, target: &Target) -> Option<TargetClones> {
        if target.os == Os::MacOS {
            return None;
        }
        let attr = self.find("target_clones")?;
        let strings = string_args(&attr.args)?;
        parse_target_clones(&clone_items(&strings), target.arch).0
    }
}

/// `target` and `target_clones` on one function: gcc keeps the `target`
/// and warns that the clones are ignored.
pub(super) fn resolve_target_conflict(attrs: &mut super::ast::FunctionAttrs, pos: Position) {
    if attrs.target.is_some() && attrs.clones.take().is_some() {
        attr_warning(
            pos,
            "'{0}' attribute ignored due to conflict with '{1}' attribute",
            &["target_clones", "target"],
        );
    }
}
