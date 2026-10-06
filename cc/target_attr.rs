//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The strings of `__attribute__((target("...")))` and
// `__attribute__((target_clones("...", ...)))`: what each asks of a
// function's ISA, and what in them c17 refuses or does not support.
//
// c17 models the x86-64 SIMD extensions up to SSE4.2 and POPCNT
// (`target::X86Isa`) and nothing above: an ISA gcc knows beyond that is
// warned about by name and ignored, so the function is compiled at the
// translation unit's ISA. Every name gcc 13 rejects is an error here too,
// in gcc's words.
//

use crate::target::{isa_edit, Arch, IsaRequest, IsaSet};

/// gcc 13's x86-64 `-march=` roster, which `target("arch=...")` and
/// `target("tune=...")` accept: the list its "valid arguments" note prints.
const X86_CPUS: &[&str] = &[
    "nocona",
    "core2",
    "nehalem",
    "corei7",
    "westmere",
    "sandybridge",
    "corei7-avx",
    "ivybridge",
    "core-avx-i",
    "haswell",
    "core-avx2",
    "broadwell",
    "skylake",
    "skylake-avx512",
    "cannonlake",
    "icelake-client",
    "rocketlake",
    "icelake-server",
    "cascadelake",
    "tigerlake",
    "cooperlake",
    "sapphirerapids",
    "emeraldrapids",
    "alderlake",
    "raptorlake",
    "meteorlake",
    "graniterapids",
    "graniterapids-d",
    "bonnell",
    "atom",
    "silvermont",
    "slm",
    "goldmont",
    "goldmont-plus",
    "tremont",
    "gracemont",
    "sierraforest",
    "grandridge",
    "knl",
    "knm",
    "x86-64",
    "x86-64-v2",
    "x86-64-v3",
    "x86-64-v4",
    "eden-x2",
    "nano",
    "nano-1000",
    "nano-2000",
    "nano-3000",
    "nano-x2",
    "eden-x4",
    "nano-x4",
    "lujiazui",
    "k8",
    "k8-sse3",
    "opteron",
    "opteron-sse3",
    "athlon64",
    "athlon64-sse3",
    "athlon-fx",
    "amdfam10",
    "barcelona",
    "bdver1",
    "bdver2",
    "bdver3",
    "bdver4",
    "znver1",
    "znver2",
    "znver3",
    "znver4",
    "btver1",
    "btver2",
    "native",
];

/// The x86-64 baseline: extensions every x86-64 CPU has, so asking for one
/// asks for nothing.
const X86_BASELINE: &[&str] = &["mmx", "sse", "sse2", "fxsr"];

/// Every other ISA name gcc 13's `target` attribute accepts and c17 does not
/// model: each was probed with gcc. c17's SIMD ceiling is SSE4.2, so all of
/// these are beyond what it generates code for.
const X86_UNMODELLED_ISAS: &[&str] = &[
    "3dnow",
    "3dnowa",
    "abm",
    "adx",
    "aes",
    "amx-bf16",
    "amx-complex",
    "amx-fp16",
    "amx-int8",
    "amx-tile",
    "avx",
    "avx2",
    "avx5124fmaps",
    "avx5124vnniw",
    "avx512bf16",
    "avx512bitalg",
    "avx512bw",
    "avx512cd",
    "avx512dq",
    "avx512er",
    "avx512f",
    "avx512fp16",
    "avx512ifma",
    "avx512pf",
    "avx512vbmi",
    "avx512vbmi2",
    "avx512vl",
    "avx512vnni",
    "avx512vp2intersect",
    "avx512vpopcntdq",
    "avxifma",
    "avxneconvert",
    "avxvnni",
    "avxvnniint8",
    "bmi",
    "bmi2",
    "cldemote",
    "clflushopt",
    "clwb",
    "clzero",
    "cmpccxadd",
    "crc32",
    "cx16",
    "enqcmd",
    "f16c",
    "fma",
    "fma4",
    "fsgsbase",
    "gfni",
    "hle",
    "hreset",
    "kl",
    "lwp",
    "lzcnt",
    "movbe",
    "movdir64b",
    "movdiri",
    "mwait",
    "mwaitx",
    "pclmul",
    "pconfig",
    "pku",
    "prefetchi",
    "prefetchwt1",
    "prfchw",
    "ptwrite",
    "raoint",
    "rdpid",
    "rdrnd",
    "rdseed",
    "rtm",
    "sahf",
    "serialize",
    "sgx",
    "sha",
    "shstk",
    "sse4a",
    "tbm",
    "tsxldtrk",
    "uintr",
    "vaes",
    "vpclmulqdq",
    "waitpkg",
    "wbnoinvd",
    "widekl",
    "xop",
    "xsave",
    "xsavec",
    "xsaveopt",
    "xsaves",
];

/// gcc 13's code-generation switches that `target` accepts, each with a
/// `no-` form: none is an ISA, and c17 implements none of them.
const X86_SWITCHES: &[&str] = &[
    "cld",
    "fancy-math-387",
    "ieee-fp",
    "align-stringops",
    "inline-all-stringops",
    "inline-stringops-dynamically",
    "recip",
    "general-regs-only",
];

/// Something in a `target` string that c17 refuses or ignores.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetIssue {
    /// The whole string is empty: gcc warns.
    Empty,
    /// A name gcc does not know: an error.
    Unknown(String),
    /// `arch=` or `tune=` with a CPU gcc does not know: an error.
    BadCpu { option: &'static str, value: String },
    /// `fpmath=` or `prefer-vector-width=` with a value gcc does not know:
    /// an error.
    BadOptionValue(String),
    /// An ISA beyond what c17 models: warned about by name and ignored.
    BeyondCeiling(String),
    /// A baseline ISA turned off, or a code-generation switch: c17 cannot do
    /// either, and says so.
    Unsupported(String),
    /// aarch64: a string gcc's aarch64 back end refuses, an error.
    NotValid(String),
}

impl TargetIssue {
    /// Whether gcc refuses the program for it.
    pub fn is_error(&self) -> bool {
        !matches!(
            self,
            Self::Empty | Self::BeyondCeiling(_) | Self::Unsupported(_)
        )
    }
}

/// What one x86-64 `target` item asks for.
enum X86Item {
    /// Nothing c17 acts on: `tune=`, `fpmath=sse`, a baseline ISA, a
    /// disabled ISA c17 never generates.
    Nothing,
    Arch(IsaSet),
    Edit(crate::target::IsaEdit),
}

/// Read one comma-separated item of an x86-64 `target` string.
fn x86_item(item: &str) -> Result<X86Item, TargetIssue> {
    if let Some(cpu) = item.strip_prefix("arch=") {
        return if X86_CPUS.contains(&cpu) {
            Ok(X86Item::Arch(IsaSet::of_arch(cpu)))
        } else {
            Err(TargetIssue::BadCpu {
                option: "arch=",
                value: cpu.to_string(),
            })
        };
    }
    if let Some(cpu) = item.strip_prefix("tune=") {
        return if X86_CPUS.contains(&cpu) || matches!(cpu, "generic" | "intel") {
            Ok(X86Item::Nothing)
        } else {
            Err(TargetIssue::BadCpu {
                option: "tune=",
                value: cpu.to_string(),
            })
        };
    }
    if let Some(how) = item.strip_prefix("fpmath=") {
        return match how {
            "sse" => Ok(X86Item::Nothing),
            "387" | "sse+387" | "387+sse" | "both" => {
                Err(TargetIssue::Unsupported(item.to_string()))
            }
            _ => Err(TargetIssue::BadOptionValue(item.to_string())),
        };
    }
    if let Some(width) = item.strip_prefix("prefer-vector-width=") {
        return match width {
            "none" | "128" | "256" | "512" => Ok(X86Item::Nothing),
            _ => Err(TargetIssue::BadOptionValue(item.to_string())),
        };
    }
    if let Some(edit) = isa_edit(&format!("-m{item}")) {
        return Ok(X86Item::Edit(edit));
    }
    let (negated, name) = match item.strip_prefix("no-") {
        Some(name) => (true, name),
        None => (false, item),
    };
    if X86_BASELINE.contains(&name) {
        return if negated {
            Err(TargetIssue::Unsupported(item.to_string()))
        } else {
            Ok(X86Item::Nothing)
        };
    }
    if X86_UNMODELLED_ISAS.contains(&name) {
        // Turning off what c17 never generates asks for what is already so.
        return if negated {
            Ok(X86Item::Nothing)
        } else {
            Err(TargetIssue::BeyondCeiling(item.to_string()))
        };
    }
    if X86_SWITCHES.contains(&name) {
        return Err(TargetIssue::Unsupported(item.to_string()));
    }
    Err(TargetIssue::Unknown(item.to_string()))
}

/// Whether gcc 13's aarch64 back end accepts `item` of a `target` string:
/// an architecture, CPU or tuning, `+extension`s, and its own switches.
/// None of them changes c17's code.
fn aarch64_item_valid(item: &str) -> bool {
    const SWITCHES: &[&str] = &[
        "general-regs-only",
        "fix-cortex-a53-835769",
        "fix-cortex-a53-843419",
        "strict-align",
        "omit-leaf-frame-pointer",
        "outline-atomics",
    ];
    let valued = [
        "arch=",
        "cpu=",
        "tune=",
        "branch-protection=",
        "sign-return-address=",
    ]
    .iter()
    .any(|p| item.strip_prefix(p).is_some_and(|v| !v.is_empty()));
    valued
        || (item.len() > 1 && item.starts_with('+'))
        || SWITCHES.contains(&item.strip_prefix("no-").unwrap_or(item))
}

/// Read the string of `__attribute__((target("...")))` for a target of
/// architecture `arch`: what it asks of the function's ISA, and anything in
/// it to diagnose.
pub fn parse_target(text: &str, arch: Arch) -> (IsaRequest, Vec<TargetIssue>) {
    let mut request = IsaRequest::default();
    let mut issues = Vec::new();
    if text.is_empty() {
        issues.push(TargetIssue::Empty);
        return (request, issues);
    }
    for item in text.split(',') {
        match arch {
            Arch::X86_64 => match x86_item(item) {
                Ok(X86Item::Nothing) => {}
                Ok(X86Item::Arch(set)) => {
                    request.arch = Some(request.arch.map_or(set, |a| a.with(set)))
                }
                Ok(X86Item::Edit(edit)) => request.edits.push(edit),
                Err(issue) => issues.push(issue),
            },
            Arch::Aarch64 => {
                if !aarch64_item_valid(item) {
                    issues.push(TargetIssue::NotValid(item.to_string()));
                }
            }
        }
    }
    (request, issues)
}

/// One non-default version a `target_clones` function is compiled as.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloneVersion {
    /// What gcc appends to the function's name: the item with every
    /// character that is not a letter or digit made `_` (`sse4_2`).
    pub suffix: String,
    /// The ISA the version is compiled for.
    pub request: IsaRequest,
    /// The `__builtin_cpu_supports` name the resolver tests for it.
    pub feature: &'static str,
    /// gcc's dispatch priority: the resolver tests the highest first.
    priority: u8,
}

/// The versions of a `target_clones` function besides `default`, in the
/// order its resolver tests them: best first.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TargetClones {
    pub versions: Vec<CloneVersion>,
}

/// Something in a `target_clones` list that c17 refuses or ignores.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClonesIssue {
    /// One item: there is nothing to dispatch between, and gcc warns.
    Single,
    /// No `default`: an error.
    NoDefault,
    /// More than one `default`: an error.
    MultipleDefault,
    /// A name gcc does not know: an error.
    Unknown(String),
    /// A `no-` item, which gcc refuses in a clone: an error.
    Negated(String),
    /// A version c17 does not build -- an ISA beyond its ceiling, or a CPU
    /// it cannot dispatch on -- dropped with a warning.
    Dropped(String),
    /// aarch64: an item gcc's aarch64 back end refuses, an error.
    NotValid(String),
    /// aarch64: gcc 13 has no function version dispatcher there, an error.
    NoDispatcher,
}

impl ClonesIssue {
    /// Whether gcc refuses the program for it.
    pub fn is_error(&self) -> bool {
        !matches!(self, Self::Single | Self::Dropped(_))
    }
}

/// The versions c17 builds, with gcc 13's priority for each (its
/// `feature_priority`): what a resolver tests and in which order, which
/// `gcc -O2 -S` of each pair confirms.
const CLONE_FEATURES: &[(&str, u8)] = &[
    ("mmx", 1),
    ("sse", 2),
    ("sse2", 3),
    ("sse3", 5),
    ("ssse3", 6),
    ("sse4.1", 10),
    ("sse4.2", 11),
    ("popcnt", 13),
];

/// gcc's suffix for the version `item`.
fn clone_suffix(item: &str) -> String {
    item.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Read one non-default x86-64 `target_clones` item.
fn x86_clone(item: &str) -> Result<CloneVersion, ClonesIssue> {
    let version = |feature: &'static str, priority: u8| {
        let (request, _) = parse_target(item, Arch::X86_64);
        CloneVersion {
            suffix: clone_suffix(item),
            request,
            feature,
            priority,
        }
    };
    if let Some(&(feature, priority)) = CLONE_FEATURES.iter().find(|(f, _)| *f == item) {
        return Ok(version(feature, priority));
    }
    if item == "arch=x86-64-v2" {
        return Ok(version("x86-64-v2", 14));
    }
    if item.starts_with("no-") {
        return Err(ClonesIssue::Negated(item.to_string()));
    }
    let known_cpu = item
        .strip_prefix("arch=")
        .is_some_and(|cpu| X86_CPUS.contains(&cpu));
    if known_cpu || item == "sse4" || X86_UNMODELLED_ISAS.contains(&item) {
        return Err(ClonesIssue::Dropped(item.to_string()));
    }
    Err(ClonesIssue::Unknown(item.to_string()))
}

/// Read the items of `__attribute__((target_clones(...)))` -- every string
/// argument, split at its commas -- for a target of architecture `arch`.
///
/// `None` when there is nothing to dispatch: the function is compiled once,
/// as its `default` version, under its own name. gcc's checks come in its
/// order: the count, the `default`s, then each item.
pub fn parse_target_clones(
    items: &[String],
    arch: Arch,
) -> (Option<TargetClones>, Vec<ClonesIssue>) {
    if items.len() == 1 {
        return (None, vec![ClonesIssue::Single]);
    }
    let defaults = items.iter().filter(|i| *i == "default").count();
    match defaults {
        0 => return (None, vec![ClonesIssue::NoDefault]),
        1 => {}
        _ => return (None, vec![ClonesIssue::MultipleDefault]),
    }
    let mut issues = Vec::new();
    let others = items.iter().filter(|i| *i != "default");
    if arch == Arch::Aarch64 {
        if let Some(bad) = others.clone().find(|i| !aarch64_item_valid(i)) {
            issues.push(ClonesIssue::NotValid(bad.clone()));
        } else {
            issues.push(ClonesIssue::NoDispatcher);
        }
        return (None, issues);
    }
    let mut clones = TargetClones::default();
    for item in others {
        match x86_clone(item) {
            Ok(v) if clones.versions.iter().any(|c| c.suffix == v.suffix) => {}
            Ok(v) => clones.versions.push(v),
            Err(issue) => issues.push(issue),
        }
    }
    if issues.iter().any(ClonesIssue::is_error) || clones.versions.is_empty() {
        return (None, issues);
    }
    // Stable: equal priorities keep the order they were written in.
    clones
        .versions
        .sort_by_key(|v| std::cmp::Reverse(v.priority));
    (Some(clones), issues)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{X86Isa, X86Simd};

    fn isa(text: &str) -> X86Isa {
        let (request, issues) = parse_target(text, Arch::X86_64);
        assert!(issues.is_empty(), "{text}: {issues:?}");
        X86Isa::default().with_request(&request)
    }

    #[test]
    fn target_strings_edit_the_unit_isa() {
        assert_eq!(isa("sse4.1").simd, X86Simd::Sse41);
        assert!(!isa("sse4.1").popcnt);
        assert_eq!(isa("sse4.2"), isa("arch=x86-64-v2"));
        assert!(isa("sse4.2").popcnt);
        assert!(!isa("sse4.2,no-popcnt").popcnt);
        assert_eq!(isa("sse4.1,popcnt").simd, X86Simd::Sse41);
        assert!(isa("sse4.1,popcnt").popcnt);
        assert_eq!(isa("tune=generic,fpmath=sse"), X86Isa::default());
        // A `no-` lowers a unit compiled higher.
        let unit = X86Isa {
            simd: X86Simd::Sse42,
            popcnt: true,
        };
        let (no42, _) = parse_target("no-sse4.2", Arch::X86_64);
        assert_eq!(unit.with_request(&no42).simd, X86Simd::Sse41);
        let (no3, _) = parse_target("no-sse3", Arch::X86_64);
        assert_eq!(unit.with_request(&no3).simd, X86Simd::Sse2);
        assert!(unit.with_request(&no3).popcnt);
    }

    #[test]
    fn target_issues_match_gcc() {
        let issues = |t: &str| parse_target(t, Arch::X86_64).1;
        assert_eq!(issues(""), vec![TargetIssue::Empty]);
        assert_eq!(issues("foo"), vec![TargetIssue::Unknown("foo".into())]);
        assert_eq!(
            issues("sse4.1,bogus"),
            vec![TargetIssue::Unknown("bogus".into())]
        );
        assert_eq!(
            issues("arch=foo"),
            vec![TargetIssue::BadCpu {
                option: "arch=",
                value: "foo".into()
            }]
        );
        assert_eq!(
            issues("avx2,sse4.1"),
            vec![TargetIssue::BeyondCeiling("avx2".into())]
        );
        assert!(issues("no-avx2").is_empty());
        assert_eq!(
            issues("no-sse2"),
            vec![TargetIssue::Unsupported("no-sse2".into())]
        );
        assert!(issues("arch=haswell,tune=intel,sse2").is_empty());
        let a64 = |t: &str| parse_target(t, Arch::Aarch64).1;
        assert!(a64("arch=armv8-a+crc").is_empty());
        assert!(a64("+crc,cpu=cortex-a72,tune=cortex-a72").is_empty());
        assert_eq!(a64("sse4.2"), vec![TargetIssue::NotValid("sse4.2".into())]);
    }

    #[test]
    fn target_clones_order_and_names_match_gcc() {
        let items = |list: &[&str]| list.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let (clones, issues) = parse_target_clones(
            &items(&["sse3", "avx2", "sse4.2", "popcnt", "default"]),
            Arch::X86_64,
        );
        assert_eq!(issues, vec![ClonesIssue::Dropped("avx2".into())]);
        let names: Vec<_> = clones
            .unwrap()
            .versions
            .iter()
            .map(|v| v.suffix.clone())
            .collect();
        assert_eq!(names, ["popcnt", "sse4_2", "sse3"]);
        let check = |list: &[&str], want: ClonesIssue| {
            let (clones, issues) = parse_target_clones(&items(list), Arch::X86_64);
            assert!(clones.is_none(), "{list:?}");
            assert_eq!(issues, vec![want], "{list:?}");
        };
        check(&["sse4.2"], ClonesIssue::Single);
        check(&["default"], ClonesIssue::Single);
        check(&["sse4.1", "sse4.2"], ClonesIssue::NoDefault);
        check(
            &["sse4.2", "default", "default"],
            ClonesIssue::MultipleDefault,
        );
        check(&["bogus", "default"], ClonesIssue::Unknown("bogus".into()));
        check(
            &["no-sse4.2", "default"],
            ClonesIssue::Negated("no-sse4.2".into()),
        );
        check(&["avx2", "default"], ClonesIssue::Dropped("avx2".into()));
        let a64 = |list: &[&str]| parse_target_clones(&items(list), Arch::Aarch64).1;
        assert_eq!(
            a64(&["sse4.2", "default"]),
            vec![ClonesIssue::NotValid("sse4.2".into())]
        );
        assert_eq!(a64(&["+crc", "default"]), vec![ClonesIssue::NoDispatcher]);
        assert_eq!(a64(&["default"]), vec![ClonesIssue::Single]);
    }

    #[test]
    fn target_clones_version_isa() {
        let (clones, _) = parse_target_clones(
            &["arch=x86-64-v2".to_string(), "default".to_string()],
            Arch::X86_64,
        );
        let v = &clones.unwrap().versions[0];
        assert_eq!(v.suffix, "arch_x86_64_v2");
        assert_eq!(v.feature, "x86-64-v2");
        assert_eq!(
            X86Isa::default().with_request(&v.request).simd,
            X86Simd::Sse42
        );
    }
}
