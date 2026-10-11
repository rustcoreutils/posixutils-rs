//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// aarch64's `-mbranch-protection=`: how gcc 13 reads its value, the macros
// each value defines, its errors, and the targets that have no such option.
// Every expectation is aarch64-linux-gnu-gcc 13.3's.
//

use super::{s, WorkDir};
use crate::common::{preprocess_text, run_c17};

const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// The values of `__ARM_FEATURE_BTI_DEFAULT` and `__ARM_FEATURE_PAC_DEFAULT`
/// that `flags` define, "-" for an undefined one.
fn feature_macros(flags: &[&str]) -> (String, String) {
    let mut argv = vec![AARCH64, "-dM"];
    argv.extend_from_slice(flags);
    let r = preprocess_text("bp_macros", "", &argv);
    assert!(r.success, "{flags:?}: {}", r.stderr);
    let value = |name: &str| {
        r.stdout
            .lines()
            .filter_map(|l| l.strip_prefix("#define "))
            .find_map(|l| l.strip_prefix(name)?.strip_prefix(' '))
            .unwrap_or("-")
            .to_string()
    };
    (
        value("__ARM_FEATURE_BTI_DEFAULT"),
        value("__ARM_FEATURE_PAC_DEFAULT"),
    )
}

/// What each value defines, read as gcc reads it: `+`-separated words with
/// empty ones skipped, `pac-ret` taking `leaf` and `b-key` after it, `none`
/// resetting everything, and the last option winning. The PAC value has bit
/// 0 for the A key, bit 1 for the B key and bit 2 for `leaf`.
#[test]
fn branch_protection_defines_gccs_macros() {
    for (value, bti, pac) in [
        ("none", "-", "-"),
        ("standard", "1", "1"),
        ("bti", "1", "-"),
        ("pac-ret", "-", "1"),
        ("pac-ret+leaf", "-", "5"),
        ("pac-ret+b-key", "-", "2"),
        ("pac-ret+leaf+b-key", "-", "6"),
        ("pac-ret+b-key+leaf", "-", "6"),
        ("bti+pac-ret+leaf", "1", "5"),
        ("pac-ret+bti", "1", "1"),
        ("bti+none", "-", "-"),
        ("pac-ret+standard", "1", "1"),
        ("bti+bti", "1", "-"),
        ("+bti", "1", "-"),
        ("bti+", "1", "-"),
        ("pac-ret++leaf", "-", "5"),
    ] {
        let flag = format!("-mbranch-protection={value}");
        assert_eq!(
            feature_macros(&[&flag]),
            (bti.to_string(), pac.to_string()),
            "{flag}"
        );
    }
    assert_eq!(feature_macros(&[]), ("-".into(), "-".into()));
    assert_eq!(
        feature_macros(&["-mbranch-protection=bti", "-mbranch-protection=pac-ret"]),
        ("-".into(), "1".into())
    );
}

/// `__ARM_FEATURE_BTI` says the architecture has BTI, whatever
/// `-mbranch-protection` asks for: gcc 13 defines it for armv8.5-a and
/// later, armv9-a and later, and the CPUs that implement one of them. The
/// last `-march=` decides, then the last `-mcpu=`; a `+feature` suffix
/// changes nothing about it.
#[test]
fn branch_protection_bti_feature_follows_the_architecture() {
    let bti = |flags: &[&str]| {
        let mut argv = vec![AARCH64, "-dM"];
        argv.extend_from_slice(flags);
        let r = preprocess_text("bti_feature", "", &argv);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        r.stdout.contains("#define __ARM_FEATURE_BTI 1\n")
    };
    for (flags, want) in [
        (&[][..], false),
        (&["-march=armv8-a"][..], false),
        (&["-march=armv8.4-a"][..], false),
        (&["-march=armv8.5-a"][..], true),
        (&["-march=armv8.8-a+crc"][..], true),
        (&["-march=armv8-r"][..], false),
        (&["-march=armv9-a"][..], true),
        (&["-march=armv9.3-a"][..], true),
        (&["-march=armv8.5-a", "-march=armv8-a"][..], false),
        (&["-mbranch-protection=bti"][..], false),
        (&["-march=armv8.5-a", "-mbranch-protection=none"][..], true),
        (&["-mcpu=cortex-a76"][..], false),
        (&["-mcpu=neoverse-n2"][..], true),
        (&["-mcpu=ampere1+nocrc"][..], true),
        (&["-mcpu=neoverse-n2", "-march=armv8-a"][..], false),
        (&["-mcpu=cortex-a53", "-march=armv8.5-a"][..], true),
    ] {
        assert_eq!(bti(flags), want, "{flags:?}");
    }
}

/// Compile an empty unit with `flags`, returning whether it succeeded and
/// its diagnostics.
fn compile(flags: &[&str]) -> (bool, String) {
    let w = WorkDir::new("bp");
    let src = w.write("t.c", "int x;\n");
    let out = w.join("t.s");
    let mut argv = flags.to_vec();
    let (src, out) = (s(&src), s(&out));
    argv.extend_from_slice(&["-S", &src, "-o", &out]);
    let r = run_c17(&argv);
    (r.success, r.stderr)
}

/// gcc's errors: a word it does not know where it stands, a word after
/// `none` or `standard`, nothing but separators, and nothing at all.
#[test]
fn branch_protection_errors() {
    for (value, message) in [
        ("foo", "invalid argument 'foo' for '-mbranch-protection='"),
        ("BTI", "invalid argument 'BTI' for '-mbranch-protection='"),
        (
            "pac-ret+foo",
            "invalid argument 'foo' for '-mbranch-protection='",
        ),
        ("leaf", "invalid argument 'leaf' for '-mbranch-protection='"),
        (
            "pac-ret+bti+leaf",
            "invalid argument 'leaf' for '-mbranch-protection='",
        ),
        ("standard+leaf", "unexpected 'leaf' after 'standard'"),
        ("none+bti", "unexpected 'bti' after 'none'"),
        ("+", "missing argument for '-mbranch-protection='"),
        ("", "missing argument to '-mbranch-protection='"),
    ] {
        let flag = format!("-mbranch-protection={value}");
        let (ok, stderr) = compile(&[AARCH64, &flag]);
        assert!(!ok, "{flag} accepted");
        assert!(stderr.contains(message), "{flag}: {stderr}");
    }
}

/// gcc 13 no longer supports `-msign-return-address=`: it warns and ignores
/// it, whatever the value.
#[test]
fn branch_protection_sign_return_address_is_no_longer_supported() {
    for value in ["all", "non-leaf", "none", "foo"] {
        let flag = format!("-msign-return-address={value}");
        let (ok, stderr) = compile(&[AARCH64, &flag]);
        assert!(ok, "{flag}: {stderr}");
        assert!(
            stderr.contains(&format!("switch '{flag}' is no longer supported")),
            "{flag}: {stderr}"
        );
    }
    assert_eq!(
        feature_macros(&["-msign-return-address=all"]),
        ("-".into(), "-".into())
    );
}

/// Neither option exists for x86-64, as in gcc. Darwin has no GNU property
/// note to say what was built, so a protection is refused there; `none`
/// asks for nothing and is accepted.
#[test]
fn branch_protection_other_targets() {
    for flag in ["-mbranch-protection=standard", "-msign-return-address=all"] {
        let (ok, stderr) = compile(&["--target=x86_64-unknown-linux-gnu", flag]);
        assert!(!ok, "{flag} accepted for x86-64");
        assert!(
            stderr.contains(&format!("unrecognized command-line option '{flag}'")),
            "{flag}: {stderr}"
        );
    }
    let darwin = "--target=aarch64-apple-darwin";
    let (ok, stderr) = compile(&[darwin, "-mbranch-protection=standard"]);
    assert!(!ok, "accepted for Darwin");
    assert!(
        stderr.contains("'-mbranch-protection=standard' is not supported for this target"),
        "{stderr}"
    );
    let (ok, stderr) = compile(&[darwin, "-mbranch-protection=none"]);
    assert!(ok, "{stderr}");
}
