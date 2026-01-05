//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - How to obtain a complete list of sysconf and pathconf variables,
//   POSIX spec, OS headers, or another source?
//

use std::collections::HashMap;
use std::ffi::CString;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use libc::{pathconf, sysconf};

#[derive(Parser)]
#[command(version, about = gettext("getconf - get configuration values"))]
struct Args {
    #[arg(help = gettext("Variable to get the value of"))]
    var: String,

    #[arg(help = gettext("Pathname for path configuration variables"))]
    pathname: Option<String>,

    #[arg(
        short = 'v',
        long,
        help = gettext("Specification for the variable (optional)")
    )]
    specification: Option<String>,
}

fn get_mapping(
    prefix: &str,
    var: &str,
    mapping: &HashMap<&'static str, libc::c_int>,
) -> Option<libc::c_int> {
    let mut key = var.to_string();
    if !mapping.contains_key(&key.as_str()) {
        key = format!("{}{}", prefix, var);
        if !mapping.contains_key(&key.as_str()) {
            return None;
        }
    }

    Some(*mapping.get(&key.as_str()).unwrap())
}

fn handle_sysconf(
    var: &str,
    sysconf_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    let sc_value = match get_mapping("_SC_", var, sysconf_mappings) {
        Some(value) => value,
        None => {
            eprintln!("getconf: {}: {}", gettext("unrecognized variable"), var);
            std::process::exit(1);
        }
    };

    // Clear errno before sysconf call
    errno::set_errno(errno::Errno(0));

    // Get the value using sysconf
    let value = unsafe { sysconf(sc_value) };

    if value == -1 {
        // Check errno to distinguish "undefined" from error
        let errno_val = errno::errno().0;
        if errno_val == 0 {
            // Variable is valid but undefined on this system
            println!("undefined");
        } else {
            // Actual error occurred
            let e = std::io::Error::from_raw_os_error(errno_val);
            eprintln!("getconf: {}: {}", var, e);
            std::process::exit(1);
        }
    } else {
        println!("{}", value);
    }

    Ok(())
}

fn handle_confstr(
    var: &str,
    confstr_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    let cs_value = match get_mapping("_CS_", var, confstr_mappings) {
        Some(value) => value,
        None => {
            eprintln!("getconf: {}: {}", gettext("unrecognized variable"), var);
            std::process::exit(1);
        }
    };

    // Clear errno before confstr call
    errno::set_errno(errno::Errno(0));

    // Get the required buffer length
    let buflen = unsafe { libc::confstr(cs_value, std::ptr::null_mut(), 0) };
    if buflen == 0 {
        // Check errno to distinguish undefined from error
        let errno_val = errno::errno().0;
        if errno_val == 0 || errno_val == libc::EINVAL {
            // Variable is valid but undefined on this system
            println!("undefined");
        } else {
            let e = std::io::Error::from_raw_os_error(errno_val);
            eprintln!("getconf: {}: {}", var, e);
            std::process::exit(1);
        }
        return Ok(());
    }

    let mut buf = vec![0u8; buflen as usize];
    let result = unsafe {
        libc::confstr(
            cs_value,
            buf.as_mut_ptr() as *mut libc::c_char,
            buf.len() as libc::size_t,
        )
    };
    if result == 0 {
        let errno_val = errno::errno().0;
        let e = std::io::Error::from_raw_os_error(errno_val);
        eprintln!("getconf: {}: {}", var, e);
        std::process::exit(1);
    } else {
        // Trim the null terminator and any trailing whitespace
        let s = std::str::from_utf8(&buf)
            .unwrap()
            .trim_end_matches('\0')
            .trim_end();
        println!("{}", s);
    }

    Ok(())
}

fn handle_pathconf(
    var: &str,
    pathname: &str,
    pathconf_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    let pc_value = match get_mapping("_PC_", var, pathconf_mappings) {
        Some(value) => value,
        None => {
            eprintln!("getconf: {}: {}", gettext("unrecognized variable"), var);
            std::process::exit(1);
        }
    };

    // Clear errno before pathconf call
    errno::set_errno(errno::Errno(0));

    // Get the value using pathconf
    let c_path = CString::new(pathname)?;
    let value = unsafe { pathconf(c_path.as_ptr(), pc_value) };

    if value == -1 {
        // Check errno to distinguish "undefined" from error
        let errno_val = errno::errno().0;
        if errno_val == 0 {
            // Variable is valid but undefined for this path
            println!("undefined");
        } else {
            // Actual error occurred
            let e = std::io::Error::from_raw_os_error(errno_val);
            eprintln!("getconf: {}: {}", pathname, e);
            std::process::exit(1);
        }
    } else {
        println!("{}", value);
    }

    Ok(())
}

fn load_confstr_mapping() -> HashMap<&'static str, libc::c_int> {
    #[cfg(target_os = "macos")]
    {
        // POSIX_V6_* constants are defined in macOS headers but not in libc crate
        const _CS_POSIX_V6_ILP32_OFF32_CFLAGS: libc::c_int = 2;
        const _CS_POSIX_V6_ILP32_OFF32_LDFLAGS: libc::c_int = 3;
        const _CS_POSIX_V6_ILP32_OFF32_LIBS: libc::c_int = 4;
        const _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS: libc::c_int = 5;
        const _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS: libc::c_int = 6;
        const _CS_POSIX_V6_ILP32_OFFBIG_LIBS: libc::c_int = 7;
        const _CS_POSIX_V6_LP64_OFF64_CFLAGS: libc::c_int = 8;
        const _CS_POSIX_V6_LP64_OFF64_LDFLAGS: libc::c_int = 9;
        const _CS_POSIX_V6_LP64_OFF64_LIBS: libc::c_int = 10;
        const _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS: libc::c_int = 11;
        const _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS: libc::c_int = 12;
        const _CS_POSIX_V6_LPBIG_OFFBIG_LIBS: libc::c_int = 13;
        const _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS: libc::c_int = 14;

        HashMap::from([
            ("_CS_DARWIN_USER_DIR", libc::_CS_DARWIN_USER_DIR),
            ("_CS_DARWIN_USER_TEMP_DIR", libc::_CS_DARWIN_USER_TEMP_DIR),
            ("_CS_DARWIN_USER_CACHE_DIR", libc::_CS_DARWIN_USER_CACHE_DIR),
            ("_CS_PATH", libc::_CS_PATH),
            // POSIX_V6 compilation environment strings
            (
                "_CS_POSIX_V6_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFF32_LIBS",
                _CS_POSIX_V6_ILP32_OFF32_LIBS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_LIBS",
                _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
            ),
            (
                "_CS_POSIX_V6_LP64_OFF64_CFLAGS",
                _CS_POSIX_V6_LP64_OFF64_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
            ),
            ("_CS_POSIX_V6_LP64_OFF64_LIBS", _CS_POSIX_V6_LP64_OFF64_LIBS),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_LIBS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
            ),
            (
                "_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS,
            ),
            // Aliases without _CS_ prefix (as POSIX requires)
            ("PATH", libc::_CS_PATH),
            ("DARWIN_USER_DIR", libc::_CS_DARWIN_USER_DIR),
            ("DARWIN_USER_TEMP_DIR", libc::_CS_DARWIN_USER_TEMP_DIR),
            ("DARWIN_USER_CACHE_DIR", libc::_CS_DARWIN_USER_CACHE_DIR),
            (
                "POSIX_V6_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
            ),
            ("POSIX_V6_ILP32_OFF32_LIBS", _CS_POSIX_V6_ILP32_OFF32_LIBS),
            (
                "POSIX_V6_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
            ),
            ("POSIX_V6_ILP32_OFFBIG_LIBS", _CS_POSIX_V6_ILP32_OFFBIG_LIBS),
            ("POSIX_V6_LP64_OFF64_CFLAGS", _CS_POSIX_V6_LP64_OFF64_CFLAGS),
            (
                "POSIX_V6_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
            ),
            ("POSIX_V6_LP64_OFF64_LIBS", _CS_POSIX_V6_LP64_OFF64_LIBS),
            (
                "POSIX_V6_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V6_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
            ),
            ("POSIX_V6_LPBIG_OFFBIG_LIBS", _CS_POSIX_V6_LPBIG_OFFBIG_LIBS),
            (
                "POSIX_V6_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS,
            ),
        ])
    }

    // Linux confstr definitions - libc crate exposes confstr() but not all constants
    #[cfg(target_os = "linux")]
    {
        // Linux confstr constants from /usr/include/bits/confname.h
        const _CS_PATH: libc::c_int = 0;
        const _CS_V6_WIDTH_RESTRICTED_ENVS: libc::c_int = 1;
        const _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS: libc::c_int = 1;
        const _CS_GNU_LIBC_VERSION: libc::c_int = 2;
        const _CS_GNU_LIBPTHREAD_VERSION: libc::c_int = 3;
        const _CS_V5_WIDTH_RESTRICTED_ENVS: libc::c_int = 4;
        const _CS_V7_WIDTH_RESTRICTED_ENVS: libc::c_int = 5;
        const _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS: libc::c_int = 5;
        // POSIX_V6 environment strings
        const _CS_POSIX_V6_ILP32_OFF32_CFLAGS: libc::c_int = 1116;
        const _CS_POSIX_V6_ILP32_OFF32_LDFLAGS: libc::c_int = 1117;
        const _CS_POSIX_V6_ILP32_OFF32_LIBS: libc::c_int = 1118;
        const _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS: libc::c_int = 1120;
        const _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS: libc::c_int = 1121;
        const _CS_POSIX_V6_ILP32_OFFBIG_LIBS: libc::c_int = 1122;
        const _CS_POSIX_V6_LP64_OFF64_CFLAGS: libc::c_int = 1124;
        const _CS_POSIX_V6_LP64_OFF64_LDFLAGS: libc::c_int = 1125;
        const _CS_POSIX_V6_LP64_OFF64_LIBS: libc::c_int = 1126;
        const _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS: libc::c_int = 1128;
        const _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS: libc::c_int = 1129;
        const _CS_POSIX_V6_LPBIG_OFFBIG_LIBS: libc::c_int = 1130;
        // POSIX_V7 environment strings
        const _CS_POSIX_V7_ILP32_OFF32_CFLAGS: libc::c_int = 1132;
        const _CS_POSIX_V7_ILP32_OFF32_LDFLAGS: libc::c_int = 1133;
        const _CS_POSIX_V7_ILP32_OFF32_LIBS: libc::c_int = 1134;
        const _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS: libc::c_int = 1136;
        const _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS: libc::c_int = 1137;
        const _CS_POSIX_V7_ILP32_OFFBIG_LIBS: libc::c_int = 1138;
        const _CS_POSIX_V7_LP64_OFF64_CFLAGS: libc::c_int = 1140;
        const _CS_POSIX_V7_LP64_OFF64_LDFLAGS: libc::c_int = 1141;
        const _CS_POSIX_V7_LP64_OFF64_LIBS: libc::c_int = 1142;
        const _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS: libc::c_int = 1144;
        const _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS: libc::c_int = 1145;
        const _CS_POSIX_V7_LPBIG_OFFBIG_LIBS: libc::c_int = 1146;

        HashMap::from([
            ("_CS_PATH", _CS_PATH),
            ("PATH", _CS_PATH),
            ("_CS_GNU_LIBC_VERSION", _CS_GNU_LIBC_VERSION),
            ("GNU_LIBC_VERSION", _CS_GNU_LIBC_VERSION),
            ("_CS_GNU_LIBPTHREAD_VERSION", _CS_GNU_LIBPTHREAD_VERSION),
            ("GNU_LIBPTHREAD_VERSION", _CS_GNU_LIBPTHREAD_VERSION),
            // V6 width restricted envs
            (
                "_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS,
            ),
            (
                "POSIX_V6_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS,
            ),
            // V7 width restricted envs
            (
                "_CS_POSIX_V7_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS,
            ),
            (
                "POSIX_V7_WIDTH_RESTRICTED_ENVS",
                _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS,
            ),
            // POSIX_V6 compilation environment strings
            (
                "_CS_POSIX_V6_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFF32_LIBS",
                _CS_POSIX_V6_ILP32_OFF32_LIBS,
            ),
            ("POSIX_V6_ILP32_OFF32_LIBS", _CS_POSIX_V6_ILP32_OFF32_LIBS),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
            ),
            (
                "POSIX_V6_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_ILP32_OFFBIG_LIBS",
                _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
            ),
            ("POSIX_V6_ILP32_OFFBIG_LIBS", _CS_POSIX_V6_ILP32_OFFBIG_LIBS),
            (
                "_CS_POSIX_V6_LP64_OFF64_CFLAGS",
                _CS_POSIX_V6_LP64_OFF64_CFLAGS,
            ),
            ("POSIX_V6_LP64_OFF64_CFLAGS", _CS_POSIX_V6_LP64_OFF64_CFLAGS),
            (
                "_CS_POSIX_V6_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
            ),
            (
                "POSIX_V6_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
            ),
            ("_CS_POSIX_V6_LP64_OFF64_LIBS", _CS_POSIX_V6_LP64_OFF64_LIBS),
            ("POSIX_V6_LP64_OFF64_LIBS", _CS_POSIX_V6_LP64_OFF64_LIBS),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V6_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
            ),
            (
                "POSIX_V6_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V6_LPBIG_OFFBIG_LIBS",
                _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
            ),
            ("POSIX_V6_LPBIG_OFFBIG_LIBS", _CS_POSIX_V6_LPBIG_OFFBIG_LIBS),
            // POSIX_V7 compilation environment strings
            (
                "_CS_POSIX_V7_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V7_ILP32_OFF32_CFLAGS,
            ),
            (
                "POSIX_V7_ILP32_OFF32_CFLAGS",
                _CS_POSIX_V7_ILP32_OFF32_CFLAGS,
            ),
            (
                "_CS_POSIX_V7_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,
            ),
            (
                "POSIX_V7_ILP32_OFF32_LDFLAGS",
                _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,
            ),
            (
                "_CS_POSIX_V7_ILP32_OFF32_LIBS",
                _CS_POSIX_V7_ILP32_OFF32_LIBS,
            ),
            ("POSIX_V7_ILP32_OFF32_LIBS", _CS_POSIX_V7_ILP32_OFF32_LIBS),
            (
                "_CS_POSIX_V7_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V7_ILP32_OFFBIG_CFLAGS",
                _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,
            ),
            (
                "POSIX_V7_ILP32_OFFBIG_LDFLAGS",
                _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V7_ILP32_OFFBIG_LIBS",
                _CS_POSIX_V7_ILP32_OFFBIG_LIBS,
            ),
            ("POSIX_V7_ILP32_OFFBIG_LIBS", _CS_POSIX_V7_ILP32_OFFBIG_LIBS),
            (
                "_CS_POSIX_V7_LP64_OFF64_CFLAGS",
                _CS_POSIX_V7_LP64_OFF64_CFLAGS,
            ),
            ("POSIX_V7_LP64_OFF64_CFLAGS", _CS_POSIX_V7_LP64_OFF64_CFLAGS),
            (
                "_CS_POSIX_V7_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V7_LP64_OFF64_LDFLAGS,
            ),
            (
                "POSIX_V7_LP64_OFF64_LDFLAGS",
                _CS_POSIX_V7_LP64_OFF64_LDFLAGS,
            ),
            ("_CS_POSIX_V7_LP64_OFF64_LIBS", _CS_POSIX_V7_LP64_OFF64_LIBS),
            ("POSIX_V7_LP64_OFF64_LIBS", _CS_POSIX_V7_LP64_OFF64_LIBS),
            (
                "_CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "POSIX_V7_LPBIG_OFFBIG_CFLAGS",
                _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,
            ),
            (
                "_CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,
            ),
            (
                "POSIX_V7_LPBIG_OFFBIG_LDFLAGS",
                _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,
            ),
            (
                "_CS_POSIX_V7_LPBIG_OFFBIG_LIBS",
                _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,
            ),
            ("POSIX_V7_LPBIG_OFFBIG_LIBS", _CS_POSIX_V7_LPBIG_OFFBIG_LIBS),
        ])
    }
}

fn is_confstr_var(var: &str, mapping: &HashMap<&'static str, libc::c_int>) -> bool {
    let mut key = var.to_string();
    if !mapping.contains_key(&key.as_str()) {
        key = format!("_CS_{}", var);
        if !mapping.contains_key(&key.as_str()) {
            return false;
        }
    }

    true
}

/// Check if a specification name is a valid POSIX compilation environment.
/// We accept POSIX_V6_* and POSIX_V7_* specifications as no-op (using default environment).
fn is_valid_specification(spec: &str) -> bool {
    matches!(
        spec,
        "POSIX_V7_ILP32_OFF32"
            | "POSIX_V7_ILP32_OFFBIG"
            | "POSIX_V7_LP64_OFF64"
            | "POSIX_V7_LPBIG_OFFBIG"
            | "POSIX_V6_ILP32_OFF32"
            | "POSIX_V6_ILP32_OFFBIG"
            | "POSIX_V6_LP64_OFF64"
            | "POSIX_V6_LPBIG_OFFBIG"
    )
}

fn load_sysconf_mapping() -> HashMap<&'static str, libc::c_int> {
    HashMap::from([
        ("_SC_ARG_MAX", libc::_SC_ARG_MAX),
        ("_SC_CHILD_MAX", libc::_SC_CHILD_MAX),
        ("_SC_CLK_TCK", libc::_SC_CLK_TCK),
        ("_SC_NGROUPS_MAX", libc::_SC_NGROUPS_MAX),
        ("_SC_OPEN_MAX", libc::_SC_OPEN_MAX),
        ("_SC_STREAM_MAX", libc::_SC_STREAM_MAX),
        ("_SC_TZNAME_MAX", libc::_SC_TZNAME_MAX),
        ("_SC_JOB_CONTROL", libc::_SC_JOB_CONTROL),
        ("_SC_SAVED_IDS", libc::_SC_SAVED_IDS),
        ("_SC_VERSION", libc::_SC_VERSION),
        ("_SC_PAGESIZE", libc::_SC_PAGESIZE),
        ("_SC_HOST_NAME_MAX", libc::_SC_HOST_NAME_MAX),
        ("_SC_LOGIN_NAME_MAX", libc::_SC_LOGIN_NAME_MAX),
        ("_SC_SYMLOOP_MAX", libc::_SC_SYMLOOP_MAX),
        ("_SC_TTY_NAME_MAX", libc::_SC_TTY_NAME_MAX),
        ("_SC_IOV_MAX", libc::_SC_IOV_MAX),
        ("_SC_XOPEN_VERSION", libc::_SC_XOPEN_VERSION),
        ("_SC_XOPEN_CRYPT", libc::_SC_XOPEN_CRYPT),
        ("_SC_XOPEN_ENH_I18N", libc::_SC_XOPEN_ENH_I18N),
        ("_SC_XOPEN_SHM", libc::_SC_XOPEN_SHM),
        ("_SC_XOPEN_UNIX", libc::_SC_XOPEN_UNIX),
        ("_SC_PASS_MAX", libc::_SC_PASS_MAX),
        ("_SC_ATEXIT_MAX", libc::_SC_ATEXIT_MAX),
        ("_SC_THREADS", libc::_SC_THREADS),
        ("_SC_THREAD_SAFE_FUNCTIONS", libc::_SC_THREAD_SAFE_FUNCTIONS),
        ("_SC_REGEXP", libc::_SC_REGEXP),
        ("_SC_SHELL", libc::_SC_SHELL),
        ("_SC_SPAWN", libc::_SC_SPAWN),
        ("_SC_IPV6", libc::_SC_IPV6),
        ("_SC_REALTIME_SIGNALS", libc::_SC_REALTIME_SIGNALS),
        ("_SC_PRIORITY_SCHEDULING", libc::_SC_PRIORITY_SCHEDULING),
        ("_SC_TIMERS", libc::_SC_TIMERS),
        ("_SC_ASYNCHRONOUS_IO", libc::_SC_ASYNCHRONOUS_IO),
        ("_SC_PRIORITIZED_IO", libc::_SC_PRIORITIZED_IO),
        ("_SC_SYNCHRONIZED_IO", libc::_SC_SYNCHRONIZED_IO),
        ("_SC_FSYNC", libc::_SC_FSYNC),
        ("_SC_MAPPED_FILES", libc::_SC_MAPPED_FILES),
        ("_SC_MEMLOCK", libc::_SC_MEMLOCK),
        ("_SC_MEMLOCK_RANGE", libc::_SC_MEMLOCK_RANGE),
        ("_SC_MEMORY_PROTECTION", libc::_SC_MEMORY_PROTECTION),
        ("_SC_MESSAGE_PASSING", libc::_SC_MESSAGE_PASSING),
        ("_SC_SEMAPHORES", libc::_SC_SEMAPHORES),
        ("_SC_SHARED_MEMORY_OBJECTS", libc::_SC_SHARED_MEMORY_OBJECTS),
        ("_SC_AIO_LISTIO_MAX", libc::_SC_AIO_LISTIO_MAX),
        ("_SC_AIO_MAX", libc::_SC_AIO_MAX),
        ("_SC_AIO_PRIO_DELTA_MAX", libc::_SC_AIO_PRIO_DELTA_MAX),
        ("_SC_DELAYTIMER_MAX", libc::_SC_DELAYTIMER_MAX),
        ("_SC_MQ_OPEN_MAX", libc::_SC_MQ_OPEN_MAX),
        ("_SC_MQ_PRIO_MAX", libc::_SC_MQ_PRIO_MAX),
        ("_SC_RTSIG_MAX", libc::_SC_RTSIG_MAX),
        ("_SC_SEM_NSEMS_MAX", libc::_SC_SEM_NSEMS_MAX),
        ("_SC_SEM_VALUE_MAX", libc::_SC_SEM_VALUE_MAX),
        ("_SC_SIGQUEUE_MAX", libc::_SC_SIGQUEUE_MAX),
        ("_SC_TIMER_MAX", libc::_SC_TIMER_MAX),
        ("_SC_BC_BASE_MAX", libc::_SC_BC_BASE_MAX),
        ("_SC_BC_DIM_MAX", libc::_SC_BC_DIM_MAX),
        ("_SC_BC_SCALE_MAX", libc::_SC_BC_SCALE_MAX),
        ("_SC_BC_STRING_MAX", libc::_SC_BC_STRING_MAX),
        ("_SC_COLL_WEIGHTS_MAX", libc::_SC_COLL_WEIGHTS_MAX),
        ("_SC_EXPR_NEST_MAX", libc::_SC_EXPR_NEST_MAX),
        ("_SC_LINE_MAX", libc::_SC_LINE_MAX),
        ("_SC_RE_DUP_MAX", libc::_SC_RE_DUP_MAX),
        ("_SC_2_VERSION", libc::_SC_2_VERSION),
        ("_SC_2_C_BIND", libc::_SC_2_C_BIND),
        ("_SC_2_C_DEV", libc::_SC_2_C_DEV),
        ("_SC_2_CHAR_TERM", libc::_SC_2_CHAR_TERM),
        ("_SC_2_FORT_DEV", libc::_SC_2_FORT_DEV),
        ("_SC_2_FORT_RUN", libc::_SC_2_FORT_RUN),
        ("_SC_2_LOCALEDEF", libc::_SC_2_LOCALEDEF),
        ("_SC_2_SW_DEV", libc::_SC_2_SW_DEV),
        ("_SC_2_UPE", libc::_SC_2_UPE),
        ("_SC_2_PBS", libc::_SC_2_PBS),
        ("_SC_2_PBS_ACCOUNTING", libc::_SC_2_PBS_ACCOUNTING),
        ("_SC_2_PBS_CHECKPOINT", libc::_SC_2_PBS_CHECKPOINT),
        ("_SC_2_PBS_LOCATE", libc::_SC_2_PBS_LOCATE),
        ("_SC_2_PBS_MESSAGE", libc::_SC_2_PBS_MESSAGE),
        ("_SC_2_PBS_TRACK", libc::_SC_2_PBS_TRACK),
        ("_SC_ADVISORY_INFO", libc::_SC_ADVISORY_INFO),
        // POSIX.2 compatibility aliases (names without underscore prefix)
        // Per POSIX: "For compatibility with earlier versions, the following
        // variable names shall also be supported ... and shall be equivalent
        // to the same name prefixed with an <underscore>."
        ("POSIX2_VERSION", libc::_SC_2_VERSION),
        ("POSIX2_C_BIND", libc::_SC_2_C_BIND),
        ("POSIX2_C_DEV", libc::_SC_2_C_DEV),
        ("POSIX2_CHAR_TERM", libc::_SC_2_CHAR_TERM),
        ("POSIX2_FORT_DEV", libc::_SC_2_FORT_DEV),
        ("POSIX2_FORT_RUN", libc::_SC_2_FORT_RUN),
        ("POSIX2_LOCALEDEF", libc::_SC_2_LOCALEDEF),
        ("POSIX2_SW_DEV", libc::_SC_2_SW_DEV),
        ("POSIX2_UPE", libc::_SC_2_UPE),
        // POSIX.2 limits compatibility aliases
        ("POSIX2_BC_BASE_MAX", libc::_SC_BC_BASE_MAX),
        ("POSIX2_BC_DIM_MAX", libc::_SC_BC_DIM_MAX),
        ("POSIX2_BC_SCALE_MAX", libc::_SC_BC_SCALE_MAX),
        ("POSIX2_BC_STRING_MAX", libc::_SC_BC_STRING_MAX),
        ("POSIX2_COLL_WEIGHTS_MAX", libc::_SC_COLL_WEIGHTS_MAX),
        ("POSIX2_EXPR_NEST_MAX", libc::_SC_EXPR_NEST_MAX),
        ("POSIX2_LINE_MAX", libc::_SC_LINE_MAX),
        ("POSIX2_RE_DUP_MAX", libc::_SC_RE_DUP_MAX),
        // Common aliases without _SC_ prefix
        ("PAGESIZE", libc::_SC_PAGESIZE),
        ("PAGE_SIZE", libc::_SC_PAGESIZE),
        ("_POSIX_VERSION", libc::_SC_VERSION),
        ("HOST_NAME_MAX", libc::_SC_HOST_NAME_MAX),
        ("LOGIN_NAME_MAX", libc::_SC_LOGIN_NAME_MAX),
        ("SYMLOOP_MAX", libc::_SC_SYMLOOP_MAX),
        ("TTY_NAME_MAX", libc::_SC_TTY_NAME_MAX),
        ("IOV_MAX", libc::_SC_IOV_MAX),
        ("_XOPEN_VERSION", libc::_SC_XOPEN_VERSION),
        ("_XOPEN_CRYPT", libc::_SC_XOPEN_CRYPT),
        ("_XOPEN_ENH_I18N", libc::_SC_XOPEN_ENH_I18N),
        ("_XOPEN_SHM", libc::_SC_XOPEN_SHM),
        ("_XOPEN_UNIX", libc::_SC_XOPEN_UNIX),
        ("PASS_MAX", libc::_SC_PASS_MAX),
        ("ATEXIT_MAX", libc::_SC_ATEXIT_MAX),
        ("_POSIX_THREADS", libc::_SC_THREADS),
        (
            "_POSIX_THREAD_SAFE_FUNCTIONS",
            libc::_SC_THREAD_SAFE_FUNCTIONS,
        ),
        ("_POSIX_REGEXP", libc::_SC_REGEXP),
        ("_POSIX_SHELL", libc::_SC_SHELL),
        ("_POSIX_SPAWN", libc::_SC_SPAWN),
        ("_POSIX_IPV6", libc::_SC_IPV6),
        ("_POSIX_JOB_CONTROL", libc::_SC_JOB_CONTROL),
        ("_POSIX_SAVED_IDS", libc::_SC_SAVED_IDS),
        ("_POSIX_REALTIME_SIGNALS", libc::_SC_REALTIME_SIGNALS),
        ("_POSIX_PRIORITY_SCHEDULING", libc::_SC_PRIORITY_SCHEDULING),
        ("_POSIX_TIMERS", libc::_SC_TIMERS),
        ("_POSIX_ASYNCHRONOUS_IO", libc::_SC_ASYNCHRONOUS_IO),
        ("_POSIX_PRIORITIZED_IO", libc::_SC_PRIORITIZED_IO),
        ("_POSIX_SYNCHRONIZED_IO", libc::_SC_SYNCHRONIZED_IO),
        ("_POSIX_FSYNC", libc::_SC_FSYNC),
        ("_POSIX_MAPPED_FILES", libc::_SC_MAPPED_FILES),
        ("_POSIX_MEMLOCK", libc::_SC_MEMLOCK),
        ("_POSIX_MEMLOCK_RANGE", libc::_SC_MEMLOCK_RANGE),
        ("_POSIX_MEMORY_PROTECTION", libc::_SC_MEMORY_PROTECTION),
        ("_POSIX_MESSAGE_PASSING", libc::_SC_MESSAGE_PASSING),
        ("_POSIX_SEMAPHORES", libc::_SC_SEMAPHORES),
        (
            "_POSIX_SHARED_MEMORY_OBJECTS",
            libc::_SC_SHARED_MEMORY_OBJECTS,
        ),
        ("_POSIX_ADVISORY_INFO", libc::_SC_ADVISORY_INFO),
    ])
}

fn load_pathconf_mapping() -> HashMap<&'static str, libc::c_int> {
    HashMap::from([
        ("LINK_MAX", libc::_PC_LINK_MAX),
        ("MAX_CANON", libc::_PC_MAX_CANON),
        ("MAX_INPUT", libc::_PC_MAX_INPUT),
        ("NAME_MAX", libc::_PC_NAME_MAX),
        ("PATH_MAX", libc::_PC_PATH_MAX),
        ("PIPE_BUF", libc::_PC_PIPE_BUF),
        ("CHOWN_RESTRICTED", libc::_PC_CHOWN_RESTRICTED),
        ("NO_TRUNC", libc::_PC_NO_TRUNC),
        ("VDISABLE", libc::_PC_VDISABLE),
        ("POSIX_ALLOC_SIZE_MIN", libc::_PC_ALLOC_SIZE_MIN),
        ("POSIX_REC_INCR_XFER_SIZE", libc::_PC_REC_INCR_XFER_SIZE),
        ("POSIX_REC_MAX_XFER_SIZE", libc::_PC_REC_MAX_XFER_SIZE),
        ("POSIX_REC_MIN_XFER_SIZE", libc::_PC_REC_MIN_XFER_SIZE),
        ("POSIX_REC_XFER_ALIGN", libc::_PC_REC_XFER_ALIGN),
        ("SYMLINK_MAX", libc::_PC_SYMLINK_MAX),
        ("FILESIZEBITS", libc::_PC_FILESIZEBITS),
        ("_POSIX_CHOWN_RESTRICTED", libc::_PC_CHOWN_RESTRICTED),
        ("_POSIX_NO_TRUNC", libc::_PC_NO_TRUNC),
        ("_POSIX_VDISABLE", libc::_PC_VDISABLE),
        ("_POSIX_ASYNC_IO", libc::_PC_ASYNC_IO),
        ("_POSIX_PRIO_IO", libc::_PC_PRIO_IO),
    ])
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    // Handle -v specification option
    // POSIX requires this for selecting different compilation environments.
    // We accept known specifications as a no-op (using default environment).
    if let Some(ref spec) = args.specification {
        if !is_valid_specification(spec) {
            eprintln!("getconf: {}: {}", gettext("invalid specification"), spec);
            std::process::exit(1);
        }
        // Valid specification - proceed with default environment
    }

    if let Some(pathname) = args.pathname {
        let pathconf_mappings = load_pathconf_mapping();
        handle_pathconf(&args.var, &pathname, &pathconf_mappings)?;
    } else {
        let confstr_mappings = load_confstr_mapping();

        if is_confstr_var(&args.var, &confstr_mappings) {
            handle_confstr(&args.var, &confstr_mappings)?;
        } else {
            let sysconf_mappings = load_sysconf_mapping();
            handle_sysconf(&args.var, &sysconf_mappings)?;
        }
    }

    Ok(())
}
