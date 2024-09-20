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
// - Proper -v specification support.  is it even necessary?
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use libc::{pathconf, sysconf};
use plib::PROJECT_NAME;
use std::collections::HashMap;
use std::ffi::CString;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Variable to get the value of
    var: String,

    /// Pathname for path configuration variables
    pathname: Option<String>,

    /// Specification for the variable (optional)
    #[arg(short = 'v', long)]
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
    let value = match get_mapping("_SC_", var, sysconf_mappings) {
        Some(value) => value,
        None => {
            let errstr = format!(
                "{}: {}",
                gettext("Error: Unknown system configuration variable"),
                var
            );
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                errstr,
            )));
        }
    };

    // Get the value using sysconf
    let value = unsafe { sysconf(value) };
    if value == -1 {
        eprintln!(
            "{}: {}",
            gettext("Error: Unknown system configuration variable"),
            var
        );
        std::process::exit(1);
    } else {
        println!("{}", value);
    }

    Ok(())
}

#[cfg(target_os = "linux")]
fn handle_confstr(
    _var: &str,
    _confstr_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    Err(Box::new(std::io::Error::new(
        std::io::ErrorKind::Other,
        "Not implemented (pls update libc crate)",
    )))
}

#[cfg(not(target_os = "linux"))]
fn handle_confstr(
    var: &str,
    confstr_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    let value = match get_mapping("_CS_", var, confstr_mappings) {
        Some(value) => value,
        None => {
            let errstr = format!(
                "{}: {}",
                gettext("Error: Unknown configuration string variable"),
                var
            );
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                errstr,
            )));
        }
    };

    // Get the value using confstr
    let buflen = unsafe { libc::confstr(value, std::ptr::null_mut(), 0) };
    if buflen == 0 {
        eprintln!(
            "{}: {}",
            gettext("Error: Unknown configuration string variable"),
            var
        );
        std::process::exit(1);
    }

    let mut buf = vec![0u8; buflen as usize];
    let value = unsafe {
        libc::confstr(
            value,
            buf.as_mut_ptr() as *mut libc::c_char,
            buf.len() as libc::size_t,
        )
    };
    if value == 0 {
        eprintln!(
            "{}: {}",
            gettext("Error: Unknown configuration string variable"),
            var
        );
        std::process::exit(1);
    } else {
        let s = std::str::from_utf8(&buf).unwrap();
        println!("{}", s);
    }

    Ok(())
}

fn handle_pathconf(
    var: &str,
    pathname: &str,
    pathconf_mappings: &HashMap<&'static str, libc::c_int>,
) -> Result<(), Box<dyn std::error::Error>> {
    let value = match get_mapping("_PC_", var, pathconf_mappings) {
        Some(value) => value,
        None => {
            let errstr = format!(
                "{}: {}",
                gettext("Error: Unknown path configuration variable"),
                var
            );
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                errstr,
            )));
        }
    };

    // Get the value using pathconf
    let c_path = CString::new(pathname)?;
    let value = unsafe { pathconf(c_path.as_ptr(), value) };
    if value == -1 {
        let errstr = format!(
            "{}: {}",
            gettext("Error: Unknown path configuration variable"),
            var
        );
        return Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::Other,
            errstr,
        )));
    } else {
        println!("{}", value);
    }

    Ok(())
}

fn load_confstr_mapping() -> HashMap<&'static str, libc::c_int> {
    #[cfg(target_os = "macos")]
    {
        HashMap::from([
            ("_CS_DARWIN_USER_DIR", libc::_CS_DARWIN_USER_DIR),
            ("_CS_DARWIN_USER_TEMP_DIR", libc::_CS_DARWIN_USER_TEMP_DIR),
            ("_CS_DARWIN_USER_CACHE_DIR", libc::_CS_DARWIN_USER_CACHE_DIR),
            ("_CS_PATH", libc::_CS_PATH),
        ])
    }

    // upstream libc crate needs Linux confstr definitions
    #[cfg(target_os = "linux")]
    {
        HashMap::new()
    }

    //    #[cfg(target_os = "linux")]
    //    {
    //        HashMap::from([
    //            ("_CS_GNU_LIBC_VERSION", libc::_CS_GNU_LIBC_VERSION),
    //            (
    //                "_CS_GNU_LIBPTHREAD_VERSION",
    //                libc::_CS_GNU_LIBPTHREAD_VERSION,
    //            ),
    //            ("_CS_PATH", libc::_CS_PATH),
    //        ])
    //    }
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
    // Parse command line arguments
    let args = Args::parse();

    // Set locale and text domain for localization
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
