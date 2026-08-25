//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX system logging via libc `openlog` and `syslog`.
//!
//! Messages are handed to the platform's own syslog implementation rather than
//! written to `/dev/log` directly, so whatever transport, framing and fallback
//! the host libc uses applies unchanged.

use std::ffi::{c_int, CString};
use std::str::FromStr;
use std::sync::OnceLock;

/// A syslog facility, held as the libc `LOG_*` code.
///
/// POSIX (`syslog.h`) names `LOG_USER`, `LOG_LOCAL0`..`LOG_LOCAL7`, `LOG_KERN`,
/// `LOG_MAIL`, `LOG_NEWS`, `LOG_UUCP`, `LOG_DAEMON`, `LOG_AUTH`, `LOG_CRON` and
/// `LOG_LPR`. `authpriv` and `ftp` are common extensions and are accepted too.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Facility(c_int);

impl Facility {
    /// `LOG_KERN` is 0, and libc's `openlog` only records a non-zero facility,
    /// so a message sent under this one is filed as `LOG_USER` instead. That
    /// is a property of `syslog(3)`, not of this wrapper: the kernel facility
    /// is not reachable from user space through it. Accepted so that
    /// `logger -p kern.<level>` is not rejected outright.
    pub const KERN: Facility = Facility(libc::LOG_KERN);
    pub const USER: Facility = Facility(libc::LOG_USER);
    pub const MAIL: Facility = Facility(libc::LOG_MAIL);
    pub const DAEMON: Facility = Facility(libc::LOG_DAEMON);
    pub const AUTH: Facility = Facility(libc::LOG_AUTH);
    pub const SYSLOG: Facility = Facility(libc::LOG_SYSLOG);
    pub const LPR: Facility = Facility(libc::LOG_LPR);
    pub const NEWS: Facility = Facility(libc::LOG_NEWS);
    pub const UUCP: Facility = Facility(libc::LOG_UUCP);
    pub const CRON: Facility = Facility(libc::LOG_CRON);
    pub const AUTHPRIV: Facility = Facility(libc::LOG_AUTHPRIV);
    pub const FTP: Facility = Facility(libc::LOG_FTP);
    pub const LOCAL0: Facility = Facility(libc::LOG_LOCAL0);
    pub const LOCAL1: Facility = Facility(libc::LOG_LOCAL1);
    pub const LOCAL2: Facility = Facility(libc::LOG_LOCAL2);
    pub const LOCAL3: Facility = Facility(libc::LOG_LOCAL3);
    pub const LOCAL4: Facility = Facility(libc::LOG_LOCAL4);
    pub const LOCAL5: Facility = Facility(libc::LOG_LOCAL5);
    pub const LOCAL6: Facility = Facility(libc::LOG_LOCAL6);
    pub const LOCAL7: Facility = Facility(libc::LOG_LOCAL7);
}

impl Default for Facility {
    /// `LOG_USER`, the facility POSIX specifies when none is given.
    fn default() -> Self {
        Facility::USER
    }
}

impl FromStr for Facility {
    type Err = ();

    /// Accepts both the bare name (`user`) and the constant spelling
    /// (`log_user`), case-insensitively.
    fn from_str(s: &str) -> Result<Facility, ()> {
        Ok(match s.to_ascii_lowercase().as_str() {
            "log_kern" | "kern" => Facility::KERN,
            "log_user" | "user" => Facility::USER,
            "log_mail" | "mail" => Facility::MAIL,
            "log_daemon" | "daemon" => Facility::DAEMON,
            "log_auth" | "auth" => Facility::AUTH,
            "log_syslog" | "syslog" => Facility::SYSLOG,
            "log_lpr" | "lpr" => Facility::LPR,
            "log_news" | "news" => Facility::NEWS,
            "log_uucp" | "uucp" => Facility::UUCP,
            "log_cron" | "cron" => Facility::CRON,
            "log_authpriv" | "authpriv" => Facility::AUTHPRIV,
            "log_ftp" | "ftp" => Facility::FTP,
            "log_local0" | "local0" => Facility::LOCAL0,
            "log_local1" | "local1" => Facility::LOCAL1,
            "log_local2" | "local2" => Facility::LOCAL2,
            "log_local3" | "local3" => Facility::LOCAL3,
            "log_local4" | "local4" => Facility::LOCAL4,
            "log_local5" | "local5" => Facility::LOCAL5,
            "log_local6" | "local6" => Facility::LOCAL6,
            "log_local7" | "local7" => Facility::LOCAL7,
            _ => return Err(()),
        })
    }
}

/// A syslog severity level.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Level {
    Emerg,
    Alert,
    Crit,
    Err,
    Warning,
    Notice,
    Info,
    Debug,
}

impl Level {
    fn code(self) -> c_int {
        match self {
            Level::Emerg => libc::LOG_EMERG,
            Level::Alert => libc::LOG_ALERT,
            Level::Crit => libc::LOG_CRIT,
            Level::Err => libc::LOG_ERR,
            Level::Warning => libc::LOG_WARNING,
            Level::Notice => libc::LOG_NOTICE,
            Level::Info => libc::LOG_INFO,
            Level::Debug => libc::LOG_DEBUG,
        }
    }
}

impl FromStr for Level {
    type Err = ();

    /// Accepts the POSIX level names plus the `panic`/`error`/`warn` aliases
    /// that historical `logger` implementations take.
    fn from_str(s: &str) -> Result<Level, ()> {
        Ok(match s.to_ascii_lowercase().as_str() {
            "emerg" | "panic" => Level::Emerg,
            "alert" => Level::Alert,
            "crit" => Level::Crit,
            "err" | "error" => Level::Err,
            "warning" | "warn" => Level::Warning,
            "notice" => Level::Notice,
            "info" => Level::Info,
            "debug" => Level::Debug,
            _ => return Err(()),
        })
    }
}

/// Keeps the `openlog` identity string alive.
///
/// `openlog` stores the pointer it is given rather than copying the string, so
/// the allocation has to outlive every later `syslog` call. Holding it here
/// also makes repeat calls to [`open`] a no-op, which is what callers that log
/// from several places want.
///
/// Because the first identity is the one that sticks, there is deliberately no
/// `closelog` wrapper: `closelog` clears libc's tag but could not clear this
/// cell, so a later `open` would return early and every subsequent message
/// would silently lose its tag and facility for the life of the process.
/// Process exit closes the log anyway.
static IDENT: OnceLock<CString> = OnceLock::new();

/// Establish the log identity. The first call wins; later ones do nothing.
///
/// `log_pid` selects `LOG_PID`, which stamps each message with the caller's
/// process ID.
pub fn open(ident: &str, log_pid: bool, facility: Facility) {
    // An interior NUL would truncate the tag, so drop any before converting.
    let ident = CString::new(ident.replace('\0', "")).unwrap_or_default();
    let ident = match IDENT.set(ident) {
        Ok(()) => IDENT.get().expect("just set"),
        // Another caller already opened the log; keep its identity.
        Err(_) => return,
    };

    let option = if log_pid { libc::LOG_PID } else { 0 };
    // SAFETY: `ident` lives in IDENT for the rest of the process, which is what
    // openlog requires of the pointer it retains.
    unsafe { libc::openlog(ident.as_ptr(), option, facility.0) };
}

/// Write one message to the system log.
///
/// Interior NUL bytes are dropped, since they would otherwise truncate the
/// message. The message is passed through a `%s` format so that any `%` in it
/// is taken literally.
pub fn log(level: Level, msg: &str) {
    let Ok(msg) = CString::new(msg.replace('\0', "")) else {
        return;
    };
    // SAFETY: both pointers are valid NUL-terminated C strings for the call,
    // and the format string consumes exactly the one argument supplied.
    unsafe { libc::syslog(level.code(), c"%s".as_ptr(), msg.as_ptr()) };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn facility_names_parse_both_spellings() {
        assert_eq!("user".parse(), Ok(Facility::USER));
        assert_eq!("LOG_USER".parse(), Ok(Facility::USER));
        assert_eq!("Daemon".parse(), Ok(Facility::DAEMON));
        assert_eq!("local7".parse(), Ok(Facility::LOCAL7));
        assert_eq!("nosuchfac".parse::<Facility>(), Err(()));
    }

    #[test]
    fn facility_codes_match_libc() {
        // The codes are what reaches openlog, so pin them to libc's values.
        assert_eq!(Facility::USER, Facility(libc::LOG_USER));
        assert_eq!(Facility::default(), Facility::USER);
    }

    #[test]
    fn level_names_parse_with_aliases() {
        assert_eq!("err".parse(), Ok(Level::Err));
        assert_eq!("ERROR".parse(), Ok(Level::Err));
        assert_eq!("panic".parse(), Ok(Level::Emerg));
        assert_eq!("warn".parse(), Ok(Level::Warning));
        assert_eq!("nosuchlevel".parse::<Level>(), Err(()));
    }

    #[test]
    fn level_codes_are_ordered_by_severity() {
        assert_eq!(Level::Emerg.code(), libc::LOG_EMERG);
        assert_eq!(Level::Debug.code(), libc::LOG_DEBUG);
        assert!(Level::Emerg.code() < Level::Debug.code());
    }
}
