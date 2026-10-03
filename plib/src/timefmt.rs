//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `strftime(3)` on Windows.
//!
//! The C runtime converts a time to its broken-down local form and names its
//! zone, honouring `TZ` in the forms it understands (`UTC0`, `EST5EDT`; no
//! zoneinfo names) or the system time zone when `TZ` is unset. Every other
//! conversion is done here, in the POSIX
//! locale's names (`LC_TIME` has no Windows meaning): the CRT's own
//! `strftime` lacks conversions in older runtimes and aborts the process on an
//! unknown one.
//!
//! Supported: the POSIX conversions `aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%`.
//! An `E` or `O` modifier is ignored, as the POSIX locale defines no
//! alternative forms. Any other conversion, including flags and field widths,
//! is copied to the output as written, as glibc does.

use chrono::format::{Item, StrftimeItems};
use chrono::{FixedOffset, NaiveDate, NaiveDateTime, TimeZone};
use std::io;

/// A broken-down time, its zone abbreviation and its offset east of UTC.
struct BrokenDown {
    tm: libc::tm,
    zone: String,
    gmtoff: i32,
}

fn invalid(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, msg.to_string())
}

/// `epoch` in UTC: zone `GMT`, as glibc's `gmtime_r` names it.
fn universal_time(epoch: libc::time_t) -> io::Result<BrokenDown> {
    // SAFETY: tm is plain data; gmtime_s fills it or reports failure.
    let mut tm: libc::tm = unsafe { std::mem::zeroed() };
    if unsafe { libc::gmtime_s(&mut tm, &epoch) } != 0 {
        return Err(invalid("timestamp out of range"));
    }
    Ok(BrokenDown {
        tm,
        zone: String::from("GMT"),
        gmtoff: 0,
    })
}

// The C runtime's strftime, which the libc crate does not bind on Windows.
// Called only with "%Z", which every runtime supports.
extern "C" {
    fn strftime(
        buf: *mut libc::c_char,
        size: libc::size_t,
        format: *const libc::c_char,
        tm: *const libc::tm,
    ) -> libc::size_t;
}

/// The runtime's name for the zone `tm` is in (`tm_isdst` selects standard or
/// daylight time).
fn zone_name(tm: &libc::tm) -> String {
    let mut name = [0u8; 128];
    // SAFETY: the buffer's size is passed with it; the format is a NUL-
    // terminated literal and tm a valid broken-down time.
    let len = unsafe {
        strftime(
            name.as_mut_ptr() as *mut libc::c_char,
            name.len(),
            c"%Z".as_ptr(),
            tm,
        )
    };
    String::from_utf8_lossy(&name[..len]).into_owned()
}

/// The date and time `tm`'s fields name, without a zone.
fn naive_fields(tm: &libc::tm) -> Option<NaiveDateTime> {
    let date = NaiveDate::from_ymd_opt(
        tm.tm_year + 1900,
        u32::try_from(tm.tm_mon + 1).ok()?,
        u32::try_from(tm.tm_mday).ok()?,
    )?;
    date.and_hms_opt(
        u32::try_from(tm.tm_hour).ok()?,
        u32::try_from(tm.tm_min).ok()?,
        u32::try_from(tm.tm_sec).ok()?,
    )
}

/// `epoch` in the local time zone, which `TZ` selects.
fn local_time(epoch: libc::time_t) -> io::Result<BrokenDown> {
    // SAFETY: tzset only reads TZ; tm is plain data that localtime_s fills or
    // reports failure.
    let mut tm: libc::tm = unsafe { std::mem::zeroed() };
    if unsafe {
        libc::tzset();
        libc::localtime_s(&mut tm, &epoch)
    } != 0
    {
        return Err(invalid("timestamp out of range"));
    }
    // The offset is how far the local fields run ahead of the instant.
    let gmtoff = naive_fields(&tm)
        .and_then(|local| i32::try_from(local.and_utc().timestamp() - epoch).ok())
        .ok_or_else(|| invalid("timestamp out of range"))?;
    Ok(BrokenDown {
        zone: zone_name(&tm),
        tm,
        gmtoff,
    })
}

/// Rewrite a POSIX format into chrono's: `%Z` becomes the zone name, an `E` or
/// `O` modifier is dropped, and an unsupported conversion becomes literal text.
fn chrono_format(fmt: &str, zone: &str) -> String {
    const SUPPORTED: &str = "aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYz%";
    let mut out = String::with_capacity(fmt.len());
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        if let Some('E' | 'O') = chars.peek() {
            let modifier = chars.next().unwrap();
            match chars.peek() {
                Some(&next) if next == 'Z' || SUPPORTED.contains(next) => {}
                _ => {
                    out.push_str("%%");
                    out.push(modifier);
                    continue;
                }
            }
        }
        match chars.next() {
            Some('Z') => out.push_str(&zone.replace('%', "%%")),
            Some(conv) if SUPPORTED.contains(conv) => {
                out.push('%');
                out.push(conv);
            }
            Some(other) => {
                out.push_str("%%");
                out.push(other);
            }
            None => out.push_str("%%"),
        }
    }
    out
}

/// Format a broken-down time.
fn format_broken_down(fmt: &str, t: &BrokenDown) -> io::Result<String> {
    let naive = naive_fields(&t.tm).ok_or_else(|| invalid("time out of range"))?;
    let offset = FixedOffset::east_opt(t.gmtoff).ok_or_else(|| invalid("bad zone offset"))?;
    let datetime = offset
        .from_local_datetime(&naive)
        .single()
        .ok_or_else(|| invalid("time out of range"))?;

    let format = chrono_format(fmt, &t.zone);
    let items: Vec<Item> = StrftimeItems::new(&format).collect();
    if items.iter().any(|item| matches!(item, Item::Error)) {
        return Err(invalid("invalid time format"));
    }
    Ok(datetime.format_with_items(items.into_iter()).to_string())
}

/// Format `epoch` (seconds since the Epoch) by the strftime conversion string
/// `fmt`, in local time or, when `utc` is set, in UTC.
pub fn format_time(fmt: &str, epoch: i64, utc: bool) -> io::Result<String> {
    let epoch: libc::time_t = epoch;
    let t = if utc {
        universal_time(epoch)?
    } else {
        local_time(epoch)?
    };
    format_broken_down(fmt, &t)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// 2026-01-04 05:06:07 UTC, a Sunday.
    const EPOCH: i64 = 1_767_503_167;

    fn utc(fmt: &str) -> String {
        format_time(fmt, EPOCH, true).unwrap()
    }

    #[test]
    fn every_posix_conversion_in_the_c_locale() {
        let cases = [
            ("%a", "Sun"),
            ("%A", "Sunday"),
            ("%b", "Jan"),
            ("%B", "January"),
            ("%c", "Sun Jan  4 05:06:07 2026"),
            ("%C", "20"),
            ("%d", "04"),
            ("%D", "01/04/26"),
            ("%e", " 4"),
            ("%F", "2026-01-04"),
            ("%g", "26"),
            ("%G", "2026"),
            ("%h", "Jan"),
            ("%H", "05"),
            ("%I", "05"),
            ("%j", "004"),
            ("%m", "01"),
            ("%M", "06"),
            ("%n", "\n"),
            ("%p", "AM"),
            ("%r", "05:06:07 AM"),
            ("%R", "05:06"),
            ("%S", "07"),
            ("%t", "\t"),
            ("%T", "05:06:07"),
            ("%u", "7"),
            ("%U", "01"),
            ("%V", "01"),
            ("%w", "0"),
            ("%W", "00"),
            ("%x", "01/04/26"),
            ("%X", "05:06:07"),
            ("%y", "26"),
            ("%Y", "2026"),
            ("%z", "+0000"),
            ("%Z", "GMT"),
            ("%%", "%"),
        ];
        for (fmt, want) in cases {
            assert_eq!(utc(fmt), want, "conversion {fmt}");
        }
    }

    #[test]
    fn iso_week_year_differs_from_the_calendar_year() {
        // 2027-01-01 is a Friday: ISO week 53 of 2026.
        let new_year_2027 = 1_798_761_600;
        assert_eq!(
            format_time("%G-W%V %g %U %W", new_year_2027, true).unwrap(),
            "2026-W53 26 00 00"
        );
    }

    #[test]
    fn modifiers_are_ignored_and_unknown_conversions_are_literal() {
        assert_eq!(utc("%Ey %OH %EZ"), "26 05 GMT");
        assert_eq!(utc("%q %-d %Ek"), "%q %-d %Ek");
        assert_eq!(utc("100%"), "100%");
        assert_eq!(utc("literal text"), "literal text");
        assert_eq!(utc(""), "");
    }

    #[test]
    fn zone_name_and_offset_are_used_as_given() {
        let mut t = universal_time(EPOCH).unwrap();
        t.zone = String::from("A%Z");
        t.gmtoff = -(4 * 3600 + 30 * 60);
        assert_eq!(format_broken_down("%Z %z", &t).unwrap(), "A%Z -0430");
    }

    #[test]
    fn local_time_formats() {
        // Whatever the zone, local time formats and names a zone.
        let s = format_time("%Y %z", EPOCH, false).unwrap();
        assert!(s.starts_with("2026 ") || s.starts_with("2025 "), "{s}");
    }
}
